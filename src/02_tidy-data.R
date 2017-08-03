# tidy data

library(tidyverse)
library(purrrlyr)
library(lubridate)
library(stringr)
library(edwr)

dir_raw <- "data/raw"
end_date <- "06/30/2017"

source("src/helper_functions.R")

# demographics -----------------------------------------

data_demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

# med data ---------------------------------------------

raw_meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

meds_warfarin <- raw_meds %>%
    filter(med == "warfarin") %>%
    arrange(millennium.id, med.datetime)

warfarin_dates <- meds_warfarin %>%
    group_by(millennium.id) %>%
    summarize_at("med.datetime", funs(warfarin_start = first,
                                      warfarin_stop = last))

location_start <- meds_warfarin %>%
    group_by(millennium.id) %>%
    summarize_at("med.location", funs(location_start = first,
                                      location_last = last))

data_doses_location <- meds_warfarin %>%
    filter(med.datetime >= mdy("7/1/2016"),
           med.datetime <= mdy("6/30/2017")) %>%
    count(med.location)

warfarin_doses <- meds_warfarin %>%
    dmap_at("med.datetime", floor_date, unit = "day") %>%
    group_by(millennium.id, med.datetime) %>%
    summarize_at("med.dose", sum) %>%
    mutate(warfarin_day = difftime(med.datetime, first(med.datetime), units = "days") + 1)

# orders -----------------------------------------------

# get all warfarin and consult orders
data_order_actions <- read_data(dir_raw, "order-actions", FALSE) %>%
    as.order_action() %>%
    filter(!is.na(order),
           action.type == "Order" | action.type == "Complete",
           action.provider.role != "HIN Team Licensed",
           action.provider.role != "DBA") %>%
    mutate(action.date = floor_date(action.datetime, unit = "day"),
           consult = str_detect(order, "Dosing"))

patient_groups <- data_order_actions %>%
    group_by(millennium.id) %>%
    summarize_at("consult", sum) %>%
    dmap_at("consult", ~ .x > 1) %>%
    dmap_at("consult", ~ if_else(.x, "pharmacy", "traditional")) %>%
    rename(group = consult)

# time series ------------------------------------------
data_timeseries <- data_order_actions %>%
    distinct(millennium.id, action.date, consult) %>%
    group_by(millennium.id, action.date) %>%
    mutate(value = TRUE,
           consult = if_else(consult, "pharmacy", "traditional")) %>%
    spread(consult, value) %>%
    dmap_at("traditional", ~ coalesce(.x, TRUE)) %>%
    ungroup() %>%
    distinct() %>%
    group_by(action.date) %>%
    summarize_at(c("pharmacy", "traditional"), sum, na.rm = TRUE) %>%
    mutate(traditional = traditional - pharmacy,
           month = floor_date(action.date, unit = "month"),
           fy_month = month + months(6),
           fy = year(fy_month)) %>%
    filter(action.date >= mdy("7/1/2014"),
           action.date <= mdy(end_date))

data_timeseries_location <- data_order_actions %>%
    distinct(millennium.id, action.date, consult, order.location) %>%
    group_by(millennium.id, action.date) %>%
    mutate(value = TRUE,
           consult = if_else(consult, "pharmacy", "traditional")) %>%
    spread(consult, value) %>%
    dmap_at("traditional", ~ coalesce(.x, TRUE)) %>%
    ungroup() %>%
    distinct() %>%
    group_by(action.date, order.location) %>%
    summarize_at(c("pharmacy", "traditional"), sum, na.rm = TRUE) %>%
    mutate(traditional = traditional - pharmacy,
           month = floor_date(action.date, unit = "month"),
           week = floor_date(action.date, unit = "week"),
           fy_month = month + months(6),
           fy = year(fy_month)) %>%
    filter(action.date >= mdy("7/1/2014"),
           action.date <= mdy(end_date))

# data_timeseries %>%
#     filter(month < mdy("4/1/2017")) %>%
#     group_by(fy, month, fy_month) %>%
#     summarize_at(c("pharmacy", "traditional"), sum) %>%
#     gather(service, num, pharmacy:traditional) %>%
#     mutate(month_graph = ymd(paste("2016/", month(fy_month), "/01")),
#            month_graph = month_graph - months(6)) %>%
#     ggplot(aes(x = month_graph, y = num)) +
#     # geom_line(aes(color = factor(fy)), alpha = 0.6) +
#     geom_point(aes(color = factor(fy)), shape = 1) +
#     geom_smooth(aes(color = factor(fy)), se = FALSE) +
#     facet_wrap(~ service) +
#     scale_color_brewer(palette = "Set1") +
#     themebg::theme_bg()

# lab data ---------------------------------------------
raw_labs <- read_data(dir_raw, "labs", FALSE) %>%
    as.labs() %>%
    tidy_data()

labs_inr <- raw_labs %>%
    filter(lab == "inr") %>%
    left_join(warfarin_dates, by = "millennium.id") %>%
    arrange(millennium.id, lab.datetime)

inr_baseline <- labs_inr %>%
    filter(lab.datetime <= warfarin_start) %>%
    group_by(millennium.id) %>%
    summarize_at("lab.result", funs(inr_admit = first, inr_baseline = last))

# keep last INR prior to 1400 each day
inr_daily <- labs_inr %>%
    dmap_at(c("warfarin_start", "warfarin_stop"), floor_date, unit = "day") %>%
    mutate(lab_date = floor_date(lab.datetime, unit = "day"),
           warfarin_day = difftime(lab_date, warfarin_start, units = "days") + 1) %>%
    filter(lab_date >= warfarin_start, # - days(1)
           lab_date <= warfarin_stop, # + days(1)
           hour(lab.datetime) <= 14) %>%
    group_by(millennium.id, lab_date) %>%
    arrange(millennium.id, warfarin_day, desc(lab.datetime)) %>%
    distinct(millennium.id, warfarin_day, .keep_all = TRUE) %>%
    group_by(millennium.id) %>%
    select(millennium.id, inr = lab.result, warfarin_day, lab_date) # censor.low, censor.high,

data_daily <- full_join(warfarin_doses, inr_daily, by = c("millennium.id", "warfarin_day", "med.datetime" = "lab_date")) %>%
    arrange(millennium.id, warfarin_day) %>%
    dmap_at("med.dose", ~ coalesce(.x, 0))

# warfarin information ---------------------------------
raw_warfarin <- read_data(dir_raw, "warfarin-info", FALSE) %>%
    as.warfarin()

warfarin_initiation <- raw_warfarin %>%
    filter(warfarin.event == "warfarin therapy") %>%
    arrange(millennium.id, desc(warfarin.datetime)) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, initiation = warfarin.result)

warfarin_indications <- raw_warfarin %>%
    filter(warfarin.event == "warfarin indication") %>%
    make_indications() %>%
    # rowwise() %>%
    # mutate(other = sum(afib, dvt, pe, valve, stroke, vad, thrombus, hypercoag, prophylaxis) == 0) %>%
    arrange(millennium.id, desc(warfarin.datetime)) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(-warfarin.datetime) %>%
    rowwise() %>%
    mutate(indication_group = if_else(sum(dvt, pe, thrombus) >= 1, "vte",
                                    if_else(sum(afib, stroke, valve) >= 1, "cva_prevent", "other")))

data_warfarin <- patient_groups %>%
    left_join(warfarin_dates, by = "millennium.id") %>%
    left_join(warfarin_initiation, by = "millennium.id") %>%
    left_join(warfarin_indications, by = "millennium.id")

# save data --------------------------------------------

dirr::save_rds("data/tidy", "data_")
