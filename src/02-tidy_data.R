# tidy data

library(tidyverse)
library(lubridate)
library(stringr)
library(edwr)

dir_raw <- "data/raw"
end_date <- "06/30/2017"

source("helper_functions.R")

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
    dmap_at("consult", ~ .x > 1)

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

raw_warfarin <- read_data(dir_raw, "^warfarin", FALSE) %>%
    as.warfarin()

# get new / previous data from anticoagulation goals
warfarin_new <- raw_warfarin %>%
    filter(warfarin.event == "warfarin therapy") %>%
    arrange(millennium.id, desc(warfarin.datetime)) %>%
    distinct(millennium.id, .keep_all = TRUE)


warfarin_indications <- raw_warfarin %>%
    filter(warfarin.event == "warfarin indication") %>%
    make_indications() %>%
    rowwise() %>%
    mutate(other = sum(afib, dvt, pe, valve, stroke, vad, thrombus, hypercoag, prophylaxis) == 0) %>%
    arrange(millennium.id, desc(warfarin.datetime)) %>%
    distinct(millennium.id, .keep_all = TRUE)


# save data --------------------------------------------

dirr::save_rds("data/tidy", "data_")
