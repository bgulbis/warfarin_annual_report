# tidy data

library(tidyverse)
library(lubridate)
library(stringr)
library(edwr)

dir_raw <- "data/raw"
end_date <- "06/30/2017"

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
    mutate(traditional = traditional - pharmacy) %>%
    filter(action.date >= mdy("7/1/2014"),
           action.date <= mdy(end_date))

dirr::save_rds("dir/tidy", "data_")
