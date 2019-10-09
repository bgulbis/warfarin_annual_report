# tidy data

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/fy2018"
end_date <- "06/30/2018"

# source("src/helper_functions.R")

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
    summarize_at(
        "med.datetime",
        funs(
            warfarin_start = first,
            warfarin_stop = last
        )
    )

location_start <- meds_warfarin %>%
    group_by(millennium.id) %>%
    summarize_at(
        "med.location",
        funs(
            location_start = first,
            location_last = last
        )
    )

data_doses_location <- meds_warfarin %>%
    filter(
        med.datetime >= mdy("7/1/2017"),
        med.datetime <= mdy("6/30/2018")
    ) %>%
    count(med.location)

warfarin_doses <- meds_warfarin %>%
    mutate_at("med.datetime", floor_date, unit = "day") %>%
    group_by(millennium.id, med.datetime) %>%
    summarize_at("med.dose", sum) %>%
    mutate(
        warfarin_day = difftime(med.datetime, first(med.datetime), units = "days") + 1
    )

# orders -----------------------------------------------

# get all warfarin and consult orders
data_order_actions <- read_data(dir_raw, "order-actions", FALSE) %>%
    as.order_action() %>%
    filter(
        !is.na(order),
        action.type == "Order" | action.type == "Complete",
        action.provider.role != "HIN Team Licensed",
        action.provider.role != "DBA"
    ) %>%
    mutate(
        action.date = floor_date(action.datetime, unit = "day"),
        consult = str_detect(order, "Dosing")
    )

patient_groups <- data_order_actions %>%
    group_by(millennium.id) %>%
    summarize_at("consult", sum) %>%
    mutate_at(
        "consult",
        funs(
            if_else(. > 1, "pharmacy", "traditional")
        )
    ) %>%
    rename(group = consult)

# time series ------------------------------------------
# data_timeseries <- data_order_actions %>%
#     distinct(millennium.id, action.date, consult) %>%
#     group_by(millennium.id, action.date) %>%
#     mutate(value = TRUE,
#            consult = if_else(consult, "pharmacy", "traditional")) %>%
#     spread(consult, value) %>%
#     dmap_at("traditional", ~ coalesce(.x, TRUE)) %>%
#     ungroup() %>%
#     distinct() %>%
#     group_by(action.date) %>%
#     summarize_at(c("pharmacy", "traditional"), sum, na.rm = TRUE) %>%
#     mutate(traditional = traditional - pharmacy,
#            month = floor_date(action.date, unit = "month"),
#            fy_month = month + months(6),
#            fy = year(fy_month)) %>%
#     filter(action.date >= mdy("7/1/2014"),
#            action.date <= mdy(end_date))

data_timeseries_location <- data_order_actions %>%
    distinct(millennium.id, action.date, consult, order.location) %>%
    group_by(millennium.id, action.date) %>%
    mutate(
        value = TRUE,
        consult = if_else(consult, "pharmacy", "traditional")
    ) %>%
    spread(consult, value) %>%
    mutate_at("traditional", funs(coalesce(., TRUE))) %>%
    ungroup() %>%
    distinct() %>%
    group_by(action.date, order.location) %>%
    summarize_at(c("pharmacy", "traditional"), sum, na.rm = TRUE) %>%
    mutate(
        traditional = traditional - pharmacy,
        month = floor_date(action.date, unit = "month"),
        week = floor_date(action.date, unit = "week"),
        fy_month = month + months(6),
        fy = year(fy_month)
    ) %>%
    filter(
        action.date >= mdy("7/1/2014"),
        action.date <= mdy(end_date)
    )

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
    summarize_at(
        "lab.result",
        funs(
            inr_admit = first,
            inr_baseline = last
        )
    )

# keep last INR prior to 1400 each day
inr_daily <- labs_inr %>%
    mutate_at(
        c("warfarin_start", "warfarin_stop"),
        floor_date,
        unit = "day"
    ) %>%
    mutate(
        lab_date = floor_date(lab.datetime, unit = "day"),
        warfarin_day = difftime(lab_date, warfarin_start, units = "days") + 1
    ) %>%
    filter(
        lab_date >= warfarin_start, # - days(1)
        lab_date <= warfarin_stop, # + days(1)
        hour(lab.datetime) <= 14
    ) %>%
    group_by(millennium.id, lab_date) %>%
    arrange(millennium.id, warfarin_day, desc(lab.datetime)) %>%
    distinct(millennium.id, warfarin_day, .keep_all = TRUE) %>%
    group_by(millennium.id) %>%
    select(
        millennium.id,
        inr = lab.result,
        warfarin_day,
        lab_date
    ) # censor.low, censor.high,

data_daily <- warfarin_doses %>%
    full_join(
        inr_daily,
        by = c("millennium.id", "warfarin_day", "med.datetime" = "lab_date")
    ) %>%
    arrange(millennium.id, warfarin_day) %>%
    mutate_at("med.dose", funs(coalesce(., 0)))

labs_hgb <- raw_labs %>%
    filter(lab == "hgb") %>%
    left_join(warfarin_dates, by = "millennium.id") %>%
    arrange(millennium.id, lab.datetime) %>%
    filter(
        lab.datetime >= warfarin_start,
        lab.datetime <= warfarin_stop + days(2),
        !is.na(lab.result)
    )

data_hgb_drop <- labs_hgb %>%
    group_by(millennium.id, lab) %>%
    arrange(lab.datetime, .by_group = TRUE) %>%
    lab_change("hgb", -2, max) %>%
    mutate(
        hgb_day = difftime(
            floor_date(lab.datetime, "day"),
            floor_date(warfarin_start, "day"),
            "days"
        )
    ) %>%
    mutate_at("hgb_day", as.numeric) %>%
    mutate_at("hgb_day", round, digits = 0) %>%
    arrange(millennium.id, hgb_day) %>%
    distinct(millennium.id, hgb_day, change)

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
    arrange(millennium.id, desc(warfarin.datetime)) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(-warfarin.datetime) %>%
    rowwise() %>%
    mutate(
        indication_group = if_else(
            sum(dvt, pe, thrombus) >= 1,
            "vte",
            if_else(
                sum(afib, stroke, valve) >= 1,
                "cva_prevent",
                "other"
            )
        )
    )

warfarin_ranges <- raw_warfarin %>%
    make_inr_ranges() %>%
    filter(
        !is.na(goal.low),
        !is.na(goal.high)
    ) %>%
    group_by(millennium.id) %>%
    arrange(warfarin.datetime, .by_group = TRUE) %>%
    summarize_at(c("goal.low", "goal.high"), last)

# time therapeutic -------------------------------------

time_tx <- raw_labs %>%
    left_join(warfarin_dates, by = "millennium.id") %>%
    filter(
        lab == "inr",
        lab.datetime >= warfarin_start,
        lab.datetime <= warfarin_stop + days(1)
    ) %>%
    calc_runtime() %>%
    left_join(warfarin_ranges, by = "millennium.id") %>%
    calc_perctime("lab.result >= goal.low; lab.result <= goal.high")

data_warfarin <- patient_groups %>%
    left_join(warfarin_dates, by = "millennium.id") %>%
    left_join(warfarin_initiation, by = "millennium.id") %>%
    left_join(warfarin_ranges, by = "millennium.id") %>%
    left_join(time_tx[c("millennium.id", "perc.time")], by = "millennium.id") %>%
    left_join(warfarin_indications, by = "millennium.id")

# readmission ------------------------------------------

raw_identifiers <- read_data(dir_raw, "identifiers") %>%
    as.id()

index_visits <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    left_join(
        raw_identifiers[c("millennium.id", "person.id")],
        by = "millennium.id"
    )

unique_pts <- distinct(index_visits, person.id)

readmits <- c(
    "Inpatient",
    "Emergency",
    "Observation",
    "72 Hour ER",
    "EC Fast ER Care"
)

dispos <- c("Home or Self Care", "Home Care with Home Health")

data_revisits <- read_data(dir_raw, "encounters") %>%
    as.encounters() %>%
    semi_join(unique_pts, by = "person.id") %>%
    left_join(
        index_visits[c("millennium.id", "discharge.datetime")],
        by = "millennium.id"
    ) %>%
    filter(visit.type %in% readmits | !is.na(discharge.datetime)) %>%
    group_by(person.id) %>%
    arrange(admit.datetime, .by_group = TRUE) %>%
    mutate(
        next_encounter = difftime(
            discharge.datetime,
            lag(admit.datetime),
            units = "days"
        ),
        revisit_millennium_id = lag(millennium.id),
        revisit_facility = lag(facility),
        revisit_visit_type = lag(visit.type)
    ) %>%
    filter(
        disposition %in% dispos,
        next_encounter <= 30
    ) %>%
    left_join(patient_groups, by = "millennium.id") %>%
    left_join(warfarin_dates, by = "millennium.id")

# procedures -------------------------------------------

data_procedures <- read_data(dir_raw, "procedures", FALSE) %>%
    as.procedures() %>%
    left_join(warfarin_dates, by = "millennium.id") %>%
    filter(
        proc.date >= warfarin_start + days(1),
        proc.date <= warfarin_stop + days(1)
    ) %>%
    mutate(
        warf_day = difftime(
            proc.date,
            floor_date(warfarin_start, "day"),
            "days"
        )
    ) %>%
    mutate_at("warf_day", as.numeric) %>%
    mutate_at("warf_day", round, digits = 0) %>%
    arrange(millennium.id, warf_day) %>%
    distinct(millennium.id, warf_day)

# blood produts ----------------------------------------

raw_blood <- read_data(dir_raw, "blood", FALSE) %>%
    as.blood()

data_prbc <- raw_blood %>%
    left_join(warfarin_dates, by = "millennium.id") %>%
    filter(
        blood.prod == "prbc",
        blood.datetime >= warfarin_start + days(1),
        blood.datetime <= warfarin_stop + days(1)
    ) %>%
    mutate(
        prbc_day = difftime(
            floor_date(blood.datetime, "day"),
            floor_date(warfarin_start, "day"),
            "days"
        )
    ) %>%
    mutate_at("prbc_day", as.numeric) %>%
    mutate_at("prbc_day", round, digits = 0) %>%
    arrange(millennium.id, prbc_day) %>%
    distinct(millennium.id, prbc_day)

data_ffp <- raw_blood %>%
    left_join(warfarin_dates, by = "millennium.id") %>%
    filter(
        blood.prod == "ffp",
        blood.datetime >= warfarin_start + days(1),
        blood.datetime <= warfarin_stop + days(1)
    ) %>%
    mutate(
        ffp_day = difftime(
            floor_date(blood.datetime, "day"),
            floor_date(warfarin_start, "day"),
            "days"
        )
    ) %>%
    mutate_at("ffp_day", as.numeric) %>%
    mutate_at("ffp_day", round, digits = 0) %>%
    arrange(millennium.id, ffp_day) %>%
    distinct(millennium.id, ffp_day)

# reversal ---------------------------------------------

rev_agent <- c(
    "phytonadione",
    "prothrombin complex",
    "coagulation factor VIIa",
    "aminocaproic acid",
    "tranexamic acid"
)

data_reversal <- raw_meds %>%
    filter(med %in% rev_agent) %>%
    left_join(warfarin_dates, by = "millennium.id") %>%
    filter(
        med.datetime >= warfarin_start + days(1),
        med.datetime <= warfarin_stop + days(1)
    ) %>%
    mutate(
        rev_day = difftime(
            floor_date(med.datetime, "day"),
            floor_date(warfarin_start, "day"),
            "days"
        )
    ) %>%
    mutate_at("rev_day", as.numeric) %>%
    mutate_at("rev_day", round, digits = 0) %>%
    arrange(millennium.id, rev_day) %>%
    distinct(millennium.id, rev_day, med)

# save data --------------------------------------------

dir_tidy <- "data/tidy/fy2018"

if (!dir.exists(dir_tidy)) dir.create(dir_tidy)

dirr::save_rds(dir_tidy, "data_")
