library(data.table)
library(tidyverse)
library(lubridate)
library(mbohelpr)
library(themebg)
library(openxlsx)

data_dir <- "data/tidy/fy2019"

find_ind <- function(x, reg) {
    stringr::str_detect(x, regex(reg, ignore_case = TRUE))
}

data_consult_orders <- get_data(data_dir, "consult_orders") %>%
    filter(str_detect(nurse_unit, "^HH|^HVI")) %>%
    mutate(
        order_month = floor_date(order_datetime, unit = "month"),
        fiscal_year = year(order_month %m+% months(6))
    )

data_consult_tasks <- get_data(data_dir, "consult_tasks") %>%
    filter(
        str_detect(nurse_unit, "^HH|^HVI"),
        task_date < ymd("2019-07-01")
    ) %>%
    mutate(
        task_month = floor_date(task_date, unit = "month"),
        fiscal_year = year(task_month %m+% months(6))
    )

data_demographics <- get_data(data_dir, "demographics")

data_doac_doses <- get_data(data_dir, "doac_doses") %>%
    filter(str_detect(nurse_unit, "^HH|^HVI")) %>%
    mutate(
        med_month = floor_date(med_datetime, unit = "month"),
        fiscal_year = year(med_month %m+% months(6))
    )

data_labs <- get_data(data_dir, "labs")
data_measures <- get_data(data_dir, "measures")
data_reencounters <- get_data(data_dir, "reencounters")
data_reversal_meds <- get_data(data_dir, "reversal_meds")
data_warfarin_details <- get_data(data_dir, "warfarin_details")

data_warfarin_doses <- get_data(data_dir, "warfarin_doses") %>%
    filter(str_detect(nurse_unit, "^HH|^HVI")) %>%
    mutate(
        med_day = floor_date(med_datetime, unit = "day"),
        med_month = floor_date(med_datetime, unit = "month"),
        fiscal_year = year(med_month %m+% months(6))
    )

data_warfarin_home_meds <- get_data(data_dir, "warfarin_home_meds")
data_warfarin_orders <- get_data(data_dir, "warfarin_orders")

# indications ------------------------------------------
df_indications <- data_warfarin_details %>%
    filter(detail == "Warfarin Indication") %>%
    mutate(
        indication = case_when(
            find_ind(
                result_value,
                "Atrial fibrillation|a(.*)?fib|a(.*)?flutter"
            ) ~ "afib",
            find_ind(
                result_value,
                "Deep vein thrombosis|DVT(?!( prophylaxis))|VTE"
            ) ~ "dvt",
            find_ind(
                result_value,
                "Pulmonary embolism|PE"
            ) ~ "pe",
            find_ind(
                result_value,
                "Heart valve \\(Mech/porc/bioprost\\)|valve|avr|mvr"
            ) ~ "valve",
            find_ind(
                result_value,
                "st(ro|or)ke|cva|ica|mca"
            ) ~ "stroke",
            find_ind(
                result_value,
                "vad|hm[ ]?ii|heart( )?mate|heartware|syncardia|total artificial heart|tah"
            ) ~ "vad",
            find_ind(
                result_value,
                "throm|clot|emboli|occl"
            ) ~ "thrombus",
            find_ind(
                result_value,
                "malig|anti(.)?phos|lupus|apla|hypercoag|deficien|leiden|fvl|factor v"
            ) ~ "hypercoag",
            find_ind(
                result_value,
                "prophylax"
            ) ~ "prophylax",
            TRUE ~ "other"
        )
    ) %>%
    distinct(encounter_id, indication)
# mutate(value = TRUE) %>%
# spread(indication, value, fill = FALSE)

# inr ranges -------------------------------------------
df_inr_goal <- data_warfarin_details %>%
    filter(
        detail == "INR Range",
        result_value != ""
    ) %>%
    mutate(inr_range = result_value) %>%
    mutate_at(
        "inr_range",
        str_replace_all,
        pattern = " |",
        replacement = ""
    ) %>%
    mutate_at(
        "inr_range",
        str_replace_all,
        pattern = regex("to|--|/|=|,|-\\.", ignore_case = TRUE),
        replacement = "-"
    ) %>%
    mutate_at(
        "inr_range",
        str_replace_all,
        pattern = ">1.5-<2.5",
        replacement = "1.5-2.5"
    ) %>%
    mutate_at(
        "inr_range",
        str_replace_all,
        pattern = regex(
            c(
                # "^1.[5-9]$" = "1.5-2",
                "^2$|^2.0$" = "1.5-2.5",
                "^2\\.[1-4]$" = "2-2.5",
                "^2.5$" = "2-3",
                # "^2.[6-9]$" = "2.5-3",
                "^3$" = "2.5-3.5",
                # "^3.5$" = "3-4",
                # "^22" = "2",
                # "2.53.5" = "2.5-3.5",
                "^23$" = "2-3",
                "30$" = "3",
                "^1-5" = "1.5"
            ),
            ignore_case = TRUE
        )
    ) %>%
    extract(
        inr_range,
        c("goal_low", "goal_high"),
        regex = "([0-9\\.]+ ?)-( ?[0-9\\.]+)",
        remove = FALSE,
        convert = TRUE
    ) %>%
    filter(
        !is.na(goal_low),
        goal_low < goal_high
    ) %>%
    mutate_at(c("goal_low", "goal_high"), as.numeric) %>%
    select(-result_value, -inr_range)

# monthly doses ----------------------------------------
df_warf_month <- data_warfarin_doses %>%
    count(med_month, name = "warfarin") %>%
    mutate_at("med_month", as_date)

df_consult_month <- data_consult_tasks %>%
    count(task_month, name = "consults")

df_doac_month <- data_doac_doses %>%
    mutate(med_day = floor_date(med_datetime, unit="day")) %>%
    distinct(encounter_id, medication, med_month, med_day) %>%
    count(med_month, name = "doac") %>%
    mutate_at("med_month", as_date)

xl_monthly_doses <- df_warf_month %>%
    inner_join(df_consult_month, by = c("med_month" = "task_month")) %>%
    inner_join(df_doac_month, by = "med_month")

# inr response -----------------------------------------

df_warf_duration <- data_warfarin_doses %>%
    group_by(encounter_id) %>%
    summarize_at("med_day", list(warfarin_start = first, warfarin_stop = last))

df_doses_daily <- data_warfarin_doses %>%
    group_by(encounter_id, fiscal_year, med_day) %>%
    summarize_at("dose", sum, na.rm = TRUE) %>%
    group_by(encounter_id, fiscal_year) %>%
    mutate(warf_day = difftime(med_day, first(med_day), units = "days") + 1) %>%
    mutate_at("warf_day", as.integer) %>%
    filter(warf_day <= 10) %>%
    ungroup() %>%
    select(encounter_id, fiscal_year, warf_day, dose)

df_inr_daily <- df_warf_duration %>%
    inner_join(data_labs, by = "encounter_id") %>%
    filter(lab == "INR") %>%
    mutate(
        lab_day = floor_date(lab_datetime, unit = "days"),
        lab_month = floor_date(lab_datetime, unit = "month"),
        warf_day = difftime(lab_day, warfarin_start, units = "days") + 1,
        fiscal_year = year(lab_month %m+% months(6))
    ) %>%
    mutate_at("warf_day", as.integer) %>%
    filter(
        lab_day >= warfarin_start,
        lab_day <= warfarin_stop,
        warf_day <= 10
    ) %>%
    group_by(encounter_id, fiscal_year, lab_day, warf_day) %>%
    summarize_at("result_value", max, na.rm = TRUE) %>%
    ungroup() %>%
    select(encounter_id, fiscal_year, warf_day, inr = result_value)

df_consult_daily <- df_warf_duration %>%
    mutate_at(c("warfarin_start", "warfarin_stop"), as_date) %>%
    inner_join(data_consult_tasks, by = "encounter_id") %>%
    mutate(
        warf_day = difftime(task_date, warfarin_start, units = "days") + 1,
        task_month = floor_date(task_date, unit = "month"),
        fiscal_year = year(task_month %m+% months(6))
    ) %>%
    mutate_at("warf_day", as.integer) %>%
    filter(
        task_date >= warfarin_start,
        task_date <= warfarin_stop,
        warf_day <= 10
    ) %>%
    select(encounter_id, fiscal_year, warf_day) %>%
    mutate(consult = TRUE)

df_doses_inr <- df_doses_daily %>%
    full_join(
        df_inr_daily,
        by = c("encounter_id", "fiscal_year", "warf_day")
    ) %>%
    full_join(
        df_consult_daily,
        by = c("encounter_id", "fiscal_year", "warf_day")
    ) %>%
    arrange(encounter_id, warf_day)

df_goal_eq2_3 <- df_inr_goal %>%
    filter(
        goal_low == 2,
        goal_high == 3
    )

xl_doses_daily <- df_doses_inr %>%
    semi_join(df_goal_eq2_3, by = "encounter_id") %>%
    filter(fiscal_year == 2019) %>%
    mutate_at(
        "dose",
        list(~coalesce(., 0))
    ) %>%
    mutate(
        pharmacy = if_else(consult, dose, NA_real_),
        traditional = if_else(is.na(consult), dose, NA_real_)
    ) %>%
    select(warf_day, pharmacy, traditional)

# xl_inr_daily <- df_doses_inr %>%
#     semi_join(df_goal_eq2_3, by = "encounter_id") %>%
#     filter(
#         fiscal_year == 2019,
#         !is.na(inr)
#     ) %>%
#     mutate(
#         pharmacy = if_else(consult, inr, NA_real_),
#         traditional = if_else(is.na(consult), inr, NA_real_)
#     ) %>%
#     select(warf_day, pharmacy, traditional)

g <- df_doses_inr %>%
    semi_join(df_goal_eq2_3, by = "encounter_id") %>%
    filter(fiscal_year == 2019) %>%
    ggplot(aes(x = warf_day, y = inr, color = consult)) +
    geom_smooth()

xl_inr_daily <- layer_data(g) %>%
    select(group, x, y, ymin, ymax) %>%
    mutate_at("group", list(~if_else(. == 1, "Pharmacy", "Traditional"))) %>%
    pivot_wider(names_from = group, values_from = c(y, ymin, ymax))

# primary service --------------------------------------

df_warf_start <- data_warfarin_doses %>%
    arrange(encounter_id, med_datetime) %>%
    distinct(encounter_id, .keep_all = TRUE) %>%
    mutate(warfarin_start = floor_date(med_datetime, unit = "day")) %>%
    select(
        encounter_id,
        med_month,
        warfarin_start,
        nurse_unit_warfarin = nurse_unit,
        fiscal_year
    )

df_consult_pts <- data_consult_orders %>%
    arrange(encounter_id, order_datetime) %>%
    distinct(encounter_id, .keep_all = TRUE) %>%
    mutate(
        consult = TRUE,
        consult_date = floor_date(order_datetime, unit = "day")
    ) %>%
    select(
        encounter_id,
        consult_date,
        nurse_unit_consult = nurse_unit,
        med_service,
        consult
    )

df_warf_ord <- data_warfarin_orders %>%
    rename(
        nurse_unit_order = nurse_unit,
        med_service_order = med_service
    ) %>%
    arrange(encounter_id, order_datetime) %>%
    mutate(order_date = floor_date(order_datetime, unit = "day")) %>%
    inner_join(df_warf_start, by = c("encounter_id", "order_date" = "warfarin_start")) %>%
    left_join(df_consult_pts, by = "encounter_id") %>%
    mutate_at("consult", list(~coalesce(., FALSE)))

# medical service --------------------------------------
df_warf_ord %>%
    filter(fiscal_year == 2019) %>%
    mutate_at("med_service_order", fct_infreq) %>%
    mutate_at("med_service_order", fct_lump, n = 10) %>%
    mutate_at("med_service_order", fct_rev) %>%
    ggplot(aes(x = med_service_order, fill = consult)) +
    geom_bar(position = "dodge") +
    coord_flip() +
    theme_bg()

xl_med_service <- df_warf_ord %>%
    filter(fiscal_year == 2019) %>%
    mutate_at("med_service_order", fct_infreq) %>%
    mutate_at("med_service_order", fct_lump, n = 10, other_level = "All Others") %>%
    mutate_at("med_service_order", fct_rev) %>%
    add_count(med_service_order, name = "n_unit") %>%
    count(med_service_order, n_unit, consult) %>%
    select(-n_unit) %>%
    mutate_at("consult", list(~if_else(. == 1, "Pharmacy", "Traditional"))) %>%
    pivot_wider(names_from = consult, values_from = n) %>%
    select(med_service_order, Pharmacy, Traditional)

# nurse unit order -------------------------------------
df_warf_ord %>%
    filter(fiscal_year == 2019) %>%
    mutate_at("nurse_unit_order", fct_infreq) %>%
    mutate_at("nurse_unit_order", fct_lump, n = 15) %>%
    mutate_at("nurse_unit_order", fct_rev) %>%
    mutate_at("fiscal_year", as_factor) %>%
    ggplot(aes(x = nurse_unit_order, fill = consult)) +
    geom_bar() +
    # scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    theme_bg()

xl_nurse_unit <- df_warf_ord %>%
    filter(fiscal_year == 2019) %>%
    mutate_at("nurse_unit_order", fct_infreq) %>%
    mutate_at("nurse_unit_order", fct_lump, n = 15, other_level = "All Others") %>%
    mutate_at("nurse_unit_order", fct_rev) %>%
    add_count(nurse_unit_order, name = "n_unit") %>%
    count(nurse_unit_order, n_unit, consult) %>%
    select(-n_unit) %>%
    mutate_at("consult", list(~if_else(. == 1, "Pharmacy", "Traditional"))) %>%
    pivot_wider(names_from = consult, values_from = n) %>%
    select(nurse_unit_order, Pharmacy, Traditional)

# export -----------------------------------------------

export <- list(
    "monthly_doses" = xl_monthly_doses,
    "daily_doses_box" = xl_doses_daily,
    "inr_daily" = xl_inr_daily,
    "nurse_unit_order" = xl_nurse_unit,
    "med_serv_order" = xl_med_service
)

write.xlsx(export, "data/external/data_for_handout_fy19.xlsx")
