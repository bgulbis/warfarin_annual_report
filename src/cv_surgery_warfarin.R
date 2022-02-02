library(data.table)
library(tidyverse)
library(mbohelpr)
library(lubridate)

data_dir <- set_data_path("warfarin_annual_report", "fy21")

find_ind <- function(x, reg) {
    stringr::str_detect(x, regex(reg, ignore_case = TRUE))
}

raw_doac_doses <- get_data(paste0(data_dir, "/raw"), "doac_doses") |>
    filter(str_detect(nurse_unit, "HH CVICU|HH CVIMU")) |>
    mutate(
        med_month = floor_date(med_datetime, unit = "month"),
        fiscal_year = year(med_month %m+% months(6))
    )

raw_warfarin_doses <- get_data(paste0(data_dir, "/raw"), "warfarin_doses") |>
    filter(str_detect(nurse_unit, "HH CVICU|HH CVIMU")) |>
    mutate(
        med_day = floor_date(med_datetime, unit = "day"),
        med_month = floor_date(med_datetime, unit = "month"),
        fiscal_year = year(med_month %m+% months(6))
    )

raw_disch_meds <- get_data(paste0(data_dir, "/raw"), "disch_meds") |>
    mutate(across(disch_med, str_to_lower))

pts_doac <- raw_doac_doses |>
    filter(fiscal_year == 2021) |>
    distinct(encounter_id)

pts_warfarin <- raw_warfarin_doses |>
    filter(fiscal_year == 2021) |>
    distinct(encounter_id)

pts_oac <- pts_warfarin |>
    bind_rows(pts_doac) |>
    distinct(encounter_id)

mbo_id <- concat_encounters(pts_oac$encounter_id)
print(mbo_id)

df_disch_warfarin <- raw_disch_meds |>
    semi_join(pts_warfarin, by = "encounter_id") |>
    filter(
        nurse_unit %in% c("HH CVICU", "HH CVIMU"),
        disch_med == "warfarin"
    )

df_disch_enox <- raw_disch_meds |>
    semi_join(df_disch_warfarin, by = "encounter_id") |>
    filter(disch_med == "enoxaparin") |>
    mutate(disch_enox = TRUE) |>
    select(encounter_id, disch_enox)

raw_demographics <- get_data(paste0(data_dir, "/raw"), "demographics") |>
    semi_join(pts_warfarin, by = "encounter_id")

raw_warfarin_details <- get_data(paste0(data_dir, "/raw"), "warfarin_details") |>
    semi_join(pts_warfarin, by = "encounter_id")

raw_warfarin_orders <- get_data(paste0(data_dir, "/raw"), "warfarin_orders") |>
    semi_join(pts_warfarin, by = "encounter_id")

raw_labs <- get_data(paste0(data_dir, "/raw"), "labs") |>
    mutate(
        censor_high = str_detect(result_value, ">"),
        censor_low = str_detect(result_value, "<"),
        across(result_value, str_replace_all, pattern = "<|>", replacement = ""),
        across(result_value, as.numeric)
    )

df_indications <- raw_warfarin_details |>
    semi_join(pts_oac, by = "encounter_id") |>
    filter(detail == "Warfarin Indication") |>
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
    ) |>
    distinct(encounter_id, indication) |>
    mutate(value = TRUE) |>
    spread(indication, value, fill = FALSE)

df_inr_goal <- raw_warfarin_details |>
    semi_join(pts_oac, by = "encounter_id") |>
    filter(
        detail == "INR Range",
        result_value != ""
    ) |>
    mutate(
        inr_range = result_value,
        across(inr_range, str_replace_all, pattern = " |", replacement = ""),
        across(inr_range, str_replace_all, pattern = regex("to|--|/|=|,|-\\.", ignore_case = TRUE), replacement = "-"),
        across(inr_range, str_replace_all, pattern = ">1.5-<2.5", replacement = "1.5-2.5"),
        across(
            inr_range,
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
        )
    ) |>
    extract(
        inr_range,
        c("goal_low", "goal_high"),
        regex = "([0-9\\.]+ ?)-( ?[0-9\\.]+)",
        remove = FALSE,
        convert = TRUE
    ) |>
    filter(
        !is.na(goal_low),
        goal_low < goal_high
    ) |>
    mutate(across(c(goal_low, goal_high), as.numeric)) |>
    select(-result_value, -inr_range) |>
    arrange(encounter_id, desc(event_datetime)) |>
    distinct(encounter_id, .keep_all = TRUE)

df_service <- raw_warfarin_orders |>
    arrange(encounter_id, order_datetime) |>
    group_by(encounter_id) |>
    summarize(across(med_service, list(first = first, last = last)))

    # distinct(encounter_id, .keep_all = TRUE) |>
    # select(encounter_id, med_service)

df_inr_last <- raw_labs |>
    semi_join(df_disch_warfarin, by = "encounter_id") |>
    filter(lab == "INR") |>
    arrange(encounter_id, desc(lab_datetime)) |>
    distinct(encounter_id, .keep_all = TRUE) |>
    select(encounter_id, last_inr = result_value)

data_disch_warfarin <- df_disch_warfarin |>
    left_join(raw_demographics[c("encounter_id", "los", "age", "sex", "disch_disposition")], by = "encounter_id") |>
    left_join(df_disch_enox, by = "encounter_id") |>
    left_join(df_inr_last, by = "encounter_id") |>
    left_join(df_indications, by = "encounter_id") |>
    left_join(df_inr_goal[c("encounter_id", "goal_low", "goal_high")], by = "encounter_id") |>
    filter(med_service == "Surgery, Thoracic") |>
    mutate(across(where(is.character), as_factor))

summary(data_disch_warfarin)


# data_cvs <- raw_demographics |>
#     left_join(df_service, by = "encounter_id") |>
#     left_join(df_indications, by = "encounter_id") |>
#     filter(med_service_first == "Surgery, Thoracic") |>
#     mutate(across(where(is.character), as_factor))

# summary(data_cvs)
