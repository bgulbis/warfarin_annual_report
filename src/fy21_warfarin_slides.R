library(data.table)
library(tidyverse)
library(lubridate)
library(broom)
library(mbohelpr)
library(officer)
library(mschart)
library(flextable)
library(openxlsx)

cur_fy <- 2021

data_dir <- set_data_path("warfarin_annual_report", "fy21")

find_ind <- function(x, reg) {
    stringr::str_detect(x, regex(reg, ignore_case = TRUE))
}

data_blood <- get_data(paste0(data_dir, "/raw"), "blood_products")

data_consult_orders <- get_data(paste0(data_dir, "/raw"), "consult_orders") |>
    filter(str_detect(nurse_unit, "^HH|^HVI")) |>
    mutate(
        order_month = floor_date(order_datetime, unit = "month"),
        fiscal_year = year(order_month %m+% months(6))
    )

data_consult_tasks <- get_data(paste0(data_dir, "/raw"), "consult_tasks") |>
    filter(
        str_detect(nurse_unit, "^HH|^HVI"),
        task_date < ymd(paste(cur_fy, "07-01", sep = "-"))
    ) |>
    mutate(
        task_month = floor_date(task_date, unit = "month"),
        fiscal_year = year(task_month %m+% months(6))
    )

data_demographics <- get_data(paste0(data_dir, "/raw"), "demographics")

data_doac_doses <- get_data(paste0(data_dir, "/raw"), "doac_doses") |>
    filter(str_detect(nurse_unit, "^HH|^HVI")) |>
    mutate(
        med_month = floor_date(med_datetime, unit = "month"),
        fiscal_year = year(med_month %m+% months(6))
    )

data_labs <- get_data(paste0(data_dir, "/raw"), "labs") |>
    mutate(
        censor_high = str_detect(result_value, ">"),
        censor_low = str_detect(result_value, "<"),
        across(result_value, str_replace_all, pattern = "<|>", replacement = ""),
        across(result_value, as.numeric)
    )

data_measures <- get_data(paste0(data_dir, "/raw"), "measures")
data_reencounters <- get_data(paste0(data_dir, "/raw"), "reencounters")
data_reversal_meds <- get_data(paste0(data_dir, "/raw"), "reversal_meds")
data_warfarin_details <- get_data(paste0(data_dir, "/raw"), "warfarin_details")

data_warfarin_doses <- get_data(paste0(data_dir, "/raw"), "warfarin_doses") |>
    filter(str_detect(nurse_unit, "^HH|^HVI")) |>
    mutate(
        med_day = floor_date(med_datetime, unit = "day"),
        med_month = floor_date(med_datetime, unit = "month"),
        fiscal_year = year(med_month %m+% months(6))
    )

data_warfarin_home_meds <- get_data(paste0(data_dir, "/raw"), "warfarin_home_meds")
data_warfarin_orders <- get_data(paste0(data_dir, "/raw"), "warfarin_orders")

df_indications <- data_warfarin_details |>
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
    distinct(encounter_id, indication)
# mutate(value = TRUE) |>
# spread(indication, value, fill = FALSE)

df_inr_goal <- data_warfarin_details |>
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
    select(-result_value, -inr_range)

df_consult_pts <- data_consult_orders |>
    arrange(encounter_id, order_datetime) |>
    distinct(encounter_id, .keep_all = TRUE) |>
    mutate(
        consult = TRUE,
        consult_date = floor_date(order_datetime, unit = "day")
    ) |>
    select(
        encounter_id,
        consult_date,
        nurse_unit_consult = nurse_unit,
        med_service,
        consult
    )

df_warf_start <- data_warfarin_doses |>
    arrange(encounter_id, med_datetime) |>
    distinct(encounter_id, .keep_all = TRUE) |>
    mutate(warfarin_start = floor_date(med_datetime, unit = "day")) |>
    select(
        encounter_id,
        med_month,
        warfarin_start,
        nurse_unit_warfarin = nurse_unit,
        fiscal_year
    )

df_demog <- data_demographics |>
    inner_join(df_warf_start, by = "encounter_id") |>
    left_join(df_consult_pts, by = "encounter_id") |>
    mutate(
        across(consult, ~coalesce(., FALSE)),
        consult_day = difftime(
            consult_date,
            warfarin_start,
            units = "days"
        )
    )

df_warf_month <- data_warfarin_doses |>
    count(med_month, name = "warfarin") |>
    mutate(across(med_month, as_date))

df_consult_month <- data_consult_tasks |>
    count(task_month, name = "consults")

df_doac_month <- data_doac_doses |>
    mutate(med_day = floor_date(med_datetime, unit="day")) |>
    distinct(encounter_id, medication, med_month, med_day) |>
    count(med_month, name = "doac") |>
    mutate(across(med_month, as_date))

df_warf_ord <- data_warfarin_orders |>
    rename(
        nurse_unit_order = nurse_unit,
        med_service_order = med_service
    ) |>
    arrange(encounter_id, order_datetime) |>
    mutate(order_date = floor_date(order_datetime, unit = "day")) |>
    inner_join(df_warf_start, by = c("encounter_id", "order_date" = "warfarin_start")) |>
    left_join(df_consult_pts, by = "encounter_id") |>
    mutate(across(consult, ~coalesce(., FALSE)))

df_warf_duration <- data_warfarin_doses |>
    arrange(encounter_id, med_datetime) |>
    group_by(encounter_id) |>
    summarize(across(med_day, list(warfarin_start = first, warfarin_stop = last), .names = "{.fn}"))

df_doses_daily <- data_warfarin_doses |>
    arrange(encounter_id, med_datetime) |>
    group_by(encounter_id, fiscal_year, med_day) |>
    summarize(across(dose, sum, na.rm = TRUE), .groups = "drop") |>
    group_by(encounter_id) |>
    mutate(
        across(med_day, as.Date),
        warf_day = difftime(med_day, first(med_day), units = "days") + 1,
        across(warf_day, as.integer)
    ) |>
    filter(warf_day <= 10) |>
    select(-med_day) |>
    group_by(encounter_id, fiscal_year) |>
    mutate(last_day = last(warf_day)) |>
    ungroup() |>
    pivot_wider(names_from = warf_day, names_prefix = "day_", values_from = dose) |>
    pivot_longer(starts_with("day_"), names_to = "warf_day", values_to = "dose") |>
    mutate(
        across(warf_day, str_replace_all, pattern = "day_", replacement = ""),
        across(warf_day, as.integer),
        across(dose, ~coalesce(., 0))
    ) |>
    filter(warf_day <= last_day)

df_inr_daily <- df_warf_duration |>
    inner_join(data_labs, by = "encounter_id") |>
    filter(lab == "INR") |>
    mutate(
        lab_day = floor_date(lab_datetime, unit = "days"),
        lab_month = floor_date(lab_datetime, unit = "month"),
        across(c(lab_day, lab_month, warfarin_start), as_date),
        warf_day = difftime(lab_day, warfarin_start, units = "days") + 1,
        across(warf_day, as.integer),
        fiscal_year = year(lab_month %m+% months(6))
    ) |>
    filter(
        lab_day >= warfarin_start,
        lab_day <= warfarin_stop,
        warf_day <= 10
    ) |>
    group_by(encounter_id, fiscal_year, lab_day, warf_day) |>
    summarize(across(result_value, max, na.rm = TRUE), .groups = "drop") |>
    select(encounter_id, fiscal_year, warf_day, inr = result_value)

df_consult_daily <- df_warf_duration |>
    mutate(across(c(warfarin_start, warfarin_stop), as_date)) |>
    inner_join(data_consult_tasks, by = "encounter_id") |>
    mutate(
        warf_day = difftime(task_date, warfarin_start, units = "days") + 1,
        across(warf_day, as.integer),
        task_month = floor_date(task_date, unit = "month"),
        fiscal_year = year(task_month %m+% months(6))
    ) |>
    filter(
        task_date >= warfarin_start,
        task_date <= warfarin_stop,
        warf_day <= 10
    ) |>
    select(encounter_id, fiscal_year, warf_day) |>
    mutate(consult = TRUE)

df_doses_inr <- df_doses_daily |>
    full_join(df_inr_daily, by = c("encounter_id", "fiscal_year", "warf_day")) |>
    full_join(df_consult_daily, by = c("encounter_id", "fiscal_year", "warf_day")) |>
    arrange(encounter_id, warf_day) |>
    group_by(encounter_id) |>
    fill(last_day, .direction = "down") |>
    filter(warf_day <= last_day + 1) |>
    distinct()

df_group <- df_doses_inr |>
    filter(warf_day < 3) |>
    group_by(encounter_id, fiscal_year) |>
    summarize(across(consult, sum, na.rm = TRUE), .groups = "drop") |>
    mutate(across(consult, ~. > 0))

df_doses_cnt <- df_doses_inr |>
    group_by(encounter_id, fiscal_year) |>
    summarize(doses_n = sum(!is.na(dose), na.rm = TRUE), .groups = "drop")

df_doses_3 <- df_doses_cnt |>
    inner_join(df_group, by = c("encounter_id", "fiscal_year")) |>
    filter(
        doses_n >= 3,
        fiscal_year == cur_fy
    )

df_goal_eq2_3 <- df_inr_goal |>
    semi_join(df_doses_3, by = "encounter_id") |>
    filter(
        goal_low == 2,
        goal_high == 3
    ) |>
    distinct(encounter_id) |>
    mutate(goal2_3 = TRUE)

tidy_pts <- df_doses_3 |>
    left_join(df_goal_eq2_3, by = "encounter_id") |>
    mutate(across(goal2_3, ~coalesce(., FALSE))) |>
    ungroup()


# data sets for graphs ----------------------------------------------------

df_fig1 <- df_warf_month |>
    inner_join(df_consult_month, by = c("med_month" = "task_month")) |>
    inner_join(df_doac_month, by = "med_month") |>
    mutate(traditional = warfarin - consults) |>
    select(-warfarin) |>
    rename(
        Pharmacy = consults,
        DOAC = doac,
        Traditional = traditional
    ) |>
    gather(key, value, -med_month) |>
    mutate(
        across(key, as_factor),
        across(key, fct_rev)
    )

services <- c(
    "Thoracic/Cardiac Sur Service" = "CV Surgery",
    "Therapy - Phys/Occ/Speech/Cardiac" = "Rehab",
    "Pulmonology/Respiratory Therapy" = "Pulmonary"
)

df_fig2 <- df_warf_ord |>
    filter(fiscal_year == cur_fy) |>
    mutate(
        across(med_service_order, str_replace_all, pattern = services),
        across(med_service_order, fct_infreq),
        across(med_service_order, fct_lump, n = 7),
        across(med_service_order, fct_rev),
        across(consult, ~if_else(., "Pharmacy", "Traditional"))
    ) |>
    count(med_service_order, consult)

indications <- c(
    "afib" = "A.fib",
    "vad" = "VAD",
    "valve" = "Valve",
    "dvt" = "DVT",
    "hypercoag" = "Hypercoaguable",
    "^pe" = "PE",
    "thrombus" = "Thrombus",
    "stroke" = "Stroke",
    "other" = "Other"
)

df_fig3 <- df_demog |>
    filter(fiscal_year == cur_fy) |>
    inner_join(df_indications, by = "encounter_id") |>
    mutate(
        across(indication, str_replace_all, pattern = indications),
        across(indication, fct_infreq),
        across(indication, fct_rev),
        across(consult, ~if_else(., "Pharmacy", "Traditional"))
    ) |>
    count(indication, consult)

df_fig4 <- df_doses_inr |>
    semi_join(tidy_pts, by = "encounter_id") |>
    filter(
        warf_day <= 10,
        fiscal_year == cur_fy
    ) |>
    mutate(
        across(consult, ~coalesce(., FALSE)),
        across(consult, ~if_else(., "Pharmacy", "Traditional")),
        across(warf_day, as_factor)
    )

# gf1 <- df_fig4 |>
#     ggplot(aes(x = warf_day, y = dose, fill = consult)) + #, color = consult
#     geom_boxplot(outlier.shape = NA) +
#     xlab("Day of therapy") +
#     ylab("Dose (mg)") +
#     scale_fill_manual(
#         NULL,
#         values = c("#4daf4a", "#377eb8"),
#         labels = c("Traditional", "Pharmacy")
#     ) +
#     coord_cartesian(ylim = c(0, 15)) +
#     themebg::theme_bg_print(base_family = "serif") +
#     theme(legend.position = "top")
#
# set.seed(77123)
# gf2 <- df_fig4 |>
#     group_by(consult, warf_day) |>
#     slice_sample(n = 500) |>
#     ggplot(aes(x = warf_day, y = dose, fill = consult)) + #, color = consult
#     geom_boxplot(outlier.shape = NA) +
#     xlab("Day of therapy") +
#     ylab("Dose (mg)") +
#     scale_fill_manual(
#         NULL,
#         values = c("#4daf4a", "#377eb8"),
#         labels = c("Traditional", "Pharmacy")
#     ) +
#     coord_cartesian(ylim = c(0, 15)) +
#     themebg::theme_bg_print(base_family = "serif") +
#     theme(legend.position = "top")

cnt_fig4 <- df_fig4 |>
    group_by(encounter_id) |>
    mutate(first_group = first(consult)) |>
    count(encounter_id, consult, first_group) |>
    pivot_wider(names_from = consult, values_from = n) |>
    mutate(num_doses = sum(Traditional, Pharmacy, na.rm = TRUE)) |>
    filter(is.na(Traditional) | is.na(Pharmacy))

df_fig4_box <- df_fig4 |>
    semi_join(cnt_fig4, by = "encounter_id") |>
    ungroup() |>
    # group_by(consult, warf_day) |>
    # slice_sample(n = 250) |>
    select(encounter_id, last_day, warf_day, dose, consult)
    # pivot_wider(names_from = consult, values_from = dose)
    # mutate(across(warf_day, as.character))

df_fig4_box_pharm <- df_fig4_box |>
    filter(consult == "Pharmacy") |>
    pivot_wider(names_from = warf_day, values_from = dose, names_prefix = "day_", names_sort = TRUE) |>
    rowid_to_column() |>
    select(-encounter_id) |>
    pivot_longer(starts_with("day_"), names_to = "warf_day", values_to = "dose") |>
    mutate(
        across(warf_day, str_replace_all, pattern = "day_", replacement = ""),
        across(warf_day, as.numeric)
    ) |>
    arrange(rowid, warf_day) |>
    filter(!is.na(dose)) |>
    select(rowid, warf_day, pharmacy = dose)


df_fig4_box_trad <- df_fig4_box |>
    filter(consult == "Traditional") |>
    pivot_wider(names_from = warf_day, values_from = dose, names_prefix = "day_", names_sort = TRUE) |>
    rowid_to_column() |>
    select(-encounter_id) |>
    pivot_longer(starts_with("day_"), names_to = "warf_day", values_to = "dose") |>
    mutate(
        across(warf_day, str_replace_all, pattern = "day_", replacement = ""),
        across(warf_day, as.numeric)
    ) |>
    arrange(rowid, warf_day) |>
    filter(!is.na(dose)) |>
    select(rowid, warf_day, traditional = dose)

df_box_data <- df_fig4_box_pharm |>
    full_join(df_fig4_box_trad, by = c("rowid", "warf_day")) |>
    arrange(warf_day) |>
    mutate(across(warf_day, as.character)) |>
    select(Day = warf_day, Pharmacy = pharmacy, Traditional = traditional)

write.xlsx(df_box_data, paste0(data_dir, "final/fig4_boxplot_data.xlsx"), overwrite = TRUE)

smth_fig4 <- df_fig4 |>
    filter(!is.na(dose)) |>
    group_by(consult) |>
    nest() |>
    mutate(
        smth = map(data, loess, formula = dose ~ warf_day),
        fit = map(smth, `[[`, "fitted")
    ) |>
    select(-smth) |>
    unnest(c(data, fit)) |>
    distinct(consult, warf_day, fit) |>
    arrange(consult, warf_day)

smth_fig5 <- df_fig4 |>
    filter(!is.na(inr)) |>
    group_by(consult) |>
    nest() |>
    mutate(
        smth = map(data, loess, formula = inr ~ warf_day),
        fit = map(smth, `[[`, "fitted")
    ) |>
    select(-smth) |>
    unnest(c(data, fit)) |>
    distinct(consult, warf_day, fit) |>
    arrange(consult, warf_day)


# outcomes and events -----------------------------------------------------

df_high_inr <- df_doses_inr |>
    filter(inr >= 5) |>
    distinct(encounter_id) |>
    mutate(high_inr = TRUE)

df_hgb <- df_warf_duration |>
    inner_join(data_labs, by = "encounter_id") |>
    arrange(encounter_id, lab_datetime) |>
    filter(lab == "Hgb") |>
    mutate(
        lab_day = floor_date(lab_datetime, unit = "days"),
        lab_month = floor_date(lab_datetime, unit = "month"),
        warf_day = difftime(lab_day, warfarin_start, units = "days") + 1,
        across(warf_day, as.integer),
        fiscal_year = year(lab_month %m+% months(6))
    ) |>
    filter(
        lab_day >= warfarin_start,
        lab_day <= warfarin_stop + 1,
    )

dt_hgb <- as.data.table(df_hgb)

dt_window <- dt_hgb[, .(
    encounter_id,
    window_low = lab_datetime - hours(48),
    window_high = lab_datetime
)]

dt_hgb_max <- dt_hgb[
    dt_window,
    on=.(
        encounter_id,
        lab_datetime >= window_low,
        lab_datetime <= window_high
    ),
    .(max_hgb = max(result_value)),
    by=.EACHI
    ][, lab_datetime := NULL
    ][, head(.SD, 1), by = c("encounter_id", "lab_datetime")]

dt_hgb_drop <- dt_hgb[dt_hgb_max, on=.(encounter_id, lab_datetime)]

df_hgb_drop <- as_tibble(dt_hgb_drop) |>
    arrange(encounter_id, lab_datetime) |>
    mutate(hgb_chg = result_value - max_hgb) |>
    filter(
        hgb_chg <= -2,
        warf_day <= 10
    ) |>
    distinct(encounter_id) |>
    mutate(hgb_drop = TRUE)

df_prbc <- data_blood |>
    inner_join(df_warf_duration, by = "encounter_id") |>
    filter(
        str_detect(product, regex("rbc", ignore_case = TRUE)),
        event_datetime >= warfarin_start,
        event_datetime <= warfarin_stop
    ) |>
    distinct(encounter_id) |>
    mutate(prbc = TRUE)

df_ffp <- data_blood |>
    inner_join(df_warf_duration, by = "encounter_id") |>
    filter(
        str_detect(product, regex("ffp|plasma", ignore_case = TRUE)),
        event_datetime >= warfarin_start,
        event_datetime <= warfarin_stop
    ) |>
    distinct(encounter_id) |>
    mutate(ffp = TRUE)

df_reversal <- data_reversal_meds |>
    inner_join(df_warf_duration, by = "encounter_id") |>
    filter(
        med_datetime >= warfarin_start,
        med_datetime <= warfarin_stop
    ) |>
    distinct(encounter_id) |>
    mutate(reversal_med = TRUE)

df_dispo <- df_demog |>
    select(encounter_id, disch_disposition) |>
    # inner_join(tidy_pts, by = "encounter_id") |>
    mutate(
        dispo_group = case_when(
            str_detect(disch_disposition, "Hospice|Deceased") ~ "Deceased",
            str_detect(disch_disposition, "Home|Left") ~ "Home",
            str_detect(disch_disposition, "Facility|REHAB|Care|DC/TF") ~ "Transfer"
        )
    )

df_revisit <- df_dispo |>
    filter(dispo_group == "Home") |>
    inner_join(data_reencounters, by = "encounter_id") |>
    distinct(encounter_id) |>
    mutate(revisit = TRUE)

df_readmit <- df_dispo |>
    filter(dispo_group == "Home") |>
    inner_join(data_reencounters, by = "encounter_id") |>
    filter(readmit_type == "Inpatient") |>
    distinct(encounter_id) |>
    mutate(readmit = TRUE)

df_outcomes <- tidy_pts |>
    left_join(df_dispo, by = "encounter_id") |>
    left_join(df_high_inr, by = "encounter_id") |>
    left_join(df_hgb_drop, by = "encounter_id") |>
    left_join(df_prbc, by = "encounter_id") |>
    left_join(df_ffp, by = "encounter_id") |>
    left_join(df_reversal, by = "encounter_id") |>
    left_join(df_revisit, by = "encounter_id") |>
    left_join(df_readmit, by = "encounter_id") |>
    # filter(fiscal_year == cur_fy) |>
    select(encounter_id, consult, dispo_group, high_inr:readmit) |>
    mutate(
        across(c(high_inr:reversal_med), ~coalesce(., FALSE)),
        across(c(revisit, readmit), ~if_else(dispo_group == "Home", coalesce(., FALSE), .)),
        across(consult, ~if_else(., "Pharmacy", "Traditional"))
    ) |>
    add_count(consult, name = "group_n")

# df_outcomes_cnt <- df_outcomes |>
#     group_by(consult, group_n) |>
#     summarize(across(c(high_inr:readmit), sum, na.rm = TRUE), .groups = "drop")

# pptx figures ------------------------------------------------------------

my_theme <- mschart_theme(
    grid_major_line = fp_border(width = 0),
    date_fmt = "[$-en-US]mmm yyyy;@",
    # main_title = fp_text(color = "#404040", font.size = 24, bold = FALSE, font.family = "Calibri"),
    main_title = fp_text(color = "#595959", font.size = 16, bold = FALSE, font.family = "Calibri"),
    axis_title = fp_text(color = "#595959", font.size = 16, bold = FALSE, font.family = "Calibri"),
    axis_text = fp_text(color = "#7F7F7F", font.size = 14, bold = FALSE, font.family = "Calibri"),
    legend_position = "n",
    legend_text = fp_text(color = "#7F7F7F", font.size = 14, bold = FALSE, font.family = "Calibri")
)

slide_layout <- "Title and Chart"
slide_master <- "Office Theme"

# layout_properties(pptx, layout = "Title and Chart")
# layout_properties(pptx, layout = "Title Slide")

# graph_loc <- ph_location(left = 0.67, top = 0.25, width = 12, height = 7)
title_loc <- ph_location_label("Title 1")
chart_loc <- ph_location_label("Content Placeholder 2")

slide_title_format <- fp_text(color = "#404040", font.size = 24, bold = FALSE, font.family = "Calibri")
pharm_color <- "#1F78B4"
trad_color <- "#A6CEE3"
doac_color <- "#FDBF6F"
chart_colors <- c(Pharmacy = pharm_color, Traditional = trad_color)

fig1_format_data <- tibble(med_month = mdy(paste0("4/1/", cur_fy - 3)), key = "Spacer", value = 0)

title_fig1 <- fpar(ftext("Monthly doses of oral anticoagulants", slide_title_format))

p_fig1 <- df_fig1 |>
    bind_rows(fig1_format_data) |>
    mutate(across(key, factor, levels = c("Traditional", "DOAC", "Pharmacy", "Spacer"))) |>
    ms_linechart(x = "med_month", y = "value", group = "key") |>
    chart_ax_x(num_fmt = "[$-en-US]mmm yy;@", cross_between = "midCat", major_tick_mark = "in", limit_max = mdy(paste0("7/1/", cur_fy))) |>
    chart_ax_y(num_fmt = "#,##0", major_tick_mark = "none") |>
    chart_labels(title = "Doses") |>
    chart_settings(style = "line") |>
    chart_data_fill(values = c(chart_colors, DOAC = doac_color)) |>
    chart_data_stroke(values = c(chart_colors, DOAC = doac_color)) |>
    chart_labels_text(
        values = list(
            Pharmacy = fp_text(color = pharm_color, font.size = 14, font.family = "Calibri"),
            Traditional = fp_text(color = trad_color, font.size = 14, font.family = "Calibri"),
            DOAC = fp_text(color = doac_color, font.size = 14, font.family = "Calibri")
        )
    ) |>
    chart_data_line_width(values = c(Pharmacy = 3, Traditional = 2, DOAC = 2)) |>
    set_theme(my_theme)

title_fig2 <- fpar(ftext("Warfarin utilization by primary service", slide_title_format))

p_fig2 <- df_fig2 |>
    ms_barchart(x = "med_service_order", y = "n", group = "consult") |>
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "stacked", overlap = 100) |>
    chart_labels(ylab = "Patients") |>
    chart_data_fill(values = chart_colors) |>
    chart_data_stroke(values = chart_colors) |>
    chart_ax_x(major_tick_mark = "none") |>
    chart_ax_y(num_fmt = "#,##0", major_tick_mark = "in") |>
    set_theme(my_theme) |>
    chart_theme(
        title_y_rot = 0,
        legend_position = "t"
    )

title_fig3 <- fpar(ftext("Warfarin indications", slide_title_format))

p_fig3 <- df_fig3 |>
    ms_barchart(x = "indication", y = "n", group = "consult") |>
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "stacked", overlap = 100) |>
    chart_labels(ylab = "Patients") |>
    chart_data_fill(values = chart_colors) |>
    chart_data_stroke(values = chart_colors) |>
    chart_ax_x(major_tick_mark = "none") |>
    chart_ax_y(num_fmt = "#,##0", major_tick_mark = "in") |>
    set_theme(my_theme) |>
    chart_theme(
        title_y_rot = 0,
        legend_position = "t"
    )

fig4_format_data <- tibble(consult = "Goal", warf_day = 0, fit = 0)

title_fig4 <- fpar(ftext("Warfarin daily doses", slide_title_format))

p_fig4 <- smth_fig4 |>
    bind_rows(fig4_format_data) |>
    ungroup() |>
    mutate(
        across(consult, factor, levels = c("Goal", "Traditional", "Pharmacy")),
        across(warf_day, factor)
    ) |>
    ms_linechart(x = "warf_day", y = "fit", group = "consult") |>
    chart_settings(style = "line") |>
    chart_labels(title = "Dose (mg)", xlab = "Day of therapy") |>
    chart_data_fill(values = chart_colors) |>
    chart_data_stroke(values = chart_colors) |>
    chart_labels_text(
        values = list(
            Pharmacy = fp_text(color = pharm_color, font.size = 14, font.family = "Calibri"),
            Traditional = fp_text(color = trad_color, font.size = 14, font.family = "Calibri")
        )
    ) |>
    chart_ax_x(major_tick_mark = "in") |>
    set_theme(my_theme)

fig5_format_data <- tibble(consult = "Goal", warf_day = 0:10, fit = 2)

title_fig5 <- fpar(ftext("INR response", slide_title_format))

p_fig5 <- smth_fig5 |>
    bind_rows(fig5_format_data) |>
    ungroup() |>
    mutate(
        across(consult, factor, levels = c("Goal", "Traditional", "Pharmacy")),
        across(warf_day, factor)
    ) |>
    ms_linechart(x = "warf_day", y = "fit", group = "consult") |>
    chart_settings(style = "line") |>
    chart_labels(title = "INR", xlab = "Day of therapy") |>
    chart_data_fill(values = c(chart_colors, Goal = "#BFBFBF")) |>
    chart_data_stroke(values = c(chart_colors, Goal = "#BFBFBF")) |>
    chart_data_line_style(values = c(Goal = "dashed", Pharmacy = "solid", Traditional = "solid")) |>
    chart_data_line_width(values = c(Goal = 1.5, Pharmacy = 2, Traditional = 2)) |>
    chart_labels_text(
        values = list(
            Goal = fp_text(color = "#BFBFBF", font.size = 14, font.family = "Calibri"),
            Pharmacy = fp_text(color = pharm_color, font.size = 14, font.family = "Calibri"),
            Traditional = fp_text(color = trad_color, font.size = 14, font.family = "Calibri")
        )
    ) |>
    chart_ax_x(major_tick_mark = "in", cross_between = "between") |>
    chart_ax_y(limit_min = 1, limit_max = 3) |>
    set_theme(my_theme)


# pptx tables -------------------------------------------------------------

set_flextable_defaults(
    font.family = "Calibri",
    font.size = 18,
    font.color = "#7F7F7F",
    digits = 0,
    theme_fun = theme_alafoli
)

tbl1_labels <- c(
    "high_inr" = "High INR",
    "hgb_drop" = "Hemoglobin drop",
    "prbc" = "Transfuse PRBC",
    "ffp" = "Transfuse FFP",
    "reversal_med" = "Administer reversal agent"
    # "revisit" = "Unplanned return*",
    # "readmit" = "Hospital readmission*"
)

title_tbl1 <- fpar(ftext("Events during the first 10 days of warfarin therapy", slide_title_format))

df_p <- df_outcomes |>
    mutate(across(c("consult", "dispo_group"), as_factor)) |>
    summarize(across(c(dispo_group, where(is.logical)), ~chisq.test(consult, .)$p.value)) |>
    pivot_longer(everything(), names_to = "outcome", values_to = "p_value") |>
    mutate(across(p_value, round, digits = 3))

tbl1 <- df_outcomes |>
    select(-group_n) |>
    group_by(consult) |>
    summarize(across(c(high_inr:reversal_med), mean, na.rm = TRUE), .groups = "drop") |>
    pivot_longer(high_inr:reversal_med, names_to = "outcome", values_to = "pct") |>
    pivot_wider(names_from = "consult", values_from = "pct") |>
    left_join(df_p, by = "outcome") |>
    mutate(
        across(outcome, str_replace_all, pattern = tbl1_labels),
        across(c(Pharmacy, Traditional), ~ . * 100),
        across(c(Pharmacy, Traditional), round, digits = 0)
    )

ft1 <- flextable(tbl1) |>
    set_header_labels(
        outcome = "Outcome",
        Pharmacy = "Pharmacy",
        Traditional = "Traditional",
        p_value = "P-value"
    ) |>
    color(i = ~ p_value < 0.05, color = "#000000") |>
    colformat_num(j = c("Pharmacy", "Traditional"), suffix = "%") |>
    width(j = "outcome", width = 4) |>
    width(j = c("Pharmacy", "Traditional", "p_value"), width = 2) |>
    height_all(0.7)

df_dispo_pct <- df_outcomes |>
    add_count(consult, name = "n_group") |>
    count(consult, dispo_group, n_group, name = "n_dispo") |>
    mutate(pct_dispo = round(n_dispo / n_group * 100)) |>
    select(-n_dispo, -n_group) |>
    pivot_wider(names_from = consult, values_from = pct_dispo) |>
    rename(outcome = dispo_group) |>
    arrange(desc(Pharmacy), desc(Traditional))

df_readmit_pct <- df_outcomes |>
    select(encounter_id, consult, revisit, readmit) |>
    filter(!is.na(revisit)) |>
    add_count(consult, name = "n_group") |>
    group_by(consult, n_group) |>
    summarize(across(c(revisit, readmit), sum, na.rm = TRUE), .groups = "drop") |>
    mutate(across(c(revisit, readmit), ~ round(. / n_group * 100))) |>
    select(-n_group) |>
    pivot_longer(cols = c(revisit, readmit), names_to = "outcome") |>
    pivot_wider(names_from = consult, values_from = value)

tbl2_labels <- c(
    "dispo_group" = "Discharge disposition",
    "revisit" = "Unplanned return",
    "readmit" = "Hospital readmission"
)

title_tbl2 <- fpar(ftext("Disposition and unplanned return within 30 days", slide_title_format))

tbl2 <- tibble(outcome = "dispo_group") |>
    bind_rows(df_dispo_pct, df_readmit_pct) |>
    left_join(df_p, by = "outcome") |>
    mutate(across(outcome, str_replace_all, pattern = tbl2_labels))

ft2 <- flextable(tbl2) |>
    set_header_labels(
        outcome = "Outcome",
        Pharmacy = "Pharmacy",
        Traditional = "Traditional",
        p_value = "P-value"
    ) |>
    color(i = ~ p_value < 0.05, color = "#000000") |>
    colformat_num(j = c("Pharmacy", "Traditional"), suffix = "%") |>
    colformat_char(i = 2:4, j = "outcome", prefix = "    ") |>
    width(j = "outcome", width = 4) |>
    width(j = c("Pharmacy", "Traditional", "p_value"), width = 2) |>
    height_all(0.7)

pptx <- read_pptx("doc/template.pptx") |>
    set_theme(my_theme) |>
    add_slide(layout = "Title Slide", master = slide_master) |>
    ph_with("Pharmacy Warfarin Dosing Service", location = title_loc) |>
    ph_with(
        paste("Analysis for fiscal year", cur_fy, "\nBrian Gulbis, PharmD, BCPS"),
        location = ph_location_label("Subtitle 2")
    ) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = title_fig1, location = title_loc) |>
    ph_with(value = p_fig1, location = chart_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = title_fig2, location = title_loc) |>
    ph_with(value = p_fig2, location = chart_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = title_fig3, location = title_loc) |>
    ph_with(value = p_fig3, location = chart_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = title_fig4, location = title_loc) |>
    ph_with(value = p_fig4, location = chart_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = title_fig5, location = title_loc) |>
    ph_with(value = p_fig5, location = chart_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = title_tbl1, location = title_loc) |>
    ph_with(value = ft1, location = chart_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = title_tbl2, location = title_loc) |>
    ph_with(value = ft2, location = chart_loc)


print(pptx, target = paste0(data_dir, "report/fy2021_pt_slides.pptx"))
''
