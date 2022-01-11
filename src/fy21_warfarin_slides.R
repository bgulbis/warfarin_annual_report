library(data.table)
library(tidyverse)
library(lubridate)
library(broom)
library(mbohelpr)
library(officer)
library(mschart)

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
        warf_day = difftime(med_day, first(med_day), units = "days") + 1,
        across(warf_day, as.integer)
    ) |>
    filter(warf_day <= 10) |>
    ungroup() |>
    select(encounter_id, fiscal_year, warf_day, dose)

df_inr_daily <- df_warf_duration |>
    inner_join(data_labs, by = "encounter_id") |>
    filter(lab == "INR") |>
    mutate(
        lab_day = floor_date(lab_datetime, unit = "days"),
        lab_month = floor_date(lab_datetime, unit = "month"),
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
    arrange(encounter_id, warf_day)

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
        across(consult, ~if_else(., "Pharmacy", "Traditional"))
        # across(dose, ~coalesce(., 0L))
    )

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

# pptx figures ------------------------------------------------------------

my_theme <- mschart_theme(
    grid_major_line = fp_border(width = 0),
    date_fmt = "[$-en-US]mmm yyyy;@",
    main_title = fp_text(color = "#404040", font.size = 24, bold = FALSE, font.family = "Calibri"),
    axis_title = fp_text(color = "#595959", font.size = 16, bold = FALSE, font.family = "Calibri"),
    axis_text = fp_text(color = "#7F7F7F", font.size = 14, bold = FALSE, font.family = "Calibri"),
    legend_position = "n",
    legend_text = fp_text(color = "#7F7F7F", font.size = 14, bold = FALSE, font.family = "Calibri")
)

slide_layout <- "Blank"
slide_master <- "Office Theme"
graph_loc <- ph_location(left = 0.67, top = 0.25, width = 12, height = 7)

pharm_color <- "#1F78B4"
trad_color <- "#A6CEE3"
doac_color <- "#FDBF6F"
chart_colors <- c(Pharmacy = phar_color, Traditional = trad_color)

p_fig1 <- df_fig1 |>
    ms_linechart(x = "med_month", y = "value", group = "key") |>
    chart_ax_x(num_fmt = "[$-en-US]mmm yy;@") |>
    chart_ax_y(num_fmt = "#,##0") |>
    chart_labels(title = "Monthly doses of oral anticoagulants", ylab = "Doses") |>
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
    chart_ax_x(major_tick_mark = "in") |>
    set_theme(my_theme)

p_fig2 <- df_fig2 |>
    ms_barchart(x = "med_service_order", y = "n", group = "consult") |>
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "stacked", overlap = 100) |>
    chart_labels(title = "Warfarin utilization by primary service", ylab = "Patients") |>
    chart_data_fill(values = chart_colors) |>
    chart_data_stroke(values = chart_colors) |>
    chart_ax_x(major_tick_mark = "in") |>
    chart_ax_y(num_fmt = "#,##0") |>
    set_theme(my_theme) |>
    chart_theme(
        title_y_rot = 0,
        legend_position = "t"
    )

p_fig3 <- df_fig3 |>
    ms_barchart(x = "indication", y = "n", group = "consult") |>
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "stacked", overlap = 100) |>
    chart_ax_y(num_fmt = "#,##0") |>
    chart_labels(title = "Warfarin indications", ylab = "Patients") |>
    chart_data_fill(values = chart_colors) |>
    chart_data_stroke(values = chart_colors) |>
    chart_ax_x(major_tick_mark = "in") |>
    set_theme(my_theme) |>
    chart_theme(
        title_y_rot = 0,
        legend_position = "t"
    )

p_fig4 <- smth_fig4 |>
    mutate(across(warf_day, factor)) |>
    ms_linechart(x = "warf_day", y = "fit", group = "consult") |>
    chart_settings(style = "line") |>
    chart_labels(title = "Warfarin daily doses", xlab = "Day of therapy", ylab = "Dose (mg)") |>
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

p_fig5 <- smth_fig5 |>
    mutate(across(warf_day, factor)) |>
    ms_linechart(x = "warf_day", y = "fit", group = "consult") |>
    chart_settings(style = "line") |>
    chart_labels(title = "INR response", xlab = "Day of therapy", ylab = "INR") |>
    chart_data_fill(values = chart_colors) |>
    chart_data_stroke(values = chart_colors) |>
    chart_labels_text(
        values = list(
            Pharmacy = fp_text(color = pharm_color, font.size = 14, font.family = "Calibri"),
            Traditional = fp_text(color = trad_color, font.size = 14, font.family = "Calibri")
        )
    ) |>
    chart_ax_x(major_tick_mark = "in") |>
    chart_ax_y(limit_min = 1, limit_max = 3) |>
    set_theme(my_theme)

pptx <- read_pptx("doc/template.pptx") |>
    set_theme(my_theme) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = p_fig1, location = graph_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = p_fig2, location = graph_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = p_fig3, location = graph_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = p_fig4, location = graph_loc) |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = p_fig5, location = graph_loc)

print(pptx, target = paste0(data_dir, "report/fy2021_pt_slides.pptx"))


