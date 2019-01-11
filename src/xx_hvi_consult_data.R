library(tidyverse)
library(lubridate)
library(broom)
library(themebg)
library(officer)
library(rvg)

x <- dirr::get_rds("data/tidy/fy2018")

start_date <- mdy("7/1/2017", tz = "US/Central")
end_date <- mdy("6/30/2018", tz = "US/Central")

data_ts_2017 <- read_rds("data/tidy/fy2017/data_timeseries_location.Rds")

# ttr_pharm_prev <- data_warfarin %>%
#     filter(!is.na(perc.time),
#            warfarin_start >= mdy("7/1/2016", tz = "US/Central"),
#            warfarin_start <= mdy("6/30/2017", tz = "US/Central"),
#            group == "pharmacy") %>%
#     mutate_at("perc.time", as.numeric) %>%
#     mutate_at("perc.time", funs(. * 100)) %>%
#     mutate_at("group", str_replace_all, pattern = "pharmacy", replacement = "pharmacy_fy16")

ttr_pharm_curr <- data_warfarin %>%
    filter(
        !is.na(perc.time),
        warfarin_start >= start_date,
        warfarin_start <= end_date,
        group == "pharmacy"
    ) %>%
    mutate_at("perc.time", as.numeric) %>%
    mutate_at("perc.time", funs(. * 100))

ttr_trad_curr <- data_warfarin %>%
    filter(
        !is.na(perc.time),
        warfarin_start >= start_date,
        warfarin_start <= end_date,
        group == "traditional"
    ) %>%
    mutate_at("perc.time", as.numeric) %>%
    mutate_at("perc.time", funs(. * 100))

my_scale_fill <- function(..., values) {
    scale_fill_manual(
        "",
        values = c("white", "grey50"),
        # values = c("#969696", "#636363"),
        labels = c("Traditional", "Pharmacy")
    )
}

df_ts <- data_timeseries_location %>%
    bind_rows(data_ts_2017) %>%
    filter(fy == 2017 | fy == 2018) %>%
    group_by(week) %>%
    summarize_at(c("pharmacy", "traditional"), sum, na.rm = TRUE)

df_ts_fy <- df_ts %>%
    mutate(fy = year(week) + months(6)) %>%
    mutate_at("fy", as.integer) %>%
    group_by(fy) %>%
    summarize_at(c("pharmacy", "traditional"), mean, na.rm = TRUE)

chg_pharm <- round(((df_ts_fy$pharmacy[nrow(df_ts_fy)] / df_ts_fy$pharmacy[1]) - 1) * 100, 0)
chg_trad <- round((1 - (df_ts_fy$traditional[nrow(df_ts_fy)] / df_ts_fy$traditional[1])) * 100, 0)


g_pharm_utilization <- data_timeseries_location %>%
    filter(
        fy == 2018,
        order.location %in% c(
            "HH CVICU",
            "HH CVIMU",
            "HH HFIC",
            "HH HFIM",
            "HH CCU",
            "HVI CIMU"
        )
    ) %>%
    group_by(order.location) %>%
    summarize_at(c("pharmacy", "traditional"), sum, na.rm = TRUE) %>%
    mutate(total = pharmacy + traditional) %>%
    arrange(desc(total)) %>%
    # top_n(10, total) %>%
    gather(group, num, pharmacy, traditional) %>%
    mutate_at("group", factor, levels = c("traditional", "pharmacy")) %>%
    mutate_at("order.location", as_factor) %>%
    mutate_at("order.location", fct_rev) %>%
    ggplot(aes(x = order.location, y = num, fill = group)) +
    geom_bar(stat = "identity") +
    # geom_errorbar(aes(x = order.location, ymin = total, ymax = total), data = units16, width = 0.5, color = "grey35") +
    # geom_point(aes(x = order.location, y = pharmacy), data = units16, shape = 4, color = "grey35") +
    xlab("Hospital Unit") +
    ylab("Warfarin Doses Administered") +
    scale_fill_brewer(NULL, palette = "Dark2", labels = c("Traditional", "Pharmacy")) +
    # my_scale_fill() +
    coord_flip() +
    theme_bg_print(base_family = "serif", yticks = FALSE) +
    theme(legend.position = "bottom")


inr <- data_daily %>%
    left_join(data_warfarin[c("millennium.id", "goal.low")], by = "millennium.id") %>%
    ungroup() %>%
    filter(
        warfarin_day <= 7,
        med.datetime >= start_date,
        med.datetime <= end_date,
        !is.na(inr),
        goal.low >= 2
    ) %>%
    left_join(
        data_warfarin[c("millennium.id", "group", "initiation", "indication_group")],
        by = "millennium.id") %>%
    mutate_at("warfarin_day", as.numeric) %>%
    mutate_at("group", factor, levels = c("traditional", "pharmacy"))

g_inr_response <- ggplot(inr, aes(x = warfarin_day, y = inr)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 2, ymax = 3), fill = "grey85") +
    geom_smooth(aes(color = group), se = FALSE) +
    scale_x_continuous("Day of therapy", breaks = seq(1, 7, 1)) +
    ylab("INR") +
    scale_color_brewer(NULL, palette = "Dark2", labels = c("Traditional", "Pharmacy")) +
    coord_cartesian(ylim = c(1, 3.5)) +
    theme_bg_print(base_family = "serif") +
    theme(legend.position = "bottom")

# powerpoint -------------------------------------------

slide_layout <- "Title and Content"
slide_master <- "Office Theme"
# month_abbrv <- format(end_month, "%Y-%m")
title_size <- fp_text(font.size = 32)

# layout_properties(read_pptx(), layout = "Title Slide", master = slide_master)

read_pptx() %>%
    add_slide(layout = "Title Slide", master = slide_master) %>%
    ph_with_text(type = "ctrTitle", str = "Pharmacy Warfarin Dosing FY2018") %>%
    ph_with_text(type = "subTitle", str = "Brian Gulbis") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_empty() %>%
    ph_add_par() %>%
    ph_add_text(
        str = "Pharmacy dosing service utilization in HVI",
        style = title_size
    ) %>%
    ph_with_vg(ggobj = g_pharm_utilization, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_empty() %>%
    ph_add_par() %>%
    ph_add_text(
        str = "INR response in pharmacy managed patients",
        style = title_size
    ) %>%
    ph_with_vg(ggobj = g_inr_response, type = "body") %>%
    print(target = "figs/hvi-pharmacy-warfarin-dosing.pptx")

