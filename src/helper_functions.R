
make_indications <- function(d) {
    tidy <- purrr::dmap_at(d, "warfarin.result",
                       stringr::str_replace_all,
                       pattern = "Deep vein thrombosis",
                       replacement = "D-V-T") %>%
        purrr::dmap_at("warfarin.result",
                       stringr::str_replace_all,
                       pattern = "Pulmonary embolism",
                       replacement = "P-E")

    find_string <- function(x) {
        lazyeval::interp(
            ~stringr::str_detect(
                tidy$warfarin.result,
                stringr::regex(y, ignore_case = TRUE)),
            y = x)
    }

    find <- c("Atrial fibrillation|a(.*)?fib|a(.*)?flutter",
              "D-V-T|DVT(?!( prophylaxis))|VTE",
              "P-E|PE",
              "Heart valve \\(Mech/porc/bioprost\\)|valve|avr|mvr",
              "st(ro|or)ke|cva|ica|mca",
              "vad|hm[ ]?ii|heart( )?mate|heartware|syncardia|total artificial heart|tah",
              "throm|clot|emboli|occl",
              "malig|anti(.)?phos|lupus|apla|hypercoag|deficien|leiden|fvl|factor v",
              "prophylax")

    dots <- purrr::map(find, find_string)
    nm <- c("afib", "dvt", "pe", "valve", "stroke", "vad", "thrombus",
            "hypercoag", "prophylaxis")
    tidy <- dplyr::mutate_(tidy, .dots = setNames(dots, nm)) %>%
    # dots <- list(~sum(afib, dvt, pe, valve, stroke, vad, thrombus, hypercoag, prophylaxis) == 0)
    # tidy <- dplyr::mutate_(tidy, .dots = setNames(dots, "other"))
        dplyr::select_(.dots = list(quote(-warfarin.result), quote(-warfarin.event)))
}
