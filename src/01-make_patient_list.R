# make list of patient encounters

# run MBO query
#   * Patients - by Medication (Generic) - Administration Date
#       - Date and Time - Administration: set to desired time frame

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"

# compress data files
dirr::gzip_files(dir_raw)

# get list of patients already pulled
completed_pts <- "data/final/patients_completed.csv"
pulled <- tibble("millennium.id" = "")

if (file.exists(completed_pts)) {
    pulled <- read_csv(completed_pts, col_types = "c?cdc")
}

# generate list of patients to retrieve data
raw_patients <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    arrange(millennium.id)

to_pull <- anti_join(raw_patients, pulled, by = "millennium.id")

all_mbo <- concat_encounters(raw_patients$millennium.id)
id_mbo <- concat_encounters(to_pull$millennium.id)

save_pts <- raw_patients %>%
    filter(!is.na(discharge.datetime))

if (!file.exists(completed_pts)) {
    x <- FALSE
} else {
    x <- TRUE
}
write_csv(save_pts, completed_pts, append = x)

# run MBO queries:
#   * Orders - Actions
#       - Mnemonic (Primary Generic) FILTER ON: warfarin, Pharmacy Dosing Service(Warfarin), Pharmacy Dosing Service(Warfarin)., Pharmacy Dosing Service(Coumadin)
#   * Medications - Inpatient - Prompt
#       - Medication (Generic): warfarin
#   * Demographics
#   * Labs - CBC
#   * Labs - Coags

# run EDW queries:
#   * Identifiers
#       - Millennium Encounter ID

persons <- read_data(dir_raw, "identifiers") %>%
    as.id()

persons_pull <- anti_join(persons, pulled, by = "millennium.id")

id_edw <- concat_encounters(persons_pull$person.id)
all_edw <- concat_encounters(persons$person.id)

# run EDW queries:
#   * Encounters - by Person ID

dirr::gzip_files(dir_raw)
