# make list of patient encounters

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/fy2018"

if (!dir.exists(dir_raw)) dir.create(dir_raw)

# compress data files
dirr::gzip_files(dir_raw)

# run MBO query
#   * Patients - by Medication (Generic) - Administration Date
#       - Date and Time - Administration: set to desired time frame

# generate list of patients to retrieve data
raw_patients <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients()

id_mbo <- concat_encounters(raw_patients$millennium.id)

# run MBO queries:
#   * Orders - Actions
#       - Mnemonic (Primary Generic) FILTER ON: warfarin, Pharmacy Dosing Service(Warfarin), Pharmacy Dosing Service(Warfarin)., Pharmacy Dosing Service(Coumadin)
#   * Medications - Inpatient - All
#   * Blood Products
#   * Demographics
#   * Diagnosis - ICD-9/10-CM
#   * Labs - CBC
#   * Labs - Coags
#   * Labs - LFTs
#   * Procedures - ICD-9/10-PCS
#   * Warfarin Information

# run EDW queries:
#   * Identifiers
#       - Millennium Encounter ID

persons <- read_data(dir_raw, "identifiers") %>%
    as.id()

id_edw <- concat_encounters(persons_pull$person.id)

# run EDW queries:
#   * Encounters - by Person ID
