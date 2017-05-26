library(aws.s3)
library(tidyverse)
library(edwr)
library(httr)

# prevent peer checking due to MH firewall
set_config(config(ssl_verifypeer = 0L))

bucket <- "warfarin-annual-report"

mbo_files <- c("demographics", "diagnosis", "labs", "meds-inpt",
               "order-actions", "patients", "procedures", "warfarin-info")

edw_files <- c("encounters", "identifiers")

files <- map(mbo_files, ~read_data("data/raw", .x, FALSE))
names(files) <- mbo_files
# list2env(files, .GlobalEnv)

# walk2(files, names(files), ~write_rds(.x, path = paste0("data/raw/", .y, ".Rds"), compress = "gz"))
walk2(files, mbo_files, ~s3saveRDS(.x,
                                   object = paste0("data/raw/", .y, ".Rds"),
                                   bucket = bucket,
                                   headers = list("x-amz-server-side-encryption" = "AES256"))
)
