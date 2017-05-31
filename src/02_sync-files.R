library(aws.s3)
library(tidyverse)
library(edwr)
library(httr)
library(dirr)

# compress raw data files
gzip_files()

# prevent peer checking due to MH firewall
set_config(config(ssl_verifypeer = 0L))

bucket <- "warfarin-annual-report"
read_dir <- "data/raw"
save_dir <- paste0(read_dir, "/")

# list of files with data from MBO
mbo_files <- c("demographics", "diagnosis", "labs", "meds-inpt",
               "order-actions", "patients", "procedures", "warfarin-info")

# list of files with data from EDW
edw_files <- c("encounters", "identifiers")

# read data files into data frames then save each data frame to S3
files <- map(mbo_files, ~read_data(read_dir, .x, FALSE))
save_rds_s3(files, mbo_files, bucket, save_dir)

files <- map(edw_files, ~read_data(read_dir, .x))
save_rds_s3(files, edw_files, bucket, save_dir)
