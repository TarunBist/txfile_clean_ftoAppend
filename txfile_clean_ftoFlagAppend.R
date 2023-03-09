# Template for cleaning up transactions files for use in matchbacks

# this script needs to be in your matchback's "Work" folder, and you must
# have run the script 0_initialize_matchback.R

# this is meant to be a "shell" to assist with loading and writing
# data in the proper folders. there's a section where you'd write code
# to clean the txfile

# if your (edited) script would be useful for cleaning future transactions
# files from a particular client, please rename the file as below:
## clientname_txfile_clean.R
# and add to the matchbacks-data-prep repository in this directory:
## transactions file cleanup/client specific/

library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)

# file navigation ---------------------------------------------------------

# gets the name of your project folder
project_folder <- gsub(".*\\/", "", getwd())

# gets the name of your Client
client_folder <- gsub("^.*\\/Clients\\/", "", getwd())
client_folder <- gsub("\\/.*$", "", client_folder)

# list of files in project's PII folder
files_PII <- list.files(path = paste0("../../../PII/Matchbacks/", project_folder, "/"),
                        recursive = TRUE)

# list of files in client's external transfers folder
dir_transfer <- grep("External Transfers",
                     list.dirs("../../../../..",
                               recursive=FALSE),
                     value=TRUE)

files_transfers <- list.files(path = paste0(dir_transfer, "/Incoming (to SLM)"),
                              recursive = TRUE)

# load client data --------------------------------------------------------

# ideally you can load directly from the external transfers folder, 
# but if you had to download the file to the PII project_folder that's also cool

# use data.table::fread() for delimited files
# use readxl::read_excel() for Excel files
# use readr::read_fwf() for fixed width files (godspeed)

# External transfers:
dat <- read_excel(paste0(dir_transfer, "/Incoming (to SLM)/",
                         "my_files.csv"))

# PII folder:
# dat <- fread(paste0("../../../PII/Matchbacks/", project_folder, "/",
#                     "my_file.csv"))

# if loading multiple files and can stack up:
# dat <- list.files(path = paste0("../../../PII/Matchbacks/", project_folder, "/"),
#                   pattern = "orders_export") %>%
#   lapply(fread) %>% 
#   rbindlist()

names(dat) <- make.names(names(dat))

# load prior cleaned txfile -----------------------------------------------

# if applicable. update this path manually!
old <- fread("../../../PII/Matchbacks/")

names(old) <- make.names(names(old))

old$orderDate <- ymd_hms(old$orderDate)


# Load in the lifetime file from mailplanner ------------------------------

# If there's no lifetime file in mailplanner, comment this out
dat_lifetime <- fread("../../../PII/Matchbacks/")

# Data wrangling ----------------------------------------------------------

# code to wrangle data, as needed, goes here

# standardize customerID field 
dat <- dat %>%
  mutate(customerID = tolower(trimws(customerID)))


# create ftoFlag for new customers/orders
# only the 1st order from a new customerID (that we don't have in mailplanner)
# should be fto
# get rid of prior customerIDs + only retain the first order from any new customer
dat_FTO <- dat %>%
  anti_join(dat_lifetime, by = "customerID") %>% # comment out if no lifetime file
  filter(!(is.na(customerID) | customerID == "")) %>%
  arrange(customerID, orderDate) %>%
  distinct(customerID, .keep_all = TRUE) %>%
  mutate(ftoFlag = TRUE)

# orders from missing customerIDs should be NA
dat <- dat %>%
  left_join(select(dat_FTO, orderID, ftoFlag), by = "orderID") %>% 
  mutate(ftoFlag = replace_na(ftoFlag, FALSE),
         ftoFlag = ifelse((customerID == "" | is.na(customerID)),
                          NA, ftoFlag)) # we want any order from a missing customerID to be NA



# Remove previous orders --------------------------------------------------

# any time period/orders already measured in a matchback is no longer
# needed! ensure order date is in a Date or POSIX class!


# date filters if running subsequent matchback for client:
if (exists("old")) {
  dat.cut <- dat.cut %>%
    filter(!(Name %in% old$Order.ID)) %>%
    filter(Created.at >= max(old$Date))
  
  dat.old <- dat.cut %>%
    filter(Name %in% old$Order.ID) %>%
    filter(Created.at <= max(old$Date))
}

# Check order ID is unique ------------------------------------------------

if (anyDuplicated(dat.cut$Name)) {
  stop("dupe order ID detected!")
}

# need to remove new lines in character fields
dat.cut <- dat.cut %>%
  mutate_if(is.character, function(y) gsub("\r?\n|\r", " ", y))


# write cleaned txfile ----------------------------------------------------

outputFilename <- paste0(client_folder, "_txfile_s",
                         date(min(dat.cut$orderDate)),
                         "_e", date(max(dat.cut$orderDate)),
                         ".csv")


fwrite(dat.cut, paste0("../../../PII/Matchbacks/", project_folder, "/",
                       outputFilename), row.names = FALSE)

# if you're working with an incremental file (and already have a lifetime one in mailplanner),
# comment out the below
historicalFilename <- paste0(client_folder, "_lifetime_txfile_s",
                             date(min(dat$orderDate)),
                             "_e", date(max(dat$orderDate)),
                             ".csv")

fwrite(dat, paste0("../../../PII/Matchbacks/", project_folder, "/",
                   historicalFilename), row.names = FALSE)

message(outputFilename, " is written to the PII folder! Don't forget to upload to mailplanner!")








