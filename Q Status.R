
# Q Status

rm(list = ls())
library(tidyverse)
library(Hmisc)



# Load Data ---------------------------------------------------------------

# master schools
master_schools <- read.csv("Master Schools 2019 V4.csv", 
                           header = TRUE, stringsAsFactors = FALSE)

# prior1 data
avt18_raw <- read.csv("All Valid Tests 2018V5.csv", 
                  header = TRUE, stringsAsFactors = FALSE)

schools2018 <- master_schools[master_schools$ï..SY == 2018, ]
nrow(schools2018) # N = 1788


# current year data

   


# Define Functions --------------------------------------------------------

# define function "Q_status"

Q_status <- function(raw_data) {
    
    raw_data %>%
        
        # select columns
        select(StID, schnumb, AGAID, Subtest, TestCode, SS, Istationtime) %>%
        
        # remove science assessments
        filter(Subtest != "SCI") %>%
        
        # remove Istationtime 1 and 2
        filter(is.na(Istationtime) | Istationtime == 3) %>%
        select(-Istationtime) %>%
        
        # rank by schnumb and testcode to determine Q status
        group_by(schnumb, TestCode) %>%
        mutate(n_students = n(),
               Quantile = ntile(SS, 4),
               Percentile = ntile(SS, 100),
               Qstatus = ifelse(Quantile == 1, "Q1",
                                ifelse(Quantile == 4, "Q4", "Q23"))) %>%
        
        # if there are fewer than 4 students for a testcode within a school,
        # then these 1-3 student(s) will be assigned to Q23
        mutate(Qstatus = ifelse(n_students < 4, "Q23", Qstatus)) %>%
        
        # sort data
        arrange(schnumb, TestCode, Quantile, SS)
}



# Prior1 Year Q Status ----------------------------------------------------

# school year 2017-2018
avt18_Q <- Q_status(raw_data = avt18_raw)
avt18_Q$SY <- 2018
avt18_Q

# check for duplicates by subject
avt18_Q %>%
    group_by(StID, Subtest) %>%
    filter(n() > 1)

# check counts
table(avt18_Q$Qstatus)
table(avt18_Q$Qstatus, avt18_Q$TestCode)

# save output
current_date <- Sys.Date()
file_name <- paste0("Prior1 Q Status (2017-2018) ", current_date, ".csv")
write.csv(avt18_Q, file = file_name, row.names = FALSE)



# Current Year Q Status ---------------------------------------------------

