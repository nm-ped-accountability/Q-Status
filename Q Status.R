
# Q Status

rm(list = ls())
library(tidyverse)
library(Hmisc)



# Load Data ---------------------------------------------------------------

master_schools <- read.csv("MasterSchoolsAllYearsCorrected2018V7.csv", 
                           header = TRUE, stringsAsFactors = FALSE)

avt18_raw <- read.csv("All Valid Tests 2018V5.csv", 
                  header = TRUE, stringsAsFactors = FALSE)

schools2018 <- master_schools[master_schools$ï..SY == 2018, ]


avt17_raw <- read.csv("All Valid Tests 2017 V10.csv",
                      header = TRUE, stringsAsFactors = FALSE)

schools2017 <- master_schools[master_schools$ï..SY == 2017, ]
   


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


# # define function "counts_by_Q"
# counts_by_Q <- function(data_file, school_file) {
#     data_file %>%
#         group_by(schnumb) %>%
#         count(Qstatus) %>%
#         spread(key = Qstatus, value = n) %>%
#         mutate(total_students = Q1 + Q23 + Q4,
#                percent_Q1 = Q1 / total_students,
#                percent_Q23 = Q23 / total_students,
#                percent_Q4 = Q4 / total_students) %>%
#         left_join(school_file, by = "schnumb") %>%
#         select(schnumb, AGAID, distname, schname, Q1, Q23, Q4,
#                total_students, percent_Q1, percent_Q23, percent_Q4)
#     }



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

# counts18 <- counts_by_Q(avt18_Q, schools2018)
# counts18

# # save output
# current_date <- Sys.Date()
# file_name <- paste0("Counts 2018 by Q Status by School ", current_date, ".csv")
# write.csv(avt18, file = file_name, row.names = FALSE)



# Prior2 Year Q Status ----------------------------------------------------

# school year 2016-2017
avt17_Q <- Q_status(raw_data = avt17_raw)
avt17_Q$SY <- 2017
avt17_Q

# check for duplicates by subject
avt17_Q %>%
    filter(Subtest == "READ") %>%
    group_by(StID, Subtest) %>%
    filter(n() > 1)


# save output
current_date <- Sys.Date()
file_name <- paste0("Prior2 Q Status (2016-2017) ", current_date, ".csv")
write.csv(avt17_Q, file = file_name, row.names = FALSE)




# Select the Earliest Possible Q Status -----------------------------------
combined <- rbind(avt17_Q, avt18_Q)
combined[combined$StID == 103342069, ]
