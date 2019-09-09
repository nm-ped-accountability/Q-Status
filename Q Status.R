
# Q Status

rm(list = ls())
library(tidyverse)
library(Hmisc)


# Load Data ---------------------------------------------------------------

avt18_raw <- read.csv("All Valid Tests 2018V5.csv", 
                  header = TRUE, stringsAsFactors = FALSE)

master_schools <- read.csv("MasterSchoolsAllYearsCorrected2018V7.csv", 
                           header = TRUE, stringsAsFactors = FALSE)
schools2018 <- master_schools[master_schools$ï..SY == 2018, ]
    

# Ranking by TestCode and by schnumb --------------------------------------

# if there are fewer than four students for a testcode within a school,
# then these 1-3 student(s) will be assigned to Q23
avt18 <- avt18_raw %>%
    select(StID, schnumb, AGAID, TestCode, SS) %>%
    filter(TestCode != "SCI4" & TestCode != "SCI7" & TestCode != "SCI11") %>%
    group_by(schnumb, TestCode) %>%
    mutate(n = n(),
           Quantile = ntile(SS, 4),
           Percentile = ntile(SS, 100),
           Qstatus = ifelse(Quantile == 1, "Q1",
                            ifelse(Quantile == 4, "Q4", "Q23"))) %>%
    mutate(Qstatus = ifelse(n < 4, "Q23", Qstatus)) %>%
    arrange(schnumb, TestCode, Quantile, SS)

# save outputs
current_date <- Sys.Date()
file_name <- paste0("AVT 2018 with Q Status ", current_date, ".csv")
write.csv(avt18, file = file_name, row.names = FALSE)



# counts by school
counts <- avt18 %>%
    group_by(schnumb) %>%
    count(Qstatus) %>%
    spread(key = Qstatus, value = n) %>%
    mutate(n_students = Q1 + Q23 + Q4,
           percent_Q1 = Q1 / n_students,
           percent_Q23 = Q23 / n_students,
           percent_Q4 = Q4 / n_students) %>%
    left_join(schools2018, by = "schnumb") %>%
    select(schnumb, AGAID, distname, schname, Q1, Q23, Q4, 
           n_students, percent_Q1, percent_Q23, percent_Q4)

# save outputs
current_date <- Sys.Date()
file_name <- paste0("Counts 2018 by Q Status by School ", current_date, ".csv")
write.csv(avt18, file = file_name, row.names = FALSE)




