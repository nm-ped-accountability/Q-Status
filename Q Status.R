
# Q Status

library(tidyverse)
library(Hmisc)


# Load Data ---------------------------------------------------------------

avt18_raw <- read.csv("All Valid Tests 2018V5.csv", 
                  header = TRUE, stringsAsFactors = FALSE)

master_schools <- read.csv("MasterSchoolsAllYearsCorrected2018V7.csv", 
                           header = TRUE, stringsAsFactors = FALSE)
schools2018 <- master_schools[master_schools$ï..SY == 2018, ]


# Ranking -----------------------------------------------------------------

# statwide ranking by TestCode

# by score
avt18 <- avt18_raw %>%
    select(StID, schnumb, AGAID, TestCode, SS) %>%
    filter(TestCode != "SCI4" & TestCode != "SCI7" & TestCode != "SCI11") %>%
    filter(TestCode == "ALG01") %>%
    group_by(TestCode) %>%
    mutate(Qstatus = cut(SS, 
                         breaks = quantile(SS, probs = seq(0, 1, by = 0.25), 
                                           na.rm = TRUE), 
                         include.lowest = TRUE, 
                         labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
    mutate(percentile = ntile(SS, 100)) %>%
    arrange(TestCode, Qstatus, SS)

avt18 <- avt18_raw %>%
    select(StID, schnumb, AGAID, TestCode, SS) %>%
    filter(TestCode != "SCI4" & TestCode != "SCI7" & TestCode != "SCI11") %>%
    filter(TestCode == "ALG01") %>%
    group_by(TestCode) %>%
    mutate(Qstatus = as.numeric(cut2(SS, g = 4))) %>%
    arrange(TestCode, Qstatus, SS)



# by student
avt18 <- avt18_raw %>%
    select(StID, schnumb, AGAID, TestCode, SS) %>%
    filter(TestCode != "SCI4" & TestCode != "SCI7" & TestCode != "SCI11") %>%
    group_by(TestCode) %>%
    mutate(Quantile = ntile(SS, 4),
           Percentile = ntile(SS, 100),
           Qstatus = ifelse(Quantile == 1, "Q1",
                            ifelse(Quantile == 4, "Q4", "Q23"))) %>%
    arrange(TestCode, Quantile, SS)

avt18

# counts by school
schools <- avt18 %>%
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

schools
write.csv(schools, "Qstatus statewide 2018.csv", row.names = FALSE, na = "")
    

# Ranking by TestCode and by schnumb --------------------------------------
avt18_raw[avt18_raw$TestCode == "GEO01" & avt18_raw$schnumb == 1435, ]

counts_by_school <- avt18_raw %>%
    group_by(schnumb, TestCode) %>%
    count() %>%
    arrange(n)
write.csv(counts_by_school, "counts_by_school.csv", row.names = FALSE, na = "")

counts_by_school[counts_by_school$n == 3, ]
    
avt18_school <- avt18_raw %>%
    select(StID, schnumb, AGAID, TestCode, SS) %>%
    filter(TestCode != "SCI4" & TestCode != "SCI7" & TestCode != "SCI11") %>%
    group_by(schnumb, TestCode) %>%
    mutate(Quantile = ntile(SS, 4),
           Percentile = ntile(SS, 100),
           Qstatus = ifelse(Quantile == 1, "Q1",
                            ifelse(Quantile == 4, "Q4", "Q23"))) %>%
    arrange(schnumb, TestCode, Quantile, SS)

write.csv(avt18_school, "avt18_school.csv", row.names = FALSE, na = "")

# counts by school
schools_2 <- avt18_school %>%
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

schools_2
write.csv(schools_2, "Qstatus schoolwide 2018.csv", row.names = FALSE, na = "")
