
# Q Status

library(tidyverse)
library(Hmisc)


# Load Data ---------------------------------------------------------------

avt18_raw <- read.csv("All Valid Tests 2018V5.csv", 
                  header = TRUE, stringsAsFactors = FALSE)


# Ranking -----------------------------------------------------------------

# statwide ranking by TestCode

avt18 <- avt18_raw %>%
    select(StID, schnumb, AGAID, TestCode, SS) %>%
    filter(TestCode != "SCI4" & TestCode != "SCI7" & TestCode != "SCI11") %>%
    group_by(TestCode) %>%
    mutate(Qstatus = cut(SS, 4, labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
    arrange(TestCode, Qstatus, SS)

avt18
table(avt18$Qstatus)

avt18 %>%
    group_by(TestCode) %>%
    count(Qstatus)
