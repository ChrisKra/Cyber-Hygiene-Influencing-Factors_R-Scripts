library(readr)
library(dplyr)

# UNITED KINGDOM
## Survey 1
survey_uk_1 <- read_csv("01_data/01_raw/results-survey967691_main_uk_1.csv", 
                                 col_types = cols(submitdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                  lastpage = col_double(), startdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                  datestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                  CCH01Time = col_logical()))
survey_uk_1 <- survey_uk_1 %>% mutate(survey_name = "survey967691_main_uk_1", .before = 1)

# Drop first test row
survey_uk_1 <- survey_uk_1[-c(1), ]

## Correct for missing Age category in first UK Survey
survey_uk_1[survey_uk_1$id == "55", "DEM1"] <- "DEM17"
survey_uk_1[survey_uk_1$id == "139", "DEM1"] <- "DEM17"

## Survey 2
survey_uk_2 <- read_csv("01_data/01_raw/results-survey886788_main_uk_2.csv", 
                        col_types = cols(submitdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         lastpage = col_double(), startdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         datestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         CCH01Time = col_logical()))
survey_uk_2 <- survey_uk_2 %>% mutate(survey_name = "survey886788_main_uk_2", .before = 1)

# Drop first test row
survey_uk_2 <- survey_uk_2[-c(1), ]

# GERMANY
## Survey 1
survey_de_1 <- read_csv("01_data/01_raw/results-survey226331_main_de_1.csv", 
                        col_types = cols(submitdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         lastpage = col_double(), startdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         datestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         CCH01Time = col_logical()))
survey_de_1 <- survey_de_1 %>% mutate(survey_name = "survey226331_main_de_1", .before = 1)

# Drop first test row
survey_de_1 <- survey_de_1[-c(1), ]

## Survey 2
survey_de_2 <- read_csv("01_data/01_raw/results-survey456379_main_de_2.csv", 
                        col_types = cols(submitdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         lastpage = col_double(), startdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         datestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         CCH01Time = col_logical()))
survey_de_2 <- survey_de_2 %>% mutate(survey_name = "survey456379_main_de_2", .before = 1)

# Drop first test row
survey_de_2 <- survey_de_2[-c(1), ]

# Combine all surveys
survey <- bind_rows(survey_uk_1, survey_de_1, survey_uk_2, survey_de_2)

# Remove country specific surveys
rm(survey_de_1, survey_uk_1, survey_uk_2, survey_de_2)
