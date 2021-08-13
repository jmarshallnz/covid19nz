library(tidyverse)
library(lubridate)
library(readxl)

# helper function to grab the latest spreadsheet file from the data/ folder
get_latest_sheet <- function(weeks_ago = 0) {
  vacc_data <- data.frame(file = list.files(path = "data",
                                            pattern = ".xlsx", full.names=TRUE))
  vacc_data %>%
    mutate(week = dmy(sub(".*_([0-9]+_[0-9]+_2021)(.*)xlsx", "\\1", file))) %>%
    arrange(desc(week)) %>%
    slice(weeks_ago + 1) %>%
    pull(file)
}

prioritised_ethnicity_by_dhb <- function() {
  # baselines: Prioritised ethnicity from MoH by DHB
  popn <- read_excel("data/dhb_projections/2020-21 Population Projections.xlsx", sheet=1, skip=1)
  popn_summary <- popn %>% select(DHB = DHB_name, Ethnicity, Gender=Sex, Age = Age_Group, Population = pop2020_2021) %>%
    mutate(Ethnicity = fct_collapse(Ethnicity,
                                    "European or other" = "Other",
                                    "Pacific Peoples" = "Pacific")) %>%
    extract(Age, into="Age", regex="([[:digit:]]+)", convert=TRUE) %>%
    mutate(Age = (Age %/% 10) * 10,
           Age = paste(Age, 'to', Age + 9),
           Age = if_else(Age == "90 to 99",
                         "90+/Unknown", Age)) %>%
    mutate(DHB = fct_collapse(DHB,
                              "Auckland Metro" = c("Auckland", "Waitemata", "Counties Manukau"),
                              "Capital & Coast and Hutt Valley" = c("Capital and Coast", "Hutt"))) %>%
    group_by(DHB, Ethnicity, Age, Gender) %>%
    summarise(Population = sum(Population)) %>%
    ungroup()
  
  popn_summary
}

dhb_population <- function() {
  popn <- read_csv("data/Subnational population estimates (DHB, DHB constituency), by age and sex, at 30 June 1996-2020 (2020 boundaries)/TABLECODE7509_Data_21d61ee0-b582-400b-8deb-d973e38d64ac.csv")

  # highly inefficient code to munge data for match
  popn_summary <- popn %>%
    filter(`Year at 30 June` == 2020) %>%
    select(DHB=Area, Gender=Sex, Age, Population=Value) %>%
    mutate(Age = fct_collapse(Age,
                              `10 to 19` = c("10-14 Years", "15-19 Years"),
                              `20 to 29` = c("20-24 Years", "25-29 Years"),
                              `30 to 39` = c("30-34 Years", "35-39 Years"),
                              `40 to 49` = c("40-44 Years", "45-49 Years"),
                              `50 to 59` = c("50-54 Years", "55-59 Years"),
                              `60 to 69` = c("60-64 Years", "65-69 Years"),
                              `70 to 79` = c("70-74 Years", "75-79 Years"),
                              `80 to 89` = c("80-84 Years", "85-89 Years"),
                              `90 + years / Unknown` = c("90 Years and over"))) %>%
    mutate(DHB = fct_collapse(DHB,
                              "Auckland Metro" = c("Auckland", "Waitemata", "Counties Manukau"),
                              "Hawkes Bay" = "Hawke's Bay",
                              "Capital & Coast and Hutt Valley" = c("Capital and Coast", "Hutt Valley"))) %>%
    group_by(DHB, Age) %>%
    summarise(Population = sum(Population)) %>%
    ungroup()

  popn_summary
}