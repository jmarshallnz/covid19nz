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
