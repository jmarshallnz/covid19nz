library(tidyverse)
library(lubridate)
library(readxl)

# helper function to grab the date of latest spreadsheet file from data/ folder
get_latest_date <- function() {
  vacc_data <- data.frame(file = list.files(path = "data",
                                            pattern = ".xlsx", full.names=TRUE))
  vacc_data %>%
    mutate(week = dmy(sub(".*_([0-9]+_[0-9]+_2021)(.*)xlsx", "\\1", file))) %>%
    arrange(desc(week)) %>%
    slice(1) %>%
    pull(week)
}

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

hsu_ethnicity_by_dhb <- function(file, by_gender = TRUE) {
  # baselines: HSU ethnicity from MoH by DHB
  popn <- read_excel(file, sheet="HSU Population") %>%
    select(Age = `Age group`, Ethnicity = `Ethnic group`,
           Gender, DHB = `DHB of residence`, Population)
  # redo to the age groups we likely want.
  popn_summary <- popn %>%
    filter(Age != "Various",
           DHB != "Overseas / Unknown") %>%
    mutate(Age = fct_recode(Age, `10 to 14` = "12 to 14"),
           Ethnicity = fct_collapse(Ethnicity, `European or other` = c("Unknown", "European or Other")))

  # filter out the unknown gender if that is being used, else combine
  if (by_gender) {
    popn_summary <- popn_summary %>% filter( Gender != "Unknown/Other")
  } else {
    popn_summary <- popn_summary %>%
      group_by(Age, Ethnicity, DHB) %>%
      summarise(Population = sum(Population)) %>% ungroup()
  }

  popn_5_years <- popn_summary %>% filter(str_detect(Age, "to"))
  popn_65_plus <- popn_summary %>% filter(Age == "65 and over") %>%
    select(-Age)

  # assume that the proportional splits in 65 plus are the same as prioritised ethnicity
  # age splits (this is not true, but will be OK for what we want)
  prioritised <- read_excel("data/dhb_projections/2020-21 Population Projections.xlsx", sheet=1, skip=1) %>%
    select(DHB = DHB_name, Ethnicity, Gender=Sex, Age = Age_Group, Population = pop2020_2021) %>%
    extract(Age, into="Age", regex="([[:digit:]]+)", convert=TRUE) %>%
    mutate(DHB = fct_collapse(DHB,
                              "Auckland Metro" = c("Auckland", "Waitemata", "Counties Manukau"),
                              "Capital & Coast and Hutt Valley" = c("Capital and Coast", "Hutt"))) %>%
    mutate(Ethnicity = fct_recode(Ethnicity,
                                    "European or other" = "Other",
                                    "Pacific Peoples" = "Pacific")) %>%
    filter(Age >= 65)

  # if we want gender, then it's straight forward
  if (by_gender) {
    prioritised <- prioritised %>%
      group_by(DHB, Ethnicity, Gender)
  } else { # otherwise, first summarise by gender
    prioritised <- prioritised %>%
      group_by(Ethnicity, DHB, Age) %>%
      summarise(Population = sum(Population))
  }
  prioritised_done <- prioritised %>%
    mutate(Prop = Population/sum(Population), .keep='unused')

  # Now join and compute proportions in 65+ categories
  popn_65_div <- popn_65_plus %>% left_join(prioritised_done) %>%
    mutate(Population = round(Population * Prop), .keep='unused')

  # And combine and convert ages
  combined_popn <- popn_5_years %>% extract(Age, into="Age", regex="([[:digit:]]+) ", convert=TRUE) %>%
    bind_rows(popn_65_div) %>%
    mutate(Age = (Age %/% 10) * 10,
           Age = paste(Age, 'to', Age + 9),
           Age = if_else(Age == "90 to 99",
                "90+/Unknown", Age)) %>%
    group_by(DHB, Ethnicity, Age)

  # add the gender group
  if (by_gender) {
    combined_popn <- combined_popn %>%
      group_by(Gender, .add=TRUE)
  }

  combined_popn %>%
    summarise(Population = sum(Population)) %>%
    ungroup()
}

