library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)

source(here::here("helpers.R"))

# baselines: Prioritised ethnicity
popn_dhb <- prioritised_ethnicity_by_dhb() %>%
  group_by(DHB, Ethnicity, Age) %>%
  summarise(Population = sum(Population))

popn_nz <- popn_dhb %>%
  group_by(Ethnicity, Age) %>%
  summarise(Population = sum(Population))

# grab all the excel sheets
all <- list.files(path = here::here("data"),
                  pattern = ".xlsx", full.names=TRUE)

get_dhb_data <- function(filename, doses = c(1,2)) {
  # vaccinations by various criteria
  cat("working on: ", filename, "\n")
  sheet_names <- c("DHBofResidence by ethnicity")
  sheets <- excel_sheets(filename)
  this_sheet <- first(sheet_names[sheet_names %in% sheets])
  vacc <- read_excel(filename, sheet=this_sheet)
  if (!("Ten year age group" %in% names(vacc))) {
    return(NULL)
  }
  date <- dmy(sub(".*_([0-9]+_[0-9]+_2021)(.*)xlsx", "\\1", filename))
  vacc_summary <- vacc %>% select(DHB = `DHB of residence`,
                                  Ethnicity = `Ethnic group`,
                                  Age = `Ten year age group`,
                                  Gender, Dose = `Dose number`,
                                  Vacc = `# doses administered`) %>%
    filter(Dose %in% doses) %>%
    group_by(DHB, Ethnicity, Age) %>%
    summarise(Doses=sum(Vacc)) %>%
    mutate(Ethnicity = fct_collapse(Ethnicity,
                                    `European or other` = c("European/Other", "European or other", "European / Other", "Other"),
                                    Maori = c("Māori", "Maori"))) %>%
    #    filter(Gender != "Other / Unknown",
    #           Gender != "Unknown/Other") %>%
    ungroup() %>%
    mutate(Date = date)

  vacc_summary
}

get_nz_data <- function(filename, doses = c(1,2)) {
  # vaccinations by various criteria
  cat("working on: ", filename, "\n")
  sheet_names <- c("Ethnicity, Age, Gender by dose", "DHBofResidence by ethnicity")
  sheets <- excel_sheets(filename)
  this_sheet <- first(sheet_names[sheet_names %in% sheets])
  vacc <- read_excel(filename, sheet=this_sheet)
  date <- dmy(sub(".*_([0-9]+_[0-9]+_2021)(.*)xlsx", "\\1", filename))
  vacc_summary <- vacc %>% select(Ethnicity = `Ethnic group`,
                                  Age = `Ten year age group`,
                                  Gender, Dose = `Dose number`,
                                  Vacc = `# doses administered`) %>%
    filter(Dose %in% doses) %>%
    group_by(Ethnicity, Age) %>%
    summarise(Doses=sum(Vacc)) %>%
    mutate(Ethnicity = fct_collapse(Ethnicity,
                                    `European or other` = c("European/Other", "European or other", "European / Other", "Other"),
                                    Maori = c("Māori", "Maori"))) %>%
    #    filter(Gender != "Other / Unknown",
    #           Gender != "Unknown/Other") %>%
    ungroup() %>%
    mutate(Date = date)
  
  vacc_summary
}

vacc_dhb <- all %>% map(get_dhb_data, doses = c(1,2)) %>% bind_rows()
vacc_nz <- all %>% map(get_nz_data, doses=c(1,2)) %>% bind_rows()

dhb_data <- vacc_dhb %>% left_join(popn_dhb) %>%
  filter(Ethnicity != "Unknown", Age != "90+/Unknown",
         Age != "90 + years / Unknown",
         DHB != "Overseas / Unknown") %>%
  mutate(Ethnicity = fct_collapse(Ethnicity,
                                  Baseline = c("European or other", "Asian"),
                                  Māori = "Maori")) %>%
  mutate(Age = fct_collapse(Age,
                            "10 to 29" = c("10 to 19", "20 to 29"),
                            "30 to 49" = c("30 to 39", "40 to 49"),
                            "50 to 69" = c("50 to 59", "60 to 69"),
                            "70 to 89" = c("70 to 79", "80 to 89"))) %>%
  group_by(Ethnicity, Age, Date, DHB) %>%
  summarise(Doses = sum(Doses),
            Population = sum(Population)) %>%
  ungroup() %>%
  mutate(Rate = Doses/Population, .keep='unused') %>%
  pivot_wider(names_from = Ethnicity, values_from = Rate) %>%
  pivot_longer(-c(DHB, Date, Age, Baseline), names_to = "Ethnicity", values_to="Rate") %>%
  mutate(RateRatio = Rate/Baseline, .keep='unused') %>%
  mutate(DHB = fct_recode(DHB, `Wellington Metro` = "Capital & Coast and Hutt Valley"))

nz_data <- vacc_nz %>% left_join(popn_nz) %>%
  filter(Ethnicity != "Unknown", Age != "90+/Unknown",
         Age != "90 + years / Unknown") %>%
  mutate(Ethnicity = fct_collapse(Ethnicity,
                                  Baseline = c("European or other", "Asian"),
                                  Māori = "Maori")) %>%
  mutate(Age = fct_collapse(Age,
                            "10 to 29" = c("10 to 19", "20 to 29"),
                            "30 to 49" = c("30 to 39", "40 to 49"),
                            "50 to 69" = c("50 to 59", "60 to 69"),
                            "70 to 89" = c("70 to 79", "80 to 89"))) %>%
  group_by(Ethnicity, Age, Date) %>%
  summarise(Doses = sum(Doses),
            Population = sum(Population)) %>%
  ungroup() %>%
  mutate(Rate = Doses/Population, .keep='unused') %>%
  pivot_wider(names_from = Ethnicity, values_from = Rate) %>%
  pivot_longer(-c(Date, Age, Baseline), names_to = "Ethnicity", values_to="Rate") %>%
  mutate(RateRatio = Rate/Baseline, .keep='unused')

equity <- bind_rows(dhb_data, nz_data %>% mutate(DHB = "New Zealand"))

write_csv(equity, here::here("equity/equity.csv"))
