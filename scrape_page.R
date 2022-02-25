library(tidyverse)
library(rvest)

page <- "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-vaccine-data"

foo <- read_html(page)

# Date:
current_date <- foo %>% html_element(".well-sm") %>% html_element("p") %>% html_text() %>%
  sub(".*?([0-9]+) (.*) 2022.*", "\\1-\\2-2022", x=.) %>%
  lubridate::dmy()

print(current_date)
# Table: This doesn't seem particularly robust. How can we subset the html nodes down to just what we want?

tabs <- foo %>% html_elements("h4 + table")

read_vacc_table <- function(tab, current_date) {
  # Change at 25 November
  if (all(c("Partially vaccinated", "Fully vaccinated") %in% names(tab))) {
    names(tab)[names(tab) == ""] <- paste("empty", seq_len(sum(names(tab) == "")))
    tab <- tab %>% rename(`First doses` = "Partially vaccinated",
                          `Second doses` = "Fully vaccinated")
  }

  # Change at 1 December
  if (all(c("At least partially vaccinated", "Fully vaccinated") %in% names(tab))) {
    names(tab)[names(tab) == ""] <- paste("empty", seq_len(sum(names(tab) == "")))
    tab <- tab %>% rename(`First doses` = "At least partially vaccinated",
                          `Second doses` = "Fully vaccinated")
  }
  
  # Change at 18 Feb: boosters!
  if (all(c("At least partially vacc", "Fully vacc") %in% names(tab))) {
    names(tab)[names(tab) == ""] <- paste("empty", seq_len(sum(names(tab) == "")))
    tab <- tab %>% rename(`First doses` = "At least partially vacc",
                          `Second doses` = "Fully vacc",
                          `Boosters` = "Received Booster")
  }

  final <- tab %>% select(DHB = 1, Dose1 = 'First doses', Dose2 = 'Second doses', Dose3 = 'Boosters', Eligible = 'Eligible for Booster', Population) %>%
    mutate(across(-DHB, readr::parse_number)) %>%
    mutate(Date = current_date)
  final
}

read_firstdose_table <- function(tab, current_date) {
  final <- tab %>% select(DHB = 1, Dose1 = 'At least partially vacc',
                          Dose2 = 'Fully vacc', Population) %>%
    mutate(across(-DHB, readr::parse_number)) %>%
    mutate(Date = current_date)
  final
}

out_file <- sprintf("%s.csv", current_date)
read_vacc_table(tabs[[1]] %>% html_table(), current_date) %>%
  write_csv(file.path("data/dhb_daily", out_file))

read_vacc_table(tabs[[2]] %>% html_table(), current_date) %>%
  write_csv(file.path("data/dhb_daily/maori", out_file))

read_vacc_table(tabs[[3]] %>% html_table(), current_date) %>%
  write_csv(file.path("data/dhb_daily/pacific", out_file))

read_firstdose_table(tabs[[4]] %>% html_table(), current_date) %>%
  write_csv(file.path("data/dhb_daily/5-11", out_file))

read_firstdose_table(tabs[[5]] %>% html_table(), current_date) %>%
  write_csv(file.path("data/dhb_daily/5-11/maori", out_file))

read_firstdose_table(tabs[[6]] %>% html_table(), current_date) %>%
  write_csv(file.path("data/dhb_daily/5-11/pacific", out_file))
