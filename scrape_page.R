library(tidyverse)
library(rvest)

page <- "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-vaccine-data"

foo <- read_html(page)

# Date:
current_date <- foo %>% html_element(".well-sm") %>% html_element("p") %>% html_text() %>%
  sub(".*?([0-9]+) (.*) 2021.*", "\\1-\\2-2021", x=.) %>%
  lubridate::dmy()

print(current_date)
# Table: This doesn't seem particularly robust. How can we subset the html nodes down to just what we want?

tab <- foo %>% html_element("h4 + table") %>% html_table()

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

final <- tab %>% select(DHB = 1, Dose1 = 'First doses', Dose2 = 'Second doses', Population) %>%
  mutate(across(-DHB, readr::parse_number)) %>%
  mutate(Date = current_date)

out_file <- sprintf("%s.csv", current_date)
write_csv(final, file.path("data/dhb_daily", out_file))
