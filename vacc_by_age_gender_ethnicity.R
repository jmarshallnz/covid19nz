library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)

source("helpers.R")

# baselines: Prioritised ethnicity
popn_summary <- prioritised_ethnicity_by_dhb() %>%
  group_by(Ethnicity, Age, Gender) %>%
  summarise(Pop2 = sum(Population))

# grab all the excel sheets
all <- list.files(path = "data/",
           pattern = ".xlsx", full.names=TRUE)

get_data <- function(filename, doses = c(1,2)) {
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
    group_by(Ethnicity, Age, Gender) %>%
    summarise(Doses=sum(Vacc)) %>%
    mutate(Ethnicity = fct_collapse(Ethnicity,
                                    `European or other` = c("European/Other", "European or other", "European / Other", "Other"),
                                    Maori = c("Māori", "Maori"))) %>%
    filter(Gender != "Other / Unknown",
           Gender != "Unknown/Other",
           Ethnicity != 'Other') %>%
    ungroup() %>%
    mutate(Date = date)
  
  vacc_summary
}

vacc_summary <- all %>% map(get_data, doses = 1) %>% bind_rows()

final_data <- vacc_summary %>% left_join(popn_summary) %>%
  filter(Ethnicity != "Unknown", Age != "90+/Unknown",
         Age != "90 + years / Unknown") %>%
  mutate(Ethnicity = fct_recode(Ethnicity, Māori = "Maori"))

final_data %>% count(Date)
final_data %>% count(Age)
final_data %>% count(Ethnicity)

curr_date <- get_latest_date()

png("vacc_by_popn_through_time.png", width=1920, height=1080)
ggplot(final_data) +
  geom_line(aes(x=Date, y=Doses/Population, group=Ethnicity), col = 'black', size=2.4) +
  geom_line(aes(x=Date, y=Doses/Population, col=Ethnicity), size=2) +
  facet_grid(vars(Gender), vars(Age)) +
  theme_minimal(base_size = 32) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_date(date_breaks = "6 weeks",
               date_labels = "%d %b") +
  labs(x = NULL, y = "Share of people with one or more dose",
       title = paste("New Zealand COVID-19 vaccination uptake by age, gender, and ethnicity at", format(curr_date, "%d %B %Y")),
       subtitle = "Excludes unknown gender, ethnicity and age"
       ) +
  scale_colour_manual(values = get_pal("Hoiho"))
dev.off()
