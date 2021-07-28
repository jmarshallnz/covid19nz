library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(Manu)

# baselines
popn <- read_csv("data/National ethnic population projections, by age and sex, 2018(base)-2043/TABLECODE7994_Data_babcb185-dbde-40f7-b40e-bb32a6c04373.csv")

# highly inefficient code to munge data for match
popn_summary <- popn %>% select(Ethnicity, Gender=Sex, Age, Population=Value) %>%
  mutate(Ethnicity = case_when(Ethnicity == "European or Other (including New Zealander)" ~ "European or other",
                               Ethnicity == "Pacific" ~ "Pacific Peoples",
                               TRUE ~ Ethnicity)) %>%
  mutate(Age = fct_collapse(Age,
                          `10 to 19` = c("10-14 years", "15-19 years"),
                          `20 to 29` = c("20-24 years", "25-29 years"),
                          `30 to 39` = c("30-34 years", "35-39 years"),
                          `40 to 49` = c("40-44 years", "45-49 years"),
                          `50 to 59` = c("50-54 years", "55-59 years"),
                          `60 to 69` = c("60-64 years", "65-69 years"),
                          `70 to 79` = c("70-74 years", "75-79 years"),
                          `80 to 89` = c("80-84 years", "85-89 years"),
                          `90+/Unknown` = c("90 years and over"))) %>%
  group_by(Ethnicity, Age, Gender) %>%
  summarise(Population = sum(Population)) %>%
  ungroup()

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

png("vacc_by_popn_through_time.png", width=1920, height=1080)
ggplot(final_data %>% filter(Age != "10 to 19")) +
  geom_line(aes(x=Date, y=Doses/Population, group=Ethnicity), col = 'black', size=2.4) +
  geom_line(aes(x=Date, y=Doses/Population, col=Ethnicity), size=2) +
  facet_grid(vars(Gender), vars(Age)) +
  theme_minimal(base_size = 32) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_date(date_breaks = "6 weeks",
               date_labels = "%d %b") +
  labs(x = NULL, y = "Share of people with one or more doses",
       title = "New Zealand COVID-19 vaccination uptake by age, gender, and ethnicity",
       subtitle = "Excludes unknown gender, ethnicity, age, and the MELAA level 1 ethnic group (no vaccination data)"
       ) +
  scale_colour_manual(values = get_pal("Hoiho"))
dev.off()
