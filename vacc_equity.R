library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)

source("helpers.R")

# baselines: Prioritised ethnicity
popn_summary <- prioritised_ethnicity_by_dhb() %>%
  group_by(Ethnicity, Age) %>%
  summarise(Population = sum(Population))

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

vacc_summary <- all %>% map(get_data, doses=c(1,2)) %>% bind_rows()

final_data <- vacc_summary %>% left_join(popn_summary) %>%
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

final_data %>% count(Date)
final_data %>% count(Age)
final_data %>% count(Ethnicity)

curr_date <- get_latest_date()

colours <- get_pal("Hoiho")[c(3,4)]
png("vacc_equity.png", width=1920, height=640)
ggplot(final_data) +
  geom_hline(yintercept=1, col='black', size=1.2) +
  geom_line(aes(x=Date, y=RateRatio, group=Ethnicity), col = 'black', size=2.4) +
  geom_line(aes(x=Date, y=RateRatio, col=Ethnicity), size=2) +
  facet_wrap(vars(Age), ncol=4) +
  theme_minimal(base_size = 32) +
  theme(legend.position = "bottom") +
  scale_y_log10(labels=scales::label_percent()) +
  scale_x_date(date_breaks = "6 weeks",
               date_labels = "%d %b") +
  labs(x = NULL, y = "Vaccine rate ratio",
       title = paste("New Zealand COVID-19 vaccination equity to", format(curr_date, "%d %B %Y")),
       subtitle = "The horizontal line is equity. Above the line is more doses compared to the non-Māori, non-Pacific population, below is fewer."
       ) +
  scale_colour_manual(values = colours)
dev.off()
