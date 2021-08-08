library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)

source("helpers.R")

popn_summary <- dhb_population()

# latest spreadsheet
read_vacc_sheet <- function(file) {
  vacc <- read_excel(file, sheet = "DHBofResidence by ethnicity")
  
  vacc_dhbs <- vacc %>% select(DHB = `DHB of residence`,
                               Age = `Ten year age group`,
                               Gender, Dose = `Dose number`,
                               Vacc = `# doses administered`) %>%
    filter(DHB != "Overseas / Unknown",
           Age != "90+/Unknown",
           Age != "90 + years / Unknown") %>%
    group_by(DHB, Age, Dose) %>%
    summarise(Vacc = sum(Vacc)) %>%
    left_join(popn_summary) %>%
    group_by(DHB, Age, Dose) %>%
    summarise(Vacc = sum(Vacc),
              Population = sum(Population)) %>%
    mutate(DHB = fct_recode(DHB,
                            "Wellington Metro" = "Capital & Coast and Hutt Valley"))

  # check counts:
  vacc_dhbs %>% group_by(Dose) %>%
    summarise(sum(Vacc), sum(Population)) %>%
    print()

  vacc_dhbs
}

current <- read_vacc_sheet(get_latest_sheet())
previous <- read_vacc_sheet(get_latest_sheet(weeks_ago = 1))

colours <- get_pal("Takahe")[c(1,3)]

this_week <- current %>% left_join(previous %>% rename(Prev = Vacc)) %>%
  mutate(Vacc = Vacc - Prev) %>%
  mutate(Dose = as_factor(Dose),
         Dose = fct_recode(Dose, `One dose` = '1', `Fully vaccinated` = '2'),
         Dose = fct_relevel(Dose, "Fully vaccinated")) %>%
  extract(Age, into="Age", regex="([[:digit:]]+)", convert=TRUE)

png("dhb_by_age_this_week.png", width=1980, height=1080)
ggplot(this_week) +
  geom_col(mapping=aes(x=Age, y=Vacc/Population*100, fill=Dose), alpha=0.9, col='black', size=0.2) +
  facet_wrap(vars(DHB), ncol=6) +
  scale_x_continuous(breaks=seq(5,85,by=20), labels=seq(10,90,by=20),
                     minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = colours) +
  labs(x = NULL, y = "Doses per 100 people in each age group", fill = NULL,
       title = "COVID-19 vaccinations by District Health Boards last week",
       subtitle = "The age groups being vaccinated differ by DHB") +
  theme_minimal(base_size=36) +
  theme(legend.position = 'bottom')
dev.off()
