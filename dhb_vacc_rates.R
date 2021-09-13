library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)
# JM hacked version of DHBins: https://github.com/jmarshallnz/DHBins/tree/covid_dhbs
# remotes::install_github('jmarshallnz/DHBins', ref="covid_dhbs")
library(DHBins)
library(gganimate)

vacc_dat <- read_excel("data/rate_ratio_and_vaccine_uptake_over_time_-_by_dhb_ethnicity_and_age.xlsx",
                       sheet=2) %>%
  rename(Week = `Week ending date`,
         Dose = `Dose number`,
         Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Vacc = `# doses administered`) %>%
  filter(DHB != "Overseas and undefined",
         DHB != "Unknown",
         Dose != 0)

popn_dat <- read_excel("data/rate_ratio_and_vaccine_uptake_over_time_-_by_dhb_ethnicity_and_age.xlsx",
                       sheet=3) %>%
  rename(Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Population = `# people (HSU)`)

vacc_dat %>% count(Age)
popn_dat %>% count(Age)
vacc_dat %>% count(DHB)
popn_dat %>% count(DHB)

all_dat <- vacc_dat %>% complete(Week, DHB, Age, Ethnicity, Dose, fill=list(Vacc=0)) %>%
  left_join(popn_dat)

plot_me <- all_dat %>% 
  filter(Age %in% c("20 to 24", "25 to 29")) %>%
  group_by(Week, DHB) %>%
  summarise(Vacc = sum(Vacc), Population = sum(Population, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(Rate = Vacc/Population/7*2)

highlight_increase <- plot_me %>%
  filter(Week >= max(Week) - time_length("7 days")) %>%
  group_by(DHB) %>%
  select(DHB, Week, Rate) %>%
  pivot_wider(names_from=Week, values_from=Rate) %>%
  mutate(highlight = `2021-09-05` > `2021-08-29`, .keep='unused')

png("dhb_vacc_rates_20_20.png", width=1280, height=720)
ggplot(plot_me %>% left_join(highlight_increase)) +
  geom_line(aes(x=Week, y=Rate, size=highlight)) +
  facet_wrap(vars(DHB)) +
  theme_minimal(base_size=24) +
  scale_size_manual(values = c(1,2)) +
  scale_y_continuous(labels = scales::label_percent()) +
  guides(size='none') +
  labs(x = NULL,
       y = "Population dosed per day",
       title = "DHB vaccination performance (20-29 year olds) over time",
       subtitle = "Highlighted DHBs increased rates in week to 5 September.")
dev.off()


