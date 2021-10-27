library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)
# JM hacked version of DHBins: https://github.com/jmarshallnz/DHBins/tree/covid_dhbs
# remotes::install_github('jmarshallnz/DHBins', ref="covid_dhbs")
library(DHBins)
library(gganimate)

equity_sheet <- "data/equity/rate_ratio/211024_-_cvip_equity_-_rate_ratios_and_uptake_over_time.xlsx"

vacc_dat <- read_excel(equity_sheet,
                       sheet=4) %>%
  rename(Week = `Week ending date`,
         Dose = `Dose number`,
         Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Vacc = `# doses administered`) %>%
  filter(DHB != "Overseas and undefined",
         DHB != "Unknown",
         Dose != 0)

popn_dat <- read_excel(equity_sheet,
                       sheet=5) %>%
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
  group_by(Week, DHB) %>%
  summarise(Vacc = sum(Vacc), Population = sum(Population, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(Rate = Vacc/Population/7*2)

this_week <- max(plot_me$Week)
last_week <- this_week - time_length("7 days")
this_week <- as.character(this_week)
last_week <- as.character(last_week)

highlight_best <- plot_me %>%
  filter(as.character(Week) == this_week) %>%
  slice_max(Rate, n=5) %>%
  select(DHB) %>%
  mutate(highlight = TRUE)

png("dhb_vacc_rates.png", width=1280, height=720)
ggplot(plot_me %>% left_join(highlight_best) %>% replace_na(list(highlight=FALSE))) +
  geom_line(aes(x=Week, y=Rate, size=highlight)) +
  facet_wrap(vars(DHB)) +
  theme_minimal(base_size=24) +
  scale_size_manual(values = c(1,2)) +
  scale_y_continuous(labels = scales::label_percent()) +
  guides(size='none') +
  labs(x = NULL,
       y = "Population dosed per day",
       title = "DHB vaccination performance over time",
       subtitle = "Highlighted DHBs are vaccinating fastest")
dev.off()


