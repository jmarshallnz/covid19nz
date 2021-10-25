library(tidyverse)
library(lubridate)
library(readxl)

source('helpers.R')

# statsNZ popn
statsnz <- read_excel("data/dhb_projections/2020-21 Population Projections.xlsx", sheet=1, skip=1)
statsnz_popn <- statsnz %>% select(DHB = DHB_name, Ethnicity, Gender=Sex, Age = Age_Group, Population = pop2020_2021) %>%
  mutate(Ethnicity = fct_collapse(Ethnicity,
                                  "European or Other" = "Other",
                                  "Pacific Peoples" = "Pacific")) %>%
#  extract(Age, into="Age", regex="([[:digit:]]+)", convert=TRUE) %>%
#  mutate(Age = (Age %/% 10) * 10,
#         Age = paste(Age, 'to', Age + 9),
#         Age = if_else(Age == "90 to 99",
#                       "90+/Unknown", Age)) %>%
  filter(!Age %in% c("00-04", "05-09", "10-14", "15-19")) %>%
  mutate(DHB = fct_collapse(DHB,
                            "Auckland Metro" = c("Auckland", "Waitemata", "Counties Manukau"),
                            "Capital & Coast and Hutt Valley" = c("Capital and Coast", "Hutt"))) %>%
  group_by(DHB, Ethnicity, Age, Gender) %>%
  summarise(StatsNZ = sum(Population)) %>%
  ungroup()

# latest vacc spreadsheet
hsu <- read_excel(get_latest_sheet(), sheet = "DHBofResidence by ethnicity")

hsu_dhbs <- hsu %>% select(DHB = `DHB of residence`,
                             Age = `Age group`,
                             Ethnicity = `Ethnic group`,
                           Gender = Gender,
                             Population) %>%
#  mutate(Ethnicity = fct_recode(Ethnicity, "European or other" = "European or Other")) %>%
#  filter(DHB != "Overseas / Unknown") %>%
#  filter(DHB != "Various") %>%
  filter(!Age %in% c("12-15", "16-19")) %>%
  group_by(DHB, Age, Ethnicity, Gender) %>%
  summarise(HSU = sum(Population)) %>%
  ungroup()

statsnz_popn %>% anti_join(hsu_dhbs)

hsu_dhbs %>% filter() %>%
  anti_join(statsnz_popn) %>%
  arrange(desc(HSU))

hsu_dhbs %>% group_by(DHB, Age, Ethnicity) %>%
  summarise(HSU = sum(HSU)) %>%
  left_join(
    statsnz_popn %>% group_by(DHB, Age, Ethnicity) %>%
      summarise(StatsNZ = sum(StatsNZ))
  ) %>%
  pivot_longer(HSU:StatsNZ, names_to="Method", values_to="Population") %>%
  filter(Ethnicity != "Unknown",
         Ethnicity != "Various",
         DHB != "Overseas / Unknown") %>%
  write_csv("data/dhb_projections/hsu_comparison.csv")
  ggplot() +
  geom_col(mapping=aes(x=Age, y=Population, fill=Method),
           position='dodge') +
  facet_grid(vars(DHB), vars(Ethnicity), scales='free_y') +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Ok, so the "other" ethnicity and "other" gender are a thing, as is various DHB stuff.

# Now compare total counts
hsu_dhbs %>% group_by(DHB) %>%
  summarise(HSU = sum(HSU)) %>%
  left_join(

statsnz_popn %>% group_by(DHB) %>%
  summarise(StatsNZ = sum(StatsNZ))) %>%
  mutate(Prop = HSU / StatsNZ)

hsu_dhbs %>% group_by(Gender) %>%
  summarise(HSU = sum(HSU)) %>%
  left_join(
    statsnz_popn %>% group_by(Gender) %>%
      summarise(StatsNZ = sum(StatsNZ))) %>%
  mutate(Prop = HSU / StatsNZ)

hsu_dhbs %>% group_by(Age) %>%
  summarise(HSU = sum(HSU)) %>%
  left_join(
    statsnz_popn %>% group_by(Age) %>%
      summarise(StatsNZ = sum(StatsNZ))) %>%
  mutate(Prop = HSU / StatsNZ)

hsu_dhbs %>% group_by(Ethnicity) %>%
  summarise(HSU = sum(HSU)) %>%
  left_join(
    statsnz_popn %>% group_by(Ethnicity) %>%
      summarise(StatsNZ = sum(StatsNZ))) %>%
  mutate(Prop = HSU / StatsNZ)


statsnz_popn %>% count(Gender)
statsnz_popn %>% count(Ethnicity)
hsu_dhbs %>% count(Gender)

curr_date <- get_latest_date()
current_counts <- read_vacc_sheet(get_latest_sheet())
