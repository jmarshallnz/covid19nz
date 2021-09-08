library(tidyverse)
library(lubridate)
library(readxl)

vacc_dat <- read_excel("data/rate_ratio_and_vaccine_uptake_over_time_-_by_dhb_ethnicity_and_age.xlsx",
                  sheet=2) %>%
  rename(Week = `Week ending date`,
         Dose = `Dose number`,
         Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Vacc = `# doses administered`) %>%
  filter(Dose != 0) %>%
  filter(!(Age %in% c("0 to 4", "5 to 9", "Unknown"))) %>%
  group_by(Week, Dose, Age) %>%
  summarise(Vacc = sum(Vacc))

popn_dat <- read_excel("data/rate_ratio_and_vaccine_uptake_over_time_-_by_dhb_ethnicity_and_age.xlsx",
                       sheet=3) %>%
  rename(Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Population = `# people (HSU)`) %>%
  group_by(Age) %>% summarise(Population = sum(Population))

vacc_dat %>% count(Age)
popn_dat %>% count(Age)

# see what is missing
vacc_dat %>% anti_join(popn_dat) %>% as.data.frame()

# join up
all_dat <- vacc_dat %>% ungroup() %>% complete(Week, Age, Dose, fill=list(Vacc=0)) %>%
  left_join(popn_dat)

# check:
all_dat %>% group_by(Week, Dose) %>% summarise(Total = sum(Population, na.rm=TRUE)) %>%
  pull(Total) %>% unique()

# summarise up to our age and dhb categories (could change this whenever we like now...)
age_cats <- data.frame(cuts=seq(0,80,by=10),
                       labs=c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49",
                              "50 to 59", "60 to 69", "70 to 79", "80+"))
weekly_vacc <- all_dat %>% extract(Age, into="Age", regex="([[:digit:]]*)", convert=TRUE) %>%
  mutate(Age = cut(Age, breaks=c(age_cats$cuts, Inf), labels=age_cats$labs, right=FALSE)) %>%
  filter(Age != "0 to 9") %>%
  group_by(Week, Age, Dose) %>%
  summarise(Vacc = sum(Vacc), Population = sum(Population, na.rm=TRUE))

# this is not cummaltive, so make it so
cumm_vacc <- weekly_vacc %>% group_by(Age, Dose) %>%
  arrange(Week) %>%
  mutate(Vacc = cumsum(Vacc),
         Dose = paste("Dose", Dose)) %>%
  mutate(Week = as.Date(Week))

png("vacc_by_age_through_time.png", width=1980, height=1080)
ggplot(cumm_vacc) +
  geom_line(aes(x=Week, y=Vacc/Population*100, group=Age), col='black', size=4) +
  geom_line(aes(x=Week, y=Vacc/Population*100, col=Age), size=3) +
  facet_wrap(vars(Dose)) +
  scale_color_viridis_d(direction = -1) +
  scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  guides(colour = guide_legend(nrow = 1)) +
  theme_minimal(base_size=36) +
  labs(x = NULL,
       y = "Doses per 100 people",
       title = "COVID-19 Vaccination rates by Age: The race is on") +
  theme(legend.position='bottom',
        legend.key.width = unit(48, "pt"))
dev.off()
