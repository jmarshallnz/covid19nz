library(tidyverse)
library(lubridate)
library(readxl)

vacc_dat <- read_excel("data/211017_-_cvip_equity_-_rate_ratios_and_uptake_over_time.xlsx",
                  sheet=4) %>%
  rename(Week = `Week ending date`,
         Dose = `Dose number`,
         Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Vacc = `# doses administered`) %>%
  filter(Dose != 0) %>%
#  filter(!(Age %in% c("0 to 4", "5 to 9", "Unknown"))) %>%
  filter(!(DHB %in% c("Overseas and undefined", "Unknown"))) %>%
 # filter(DHB %in% c("Auckland", "Waitemata", "Counties Manukau"),
#         Dose == 2) %>%
  group_by(Week, Dose, DHB) %>%
  summarise(Vacc = sum(Vacc))

popn_dat <- read_excel("data/211017_-_cvip_equity_-_rate_ratios_and_uptake_over_time.xlsx",
                       sheet=5) %>%
  rename(Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Population = `# people (HSU)`) %>%
#  filter(DHB %in% c("Auckland", "Waitemata", "Counties Manukau")) %>%
  group_by(DHB) %>% summarise(Population = sum(Population))

vacc_dat %>% count(Age)
popn_dat %>% count(Age)

# see what is missing
vacc_dat %>% anti_join(popn_dat) %>% as.data.frame()

# join up
all_dat <- vacc_dat %>% ungroup() %>% complete(Week, Dose, DHB, fill=list(Vacc=0)) %>%
  left_join(popn_dat)

# check:
all_dat %>% group_by(Week, Dose) %>% summarise(Total = sum(Population, na.rm=TRUE)) %>%
  pull(Total) %>% unique()

# summarise up to our age and dhb categories (could change this whenever we like now...)
#age_cats <- data.frame(cuts=seq(0,80,by=10),
#                       labs=c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49",
#                              "50 to 59", "60 to 69", "70 to 79", "80+"))
weekly_vacc <- all_dat %>% #filter(Age == "12-19") %>%
  group_by(Week, Dose, DHB) %>%
  summarise(Vacc = sum(Vacc), Population = sum(Population, na.rm=TRUE))

# this is not cummaltive, so make it so
cumm_vacc <- weekly_vacc %>% group_by(Dose, DHB) %>%
  arrange(Week) %>%
  mutate(Vacc = cumsum(Vacc)) %>%
  mutate(Week = as.Date(Week),
         Dose = paste("Dose", Dose),
         OverTheLine = Vacc/Population >= 0.9)

curr_vacc %>% filter(OverTheLine)

latest_date <- curr_vacc %>% pull(Week) %>% max()

cols <- scales:::viridis_pal(1, 0, 1, 1)(2)

png("vacc_by_dhb.png", width=1980, height=1080)
ggplot(curr_vacc) +
  geom_hline(yintercept=0.9, size=1) +
  geom_line(aes(x=Week, y=Vacc/Population, group=Dose), col='black', size=4) +
  geom_line(aes(x=Week, y=Vacc/Population, col=Dose), size=3) +
  facet_wrap(vars(DHB)) +
  scale_color_manual(values = cols) +
  scale_y_continuous(limits = c(0,0.95), breaks=0.9, labels=scales::label_percent()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  guides(colour = guide_legend(nrow = 1),
         alpha = 'none') +
  theme_minimal(base_size=36) +
  labs(x = NULL,
       y = NULL,
       colour = NULL,
       title = paste("Are we there yet? Progress to 90% at", format(latest_date, "%d %B %Y"))) +
  theme(legend.position='bottom',
        legend.key.width = unit(48, "pt"),
        axis.text = element_text(size = 18),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(hjust=0))
dev.off()
