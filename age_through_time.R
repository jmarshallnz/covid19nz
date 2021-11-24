library(tidyverse)
library(lubridate)
library(readxl)
library(showtext)

font_add_google("Source Sans Pro", "ssp", bold.wt = 600)

showtext_auto()

equity_sheet <- "data/equity/rate_ratio/20211121_-_cvip_equity_-_rate_ratios_and_uptake_over_time.xlsx"

vacc_dat <- read_excel(equity_sheet,
                  sheet=4) %>%
  rename(Week = `Week ending date`,
         Dose = `Dose number`,
         Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Vacc = `# doses administered`) %>%
  filter(Dose %in% c(1,2)) %>%
  filter(!(Age %in% c("0 to 4", "5 to 9", "Unknown"))) %>%
  group_by(Week, Dose, Age) %>%
  summarise(Vacc = sum(Vacc)) %>%
  # Filter out the last week if it has zero vacc
  group_by(Week) %>%
  mutate(Total = sum(Vacc)) %>%
  ungroup() %>%
  filter(Week != max(Week) | Total > 0) %>%
  select(-Total)

popn_dat <- read_excel(equity_sheet,
                       sheet=5) %>%
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
#age_cats <- data.frame(cuts=seq(0,80,by=10),
#                       labs=c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49",
#                              "50 to 59", "60 to 69", "70 to 79", "80+"))
weekly_vacc <- all_dat %>% filter(Age != "< 12") %>%
  group_by(Week, Age, Dose) %>%
  summarise(Vacc = sum(Vacc), Population = sum(Population, na.rm=TRUE))

# this is not cummaltive, so make it so
cumm_vacc <- weekly_vacc %>% group_by(Age, Dose) %>%
  arrange(Week) %>%
  mutate(Vacc = cumsum(Vacc),
         Dose = paste("Dose", Dose)) %>%
  mutate(Week = as.Date(Week))

todays_date <- format(cumm_vacc %>% pull(Week) %>% max(), "%e %B %Y") %>% str_trim()

png("vacc_by_age_through_time.png", width=1980, height=1080)
ggplot(cumm_vacc) +
  geom_hline(yintercept=0.9) +
  geom_line(aes(x=Week, y=Vacc/Population, group=Age), col='black', size=4) +
  geom_line(aes(x=Week, y=Vacc/Population, col=Age), size=3) +
  facet_wrap(vars(Dose)) +
  scale_color_viridis_d(direction = -1) +
  scale_y_continuous(limits = c(0,1), expand=c(0,0), breaks=seq(0,1,by=0.1), labels=c(rep("",9),"90%","")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  guides(colour = guide_legend(nrow = 1)) +
  theme_minimal(base_size=36, base_family = "ssp") +
  labs(x = NULL,
       y = "Doses per 100 people",
       title = paste("COVID-19 Vaccination rates by Age: The race at", todays_date),
       tag = "Data from Ministry of Health. Chart by Jonathan Marshall. https://github.com/jmarshallnz/covid19nz") +
  theme(legend.position='bottom',
        legend.key.width = unit(48, "pt"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color='grey90', size=0.5),
        strip.text = element_text(hjust=0),
        plot.title = element_text(face="bold"),
        plot.tag = element_text(hjust = 1, size = rel(0.6),
                                vjust = 1,
                                colour = 'grey50'),
        plot.tag.position = c(0.99, -0.02),
        plot.margin = margin(12, 12, 60, 12))
dev.off()
