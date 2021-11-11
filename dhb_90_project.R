library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)
library(showtext)

font_add_google("Source Sans Pro", "ssp", bold.wt = 600)

showtext_auto()

source('helpers.R')

# latest spreadsheet
read_vacc_sheet <- function(file, collapse_age = FALSE) {
  vacc <- read_excel(file, sheet = "DHBofResidence by ethnicity")
  
  if (!("Age group" %in% names(vacc))) {
    return(NULL)
  }
  
  vacc_dhbs <- vacc %>% select(DHB = `DHB of residence`,
                               Age = `Age group`,
                               Dose1 = `First dose administered`,
                               Dose2 = `Second dose administered`,
                               Population) %>%
    filter(DHB != "Overseas / Unknown") %>%
    filter(DHB != "Various")
  if (collapse_age) {
    vacc_dhbs <- vacc_dhbs %>% mutate(Age = fct_collapse(Age,
                              "12 to 29" = c("12-15", "16-19", "20-24", "25-29"),
                              "30 to 49" = c("30-34", "35-39", "40-44", "45-49"),
                              "50 to 64" = c("50-54", "55-59", "60-64"),
                              "65+" = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")))
  }
  vacc_dhbs <- vacc_dhbs %>%
    pivot_longer(Dose1:Dose2, names_to="Dose", values_to="Vacc", names_prefix="Dose") %>%
    group_by(DHB, Age, Dose) %>%
    summarise(Vacc = sum(Vacc), Population = sum(Population))
  
  # check counts:
  vacc_dhbs %>% group_by(Dose) %>%
    summarise(sum(Vacc), sum(Population)) %>%
    print()
  
  # combine age groups further, and munge
  # into format ready for DHBins
  vacc_dhbs
}

curr_date <- get_latest_date()
current_counts <- read_vacc_sheet(get_latest_sheet(), collapse_age=TRUE)
todays_date <- format(curr_date, "%e %B %Y") %>% str_trim()

#colours <- get_pal("Kotare")[c(6,2,1)]
colours <- get_pal("Hoiho")[c(4,2,4)]

png("vacc_90_percent.png", width=1980, height=1080)
current_counts %>%
  mutate(Prop = Vacc/Population) %>%
  mutate(Dose = fct_recode(Dose, `One Dose` = "1", `Fully vaccinated` = "2")) %>%
  ggplot() +
  geom_line(aes(x=Prop, y=fct_rev(DHB)), col="grey70", size=2) +
  geom_segment(data=NULL, aes(x=0.9,y=0.5, xend=0.9,yend=20.5), size=1.5, col="grey30") +
  geom_point(aes(x=Prop, y=fct_rev(DHB), shape=Age, fill=Prop >= 0.9), size=6) +
  facet_wrap(vars(Dose)) +
  scale_fill_manual(values = colours) +
  scale_shape_manual(values = paste(c("circle", "square", "diamond", "triangle"), "filled")) +
  scale_x_continuous(limits = c(0,1), expand=c(0,0), breaks=seq(0,1,by=0.2), labels = scales::label_percent()) +
  labs(title = paste("The 90% project: vaccination rates by DHB and Age at", todays_date),
       x = NULL,
       y = NULL,
       shape = NULL,
       tag = "Data from Ministry of Health. Chart by Jonathan Marshall. https://github.com/jmarshallnz/covid19nz") +
  annotate("text", x=0.9, y=21, label="90%", size=10) +
  scale_y_discrete(expand=c(0.03,0,0.03,0)) +
  guides(fill = 'none') +
  theme_minimal(base_size = 36, base_family = "ssp") +
  theme(panel.spacing.x = unit(108, "pt"),
        legend.position = 'bottom',
        legend.margin = margin(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color='grey90', size=0.5),
        axis.text.x = element_text(size=24, colour='grey80'),
        strip.text = element_text(hjust=0),
        plot.title = element_text(face="bold"),
        plot.tag = element_text(hjust = 1, size = rel(0.6),
                                vjust = 1,
                                colour = 'grey50'),
        plot.tag.position = c(1.005, -0.02),
        plot.margin = margin(12, 36, 60, 12)) +
  coord_cartesian(clip="off")
dev.off()

if (0) {
# same thing, but animated...
vacc_dat <- read_excel("data/210920_-_rate_ratios_and_uptake_over_time.xlsx",
                       sheet=4) %>%
  rename(Week = `Week ending date`,
         Dose = `Dose number`,
         Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Vacc = `# doses administered`) %>%
  filter(Dose != 0) %>%
  filter(!(Age %in% c("0 to 4", "5 to 9", "Unknown"))) %>%
  group_by(DHB, Week, Dose, Age) %>%
  summarise(Vacc = sum(Vacc)) %>%
  ungroup()

popn_dat <- read_excel("data/210920_-_rate_ratios_and_uptake_over_time.xlsx",
                       sheet=5) %>%
  rename(Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Population = `# people (HSU)`) %>%
  group_by(DHB, Age) %>% summarise(Population = sum(Population)) %>%
  ungroup()

vacc_dat %>% count(Age)
popn_dat %>% count(Age)

# see what is missing
vacc_dat %>% anti_join(popn_dat) %>% as.data.frame()

# join up
all_dat <- vacc_dat %>% ungroup() %>% complete(DHB, Week, Age, Dose, fill=list(Vacc=0)) %>%
  left_join(popn_dat)

# check:
all_dat %>% group_by(Week, Dose) %>% summarise(Total = sum(Population, na.rm=TRUE)) %>%
  pull(Total) %>% unique()

# OK, now cumsum up the vacc counts
ready_to_go <- all_dat %>% group_by(DHB, Age, Dose) %>%
  arrange(Week) %>%
  mutate(Vacc = cumsum(Vacc)) %>%
  filter(Age != "< 12",
         DHB != "Overseas and undefined") %>%
  ungroup() %>%
  mutate(Prop = Vacc/Population) %>%
  mutate(Dose = fct_recode(as_factor(Dose), `One Dose` = "1", `Fully vaccinated` = "2")) %>%
  mutate(Week = as_factor(format(Week, "%d %B %Y")))

library(gganimate)
#png("test.png", width=1280, height=720)
anim <- ggplot(ready_to_go) +
  geom_line(aes(x=Prop, y=fct_rev(DHB)), col="grey70", size=1.5) +
  geom_segment(data=NULL, aes(x=0.9,y=0.5, xend=0.9,yend=20.5), size=1, col="grey30") +
  geom_point(aes(x=Prop, y=fct_rev(DHB), shape=Age, fill=Prop >= 0.9), size=4) +
  facet_wrap(vars(Dose)) +
  scale_fill_manual(values = colours) +
  scale_shape_manual(values = paste(c("circle", "square", "diamond", "triangle", "triangle down"), "filled")) +
  scale_x_continuous(limits = c(0,1), expand=c(0,0), breaks=seq(0,1,by=0.2), labels = scales::label_percent()) +
  labs(title = "Progress towards 90%",
       subtitle = "Vaccination rates by DHB and Age at {current_frame}",
       x = NULL,
       y = NULL,
       shape = NULL) +
  annotate("text", x=0.9, y=21, label="90%", size=7) +
  scale_y_discrete(expand=c(0.03,0,0.03,0)) +
  guides(fill = 'none') +
  theme_minimal(base_size=24) +
  theme(panel.spacing.x = unit(72, "pt"),
        legend.position = 'bottom',
        plot.title = element_text(size=36),
        legend.margin = margin(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size=16, colour='grey70'),
        plot.margin = margin(32, 32, 32, 32, "pt")) +
  coord_cartesian(clip="off") +
  transition_manual(Week)
#dev.off()

animate(anim, renderer = gifski_renderer(file="90_project_progress.gif", loop=TRUE),
        width = 1280, height = 720, units = "px", duration=5 + length(unique(ready_to_go$Week)) / 5, end_pause=5)
}
