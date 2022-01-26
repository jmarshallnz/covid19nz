library(tidyverse)
library(lubridate)
library(Manu)
library(showtext)

source("fixup_weirdness.R")

font_add_google("Source Sans Pro", "ssp", bold.wt = 600)

showtext_auto()

# Read in our daily DHB data
dhb_files <- data.frame(files = list.files('data/dhb_daily', '*.csv', full.names = TRUE))
dailies <- dhb_files %>%
  extract(files, into="date", regex="([0-9-]+)", remove=FALSE) %>%
  mutate(date = ymd(date)) %>%
  arrange(desc(date)) %>%
#  slice(1:2) %>%
  pull(files) %>%
  map_dfr(read_csv) %>%
  pivot_longer(Dose1:Dose2,
               names_to="Dose",
               values_to = "Vacc",
               names_prefix="Dose",
               names_transform = list(Dose = as.numeric)) %>%
  filter(!(DHB %in% c("Overseas / Unknown", "Total", "New Zealand", "All DHBs to 90%"))) %>%
  mutate(DHB = fct_recode(DHB,
                          `Hawke's Bay` = "Hawkes Bay",
                          'Waitemat\u0101' = "Waitemata",
                          'Tair\u0101whiti' = "Tairawhiti"),
         DHB = fct_rev(DHB)) %>%
  group_by(DHB, Dose) %>%
  arrange(Date) %>%
  mutate(Number = Vacc - lag(Vacc)) %>%
  ungroup() %>%
  fixup_weirdness() %>%
  mutate(Number = if_else(Date == max(Date), Number, NA_real_)) %>%
  mutate(Raw = Vacc,
         Vacc = Vacc/Population,
         Today = if_else(Date == max(Date), "Today", "Previous"),
         Previous = if_else(as.numeric(max(Date) - Date, units='days') %% 7 == 0, "weeks", "days,")) %>%
  arrange(Previous, Date)

today <- format(dailies %>% pull(Date) %>% max(), "%A")
#today <- "Friday and Saturday"
todays_date <- format(dailies %>% pull(Date) %>% max(), "%e %B %Y") %>%
  str_trim()

dose1 <- dailies %>%
  filter(Dose == 1) 

label_dose1 <- dose1 %>%
  filter(DHB == "Northland", Today == "Today") %>%
  select(DHB,Vacc)

dose1_labelled <- dose1 %>% mutate(
  Label = prettyNum(Number,big.mark=","),
  Label = if_else(DHB == "Bay of Plenty" & Today == "Today",
                   paste0(Label, " doses ", today),
                  Label)
  )

colours_dose1 <- c("#C582B2", "#B7B7B2")

#size <- list(text = 6, theme=28)
size <- list(text = 8, theme=36)

png("today_dose1.png", width=1800, height=1280)
ggplot(dose1_labelled %>% filter(Today == "Today"),
       mapping = aes(y=DHB, x=Vacc)) +
  geom_line(data=dose1_labelled, col='grey40') +
  geom_point(data=dose1_labelled, aes(fill = Previous),
             size=6, shape=21, col='grey40') +
  geom_segment(aes(yend=DHB, xend=0.9, col=Vacc > 0.9, alpha=Vacc > 0.9), size=4) +
  geom_vline(xintercept=0.9) +
  geom_point(aes(col=Vacc > 0.9), size=8) +
  annotate(geom="curve",curvature=0.2,x=0.77,y=13.8,xend=0.778,yend=13,arrow=arrow(angle=20, type='closed'), col="grey70") +
#  geom_text(data=label_dose1, hjust=0, label=paste0(" doses ", today),
#            col = "grey50", vjust=-0.8, size=8) +
  geom_text(aes(label=Label, hjust=Vacc < 0.9), col="grey50", vjust=-0.8, size=size$text) +
  scale_colour_manual(values = colours_dose1,
                      guide = 'none') +
  scale_alpha_manual(values = c(1,0), guide='none') +
  scale_fill_manual(values = c(`days,` = 'white', weeks = 'grey70'),
                    guide = guide_legend(override.aes = list(size=5))) +
  theme_minimal(base_size=size$theme, base_family = "ssp") +
  scale_x_continuous(labels = scales::label_percent(), breaks=c(0.7,0.8,0.9), expand=c(0,0.005,0,0.01)) +
  theme(panel.grid.major.y = element_line(color='grey96', size=0.5),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(size = rel(0.9)),
        plot.tag.position = c(0.99, -0.02),
        plot.tag = element_text(hjust = 1, size = rel(0.6),
                                vjust = 1,
                                colour = 'grey50'),
        plot.margin = margin(12, 12, 60, 12),
        legend.direction = 'horizontal',
        legend.title = element_text(size = rel(0.65),
                                    colour = 'grey70', vjust=1),
        legend.text = element_text(size = rel(0.65),
                                   colour = 'grey70', vjust=1),
        legend.spacing.x = unit(3, units='pt'),
        legend.position=c(0.11, 0.675)) +
  labs(x = NULL,
       y = NULL,
       subtitle = paste0("How the ",
                      prettyNum(dose1 %>% filter(Today == "Today") %>% summarise(sum(Number)), big.mark=","),
                      " first doses on ",
                      today,
                      " move each DHB towards 90%"),
       title = paste("Path to 90%: First doses of those 12+ to", todays_date),
       tag = "Data from Ministry of Health. Chart by Jonathan Marshall. https://github.com/jmarshallnz/covid19nz")
dev.off()

dose2 <- dailies %>%
  filter(Dose == 2) 

dose2_labelled <- dose2 %>% mutate(
  Label = prettyNum(Number,big.mark=","),
  Label = if_else(DHB == "Bay of Plenty" & Today == "Today",
                  paste0(Label, " doses ", today),
                  Label)
)

#label_dose2 <- dose2 %>%
#  filter(DHB == "Bay of Plenty", Today == "Today") %>%
#  select(DHB,Vacc)

colours_dose2 <- c("#C89C63", "#B7B7B2")

png("today_dose2.png", width=1800, height=1280)
ggplot(dose2_labelled %>% filter(Today == "Today"),
       mapping = aes(y=DHB, x=Vacc)) +
  geom_line(data=dose2_labelled, col='grey40') +
  geom_point(data=dose2_labelled, aes(fill = Previous),
             size=6, shape=21, col='grey40') +
  geom_segment(aes(yend=DHB, xend=0.9, col=Vacc > 0.9, alpha=Vacc > 0.9), size=4) +
  geom_vline(xintercept=0.9) +
  geom_point(aes(col=Vacc > 0.9), size=8) +
  annotate(geom="curve",curvature=-0.2,x=0.63,y=17.2,xend=0.642,yend=18,arrow=arrow(angle=20, type='closed'), col="grey70") +
#  geom_text(data=label_dose2, hjust = 0, label=paste0(" doses ", today),
#            col = "grey50", vjust=-0.8, size=size$text) +
  geom_text(aes(label=Label, hjust=Vacc < 0.9), col="grey50", vjust=-0.8, size=size$text) +
  scale_colour_manual(values = colours_dose2,
                      guide = 'none') +
  scale_alpha_manual(values = c(1,0), guide='none') +
  scale_fill_manual(values = c(`days,` = 'white', weeks = 'grey70'),
                    guide = guide_legend(override.aes = list(size=5))) +
  theme_minimal(base_size=size$theme, base_family = "ssp") +
  scale_x_continuous(labels = scales::label_percent(), breaks=c(0.7,0.8,0.9), expand=c(0,0.005,0,0.02)) +
  theme(panel.grid.major.y = element_line(color='grey96', size=0.5),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(size = rel(0.9)),
        plot.tag.position = c(0.99, -0.02),
        plot.tag = element_text(hjust = 1, size = rel(0.6),
                                vjust = 1,
                                colour = 'grey50'),
        plot.margin = margin(12, 12, 60, 12),
        legend.direction = 'horizontal',
        legend.title = element_text(size = rel(0.65),
                                    colour = 'grey70', vjust=1),
        legend.text = element_text(size = rel(0.65),
                                   colour = 'grey70', vjust=1),
        legend.spacing.x = unit(3, units='pt'),
        legend.position=c(0.11, 0.82)) +
  labs(x = NULL,
       y = NULL,
       subtitle = paste0("How the ",
                         prettyNum(dose2 %>% filter(Today == "Today") %>% summarise(sum(Number)), big.mark=","),
                         " second doses on ",
                         today,
                         " move each DHB towards 90%"),
       title = paste("Path to 90%: Second doses of those 12+ to", todays_date),
       tag = "Data from Ministry of Health. Chart by Jonathan Marshall. https://github.com/jmarshallnz/covid19nz")
dev.off()

#### Summary information
dose1 %>% filter(Date == max(Date)) %>%
  mutate(ToGo95 = 0.95*Population - Raw)

dose2 %>% filter(Date == max(Date)) %>%
  mutate(ToGo90 = 0.9*Population - Raw)

# Total since traffic lights
dailies %>%
  filter(Date %in% c(min(Date), max(Date))) %>%
  group_by(Dose, Date) %>%
  summarise(Total = sum(Raw), Population = sum(Population)) %>%
  arrange(Date) %>%
  summarise(Dots = diff(Total), Population = unique(Population))

# Best performers since traffic lights
dots <- dailies %>%
  filter(Date %in% c(min(Date), max(Date))) %>%
  group_by(DHB, Dose) %>%
  arrange(Date) %>%
  summarise(Dots = diff(Vacc), Population = unique(Population)) %>%
  ungroup()

dots %>%
  filter(Dose == 1) %>%
  slice_max(Dots, n=3)

dots %>%
  filter(Dose == 2) %>%
  slice_max(Dots, n=3)
