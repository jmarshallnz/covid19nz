library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)

source('helpers.R')

popn_summary <- prioritised_ethnicity_by_dhb() %>%
  group_by(DHB, Age) %>%
  summarise(Population = sum(Population))

# latest spreadsheet
read_vacc_sheet <- function(file) {
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
    filter(DHB != "Various") %>%
    mutate(Age = fct_collapse(Age,
                              "12 to 29" = c("12-15", "16-19", "20-24", "25-29"),
                              "30 to 49" = c("30-34", "35-39", "40-44", "45-49"),
                              "50 to 64" = c("50-54", "55-59", "60-64"),
                              "65+" = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+"))) %>%
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
current_counts <- read_vacc_sheet(get_latest_sheet())

#colours <- get_pal("Kotare")[c(6,2,1)]
colours <- get_pal("Hoiho")[c(4,2,4)]

png("vacc_90_percent.png", width=1980, height=1080)
current_counts %>%
  mutate(Prop = Vacc/Population) %>%
  mutate(Dose = fct_recode(Dose, `One Dose` = "1", `Fully vaccinated` = "2")) %>%
  ggplot() +
  geom_line(aes(x=Prop, y=fct_rev(DHB)), col="grey70", size=2) +
  geom_segment(data=NULL, aes(x=0.9,y=0.5, xend=0.9,yend=17.5), size=1.5, col="grey30") +
  geom_point(aes(x=Prop, y=fct_rev(DHB), shape=Age, fill=Prop >= 0.9), size=6) +
  facet_wrap(vars(Dose)) +
  scale_fill_manual(values = colours) +
  scale_shape_manual(values = paste(c("circle", "square", "diamond", "triangle"), "filled")) +
  scale_x_continuous(limits = c(0,1), expand=c(0,0), breaks=seq(0,1,by=0.2), labels = scales::label_percent()) +
  labs(title = "The 90% project: vaccination rates by DHB and Age",
       x = NULL,
       y = NULL,
       shape = NULL) +
  annotate("text", x=0.9, y=18, label="90%", size=10) +
  scale_y_discrete(expand=c(0.03,0,0.03,0)) +
  guides(fill = 'none') +
  theme_minimal(base_size=36) +
  theme(panel.spacing.x = unit(108, "pt"),
        legend.position = 'bottom',
        plot.title = element_text(size=54),
        legend.margin = margin(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size=24, colour='grey70'),
        plot.margin = margin(48, 48, 48, 48, "pt")) +
  coord_cartesian(clip="off")
dev.off()

