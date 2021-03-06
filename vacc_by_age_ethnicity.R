library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)
library(patchwork)

source("helpers.R")

# baselines: Prioritised
popn_summary <- hsu_ethnicity_by_dhb(get_latest_sheet(), by_gender=FALSE) %>%
  group_by(Ethnicity, Age) %>%
  summarise(Population = sum(Population)) %>%
  ungroup()

# latest spreadsheet
curr_date <- get_latest_date()
vacc <- read_excel(get_latest_sheet(), sheet = "DHBofResidence by ethnicity")

vacc_eth_age <- vacc %>% select(Age = `Ten year age group`,
                             Ethnicity = `Ethnic group`, Dose = `Dose number`,
                             Vacc = `# doses administered`) %>%
  mutate(Ethnicity = fct_collapse(Ethnicity,
                                  `European or other` = c("European/Other", "European or other", "European / Other", "Other"),
                                  Maori = c("Māori", "Maori"))) %>%
  filter(Age != "90+/Unknown",
         Age != "90 + years / Unknown",
         Ethnicity != "Unknown") %>%
  group_by(Ethnicity, Age, Dose) %>%
  summarise(Vacc = sum(Vacc))

ethnicity_data <- vacc_eth_age %>%
  left_join(popn_summary) %>%
  mutate(Age = fct_collapse(Age,
                            "10 to 29" = c("10 to 19", "20 to 29"),
                            "30 to 49" = c("30 to 39", "40 to 49"),
                            "50 to 69" = c("50 to 59", "60 to 69"),
                            "70 to 89" = c("70 to 79", "80 to 89"))) %>%
  group_by(Ethnicity, Age, Dose) %>%
  summarise(Vacc = sum(Vacc),
            Population = sum(Population)) %>%
  mutate(Ethnicity = fct_recode(Ethnicity, Māori = "Maori")) %>%
  ungroup() %>%
  pivot_wider(names_from=Dose, values_from=Vacc, names_prefix="Dose") %>%
  mutate(Unprotected = Population - Dose1,
         `Partially protected` = Dose1 - Dose2,
         Protected = Dose2) %>%
  select(Ethnicity, Age, Population, Unprotected:Protected) %>%
  pivot_longer(Unprotected:Protected,names_to = "Vacc", values_to = "Count")

total_data <- ethnicity_data %>%
  group_by(Ethnicity, Vacc) %>%
  summarise(Population = sum(Population), Count = sum(Count)) %>%
  mutate(Age = "All ages")

final_data <- bind_rows(ethnicity_data, total_data) %>%
  mutate(Rate = Count/Population,
         Vacc = fct_relevel(Vacc, "Unprotected", "Partially Protected"),
         Ethnicity = fct_relevel(Ethnicity,
                                 "Māori",
                                 "Pacific Peoples",
                                 "Asian",
                                 "European or other"))

final_data %>% count(Age)
final_data %>% count(Ethnicity)
final_data %>% summarise(Population = sum(Population), Count = sum(Count)) # Count should be 2*total, Population = 4*Population

cols <- get_pal("Hoiho")[1:4] %>%
  set_names(c("Asian", "European or other",
              "Māori", "Pacific Peoples"))

plot_fun <- function(data) {
  ggplot(data) +
    geom_col(aes(x=Rate, y=Ethnicity, fill=Ethnicity, alpha=Vacc),
             col='black', size = 0.5) +
    facet_wrap(vars(Age)) +
    scale_x_continuous(labels = scales::label_percent()) +
    labs(x = NULL,
         y = NULL,
         alpha = "Share of people") +
    guides(alpha = guide_legend(reverse = TRUE),
           fill = "none") +
    scale_fill_manual(values = cols) +
    scale_alpha_manual(values = c(1,0.6,0.3))
}

g1 = plot_fun(final_data %>% filter(Age == "All ages"))
g2 = plot_fun(final_data %>% filter(Age != "All ages"))

png("vacc_by_ethnicity.png", width=1920, height=1080)
g1 + g2 + plot_layout(guides = 'collect') +
  plot_annotation(title = paste("New Zealand COVID-19 vaccinated population by ethnicity at", format(curr_date, "%d %B %Y")),
                  subtitle = "Excludes unknown ethnicity or age") &
  theme_minimal(base_size=32) +
  theme(legend.position = 'bottom',
        strip.text = element_text(size=rel(1.1)),
        panel.spacing.x = unit(50, "pt"))
dev.off()
