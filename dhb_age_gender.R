library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)
# JM hacked version of DHBins: https://github.com/jmarshallnz/DHBins/tree/covid_dhbs
# remotes::install_github('jmarshallnz/DHBins', ref="covid_dhbs")
library(DHBins) 

# baselines
popn <- read_csv("data/Subnational population estimates (DHB, DHB constituency), by age and sex, at 30 June 1996-2020 (2020 boundaries)/TABLECODE7509_Data_21d61ee0-b582-400b-8deb-d973e38d64ac.csv")

# highly inefficient code to munge data for match
popn_summary <- popn %>%
  filter(`Year at 30 June` == 2020) %>%
  select(DHB=Area, Gender=Sex, Age, Population=Value) %>%
  mutate(Age = fct_collapse(Age,
                            `10 to 19` = c("10-14 Years", "15-19 Years"),
                            `20 to 29` = c("20-24 Years", "25-29 Years"),
                            `30 to 39` = c("30-34 Years", "35-39 Years"),
                            `40 to 49` = c("40-44 Years", "45-49 Years"),
                            `50 to 59` = c("50-54 Years", "55-59 Years"),
                            `60 to 69` = c("60-64 Years", "65-69 Years"),
                            `70 to 79` = c("70-74 Years", "75-79 Years"),
                            `80 to 89` = c("80-84 Years", "85-89 Years"),
                            `90 + years / Unknown` = c("90 Years and over"))) %>%
  mutate(DHB = fct_collapse(DHB,
                            "Auckland Metro" = c("Auckland", "Waitemata", "Counties Manukau"),
                            "Hawkes Bay" = "Hawke's Bay",
                            "Capital & Coast and Hutt Valley" = c("Capital and Coast", "Hutt Valley"))) %>%
  group_by(DHB, Age) %>%
  summarise(Population = sum(Population)) %>%
  ungroup()

# latest spreadsheet
read_vacc_sheet <- function(file) {
  vacc <- read_excel(file, sheet = "DHBofResidence by ethnicity")

  vacc_dhbs <- vacc %>% select(DHB = `DHB of residence`,
                               Age = `Ten year age group`,
                               Gender, Dose = `Dose number`,
                               Vacc = `# doses administered`) %>%
    filter(DHB != "Overseas / Unknown",
           Age != "90+/Unknown",
           Age != "90 + years / Unknown") %>%
    group_by(DHB, Age, Dose) %>%
    summarise(Vacc = sum(Vacc)) %>%
    left_join(popn_summary) %>%
    group_by(DHB, Age, Dose) %>%
    summarise(Vacc = sum(Vacc),
              Population = sum(Population))

  # check counts:
  vacc_dhbs %>% group_by(Dose) %>%
    summarise(sum(Vacc), sum(Population)) %>%
    print()

  # combine age groups further, and munge
  # into format ready for DHBins
  vacc_dhbs %>%
    mutate(Age = fct_collapse(Age,
                              "10 to 29" = c("10 to 19", "20 to 29"),
                              "30 to 49" = c("30 to 39", "40 to 49"),
                              "50 to 69" = c("50 to 59", "60 to 69"),
                              "70 to 89" = c("70 to 79", "80 to 89"))) %>%
    group_by(DHB, Age, Dose) %>%
    summarise(Vacc = sum(Vacc), Population=sum(Population)) %>%
    pivot_wider(names_from=Dose, values_from=Vacc, names_prefix="Dose") %>%
    mutate(Unvaccinated = Population - Dose1,
           `One dose` = Dose1 - Dose2,
           `Fully vaccinated` = Dose2) %>%
    select(DHB, Age, Unvaccinated:`Fully vaccinated`) %>%
    ungroup() %>%
    unite(DHBAge, DHB, Age) -> vacc_counts

  # check counts
  vacc_counts %>% summarise(across(Unvaccinated:`Fully vaccinated`, sum)) %>%
    print()

  # Setup triangles
  tris <- tri_alloc(vacc_counts %>% select(-DHBAge),
                    classes = vacc_counts %>% select(-DHBAge) %>% names(),
                    names = vacc_counts %>% pull(DHBAge))

  # Convert back to long format
  tris_long <- tibble(DHB=rep(rownames(tris),6),
                      Vacc=as.vector(tris),
                      tri_id=rep(1:6,each=nrow(tris))) %>%
    separate(DHB, into=c("DHB", "Age"), sep="_") %>%
    mutate(DHB = fct_recode(DHB,
                            "Wellington" = "Capital & Coast and Hutt Valley")) %>%
    mutate(Vacc = fct_relevel(Vacc, "Unvaccinated", "One dose"))

  return(tris_long)
}

current <- read_vacc_sheet("data/covid_vaccinations_03_08_2021.xlsx")
previous <- read_vacc_sheet("data/covid_vaccinations_27_07_2021.xlsx")

# highlight the differences
plotting <- current %>%
  left_join(previous %>% rename(Prev = Vacc))

png("dhb_by_age.png", width=1980, height=1080)
ggplot(plotting) +
  geom_dhbtri(aes(map_id=DHB,class_id=tri_id, fill=Vacc, size = Vacc != Prev), alpha=0.7, colour='grey30') +
  scale_fill_manual(values = get_pal("Hoiho")[c(1,2,4)])+
  geom_label_dhb(size=7) +
  facet_wrap(vars(Age), ncol=4) +
  labs(fill="Population with doses",
       title="COVID-19 Vaccination rates by Age group and District Health Board",
       subtitle="Highlighted wedges are progress from last week\n") +
  theme_void(base_size=36) +
  theme(legend.position='bottom',
        plot.subtitle=element_text(colour='grey30')) +
  scale_size_manual(values = c(0,1), guide='none')
dev.off()


