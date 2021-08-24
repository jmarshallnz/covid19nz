library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)
# JM hacked version of DHBins: https://github.com/jmarshallnz/DHBins/tree/covid_dhbs
# remotes::install_github('jmarshallnz/DHBins', ref="covid_dhbs")
library(DHBins) 

source('helpers.R')

popn_summary <- prioritised_ethnicity_by_dhb() %>%
  group_by(DHB, Age) %>%
  summarise(Population = sum(Population))

# latest spreadsheet
read_vacc_sheet <- function(file) {
  vacc <- read_excel(file, sheet = "DHBofResidence by ethnicity")

  if (!("Ten year age group" %in% names(vacc))) {
    return(NULL)
  }

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
    mutate(Vulnerable = Population - Dose1,
           `Partially protected` = Dose1 - Dose2,
           Protected = Dose2) %>%
    select(DHB, Age, Vulnerable:Protected) %>%
    ungroup() %>%
    unite(DHBAge, DHB, Age) -> vacc_counts

  # check counts
  vacc_counts %>% summarise(across(Vulnerable:Protected, sum)) %>%
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
    mutate(Vacc = fct_relevel(Vacc, "Vulnerable", "Partially protected")) %>%
    mutate(Date = dmy(sub(".*_([0-9]+_[0-9]+_2021)(.*)xlsx", "\\1", file)))

  return(tris_long)
}

curr_date <- get_latest_date()
current <- read_vacc_sheet(get_latest_sheet())
previous <- read_vacc_sheet(get_latest_sheet(weeks_ago = 1))

# highlight the differences
plotting <- current %>%
  left_join(previous %>% rename(Prev = Vacc))

#colours <- get_pal("Kotare")[c(6,2,1)]
#colours <- get_pal("Hoiho")[c(1,2,4)]
colours <- get_pal("Takahe")[c(1,5,3)]
png("dhb_by_age.png", width=1980, height=1080)
ggplot(plotting) +
  geom_dhbtri(aes(map_id=DHB,class_id=tri_id, fill=Vacc, size = Vacc != Prev, alpha = Vacc != Prev), colour='grey30') +
  scale_fill_manual(values = colours)+
  geom_label_dhb(size=7) +
  facet_wrap(vars(Age), ncol=4) +
  labs(fill=NULL,
       title=paste("COVID-19 Vaccination rates by Age group and District Health Board at", format(curr_date, "%d %B %Y")),
       subtitle="Highlighted wedges are progress from last week\n") +
  theme_void(base_size=36) +
  theme(legend.position='bottom',
        plot.subtitle=element_text(colour='grey30')) +
  scale_size_manual(values = c(0,1), guide='none') +
  scale_alpha_manual(values = c(0.6,0.9), guide='none')
dev.off()


