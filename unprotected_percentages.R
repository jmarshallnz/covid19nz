library(tidyverse)

read_vacc_sheet <- function(file) {
  vacc <- read_excel(file, sheet = "DHBofResidence by ethnicity")

  if (!("Age group" %in% names(vacc))) {
    return(NULL)
  }

  if ("At least partially vaccinated" %in% names(vacc)) {
    vacc <- vacc %>% rename('First dose administered' = "At least partially vaccinated",
                            'Second dose administered' = "Fully vaccinated")
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
  vacc_dhbs %>%
    pivot_wider(names_from=Dose, values_from=Vacc, names_prefix="Dose") %>%
    mutate(Unprotected = Population - Dose1,
           `Partially protected` = Dose1 - Dose2,
           Protected = Dose2) %>%
    select(DHB, Age, Unprotected:Protected) %>%
    pivot_longer(Unprotected:Protected, names_to="Vacc", values_to="Count") %>%
    ungroup()
}

current_counts <- read_vacc_sheet(get_latest_sheet())

current_counts %>% group_by(DHB, Age) %>%
  mutate(Prop = Count/sum(Count)) %>%
  filter(Vacc == "Unprotected") %>% arrange(Prop) %>%
  as.data.frame()
