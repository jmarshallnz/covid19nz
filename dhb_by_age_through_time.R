library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)
# JM hacked version of DHBins: https://github.com/jmarshallnz/DHBins/tree/covid_dhbs
# remotes::install_github('jmarshallnz/DHBins', ref="covid_dhbs")
library(DHBins)
library(gganimate)

vacc_dat <- read_excel("data/rate_ratio_and_vaccine_uptake_over_time_-_by_dhb_ethnicity_and_age.xlsx",
                  sheet=2) %>%
  rename(Week = `Week ending date`,
         Dose = `Dose number`,
         Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Vacc = `# doses administered`) %>%
  filter(DHB != "Overseas and undefined",
         DHB != "Unknown")

popn_dat <- read_excel("data/rate_ratio_and_vaccine_uptake_over_time_-_by_dhb_ethnicity_and_age.xlsx",
                       sheet=3) %>%
  rename(Ethnicity = `Ethnic group`,
         Age = `Age group`,
         DHB = `DHB of residence`,
         Population = `# people (HSU)`)

vacc_dat %>% count(Age)
popn_dat %>% count(Age)
vacc_dat %>% count(DHB)
popn_dat %>% count(DHB)

# see what is missing
vacc_dat %>% anti_join(popn_dat) %>% as.data.frame()

# join up
all_dat <- vacc_dat %>% complete(Week, DHB, Age, Ethnicity, Dose, fill=list(Vacc=0)) %>%
  left_join(popn_dat)

# summarise up to our age and dhb categories (could change this whenever we like now...)
age_cats <- data.frame(cuts=c(0, 10, 30, 50, 65),
                       labs=c("0 to 9", "10 to 29", "30 to 49", "50-64", "65+"))
weekly_vacc <- all_dat %>% extract(Age, into="Age", regex="([[:digit:]]*)", convert=TRUE) %>%
  mutate(Age = cut(Age, breaks=c(age_cats$cuts, Inf), labels=age_cats$labs, right=FALSE)) %>%
  filter(Age != "0 to 9") %>%
  filter(Dose != 0) %>%
  mutate(DHB = fct_collapse(DHB, 
                            "Auckland" = c("Auckland", "Waitemata", "Counties Manukau"),
                            "Wellington" = c("Capital and Coast", "Hutt Valley"),
                            "Midcentral" = "MidCentral")) %>%
  group_by(Week, DHB, Age, Dose) %>%
  summarise(Vacc = sum(Vacc), Population = sum(Population, na.rm=TRUE))

# this is not cummaltive, so make it so
cumm_vacc <- weekly_vacc %>% group_by(DHB, Age, Dose) %>%
  arrange(Week) %>%
  mutate(Vacc = cumsum(Vacc))
cumm_vacc %>%
  pivot_wider(names_from=Dose, values_from=Vacc, names_prefix="Dose", values_fill=0) %>%
  mutate(Unprotected = Population - Dose1,
         `Partially protected` = Dose1 - Dose2,
         Protected = Dose2) %>%
  select(Week, DHB, Age, Unprotected:Protected) %>%
  ungroup() -> vacc_counts

to_triangles <- function(vacc_counts) {

  # check counts
  vacc_counts <- vacc_counts %>%
    unite(DHBAge, DHB, Age)
  
  vacc_counts %>% summarise(across(Unprotected:Protected, sum)) %>%
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
    mutate(Vacc = fct_relevel(Vacc, "Unprotected", "Partially protected"))
  
  return(tris_long)
}

triangle_final <- vacc_counts %>% nest_by(Week) %>%
  mutate(out = list(to_triangles(data))) %>%
  select(-data) %>%
  unnest(out) %>%
  ungroup()

#colours <- get_pal("Kotare")[c(6,2,1)]
#colours <- get_pal("Hoiho")[c(1,2,4)]
colours <- get_pal("Takahe")[c(1,4,3)]
colours[2] <- "#7cacbf"

# animate across the dates
triangles <- DHBins:::dhmap_tri()
plotting <- triangles %>% separate(id, into=c("DHB", "tri_id"), sep = "_", remove = FALSE, convert=TRUE) %>%
  left_join(triangle_final) %>% arrange(Week) %>%
  mutate(Week = as_factor(format(Week, "%d %B %Y")))

g <- ggplot(plotting) +
  geom_polygon(aes(x=x, y=y, group=id, fill=Vacc), alpha=0.8) +
  geom_text(data=DHBins:::dhbs, aes(x=x, y=y, label=printname)) +
  facet_wrap(vars(Age), ncol=4) +
  scale_fill_manual(values = colours)+
  coord_fixed() +
  theme_void(base_size=24) +
  labs(fill=NULL,
       title=labs(title = 'COVID-19 Vaccination rates by Age group and District Health Board at {current_frame}\n\n')) +
  theme(legend.position='bottom') +
  transition_manual(Week)

animate(g, renderer = gifski_renderer(file="dhb_progress.gif", loop=TRUE),
        width = 1280, height = 720, units = "px", duration=5 + length(unique(plotting$Week)), fps=1, end_pause=5)
