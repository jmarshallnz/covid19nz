library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)
# JM hacked version of DHBins: https://github.com/jmarshallnz/DHBins/tree/covid_dhbs
# remotes::install_github('jmarshallnz/DHBins', ref="covid_dhbs")
library(DHBins)
library(gganimate)

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
    mutate(Unprotected = Population - Dose1,
           `Partially protected` = Dose1 - Dose2,
           Protected = Dose2) %>%
    select(DHB, Age, Unprotected:Protected) %>%
    ungroup() %>%
    unite(DHBAge, DHB, Age) -> vacc_counts

  # check counts
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
    mutate(DHB = fct_recode(DHB,
                            "Wellington" = "Capital & Coast and Hutt Valley")) %>%
    mutate(Vacc = fct_relevel(Vacc, "Unprotected", "Partially protected")) %>%
    mutate(Date = dmy(sub(".*_([0-9]+_[0-9]+_2021)(.*)xlsx", "\\1", file)))

  return(tris_long)
}

curr_date <- get_latest_date()
current <- read_vacc_sheet(get_latest_sheet())

#colours <- get_pal("Kotare")[c(6,2,1)]
#colours <- get_pal("Hoiho")[c(1,2,4)]
colours <- get_pal("Takahe")[c(1,4,3)]
colours[2] <- "#7cacbf"
#colours <- c("#e36879", "#7cacbf", "#1F6683")
png("dhb_by_age.png", width=1980, height=1080)
ggplot(current) +
  geom_dhbtri(aes(map_id=DHB,class_id=tri_id, fill=Vacc), alpha=0.8) +
  scale_fill_manual(values = colours)+
  geom_label_dhb(size=7) +
  facet_wrap(vars(Age), ncol=4) +
  labs(fill=NULL,
       title=paste("COVID-19 Vaccination rates by Age group and District Health Board at", format(curr_date, "%d %B %Y"),"\n")) +
  theme_void(base_size=36) +
  theme(legend.position='bottom')
dev.off()

# Now do the same, but animate it...

# grab all the excel sheets
all <- list.files(path = "data",
                  pattern = ".xlsx", full.names=TRUE)
test <- map_dfr(all, read_vacc_sheet) 

# animate across the dates
final <- test %>% mutate(DHB = fct_recode(DHB,
                                         Auckland = "Auckland Metro",
                                         Midcentral = "MidCentral",
                                         `Hawke's Bay` = "Hawkes Bay"))

triangles <- DHBins:::dhmap_tri()
plotting <- triangles %>% separate(id, into=c("DHB", "tri_id"), sep = "_", remove = FALSE, convert=TRUE) %>%
  left_join(final) %>% arrange(Date) %>%
  mutate(Date = as_factor(format(Date, "%d %B %Y")))

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
  transition_manual(Date)

animate(g, renderer = gifski_renderer(file="dhb_progress.gif", loop=TRUE),
        width = 1280, height = 720, units = "px", duration=5 + length(unique(plotting$Date)), fps=1, end_pause=5)
