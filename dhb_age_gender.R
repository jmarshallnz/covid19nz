library(tidyverse)
library(lubridate)
library(readxl)
library(Manu)
# JM hacked version of DHBins: https://github.com/jmarshallnz/DHBins/tree/covid_dhbs
# remotes::install_github('jmarshallnz/DHBins', ref="covid_dhbs")
library(DHBins)
library(gganimate)
library(showtext)

font_add_google("Source Sans Pro", "ssp", bold.wt = 600)

showtext_auto()

source('helpers.R')

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
  vacc_dhbs %>%
    pivot_wider(names_from=Dose, values_from=Vacc, names_prefix="Dose") %>%
    mutate(Unprotected = Population - Dose1,
           `Partially protected` = Dose1 - Dose2,
           Protected = Dose2) %>%
    select(DHB, Age, Unprotected:Protected) %>%
    pivot_longer(Unprotected:Protected, names_to="Vacc", values_to="Count") %>%
    ungroup()
}

to_triangles <- function(vacc_counts) {
  # check counts
  vacc_counts <- vacc_counts %>%
    pivot_wider(names_from=Vacc, values_from=Count) %>%
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
    mutate(DHB = fct_recode(DHB,
                            "Wellington" = "Capital & Coast and Hutt Valley")) %>%
    mutate(Vacc = fct_relevel(Vacc, "Unprotected", "Partially protected"))

  return(tris_long)
}

curr_date <- get_latest_date()
current_counts <- read_vacc_sheet(get_latest_sheet())
current <- to_triangles(current_counts)

current_counts %>% group_by(DHB, Age) %>%
  mutate(Prop = Count/sum(Count)) %>%
  filter(Vacc == "Unprotected") %>% arrange(Prop)

#labs <- expand(, 
labs <- DHBins:::dhbs
labs <- labs %>% mutate(Age = "65+",
           size = if_else(shortname == "NM", 5.5, 6),
           vjust = case_when(str_detect(printname, "Metro") ~ 0.6,
                             shortname == "HB" ~ 0.6,
                             shortname == "NM" ~ 0.3,
                             shortname == "SC" ~ 0.4,
                             TRUE ~ 0.5))

#colours <- get_pal("Kotare")[c(6,2,1)]
#colours <- get_pal("Hoiho")[c(1,2,4)]
colours <- get_pal("Takahe")[c(1,4,3)]
colours[2] <- "#7cacbf"
#colours <- c("#e36879", "#7cacbf", "#1F6683")
png("dhb_by_age.png", width=1980, height=1080)
ggplot(current) +
  geom_dhbtri(aes(map_id=DHB,class_id=tri_id, fill=Vacc), alpha=0.9) +
  scale_fill_manual(values = colours)+
  geom_text(data=labs, aes(x=x, y=y, label=printname, size=size, vjust=vjust), col="white") +
#  geom_label_dhb(size=7) +
  facet_wrap(vars(Age), ncol=4) +
  scale_size_identity(guide = 'none') +
  labs(fill=NULL,
       title=paste("COVID-19 Vaccination rates by Age group and District Health Board at", format(curr_date, "%d %B %Y"), "\n"),
       tag = "Data from Ministry of Health. Chart by Jonathan Marshall. https://github.com/jmarshallnz/covid19nz") +
  theme_void(base_size=36, base_family="ssp") +
  theme(legend.position='bottom',
        strip.text = element_text(hjust=0),
        plot.title = element_text(face="bold"),
        plot.tag = element_text(hjust = 1, size = rel(0.6),
                                vjust = 1,
                                colour = 'grey50'),
        plot.tag.position = c(1.01, -0.02),
        plot.margin = margin(12, 12, 60, 12))
dev.off()

# Now do the same, but animate it...
if (0) {
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
}