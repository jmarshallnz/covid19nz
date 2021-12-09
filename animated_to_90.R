library(tidyverse)
library(stringr)
library(lubridate)
library(Manu)
library(showtext)
library(animation)

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
  fixup_weirdness()

do_graph <- function(dat, dose=1, max_lim) {

  to_date <- dat %>%
    mutate(Number = if_else(Date == max(Date), Number, NA_real_)) %>%
    mutate(Raw = Vacc,
           Vacc = Vacc/Population,
           Today = if_else(Date == max(Date), "Today", "Previous"),
           Previous = if_else(as.numeric(max(Date) - Date, units='days') %% 7 == 0, "weeks", "days,")) %>%
    arrange(Previous, Date)

  today <- format(to_date %>% pull(Date) %>% max(), "%A")
  todays_date <- format(to_date %>% pull(Date) %>% max(), "%e %B %Y") %>%
    str_trim()

  doses <- to_date %>%
    filter(Dose == dose) 

  doses_labelled <- doses %>% mutate(
    Label = prettyNum(Number,big.mark=","),
    Label = if_else(DHB == "Bay of Plenty" & Today == "Today",
                    paste0(Label, " doses ", today),
                    Label)
  )

  colours <- if (dose == 1) { 
    c("#C582B2", "#B7B7B2")
  } else {
    c("#C89C63", "#B7B7B2")
  }
  which_dose <- if_else(dose == 1, "first", "second")
  legend_y <- if_else(dose == 1, 0.675, 0.82)
  arrow <- if (dose == 1) {
    annotate(geom="curve",curvature=0.2,x=0.77,y=13.8,xend=0.778,yend=13,arrow=arrow(angle=20, type='closed', length=unit(0.18, "inches")), col="grey70")
  } else {
    annotate(geom="curve",curvature=-0.2,x=0.63,y=17.2,xend=0.642,yend=18,arrow=arrow(angle=20, type='closed', length=unit(0.18, "inches")), col="grey70")
  }

  ggplot(doses_labelled %>% filter(Today == "Today"),
         mapping = aes(y=DHB, x=Vacc)) +
    geom_line(data=doses_labelled, col='grey40') +
    geom_point(data=doses_labelled, aes(fill = Previous),
               size=4.5, shape=21, col='grey40') +
    geom_segment(aes(yend=DHB, xend=0.9, col=Vacc > 0.9), size=3) +
    geom_vline(xintercept=0.9) +
    geom_point(aes(col=Vacc > 0.9), size=4.5) +
    arrow +
  #  geom_text(aes(label=Label, hjust=Vacc < 0.9), col="grey50", vjust=-0.8, size=6) +
    scale_colour_manual(values = colours,
                        guide = 'none') +
    scale_fill_manual(values = c(`days,` = 'white', weeks = 'grey70'),
                      guide = guide_legend(override.aes = list(size=4))) +
    theme_minimal(base_size=24, base_family = "ssp") +
    scale_x_continuous(labels = scales::label_percent(), breaks=c(0.7,0.8,0.9), 
                       limits = max_lim, 
                       expand=c(0,0.005,0,0.01)) +
    theme(panel.grid.major.y = element_line(color='grey96', size=0.5),
          axis.text = element_text(size = rel(0.7)),
          plot.title = element_text(face="bold"),
          plot.subtitle = element_text(size = rel(0.9)),
          plot.tag.position = c(0.99, -0.02),
          plot.tag = element_text(hjust = 1, size = rel(0.6),
                                  vjust = 1,
                                  colour = 'grey50'),
          plot.margin = margin(8, 8, 40, 8),
          legend.direction = 'horizontal',
          legend.title = element_text(size = rel(0.65),
                                      colour = 'grey70', vjust=0.6),
          legend.text = element_text(size = rel(0.65),
                                     colour = 'grey70', vjust=0.6),
          legend.spacing.x = unit(3, units='pt'),
          legend.position=c(0.11, legend_y)) +
    labs(x = NULL,
         y = NULL,
         subtitle = paste0("How the ",
                           prettyNum(doses %>% filter(Today == "Today") %>% summarise(sum(Number)), big.mark=","),
                           " ", which_dose, " doses on ",
                           today,
                           " move each DHB towards 90%"),
         title = paste("Path to 90%:", str_to_title(which_dose), "doses to", todays_date),
         tag = "Data from Ministry of Health. Chart by Jonathan Marshall. https://github.com/jmarshallnz/covid19nz")
}

# OK, now run over our date and accumulate into a list
dates <- dailies %>% pull(Date) %>% unique() %>% sort()

test <- map(dates, function(date) { dailies %>% filter(Date <= date) })
test <- test[-1]
test <- c(test, rep(test[length(test)], 5))

limits <- dailies %>% filter(Dose == 1) %>% mutate(Vacc=Vacc/Population) %>% pull(Vacc) %>% range()
dose1 <- map(test, do_graph, dose=1, max_lim = limits)
saveGIF(
  { map(dose1, print) },
  "dose1.gif",
  ani.options = list(interval=0.5, ani.width=1200, ani.height=864)
)

limits <- dailies %>% filter(Dose == 2) %>% mutate(Vacc=Vacc/Population) %>% pull(Vacc) %>% range()
dose2 <- map(test, do_graph, dose=2, max_lim=limits)
saveGIF(
  { map(dose2, print) },
  "dose2.gif",
  ani.options = list(interval=0.5, ani.width=1200, ani.height=864)
)
