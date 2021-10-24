library(tidyverse)
library(lubridate)
library(Manu)

# Read in our daily DHB data
dhb_files <- data.frame(files = list.files('data/dhb_daily', '*.csv', full.names = TRUE))
dailies <- dhb_files %>%
  extract(files, into="date", regex="([0-9-]+)", remove=FALSE) %>%
  mutate(date = ymd(date)) %>%
  arrange(desc(date)) %>%
  slice(1:2) %>%
  pull(files) %>%
  map_dfr(read_csv) %>%
  pivot_longer(Dose1:Dose2,
               names_to="Dose",
               values_to = "Vacc",
               names_prefix="Dose",
               names_transform = list(Dose = as.numeric)) %>%
  filter(!(DHB %in% c("Overseas / Unknown", "Total"))) %>%
  mutate(DHB = fct_recode(DHB,
                          `Hawke's Bay` = "Hawkes Bay"))

# Pull out today and yesterady
today <- dailies %>% filter(Date == max(Date))
yesterday <- dailies %>% filter(Date == min(Date))

# Combine into one ready for plotting
ready <- today %>% rename(Today = Vacc) %>% left_join(yesterday %>% rename(Yesterday = Vacc) %>% select(-Date)) %>%
  mutate(Number = Today - Yesterday) %>%
  pivot_longer(c(Today, Yesterday), names_to="Day", values_to="Vacc") %>%
  mutate(Vacc = Vacc/Population)

do_plot <- function(data, label) {
  cols <- data.frame(name = c("TodayLow",
                              "YesterdayLow",
                              "YesterdayHigh",
                              "TodayHigh"),
                     values = get_pal("Takapu")[1:4])

  numbers <- data %>% select(Dose, DHB, Vacc, Number) %>%
    group_by(Dose, DHB) %>% mutate(Test = if_else(max(Vacc) > 0.9, "Right", "Left")) %>%
    filter((Test == "Left" & Vacc == min(Vacc)) |
             (Test == "Right" & Vacc == max(Vacc))) %>%
    mutate(Just = if_else(Test == "Left", 1.1, -0.1),
           Number = prettyNum(Number, big.mark=","))

  plotme <- data %>% mutate(Side = if_else(Vacc < 0.9, "Low", "High")) %>%
    mutate(Col = paste0(Day, Side),
           Min = pmin(Vacc, 0.9),
           Max = pmax(Vacc, 0.9)) %>%
    arrange(Min, desc(Max))

  ggplot(plotme) +
    geom_segment(aes(y = fct_rev(DHB), yend=DHB, x = Min, xend = Max, col=Col), size=5) +
    geom_vline(aes(xintercept=0.9))+
    geom_text(data=numbers, aes(y=fct_rev(DHB), x = Vacc, label=Number, hjust=Just),
              size = 4) +
    theme_minimal(base_size = 18) +
    scale_x_continuous(labels = scales::label_percent(), breaks=c(0.7,0.8,0.9), expand=c(0.05,0.01)) +
    scale_colour_manual(values = cols %>% deframe(),
                        guide = 'none') +
    theme(panel.grid.major.y = element_blank(),
          axis.text = element_text(size = rel(0.8)),
          plot.tag.position = c(0.99, -0.02),
          plot.tag = element_text(hjust = 1, size = rel(0.6),
                                  vjust = 1,
                                  colour = 'grey50'),
          plot.margin = margin(6, 6, 30, 6)) +
    labs(x = NULL,
         y = NULL,
         title = "Road to 90%",
         tag = "Data from Ministry of Health. Chart by Jonathan Marshall. https://github.com/jmarshallnz/covid19nz")
}

png("today_dose1.png", width=900, height=640)
do_plot(ready %>% filter(Dose == 1)) +
  labs(  
    subtitle=paste("First doses given on", format(today %>% pull(Date) %>% unique(), "%A, %d %B %Y"))
    )
dev.off()

png("today_dose2.png", width=900, height=700)
do_plot(ready %>% filter(Dose == 2)) +
  labs(  
    subtitle=paste("Second doses given on", format(today %>% pull(Date) %>% unique(), "%A, %d %B %Y"))
  )
dev.off()
