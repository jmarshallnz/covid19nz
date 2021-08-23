library(shiny)
library(tidyverse)
library(Manu)

# load our data up
load_data <- function() {
  read_csv("equity.csv") %>%
    complete(Date, Age, DHB, Ethnicity) %>%
    mutate(DHB = fct_relevel(DHB,
                             "New Zealand", after=Inf))
}

curr_date <- function(data) {
  max(data$Date)
}

dhbs <- function(data) {
  setdiff(unique(data$DHB), "New Zealand")
}

colours <- get_pal("Hoiho")[c(3,4)]

do_plot <- function(data, dhb) {
  ggplot(data %>% filter(DHB %in% c(dhb, "New Zealand"))) +
    geom_hline(yintercept=1, col='black', size=1.2) +
    geom_line(aes(x=Date, y=RateRatio, group=Ethnicity), col = 'black', size=2.4) +
    geom_line(aes(x=Date, y=RateRatio, col=Ethnicity), size=2) +
    facet_grid(vars(DHB), vars(Age), scales = 'free_y') +
    theme_minimal(base_size = 24) +
    theme(legend.position = "bottom",
          panel.spacing.y = unit(30, "points")) +
    scale_y_log10(labels=scales::label_percent()) +
    scale_x_date(date_breaks = "6 weeks",
                 date_labels = "%d %b") +
    labs(x = NULL, y = "Vaccine rate ratio") +
    scale_colour_manual(values = colours)
}

server <- function(input, output, session) {

  dat <- reactiveValues(equity=load_data())

  observeEvent(dat$equity,{
               updateSelectInput(session,
                                 "dhb",
                                 choices = dhbs(dat$equity))})

  output$title <- renderUI({
    h2(paste("New Zealand COVID-19 vaccination equity to", format(curr_date(dat$equity), "%d %B %Y")))
  })
  output$dhb_plot <- renderPlot({
    if (!is.null(input$dhb)) {
      do_plot(dat$equity, input$dhb)
    }
  })

}

ui <- fluidPage(
  fluidRow(column(width=9,
                  htmlOutput("title"),
                  p("The horizontal line is equity. Above the line is more doses compared to the non-Māori, non-Pacific population, below is fewer.")),
           column(width=3,
                  selectInput("dhb", "Select DHB", choices = NULL))),
  fluidRow(plotOutput("dhb_plot", height='720px'))
)

shinyApp(ui, server)
