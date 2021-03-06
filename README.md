
# New Zealand COVID-19 Vaccination data

This repository contains data from the Ministry of Health, available
here:

<https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-vaccine-data>

Each week there is a new spreadsheet, and this repository contains them
all for your convenience, as the website only provides the current
sheet.

In addition, population baselines are available as well, from Statistics
NZ.

The data are raw, as provided by the Ministry. They are not consistent.
You will need to do some data munging to get them to line up.

Some (not particularly nice) data wrangling code is in
`vacc_by_age_gender_ethnicity.R` which also produces a chart of
vaccination progress using `ggplot2`.
