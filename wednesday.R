# Wednesday script

# 1. scrape the page if not done
source("scrape_page.R")
# 2. run the daily
source("dhbs_to_90.R")

# 3. Download weekly data: Equity sheet through time and weekly vaccination details
source("dhb_90_project.R")
source("age_through_time.R") #< update equity sheet first
source("dhb_age_gender.R")
