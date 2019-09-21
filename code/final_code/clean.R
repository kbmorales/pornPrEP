# Clean data from scrape


# Setup -------------------------------------------------------------------

library(lubridate)
library(dplyr)
library(stringr)

options(stringsAsFactors=FALSE)

# Read in data ------------------------------------------------------------

# Load
scraped_files <- list.files(path = here::here("data/raw_data/scrape_2019"))

for(i in 1:length(scraped_files)) {
  load(here::here("data/raw_data/scrape_2019",
                  scraped_files[i]))
  assign(str_c("scrape_", i), all_data)
}

rm(i, all_data)

# Combine data
scraped_data <- do.call(rbind, lapply(ls(pattern="scrape_\\d"), get))

rm(list = ls(pattern="scrape_\\d"))

# Coerce into proper type
scraped_data <- scraped_data %>% mutate(
  title = as.character(title),
  categories = as.character(categories),
  tags = as.character(tags),
  added = ymd(added),
  viewkey = as.character(viewkey)
)

# Check for duplicates in final dataset by viewkey
duplicate <- duplicated(scraped_data$viewkey)
table(duplicate) # About 33% duplicates

# Delete duplicate rows
scraped_data <- scraped_data[!duplicate,]

# See how many videos were uploaded in each year
table(year(scraped_data$added)) # 12 in 2009, 277 in 2018

### CONSIDER REMOVING 2009 and 2019 for data viz

# Add year variable to dataset
scraped_data$year <- year(scraped_data$added)

rm(duplicate, scraped_files)

# Bareback ----------------------------------------------------------------

# Identify BB videos by category

scraped_data <- scraped_data %>% mutate(bb_cat = if_else(
  str_detect(categories, "Bareback"), "Yes", "No"
))

# Identify BB videos by pattern

bb_pattern <- "[Bb]are|(?:^| )bb(?:$| )|breed|[Cc]ream|cum dump|felch|raw|condomless|no condom"

scraped_data <- scraped_data %>% mutate(bb_pat = if_else(
  str_detect(title, bb_pattern) | str_detect(categories, bb_pattern) | str_detect(tags, bb_pattern),
  "Yes",
  "No"
))

# Quick comparison
table(scraped_data$bb_cat, scraped_data$bb_pat)

# Save as test data for analysis
save(scraped_data, file = here::here("data/tidy_data",
                                     "scraped_test.Rda"))
