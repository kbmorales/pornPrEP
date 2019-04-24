library(lubridate)
library(dplyr)

##
## Analysis 2 work 03.22.2018
##

#
# Joining all scraped datasets together
#

### Load and rbind together the separate files 
load("~/adsfinalproj/data/scrape/scrape02142018_1.Rda")
scrape1 <- all_data
load("~/adsfinalproj/data/scrape/scrape02142018_2.Rda")
scrape2 <- all_data
load("~/adsfinalproj/data/scrape/scrape02142018_3.Rda")
scrape3 <- all_data
load("~/adsfinalproj/data/scrape/scrape02142018_4.Rda")
scrape4 <- all_data
load("~/adsfinalproj/data/scrape/scrape02142018_5.Rda")
scrape5 <- all_data
load("~/adsfinalproj/data/scrape/scrape02142018_6.Rda")
scrape6 <- all_data
load("~/adsfinalproj/data/scrape/scrape02152018_1.Rda")
scrape7 <- all_data
load("~/adsfinalproj/data/scrape/scrape02152018_2.Rda")
scrape8 <- all_data
load("~/adsfinalproj/data/scrape/scrape02152018_3.Rda")
scrape9 <- all_data
load("~/adsfinalproj/data/scrape/scrape02152018_4.Rda")
scrape10 <- all_data
load("~/adsfinalproj/data/scrape/scrape02162018_1.Rda")
scrape11 <- all_data
load("~/adsfinalproj/data/scrape/scrape02162018_2.Rda")
scrape12 <- all_data
load("~/adsfinalproj/data/scrape/scrape02162018_3.Rda")
scrape13 <- all_data
load("~/adsfinalproj/data/scrape/scrape02162018_4.Rda")
scrape14 <- all_data
load("~/adsfinalproj/data/scrape/scrape02162018_5.Rda")
scrape15 <- all_data
load("~/adsfinalproj/data/scrape/scrape02172018_1.Rda")
scrape16 <- all_data
load("~/adsfinalproj/data/scrape/scrape02172018_2.Rda")
scrape17 <- all_data
load("~/adsfinalproj/data/scrape/scrape02172018_3.Rda")
scrape18 <- all_data
load("~/adsfinalproj/data/scrape/scrape02172018_4.Rda")
scrape19 <- all_data
load("~/adsfinalproj/data/scrape/scrape02182018_1.Rda")
scrape20 <- all_data
load("~/adsfinalproj/data/scrape/scrape02182018_2.Rda")
scrape21 <- all_data
load("~/adsfinalproj/data/scrape/scrape02182018_3.Rda")
scrape22 <- all_data
load("~/adsfinalproj/data/scrape/scrape02192018_1.Rda")
scrape23 <- all_data
load("~/adsfinalproj/data/scrape/scrape02202018_1.Rda")
scrape24 <- all_data
load("~/adsfinalproj/data/scrape/scrape02202018_2.Rda")
scrape25 <- all_data
load("~/adsfinalproj/data/scrape/scrape02212018_1.Rda")
scrape26 <- all_data
load("~/adsfinalproj/data/scrape/scrape02212018_2.Rda")
scrape27 <- all_data
load("~/adsfinalproj/data/scrape/scrape02222018_1.Rda")
scrape28 <- all_data
load("~/adsfinalproj/data/scrape/scrape02222018_2.Rda")
scrape29 <- all_data
load("~/adsfinalproj/data/scrape/scrape02242018_1.Rda")
scrape30 <- all_data
load("~/adsfinalproj/data/scrape/scrape02242018_2.Rda")
scrape31 <- all_data
load("~/adsfinalproj/data/scrape/scrape02252018_1.Rda")
scrape32 <- all_data

# Combine all datasets together
scrapeddats <- rbind(scrape1, scrape2, scrape3, scrape4, scrape5, scrape6, scrape7, scrape8, scrape9, scrape10, scrape11, scrape12, scrape13, scrape14, scrape15, scrape16, scrape17, scrape18, scrape19, scrape20, scrape21, scrape22, scrape23, scrape24, scrape25, scrape26, scrape27, scrape28, scrape29, scrape30, scrape31, scrape32, stringsAsFactors = FALSE)

# Remove duplicate column
scrapeddats <- scrapeddats[, -ncol(scrapeddats)]

# Check for duplicates in final dataset by viewkey
duplicate <- duplicated(scrapeddats[,8])
table(duplicate) # Fuck over 18k duplicates >.<

# Delete duplicate rows
scrapeddats <- scrapeddats[!duplicated(scrapeddats$viewkey),]

# Save
save(scrapeddats, file = "data/scraped2_nodup.Rda")

#
# Variable cleaning
#

# Correct variable types:
scrapeddats[1] <- as.character(scrapeddats[, 1]) # Title
scrapeddats[4] <- as.character(scrapeddats[, 4]) # Categories
scrapeddats[6] <- as.character(scrapeddats[, 6]) # Tags
scrapeddats[8] <- as.character(scrapeddats[, 8]) # Viewkey

# Convert date added w/ lubridate
scrapeddats[7] <- as.character(scrapeddats[, 7]) # Date added
scrapeddats[7] <- ymd(scrapeddats[, 7])

# See how many videos were uploaded in each year
table(year(scrapeddats[, 7])) # 12 in 2009, 277 in 2018

### CONSIDER REMOVING 2009 and 2018 for DATA VIZ?!!

# Create year binned variable to data viz
scrapeddats$year <- as.factor(year(scrapeddats$added))

# Histogram plot shows interesting distribution... may want to drop 2009 and 2018 from analysis?
plot(cleandata$year)

# Convert to decimal year format, add to database
decyear <- decimal_date(scrapeddats$added)
scrapeddats$dyear <- decyear
names(scrapeddats) <- c("title", "views", "rating", "categories", "production", "tags", "added", "viewkey", "year", "dyear")

# Set 2009 as origin--subtract 2009 from dyear
scrapeddats$dyear <- scrapeddats$dyear - 2009

#
# Bareback pattern work
#

# Identify BB videos by pattern

bb_pattern <- "bare|(?:^| )bb(?:$| )|breed|cream|cum dump|felch|raw|condomless|no condom"

bb_vids_titles <- scrapeddats[grepl(bb_pattern, scrapeddats$title, ignore.case=TRUE),]
bb_vids_cats <- scrapeddats[grepl(bb_pattern, scrapeddats$categories, ignore.case=TRUE),]
bb_vids_tags <- scrapeddats[grepl(bb_pattern, scrapeddats$tags, ignore.case=TRUE),]

bbvids <- rbind(bb_vids_cats, bb_vids_tags, bb_vids_titles, stringsAsFactors = FALSE)
bbvids <- unique(bbvids)
bbvids$bbpat <- 1

# Identify non-BB videos by pattern

notbbvids <- anti_join(scrapeddats, bbvids)
notbbvids$bbpat <- 0

# Combine data frames together
all_vids <- rbind(bbvids, notbbvids, stringsAsFactors = FALSE)
all_vids$bbpat <- as.factor(all_vids$bbpat)
levels(all_vids$bbpat) <- c("No", "Yes")

# Save
save(all_vids, file = "data/scrapeddats_bb_data.Rda")



##
## Analysis 1 work
##

# Purpose: To clean the data prior to analysis

# Work on time series

# constants in terms of days
year <- 365.24
month <- 30.44
week <- 7

bysiteindex_data$added <- as.character(bysiteindex_data$added)

# bysiteindex_data <- bysiteindex_data[,-ncol(bysiteindex_data)] # Remove date column if I fuck up

bysiteindex_data$dateadded <- Sys.Date()

# Generate dates added for videos
for (i in 1:nrow(bysiteindex_data)) {
  if (str_detect(bysiteindex_data$added[i], "year") == TRUE)
    bysiteindex_data$dateadded[i] <- today() - (as.numeric(str_extract(bysiteindex_data$added[i], START %R% number_range(0,9))) * year)
  if (str_detect(bysiteindex_data$added[i], "month") == TRUE)
    bysiteindex_data$dateadded[i] <- today() - (as.numeric(str_extract(bysiteindex_data$added[i], START %R% number_range(0,9))) * month)
  if (str_detect(bysiteindex_data$added[i], "week") == TRUE)
    bysiteindex_data$dateadded[i] <- today() - (as.numeric(str_extract(bysiteindex_data$added[i], START %R% number_range(0,9))) * week)
  if (str_detect(bysiteindex_data$added[i], "day") == TRUE)
    bysiteindex_data$dateadded[i] <- today() - (as.numeric(str_extract(bysiteindex_data$added[i], START %R% number_range(0,9))))
}

# Will unfortunately have to group videos by year, since we are lacking actual upload dates
bysiteindex_data$year <- year(bysiteindex_data$dateadded)

# Save at this point
save(bysiteindex_data, file = "data/siteindexvidyear_data.Rda")

# Remove extraneous data and reorder columns from site index scraping
bysiteindex_data <- bysiteindex_data[c("titles", "categories", "tags", "year", "views", "rating", "production", "viewkey", "url")]

bysiteindex_data$categories <- as.character(bysiteindex_data$categories)
bysiteindex_data$titles <- as.character(bysiteindex_data$titles)
bysiteindex_data$tags <- as.character(bysiteindex_data$tags)

#
# Bareback pattern work
#

bb_pattern <- "bare|(?:^| )bb(?:$| )|breed|cream|cum dump|felch|raw|condomless|no condom"

bb_vids_titles <- bysiteindex_data[grepl(bb_pattern, bysiteindex_data$titles, ignore.case=TRUE),]
bb_vids_cats <- bysiteindex_data[grepl(bb_pattern, bysiteindex_data$categories, ignore.case=TRUE),]
bb_vids_tags <- bysiteindex_data[grepl(bb_pattern, bysiteindex_data$tags, ignore.case=TRUE),]
bbvids <- rbind(bb_vids_cats, bb_vids_tags, bb_vids_titles, stringsAsFactors = FALSE)
bbvids <- unique(bbvids)
bbvids$bareback <- 1

notbbvids <- anti_join(bysiteindex_data, bbvids)
notbbvids$bareback <- 0
all_vids <- rbind(bbvids, notbbvids, stringsAsFactors = FALSE)

all_vids$production <- as.factor(all_vids$production)
all_vids$bareback <- as.factor(all_vids$bareback)

save(all_vids, file = "data/bysiteindex_bb_data.Rda")

# Notes

# str_detect could find bareback related tags and categories

# grep("stringofinterest",names(dataframeofinterest),ignore.case=TRUE,value=TRUE)

# To find bb-realted terms in either tags, title, or category:

# x_df[contains_bb, ]