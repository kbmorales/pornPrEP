# Purpose: To clean the data prior to analysis

library(lubridate)

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

# Bareback pattern work

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