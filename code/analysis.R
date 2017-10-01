# Purpose: To examine videos for indications that they do not use condoms

library(rebus)
library(lubridate)
library(wordcloud2)

bb <- c("bareback", "bb", "breed", "creampie", "cream pie", "raw")

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

# New guidelines recommend Truvada for PrEP: May 14, 2014


# Notes

# str_detect could find bareback related tags and categories

# grep("stringofinterest",names(dataframeofinterest),ignore.case=TRUE,value=TRUE)
  
# To find bb-realted terms in either tags, title, or category:

# x_df[contains_bb, ]