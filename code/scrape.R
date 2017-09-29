library(rvest)
library(stringr)
library(httr)
library(rebus)
library(dplyr)

# Site URL structure: si = site index (TRUE or FALSE, o = order (mv = most viewed), t = time period (w = weekly), cc = country code? (ex.: us)

# Establish category numbers from PH's system

category_numbers <- c(252, 48, 40, 66, 58, 44, 56, 362, 392, 68, 382, 71, 352, 47, 46, 52, 62, 262, 70, 64, 39, 322, 50, 45, 332, 402, 51, 60, 372, 84, 85, 312, 54, 82, 49, 272, 77, 106, 342)
category_names <- c("Amateur", "Asian", "Bareback", "Bear", "Big Dick", "Black", "Blowjob", "Casting", "Chubby", "College", "Compilation", "Creampie", "Cumshot", "Daddy", "Euro", "Fetish", "Group", "Handjob", "Hunks", "Interracial", "Japanese", "Jock", "Latino", "Massage", "Mature", "Military", "Muscle", "Pornstar", "POV", "Public", "Reality", "Rough Sex", "Solo Male", "Straight Guys", "Twink", "Uncut", "Vintage", "Virtual Reality", "Webcam")
names(category_numbers) <- category_names

# Videos by "Site Index":
# Establishes URL system for scraping from index sites
# Example: https://www.pornhub.com/gay/video?c=40&si=1
# Only C for category and si for site index (=1 for TRUE)

url_stem <- 'https://www.pornhub.com/gay/video'

urls <- paste0(url_stem, "?c=", category_numbers, "&si=1")

index_col_names <- c("Title", "Length", "Views", "Rating", "URL", "View Key")

dat.list = list()

for(i in seq_along(urls)) {
  Sys.sleep(runif(1,1,10))
  webpage <- read_html(urls[i])
  titles = html_nodes(webpage, '#categoryListContent .index-title') %>% html_text()
  vid_length = html_nodes(webpage, '#categoryListContent .index-length') %>% html_text()
  views = html_nodes(webpage, '#categoryListContent .index-views') %>% html_text() %>% str_replace_all("[,]", "") %>% as.numeric()
  rating = html_nodes(webpage, '#categoryListContent .index-rating') %>% html_text() %>% str_replace("[%]", "") %>% as.numeric()
  url = html_nodes(webpage, '.index-title a') %>% html_attr("href")
  viewkey = str_replace(url, pattern = fixed("https://www.pornhub.com/view_video.php?viewkey="), "")
  dat.list[[i]] = data.frame(category = category_names[i],titles,vid_length,views,rating, url, viewkey)
  }

# Compile into single dataframe, identifies duplicates, saves data
all_data = do.call(rbind,dat.list)
duplicate <- duplicated(all_data[,7])
all_data[,"duplicate"] <- duplicate
save(all_data, file = "data/siteindex_data.Rda")

# Delete duplicate rows -- deletes after first appearance, so first categories will be "overrepresented" (if that matters)
all_data <- subset(all_data, !duplicated(all_data[,7]))
all_data <- all_data[,-ncol(all_data)]
save(all_data, file = "data/siteindex_data_nodup.Rda")

# Grab additional information from each video
all_data$url <- as.character(all_data$url)

viddat.list = list()

for(i in seq_along(all_data$url)) {
  Sys.sleep(runif(1,0,1))
  webpage <- read_html(all_data$url[i])
  categories <- html_nodes(webpage, '.categoriesWrapper > a') %>% html_text() %>% str_c(collapse = ", ")
  production <- html_nodes(webpage, '.production') %>% html_text()
  tags <- html_nodes(webpage, '.tagsWrapper > a') %>% html_text() %>% str_c(collapse = ", ")
  added <- html_nodes(webpage, '.showLess:nth-child(6) .white') %>% html_text()
  viddat.list[[i]] = data.frame(viewkey = all_data$viewkey[i],categories,production,tags,added)
}

# Save lists of video data as a dataframe
vid_data = do.call(rbind,viddat.list)

# Merge dataframes together, save
bysiteindex_data <- merge(all_data, vid_data)
save(bysiteindex_data, file = "data/siteindexvideo_data.Rda")

# Test: 'Gay' Video Title Data (no category)
url <- 'https://www.pornhub.com/gay/video?si=1'
webpage <- read_html(url)
cat_gay_title <- webpage %>% html_nodes('.index-title') %>% html_text()
cat_gay_length <- webpage %>% html_nodes('.index-length') %>% html_text()
cat_gay_views <- webpage %>% html_nodes('.index-views') %>% html_text() %>% str_replace_all("[,]", "") %>% as.numeric()
cat_gay_rating <- webpage %>% html_nodes('.index-rating') %>% html_text() %>% str_replace("[%]", "") %>% as.numeric()

# Compile vectors into dataframe for category
cat_gay <- data.frame(cat_gay_title, cat_gay_length, cat_gay_views, cat_gay_rating)
names(cat_gay) <- index_col_names
cat_gay = cat_gay[-1,]
cat_gay[order(-cat_gay[,3]),]

# NOTES
# create list of vectors for URLs from a video page
# unlist to grab the URLs