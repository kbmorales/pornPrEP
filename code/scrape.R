library(rvest)
library(stringr)
library(httr)

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

all_data = do.call(rbind,dat.list)

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

# Framework for a scraping a video page

url <- 'https://www.pornhub.com/view_video.php?viewkey=ph59bc050e96ea6'
webpage <- read_html(url)
test_views <- webpage %>% html_nodes('.count') %>% html_text() %>% str_replace_all("[,]", "") %>% as.numeric()
test_rating <- webpage %>% html_nodes('.percent') %>% html_text() %>% str_replace("[%]", "") %>% as.numeric()
test_cat <- webpage %>% html_nodes('.categoriesWrapper') %>% html_text()
  test_cat <- str_replace_all(test_cat, "\n", "")
  test_cat <- str_replace_all(test_cat, "\t", "")
  test_cat <- str_replace(test_cat, pattern = fixed("Categories: "), "")
  test_cat <- str_replace(test_cat, pattern = fixed("+ Suggest"), "")
  # str_split(test_cat, ", ")
test_prod <- webpage %>% html_nodes('.production') %>% html_text()
test_tags <- webpage %>% html_nodes('.tagsWrapper') %>% html_text()
  test_tags <- str_replace_all(test_tags, "\n", "")
  test_tags <- str_replace_all(test_tags, "\t", "")
  # won't remove opening tags for some reason
  test_tags <- str_replace(test_tags, "Tags: ", "")
  test_tags <- str_replace(test_tags, pattern = fixed("+ Suggest"), "")
  # str_split(test_tags, ", ")
test_added <- webpage %>% html_nodes('.showLess:nth-child(6) .white') %>% html_text()

# NOTES from lab: Sep 25
# html_attrs() - grabs html attributes, grab href (link)

# create list of vectors for URLs from a video page
# unlist to grab the URLs