library(rvest)
library(stringr)

# First practicing with the site index of the so-called "gay" category of video 
# Site URL structure: si = site index (TRUE or FALSE, o = order (mv = most viewed), t = time period (w = weekly), cc = country code? (ex.: us)

# Establish category numbers from PH's system

category_numbers <- c(252, 48, 40, 66, 58, 44, 56, 362, 392, 68, 382, 71, 352, 47, 46, 52, 62, 262, 70, 64, 39, 322, 50, 45, 332, 402, 51, 60, 372, 84, 85, 312, 54, 82, 49, 272, 77, 106, 342)
category_names <- c("Amateur", "Asian", "Bareback", "Bear", "Big Dick", "Black", "Blowjob", "Casting", "Chubby", "College", "Compilation", "Creampie", "Cumshot", "Daddy", "Euro", "Fetish", "Group", "Handjob", "Hunks", "Interracial", "Japanese", "Jock", "Latino", "Massage", "Mature", "Military", "Muscle", "Pornstar", "POV", "Public", "Reality", "Rough Sex", "Solo Male", "Straight Guys", "Twink", "Uncut", "Vintage", "Virtual Reality", "Webcam")

names(category_numbers) <- category_names

url <- 'https://www.pornhub.com/gay/video?si=1'

webpage <- read_html(url)

# 'Gay' Category Video Title Data

cat_gay_title <- webpage %>%
  html_nodes('.index-title') %>%
  html_text()
head(cat_gay_title)

cat_gay_length <- webpage %>%
  html_nodes('.index-length') %>%
  html_text()
head(cat_gay_length)

cat_gay_views <- webpage %>%
  html_nodes('.index-views') %>%
  html_text() %>%
  str_replace("[,]", "") %>%
  as.numeric()
head(cat_gay_views)

cat_gay_rating <- webpage %>%
  html_nodes('.index-rating') %>%
  html_text() %>%
  str_replace("[%]", "") %>%
  as.numeric()
head(cat_gay_rating)

