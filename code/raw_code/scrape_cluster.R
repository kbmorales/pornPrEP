# Scrape file to upload to cluster

library(rvest)
library(stringr)
library(httr)
library(rebus)
library(dplyr)


# Setup -------------------------------------------------------------------


# Category system from PH
category_numbers <- c(252, 48, 40, 66, 58, 44, 56, 362, 392, 68, 382, 71, 352, 47, 46, 52, 62, 262, 70, 64, 39, 322, 50, 45, 332, 402, 51, 60, 372, 84, 85, 312, 54, 82, 49, 272, 77, 106, 342)
category_names <- c("Amateur", "Asian", "Bareback", "Bear", "Big Dick", "Black", "Blowjob", "Casting", "Chubby", "College", "Compilation", "Creampie", "Cumshot", "Daddy", "Euro", "Fetish", "Group", "Handjob", "Hunks", "Interracial", "Japanese", "Jock", "Latino", "Massage", "Mature", "Military", "Muscle", "Pornstar", "POV", "Public", "Reality", "Rough Sex", "Solo Male", "Straight Guys", "Twink", "Uncut", "Vintage", "Virtual Reality", "Webcam")
names(category_numbers) <- category_names

# Base video URL
url_stem <- 'https://www.pornhub.com/gay/video'

urls <- paste0(url_stem, "?c=", category_numbers, "&si=1")

index_col_names <- c("Title", "Length", "Views", "Rating", "URL", "View Key")

trimpatvk <- fixed("https://www.pornhub.com/view_video.php?viewkey=")

# trimpatdate <- "\\d\\d\\d\\d/\\d\\d" Isn't working
trimpatdate <- dgt(6) %R% "/" %R% dgt(2) # if Rebus package installed

url <- "https://www.pornhub.com/gay/video/random"

randviddat.list = list()


# Old Scrape Code ---------------------------------------------------------


i <- 1

x <- 10000

# Seem unable to extract length from video
# length <- html_nodes(webpage, '.mhp1138_total') %>% html_text()

for(i in 1:x) {
  Sys.sleep(runif(1,0,1))
  randurl <- HEAD(url)[[1]]
  if (status_code(GET(randurl)) == 200) {
    webpage <- read_html(randurl)
    if (length(html_attr(html_nodes(webpage, '.premiumLocked'), name = "class") != 0)) {
      x <- x+1
    } else {
      title <- html_nodes(webpage, '.inlineFree') %>% html_text()
      views <- html_nodes(webpage, '.count') %>% html_text() %>% str_replace_all("[,]", "") %>% as.numeric()
      rating <- html_nodes(webpage, '.percent') %>% html_text() %>% str_replace("[%]", "") %>% as.numeric()
      categories <- html_nodes(webpage, '.categoriesWrapper > a') %>% html_text() %>% str_c(collapse = ", ")
      production <- html_nodes(webpage, '.production') %>% html_text()
      tags <- html_nodes(webpage, '.tagsWrapper > a') %>% html_text() %>% str_c(collapse = ", ")
      added <- html_nodes(webpage, xpath = '/html/head/link[5]') %>% html_attr(name = "href") %>% str_extract(pattern = trimpatdate) %>% str_replace("/", "")
      viewkey <- html_nodes(webpage, xpath = '/html/head/link[4]') %>% html_attr(name = "href") %>% str_replace(pattern = trimpatvk, "")
      randviddat.list[[i]] = data.frame(title,views,rating,categories,production,tags,added,viewkey)
    }
  } else {
    x <- x+1
  }
}

all_data = do.call(rbind, randviddat.list)
duplicate <- duplicated(all_data[,7])
all_data[,"duplicate"] <- duplicate
save(all_data, file = "data/randvid_data.Rda")


# Reformatted Scraping Code -----------------------------------------------
# Purpose: scrape metadata from random videos on the cluster

URL <- "https://www.pornhub.com/gay/video/random"
trimpatdate <- dgt(6) %R% "/" %R% dgt(2) # if Rebus package installed
trimpatvk <- fixed("https://www.pornhub.com/view_video.php?viewkey=")

# Initialize data list and vars for storage
randviddat.list = list()
i <- 1
maxvid <- 1000

# Iterator for scraping
for(i in 1:maxvid) {    
  # Time benchmark
  t0 <- Sys.time()
  response <- GET(URL)
  t1 <- Sys.time()
  # Pull web address of random video
  randurl <- HEAD(URL)[[1]]
  # Return if bad link
  if (status_code(GET(randurl)) != 200) { 
    maxvid <- maxvid + 1 
    next
  }
  # Read in HTML document for random URL
  webpage <- read_html(randurl)
  # Check if class of premiumLocked exists
  if (length(html_attr(html_nodes(webpage, '.premiumLocked'), name = "class") != 0)) {
    # Return if video is behind paywall
    maxvid <- maxvid + 1                
    next
  }
  title <- html_nodes(webpage, '.inlineFree') %>% 
    html_text()                             # Pull title
  views <- html_nodes(webpage, '.count') %>% 
    html_text() %>%
    str_replace_all("[,]", "") %>%
    as.numeric()                            # Pull and format viewcount
  rating <- html_nodes(webpage, '.percent') %>% 
    html_text() %>%
    str_replace("[%]", "") %>%
    as.numeric()                            # Pull and format rating value
  categories <- html_nodes(webpage, '.categoriesWrapper > a') %>%
    html_text() %>%
    str_c(collapse = ", ")                  # Pull official categories
  production <- html_nodes(webpage, '.production') %>% 
    html_text()                             # Pull production type
  tags <- html_nodes(webpage, '.tagsWrapper > a') %>% 
    html_text() %>% 
    str_c(collapse = ", ")                  # Pull and format video tags
  added <- html_nodes(webpage, xpath = '/html/head/meta[9]') %>%
    html_attr(name = "content") %>% 
    str_extract(pattern = trimpatdate) %>% 
    str_replace("/", "")                    # Pull and format dates added
  viewkey <- html_nodes(webpage, xpath = '/html/head/link[4]') %>%  
    html_attr(name = "href") %>% 
    str_replace(pattern = trimpatvk, "")    # Pull unique video identifier
  # Save video data as dataframe in indexed list
  randviddat.list[[i]] = 
    data.frame(title, views, rating, categories, production, tags, added, viewkey)
  # Delay next pull to prevent server overload
  response_delay <- as.numeric(t1-t0)
  Sys.sleep(10*response_delay)
}

# Combine listed dataframes into single dataframe
all_data = do.call(rbind, randviddat.list)
# Remove duplicates
duplicate <- duplicated(all_data[,7])
all_data[,"duplicate"] <- duplicate

# Save final dataset
save(all_data, file = "data/randvid_data.Rda")