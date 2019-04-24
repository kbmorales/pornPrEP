# Cleaned scrape code


# Setup -------------------------------------------------------------------


library(rvest)
library(stringr)
library(httr)
library(rebus)
library(dplyr)


# Scrape -----------------------------------------------

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
  viewkey <- randurl %>% 
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
duplicate <- duplicated(all_data[,8])
all_data <- all_data[!duplicate,]

# Save final dataset
save(all_data, file = here::here("data/raw_data/scrape_2019",
                                 str_c("randvid_data_",
                                       format(Sys.time(), "%Y%m%d"),
                                       "_",
                                       format(Sys.time(), "%H%M"),
                                       ".Rda"
                                       )
                                 )
     )
