### Project 2

library(rvest)

url <- "https://stackoverflow.com/jobs?sort=i&q=Data+Science&pg=1"

### if you want urls for all the pages
# urls <- paste0("https://stackoverflow.com/jobs?sort=i&q=Data+Science&pg=", 1:7)

fields <- url %>% read_html() %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "-job-item", " " ))]')

names <- fields %>% html_nodes(".-name") %>% html_text() %>% trimws()

### Not every job has salary listed

fields %>% html_nodes(".-salary") %>% html_text()

salaries <- sapply(fields, function(x) {
  tmp <- html_nodes(x, ".-salary") %>% html_text()
  ifelse(length(tmp) == 0, "Not listed", trimws(tmp))
})
salaries <- trimws(gsub("\\| \r\nEquity", "", salaries))

locations <- fields %>% html_nodes(".-location") %>% html_text() %>% trimws()
locations <- gsub('- \r\n', "", locations)

#### tags

tags <- fields %>% html_nodes("div p") %>% html_text() %>% trimws()
tags <- tags[!grepl("ago", tags)]

### Get job urls

x <- fields %>% html_nodes(".job-link") %>% html_attrs()
job.urls <- paste0("https://stackoverflow.com",
                   unname(unlist(sapply(x, function(x) if(x["class"]=="job-link") x["href"]))))

#x <- url %>% read_html() %>% html_nodes("h2 a") %>% html_attrs()
#job.urls <- paste0("https://stackoverflow.com", sapply(x, "[[", "href"))

#### Single URL example

description <- job.urls[1] %>% read_html() %>% html_nodes(".description") %>% html_text()
grep("python", description, ignore.case=TRUE)
grep("\\bR\\b", description, ignore.case=TRUE)
grep("\\bSAS\\b", description, ignore.case=TRUE)


#### Find sector? These fields are not consistent across 

info <- paste0(job.urls[1] %>% read_html() %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "-key", " " ))]') %>% html_text(),
               job.urls[1] %>% read_html() %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "-value", " " ))]') %>% html_text())
gsub("Industry: ", "", info[grep("Industry", info)])

### Just words
strsplit(trimws(description) ," ")

res <- sapply(job.urls, function(x) {
  desc <- x %>% read_html() %>% html_nodes(".description") %>% html_text()
  python <- any(grepl("python", desc, ignore.case=TRUE))
  R <- any(grepl("\\bR\\b", desc, ignore.case=TRUE))
  SAS <- any(grepl("\\bSAS\\b", desc, ignore.case=TRUE))
  
  info <- paste0(x %>% read_html() %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "-key", " " ))]') %>% html_text(),
                 x %>% read_html() %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "-value", " " ))]') %>% html_text())
  ind <- grep("Industry", info)
  sector <- ifelse(length(ind) == 0, NA, gsub("Industry: ", "", info[grep("Industry", info)]))
  
  c(python=python, R=R, SAS=SAS, Sector=sector)
})
res <- unname(res)

data <- data.frame("Company" = names, "Location"=locations, "Salary"=salaries, "Sector"=res[4,],
                   "Python"=res[1,], "R"=res[2,], "SAS"=res[3,], tags = tags)

### Should clean up sector to be a small number of categories