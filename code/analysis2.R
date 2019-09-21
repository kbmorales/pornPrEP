
### ANALYSIS 2

# Purpose: to analyze the data and check primary hypothesis

library(lubridate)
library(reshape2)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(tidyr)
library(ggthemes)
library(kableExtra)
library(cowplot)
library(rdd)
library(rdrobust)
library(quantreg)

# Rename to clean_data to match old analysis

clean_data <- scraped_data

# Remove years 2009 and 2019 for now
table(clean_data$year)
clean_data <- clean_data %>% filter(year > 2009,
                      year < 2019)

# EDA ---------------------------------------------------------------------

### CONSIDER REMOVING 2009 and 2018 for DATA VIZ?!!

#
# TAGS
#

# Tags are essentially user-generated categories for videos. I created a wordcloud out of the 200 most common:

# Wordcloud of tags
tags <- clean_data[c("tags", "year")]
tags$tags <- str_split(tags$tags, ", ")
tags <- data.frame(word = names(table(unlist(tags$tags))), 
                   freq=as.numeric(table(unlist(tags$tags)))) %>% 
  arrange(desc(freq))

tags$word <- as.character(tags$word)
wordcloud(words = tags$word, 
          freq = tags$freq, 
          min.freq = 1, 
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Blues"))

# Bareback is #6, gay is #1
# Remove "gay" as not very useful
tags <- tags[-1, ]
wordcloud(words = tags$word, 
          freq = tags$freq, 
          min.freq = 1, 
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Blues"))

# Time trends in 5 most popular tags
poptags <- tags[1:5,1]
poptagspat <- str_c(poptags, collapse = "|")
tags <- clean_data[c("tags", "year")]
tags$tags <- str_extract_all(tags$tags, poptagspat)

# Convert year to character?
tags$year <- as.numeric(as.character(tags$year))

for(i in seq_along(poptags)) {
  tags[poptags[i]] <- str_count(tags$tags, poptags[i])
}

melttags <- melt(tags[,-1], id.vars = "year", factorsAsStrings = FALSE)

# Plot to see the proportion of videos by year that have the top 10 most popular tags

# Create a new factor for "Bareback"
# Add a factor for "bareback" category
melttags$bareback <- 0
melttags$bareback[melttags$variable == "bareback"] <- 1

melttags %>%
  ggplot(aes(x = year, y = value, colour = variable)) + 
  theme_minimal() + 
  scale_color_brewer(palette="Paired") + 
  stat_summary(fun.y = mean, 
               geom = "line", 
               size = 1) + 
  scale_x_continuous(breaks=seq(min(melttags$year),
                                max(melttags$year),
                                1)) + 
  theme(axis.text.x = element_text(angle=45)) + 
  labs(title = "Top 5 user tags on videos", 
       subtitle = "Proportion of videos containing tag", 
       x="Year", 
       y = "Proportion", 
       color="Tag")

#
# CATEGORIES
#

# Several of these categories would be useless to analyze: some specify the video format ("HD," "Virtual Reality"), the type of camerawork ("POV," "Webcam"), or the performer's relationship to PH itself ("Verified Models," "Verified Amateurs," "Exclusive"). Additionlly, the "Gay" category tag is useless for this analysis, since that is how the PH website filters videos for the straight and gay porn domains.

# Remove videos with only a single male performer in them as identified by category
nosolos <- clean_data[- grep("Solo Male", clean_data$categories),]
# Capture n for sample table
n3 <- nrow(nosolos)

# Wordcloud of categories for comparison; entire sample
cats <- str_split(nosolos$categories, ", ")
cats <- data.frame(category = names(table(unlist(cats))), freq=as.numeric(table(unlist(cats)))) %>% arrange(desc(freq))
cats$category <- as.character(cats$category)

# Remove analytically useless categories
badcats <- "Gay|HD|Virtual Reality|Verified Amateurs|Verified Models|Exclusive|POV|Webcam"
cats <- cats[!str_detect(cats$category, badcats), ]

# Wordcloud, removal of bad categories
wordcloud(words = cats$category, 
          freq = cats$freq, 
          scale = c(2.5,1), 
          random.order=FALSE, 
          rot.per=0.1, 
          colors=brewer.pal(8, "Blues"))

#
# Graphs for total videos and views by category
#

# Plot by video count
cats %>% 
  ggplot(aes(x = category, y = freq)) + 
  theme_minimal() + 
  scale_color_brewer(palette="Paired") + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits=rev(cats$category[1:20])) + 
  theme(legend.position = "bottom") + 
  labs(title = "Total videos by category", 
       subtitle = "Top 20 categories", 
       x="", 
       y = "Total number of videos")

# Calculate n, total views, and mean views by category and year
poprank <- cats[,1]
for(i in seq_along(poprank)) {
  if(i == 1){
    poprankyr <- data.frame(nosolos %>% group_by(year) %>% subset(subset = str_detect(categories, poprank[i])) %>% summarise(cat = poprank[i], n = n(), tviews = sum(views), mviews = mean(views)))
  }
  if(i >= 2) {
    poprankyr <- rbind(poprankyr, data.frame(nosolos %>% group_by(year) %>% subset(subset = str_detect(categories, poprank[i])) %>% summarise(cat = poprank[i], n = n(), tviews = sum(views), mviews = mean(views))))
  }
}

# Identify the most popular categories overall, and to use to filter categorical popularity ranks
topcats <- poprankyr %>% 
  group_by(cat) %>% 
  summarise(tviews = sum(mviews*n), 
            count = sum(n)) %>% 
  arrange(desc(tviews))

# Plot categories by view count across entire sample
topcats %>%  
  ggplot(aes(x = cat, y = tviews)) + 
  theme_minimal() + 
  scale_color_brewer(palette="Paired") + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits=rev(topcats$cat[1:20])) + 
  theme(legend.position = "bottom") + 
  labs(title = "Total views by category", 
       subtitle = "Top 20 categories", 
       x="", 
       y = "Total viewcount")

# Bareback is second most popular by views

#
# Category popularity over time
#

topcats <- as.data.frame(topcats)
popcats <- topcats[1:5,1]
popcatspat <- str_c(popcats, collapse = "|")
popranktop5 <- poprankyr[grep(popcatspat, poprankyr$cat),]
popranktop5 <- popranktop5[,c(1,2,4)] %>% 
  spread(cat, tviews)
viewtotalyr <- nosolos %>% 
  group_by(year) %>% 
  summarise(total = sum(views)) %>% 
  data.frame()
popranktop5 <- merge(popranktop5[-1,], viewtotalyr[-1,])
popranktop5[,2:6] <- popranktop5[,2:6] / popranktop5[,7]
popranktop5 <- gather(popranktop5[,-7])
colnames(popranktop5) <- c("year", "cat", "prop")

# Convert year variable to numeric
popranktop5$year <- as.numeric(as.character(popranktop5$year))

# Remove 2018
popranktop5 <- popranktop5[popranktop5$year != 2009 & popranktop5$year != 2018, ]

# By year plot
top5cats <- ggplot(data = popranktop5, aes(x = year, y = prop)) + theme_minimal() + scale_color_brewer(palette="Dark2") + geom_line(aes(color = cat), size = 2) + scale_x_continuous(breaks=seq(min(popranktop5$year),max(popranktop5$year),1)) + theme(legend.position = "bottom") + labs(title = "Proportion of Video Views per Upload Year by Category", subtitle = "Top 5 Categories by Overall Popularity", x="Year", y = "Proportion of Views", color = "Categories:")
top5cats

##
## Sensitivity of BB catagorized (bbcat) vs. BB identified by pattern (bbpat)
##

#
# Comparison by video count
#

# Check to see difference between bareback categories & bareback classification by string pattern
catsns <- nosolos[c("categories", "year", "views", "bbpat")]
catsns$bbcat <- factor(ifelse(str_detect(catsns$categories, "Bareback"), 1, 0), labels = c("No", "Yes"))
bbcomp <- data.frame(table(catsns$bbcat, catsns$bbpat))
names(bbcomp) <- c("BB Category", "BB Pattern", "Freq")

# Compare Number of Videos Categorized Bareback to those Identified as BB

# Formatting: defactor year; remove 2009 and 2018 videos: 
catsns$year <- as.numeric(as.character(catsns$year))
catsns <- catsns[catsns$year != 2009 & catsns$year != 2018, ]

# By count
catbbcnt.plot <- ggplot(data = catsns, aes(x = year, fill = bbcat)) + theme_minimal() + geom_bar(position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(melttags$year),max(melttags$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position="none") + labs(title = "Videos Categorized Bareback", subtitle = "Proportion Uploaded by Year", x="", y = "Proportion", fill = "Bareback")

isbbcnt.plot <- ggplot(data = catsns, aes(x = year, fill = bbpat)) + theme_minimal() + geom_bar(position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(melttags$year),max(melttags$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position= c(0.9,1.35)) + labs(title = "Videos Containing Bareback Pattern", subtitle = "Proportion Uploaded by Year", x="Year", y = "Proportion", fill = "Bareback")

plot_grid(catbbcnt.plot, isbbcnt.plot, labels = c("A", "B"), nrow = 2, align = "v", rel_heights = c(1,1))

# By views
catbb.plot <- ggplot(data = catsns, aes(x = year, y = views, fill = bbcat)) + theme_minimal() + geom_bar(stat = "identity", position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(catsns$year),max(catsns$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position="none") + labs(title = "Videos Categorized Bareback", subtitle = "Proportion of total views by binned year", x="", y = "Proportion of Views", fill = "Bareback")

patbb.plot <- ggplot(data = catsns, aes(x = year, y = views, fill = bbpat)) + theme_minimal() + geom_bar(stat = "identity", position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(catsns$year),max(catsns$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position= c(0.9,1.35)) + labs(title = "Videos Containing Bareback Pattern", subtitle = "Total views per year", x="Year", y = "Proportion of Views", fill = "Bareback")

plot_grid(catbb.plot, patbb.plot, labels = c("A", "B"), nrow = 2, align = "v", rel_heights = c(1,1))

catbb.plot + theme(legend.position="bottom") + geom_vline(xintercept = 2013.5, colour = "red")

# ONLY CATEGORIES (tailored for solo publication)

catbbcnt2.plot <- ggplot(data = catsns, aes(x = year, fill = bbcat)) + theme_minimal() + geom_bar(position = "dodge") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(catsns$year),max(catsns$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position= c(0.1,0.9), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(title = "Videos Categorized as Bareback vs. Not", subtitle = "Uploaded by Year", x="Year", y = "Total Videos", fill = "Bareback")
catbbcnt2.plot

# Boxplot for overall logpviews by bbcat and cutoff
ggplot(clean_data, aes(x = cutoff, y = logpviews)) + theme_minimal() + theme(legend.position = "right") + scale_color_brewer(palette="Paired") + geom_boxplot(aes(fill = bbcat)) + labs(title = "Log Yearly View Rate of Videos by Bareback Categorization", subtitle = "Pre- and post- cutoff", x="Cutoff", y = "log(Views / Year)", legend = "Bareback", fill = "Bareback")
# not super useful


# Boxplot for yearly logpviews by bbcat
ggplot(clean_data, aes(x = year, y = logpviews, group = interaction(year, bbcat))) + theme_minimal() + theme(legend.position = "none") + scale_color_brewer(palette="Paired") + geom_boxplot(aes(fill = bbcat)) + labs(title = "Log Yearly View Rate of Videos by Bareback Categorization", subtitle = "Entire sample", x="Bareback Category", y = "log(Views / Year)", color = "Bareback")
# Not super useful


##
## Final clean
##

# Add bbcat to nosolos
nosolos$bbcat <- factor(ifelse(str_detect(nosolos$categories, "Bareback"), 1, 0), labels = c("No", "Yes"))

# Re-arrange final dataset for analysis
clean_data <- nosolos[c("title", "views", "rating", "categories", "tags", "production", "added", "year", "dyear", "bbcat", "bbpat")]
clean_data <- arrange(clean_data, dyear)

###
### Analysis
### 

##
## Setup
##

#
# Outcome variable: views
# 

# Log of views

# Log-transform outcome varible to make approximately normal
clean_data$logviews = log10(clean_data$views)

# Check normality
plot(density(clean_data$logviews)) # looks good
qqnorm(clean_data$logviews) # looks good

# Log of penalized views: since we have no time-series data on how views were accrued, analyze as views gathered over 1 year?

# Establish earliest date data was scraped
firstscrapedate <- mdy("02-14-2018")
firstscrapedate <- decimal_date(firstscrapedate)

# Determine amount of time videos have been uploaded to PH
clean_data$dyrsup <- firstscrapedate - decimal_date(clean_data$added)  

# Remove videos uploaded after first scrape date
clean_data <- subset(clean_data, dyrsup >= 0)

# Determine yearly view count by dividing total views by upload time
clean_data$pviews <- clean_data$views / clean_data$dyrsup

# Log-transform views / year to make approximately normal
clean_data$logpviews <- log10(clean_data$pviews)
clean_data <- subset(clean_data, logpviews != Inf)
plot(density(clean_data$logpviews)) # Looks good
qqnorm(clean_data$logpviews) # Looks good

#
# Cutoff score
#

# MAY TO GIVE A LAG?
 
# Date of CDC approval: May 14, 2014
cutoffdate <- mdy("05-14-2014")
cutoffdate <- decimal_date(cutoffdate)

## SEE BELOW FOR VARIATION: center dyear at cutoff:

# Center cutoff date at meaningful 2009 origin
cutoffdate <- cutoffdate - 2009

# Assign videos to being uploaded before cutoff or not
clean_data$cutoff <- ifelse(clean_data$dyear <= cutoffdate, 0, 1)
clean_data$cutoff <- as.factor(clean_data$cutoff)
levels(clean_data$cutoff) <- c("Pre", "Post")
table(clean_data$cutoff) # Roughly half of the data set is in each category

## Centered dyear:
clean_data$cdyear <- decimal_date(clean_data$added) - cutoffdate

#
# Independent Variables?
#

# Rating - normalized
clean_data$nrating <- (clean_data$rating - mean(clean_data$rating)) / sd(clean_data$rating)


# Save dataset
save(clean_data, file = "data/a2working.Rda")

##
## Sample table
##

nrow(subset(clean_data, cutoff == "Pre"))
nrow(subset(clean_data, cutoff == "Post"))

prebbn <- subset(clean_data, cutoff == "Pre" & bbcat == "No")
prebby <- subset(clean_data, cutoff == "Pre" & bbcat == "Yes")
postbbn <- subset(clean_data, cutoff == "Post" & bbcat == "No")
postbby <- subset(clean_data, cutoff == "Post" & bbcat == "Yes")

nrow(prebbn)
mean(prebbn$logpviews)
sd(prebbn$logpviews)
mean(prebbn$rating)
sd(prebbn$rating)
mean(prebbn$dyear + 2009)
sd(prebbn$dyear + 2009)

nrow(prebby)
mean(prebby$logpviews)
sd(prebby$logpviews)
mean(prebby$rating)
sd(prebby$rating)
mean(prebby$dyear + 2009)
sd(prebby$dyear + 2009)

nrow(postbbn)
mean(postbbn$logpviews)
sd(postbbn$logpviews)
mean(postbbn$rating)
sd(postbbn$rating)
mean(postbbn$dyear + 2009)
sd(postbbn$dyear + 2009)

nrow(postbby)
mean(postbby$logpviews)
sd(postbby$logpviews)
mean(postbby$rating)
sd(postbby$rating)
mean(postbby$dyear + 2009)
sd(postbby$dyear + 2009)

tbl <- table(clean_data$cutoff, clean_data$bbcat)
chisq.test(tbl)

anova(aov(logpviews ~ cutoff * bbcat, data = clean_data))
anova(aov(rating ~ cutoff * bbcat, data = clean_data))
anova(aov(dyear + 2009 ~ cutoff * bbcat, data = clean_data))

# NOT WORKING RIGHT!!!
melted <- melt(clean_data, id.vars=c("cutoff", "bbcat"), measure.vars = c("views", "rating", "dyear"))
sumstats <- group_by(melted, cutoff, bbcat)
summarise(sumstats, n = n(), median=median(value), IQR=IQR(value))

##
## Regression
## 

# Attach final data set
attach(clean_data)

# Counts of stuff: skeleton formula
nrow(subset(clean_data, year == 2017 & bbcat == "Yes")) / nrow(subset(clean_data, year == 2017)) # example of proportion of cat per year

# Using aggregate to get some #s
bb_year_viewcounts <-aggregate(clean_data$views, list(BB = clean_data$bbcat, Year = clean_data$year), sum)

#
# Linear model 1 - entire sample
#

lm1 <- lm(logpviews ~ cdyear:cutoff + nrating + cutoff*bbcat - cutoff)
summary(lm1)

# try not controlling for rating?
lm2 <- lm(logpviews ~ cdyear  + cutoff*bbcat - cutoff)
summary(lm2)

### Plot

# Pretty data plot - FOR X AXIS
regplot <- ggplot(clean_data, aes(x = added, y = logpviews)) + theme_minimal() + theme(legend.position = "bottom") + scale_color_brewer(palette="Dark2") + geom_point(aes(color = bbcat), position = "jitter", size = 0.5) + labs(title = "Log Yearly View Rate of Videos by Bareback Categorization", subtitle = "Linear Regression of the Mean", x="Year", y = "log(Views / Year)", color = "Bareback")

# Workable plot
regplot2 <- ggplot(clean_data, aes(x = cdyear, y = logpviews)) + theme_minimal() + theme(legend.position = "bottom") + scale_color_brewer(palette="Dark2") + geom_point(aes(color = bbcat), position = "jitter", size = 0.5) + labs(title = "Log Yearly View Rate of Videos by Bareback Categorization", subtitle = "Linear Regression of the Mean", x="Year", y = "log(Views / Year)", color = "Bareback")

# Add in cutoff
regplot2 <- regplot2 + geom_vline(xintercept = 0, colour = "red")


# Add in splined regression lines
# Pre, NOT BB
regplot_test <- regplot2 + geom_segment(x = min(cdyear), y = coef(lm1)[1] + coef(lm1)[4]*min(cdyear), xend = 0, yend = coef(lm1)[1])
# Post, NOT BB
regplot_test <- regplot_test + geom_segment(x = 0, y = coef(lm1)[1], xend = ceiling(max(cdyear)), yend = coef(lm1)[1] + coef(lm1)[5]*ceiling(max(cdyear)))
# Pre, BB
regplot_test <- regplot_test + geom_segment(x = min(cdyear), y = coef(lm1)[1] + coef(lm1)[3] + coef(lm1)[4]*min(cdyear), xend = 0, yend = coef(lm1)[1] + coef(lm1)[3], colour = "blue")
# Post, BB
regplot_test + geom_segment(x = 0, y = coef(lm1)[1] + coef(lm1)[3], xend = ceiling(max(cdyear)), yend = coef(lm1)[1] + coef(lm1)[3] + coef(lm1)[5]*ceiling(max(cdyear)) + coef(lm1)[6], colour = "blue", show.legend = TRUE)

#
# Quantile Regression
#

# Quantile Regression Process
plot(summary(rq(logpviews ~ cdyear:cutoff + nrating + cutoff*bbcat - cutoff, tau = 2:98/100)))

# Khmaladze Test (dataset is too large to compute :-( )

ktls <- KhmaladzeTest(logpviews ~ cdyear:cutoff + nrating + cutoff*bbcat - cutoff, taus = -1, nullH = "location-scale", trim = c(0.05, 0.95))
ktl <- KhmaladzeTest(logpviews ~ cdyear:cutoff + nrating + cutoff*bbcat - cutoff, taus = -1, nullH = "location")

# Quantile Regression: most popular videos
taus <- c(0.8, 0.85, 0.9, 0.95)
qregquints <- rq(logpviews ~ cdyear:cutoff + nrating + cutoff*bbcat - cutoff, tau = taus)
summary(qregquints)

##
## BUNK BELOW HERE
##

#
# Linear model 1 - Top quintile of viewcounts
#

top20 <- clean_data[clean_data$logpviews >= quantile(clean_data$logpviews, probs = 0.8), ]
lmtop20 <- lm(data = top20, logpviews ~ cdyear:cutoff + nrating + cutoff*bbcat - cutoff)
summary(lmtop20)

#
# Regression Discontinuity Estimate
# 

# Plot
rdplot(logpviews, cdyear, p = 1, binselect = "qsmv", kernel = "triangular")