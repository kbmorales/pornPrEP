### Purpose: to analyze the data and check primary hypothesis
### KeMo
###


# Setup -------------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(tidyr)
library(ggthemes)
library(kableExtra)
library(cowplot)
library(reshape2)

# Loak cleaned vid data
load("scraped_test.Rda")

# Remove 2009 (small sample size)
clean_data <- scraped_data %>% 
  filter(year != "2009")

###
### Analysis 1
###



# Titles Analysis ---------------------------------------------------------

bb_vids_titles <- clean_data
bb_vids_titles$bbtitle <- grepl(bb_pattern, bb_vids_titles$title, ignore.case=TRUE)

bb_vids_titles %>% 
ggplot(aes(x = year, fill = bbtitle)) + 
  theme_minimal() + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette="Paired") + 
  scale_x_continuous(breaks=seq(min(bb_vids_titles$year),
                                max(bb_vids_titles$year),
                                1)
                     ) + 
  theme(axis.text.x = element_text(angle=45)) + 
  theme(legend.position="bottom") + 
  labs(title = "Videos with Bareback Pattern in Title", 
       subtitle = "Uploaded by year", 
       x="Year", 
       y = "Proportion", 
       fill = "Bareback")

# Not super impressive

# Tags analysis -----------------------------------------------------------

# Tags are essentially user-generated categories for videos. I created a wordcloud out of the 200 most common:

# Wordcloud of tags
tags <- clean_data[c("tags", "year")]

tags$tags <- str_split(tags$tags, ", ")

tags <- data.frame(word = names(table(unlist(tags$tags))), 
                   freq = as.numeric(table(unlist(tags$tags)))) %>% 
  arrange(desc(freq))

tags$word <- as.character(tags$word)

# Remove "gay" as it's pretty obvious
tags <- tags %>% 
  filter(word != "gay")

wordcloud(words = tags$word, 
          freq = tags$freq, 
          min.freq = 100, 
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Blues"))

# Time trends in 5 most popular tags

poptags <- tags[1:5,1]
poptagspat <- str_c(poptags, collapse = "|")
tags <- clean_data[c("tags", "year")]
tags$tags <- str_extract_all(tags$tags, poptagspat)

for(i in seq_along(poptags)) {
  tags[poptags[i]] <- str_count(tags$tags, poptags[i])
}

# Plot to see the proportion of videos by year that have the top 10 most popular tags
# Create a new factor for "Bareback" 
melttags <- melt(tags[,-1], id.vars = "year")

# Add a factor for "bareback" category
melttags$bareback <- 0
melttags$bareback[melttags$variable == "bareback"] <- 1

# EDA
melttags %>% 
  ggplot(aes(x = year, y = value, colour = variable)) + 
  theme_minimal() + 
  scale_color_brewer(palette="Paired") + 
  stat_summary(fun.y = mean, geom = "line", size = 1) + 
  scale_x_continuous(breaks=seq(min(melttags$year),
                                max(melttags$year),1)) + 
  theme(axis.text.x = element_text(angle=45)) + 
  labs(title = "Top 5 user tags on videos", 
       subtitle = "Proportion of videos containing tag", 
       x="Year", 
       y = "Proportion", 
       color="Tag")

# Categories --------------------------------------------------------------

# Check to see difference between bareback categories & bareback classification by string pattern
cats <- clean_data %>% 
  select(categories, year, views, bb_cat, bb_pat)

table(cats$bb_cat, cats$bb_pat)

###
### Compare Number of Videos Categorized Bareback to those Identified as BB
###

## By proportion

# Using category data
cats %>%   
  ggplot(aes(x = year, fill = bb_cat)) + 
  theme_minimal() + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette="Paired") + 
  scale_x_continuous(breaks=seq(min(melttags$year),
                                max(melttags$year),
                                1)
                     ) + 
  theme(axis.text.x = element_text(angle=45)) + 
  theme(legend.position="none") + 
  labs(title = "Videos categorized as bareback", 
       subtitle = "Proportion uploaded by year", 
       x="Year", 
       y = "Proportion", 
       fill = "Bareback")

# Using string pattern to identify BB vids
cats %>% 
  ggplot(aes(x = year, fill = bb_pat)) + 
  theme_minimal() + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette="Paired") + 
  scale_x_continuous(breaks=seq(min(melttags$year),
                                max(melttags$year),
                                1)) + 
  theme(axis.text.x = element_text(angle=45)) + 
  theme(legend.position= c(0.9,1.35)) + 
  labs(title = "Videos containing bareback string pattern", 
       subtitle = "Proportion uploaded by year", 
       x = "Year", 
       y = "Proportion", 
       fill = "Bareback")

# Combine plots
# plot_grid(catbbcnt.plot, 
#           isbbcnt.plot, 
#           labels = c("A", "B"), 
#           nrow = 2, 
#           align = "v", 
#           rel_heights = c(1,1))

## By views

# BB Category

cats %>% 
  ggplot(aes(x = year, y = views, fill = bb_cat)) + 
  theme_minimal() + 
  geom_bar(stat = "identity", 
           position = "fill") + 
  scale_fill_brewer(palette="Paired") + 
  scale_x_continuous(breaks=seq(min(cats$year),
                                max(cats$year),
                                1)) + 
  theme(axis.text.x = element_text(angle=45)) + 
  theme(legend.position = "bottom") + 
  labs(title = "Videos categorized as bareback", 
       subtitle = "Total views per year", 
       x="Year", y = "Proportion of Views", 
       fill = "Bareback")

# BB pattern

cats %>% 
  ggplot(aes(x = year, y = views, fill = bb_pat)) + 
  theme_minimal() + 
  geom_bar(stat = "identity", 
           position = "fill") + 
  scale_fill_brewer(palette="Paired") + 
  scale_x_continuous(breaks=seq(min(cats$year),
                                max(cats$year),
                                1)) + 
  theme(axis.text.x = element_text(angle=45)) + 
  # theme(legend.position="none") + 
  labs(title = "Total views per year", 
       subtitle = "Videos containing bareback pattern", 
       x="Year", 
       y = "Proportion of Views", 
       fill = "Bareback")

# Combine plots
# plot_grid(catbb.plot, 
#           isbb.plot, 
#           legend, 
#           labels = c("A", "B"), 
#           nrow = 3, 
#           align = "v", 
#           rel_heights = c(4,4,1))

##
## Wordcloud of Categories
##

cats <- str_split(clean_data$categories, ", ")

cats <- data.frame(category = names(table(unlist(cats))), freq=as.numeric(table(unlist(cats)))) %>% arrange(desc(freq))

cats$category <- as.character(cats$category)

# Remove categories useless from a content perspective
cats <- cats[!str_detect(cats$category, "Gay|HD|Verified Amateurs|Virtual Reality|Exclusive|Verified Models"), ]
wordcloud(words = cats$category, 
          freq = cats$freq, 
          scale = c(3,.4), 
          random.order=FALSE, 
          rot.per=0.1, 
          colors=brewer.pal(8, "Blues"))

# Attempt to do a Popularity Ranking --------------------------------------

# Popularity ranking reveals categories by the number of views generated by all videos in a given category, weighted by the number of these videos. This shows the repetition of views on videos in a given category, revealing the consistency of viewers’ requests for this content. These categories may point to content for which demand surpasses what is offered by uploaders.

poprank <- cats[,1]

for(i in seq_along(poprank)) {
  if(i == 1){
  poprankyr <- data.frame(clean_data %>% 
                            group_by(year) %>% 
                            subset(subset = str_detect(categories, 
                                                       poprank[i])) %>% 
                            summarise(cat = poprank[i], 
                                      n = n(), 
                                      tviews = sum(views), 
                                      mviews = mean(views)
                                      )
                          )
  }
  
  if(i >= 2) {
    poprankyr <- rbind(poprankyr, 
                       data.frame(clean_data %>% 
                                    group_by(year) %>% 
                                    subset(subset = str_detect(categories, 
                                                               poprank[i])) %>% 
                                    summarise(cat = poprank[i], 
                                              n = n(), 
                                              tviews = sum(views), 
                                              mviews = mean(views))
                                  )
                       )
  }
}

topcats <- 
  poprankyr %>% 
  group_by(cat) %>% 
  summarise(tviews = sum(mviews*n)) %>% 
  arrange(desc(tviews))

# View(topcats)
# Bareback is the 2nd most viewed category overall

# Overall plot
topcats %>% 
  ggplot(aes(x = cat,
             y = tviews)) + 
  theme_minimal() + 
  scale_color_brewer(palette="Blues") + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits=rev(topcats$cat[1:20])) + 
  theme(legend.position = "bottom") + 
  labs(title = "Total views by category", 
       subtitle = "Top 20 categories", 
       x="Category", 
       y = "Viewcount")

topcats <- as.data.frame(topcats)
popcats <- topcats[1:5,1]
popcatspat <- str_c(popcats, collapse = "|")
popranktop5 <- poprankyr[grep(popcatspat, poprankyr$cat),]

## Categories over time

# Filter out 2010 (noisy)
popranktop5 <- popranktop5 %>% filter(year > 2010)

# By year plot
popranktop5 %>% 
  ggplot(aes(x = year, y = mviews)) + 
  theme_minimal() + 
  scale_color_brewer(palette="Paired") + 
  geom_line(aes(color = cat)) + 
  scale_x_continuous(breaks=seq(min(popranktop5$year),
                                max(popranktop5$year),
                                1)) + 
  scale_y_log10() + 
  theme(legend.position = "bottom") + 
  labs(title = "Mean views by category and year", 
       subtitle = "Top 5 categories by overall popularity, mean views log transformed", 
       x="Year", 
       y = "log(Views)", 
       color = "Categories:")

topcatplot <- subset(poprankyr, grepl(str_c(topcats$cat[1:5], collapse = "|"), cat))

topcatplot %>%
  ggplot(aes(x = year, y = mviews, colour = cat)) + 
  theme_minimal() + 
  scale_color_brewer(palette="Paired") + 
  geom_line() + 
  scale_x_continuous(breaks=seq(min(melttags$year),
                                max(melttags$year),
                                1)) + 
  theme(axis.text.x = element_text(angle=45), 
        legend.position = "bottom") + 
  labs(title = "Trends in mean viewcount", 
       subtitle = "Top 5 categories by total views", 
       x="Year", 
       y = "Viewcount", 
       color="Category")

# Ratios of bareback to non-bareback films by year ------------------------

# Videos of "Bareback" type per year
bbbyyear <- as.data.frame(prop.table(table(cleandata$year, cleandata$bareback), 1)*100)
colnames(bbbyyear) <- c("Year", "Bareback", "Percentage")
bbbyyear$Bareback <- factor(bbbyyear$Bareback, levels = c(0,1), labels = c("No", "Yes"))
ggplot(bbbyyear, aes(x=Year, y = Percentage, fill = Bareback)) + geom_col() + theme_minimal() + scale_fill_brewer(palette="Paired") + labs(title = "Percentage of Bareback Videos by Year")

# Videos by views
ggplot(cleandata, aes(x = bbcat, y = views)) + theme_minimal() + scale_y_log10() + geom_violin()

# Total number of videos by bareback category
ggplot(cleandata, aes(x = year, fill = bareback)) + theme_minimal() + geom_bar(position = "dodge")

ggplot(cleandata, aes(x = year, year = views, fill = bbcat)) + theme_minimal() + geom_bar(position = "dodge")

# By production status  --------------------------------------------------

# (I think this is highly suspicious)

table(cleandata$production, cleandata$bareback, cleandata$year)

# Total video views by bareback category
  # By year
  ggplot(data = cleandata, aes(year, views, fill = bareback)) + stat_summary(fun.y = sum, geom="bar", position = "dodge")
  # As percentage over a year
  ggplot(data = cleandata, aes(x = year, y = views, fill = bareback)) + geom_bar(position = "fill", stat = "identity")

# Rating Analysis
cleandata %>% group_by(bareback) %>% summarise(watched = sum(views), n = n(), mean = mean(views), mrating = mean(rating)) %>% ungroup ()


##
## Regression
##

# Penalty applied to video views in order to standardize the data. Older videos will have a larger opporunity to gather more views
cleandata$penalty = 1/(2018 - cleandata$year)
cleandata$pviews = round(cleandata$views * cleandata$penalty)

# Mean Views & Mean Penalized Views
# Remove 2009 first!
meanviews <- cleandata %>% group_by(year, bareback) %>% summarise(watched = sum(views), pwatched = sum(pviews), n = n(), mean = mean(views), pmean = mean(pviews)) %>% ungroup ()
ggplot(data = meanviews, aes(year, mean)) + geom_col(aes(fill = bareback))
ggplot(data = meanviews, aes(year, pmean)) + geom_col(aes(fill = bareback))
ggplot(aes(x = year, y = watched, colour = bareback), data = meanviews) + geom_point()


# PLAY WITH A BUNCH OF STUFF
# GET MEAN VIEWS BY BB CAT FACTOR BY YEAR
test <- cleandata %>% group_by(year, bbcat) %>% summarise(mviews = mean(views)) %>% spread(bbcat, mviews, sep = "_") %>% mutate(ratio = bbcat_Yes / bbcat_No) %>% as.data.frame()

# TRY BY PROPORTION
bbprops <- as.data.frame(prop.table(table(cleandata$year, cleandata$bbcat), 1)*100)
colnames(bbprops) <- c("year", "bbcat", "prop")
bbprops <- spread(bbprops, bbcat, prop, sep = "_")
bbprops$year <- as.numeric(bbprops$year)


# See what a plot of all data would look like with regression lines
# Plot all viewcounts 
ggplot(data = cleandata, aes(x = year, y = views)) + geom_point(aes(color = bbcat), position = "jitter") + theme_minimal() + scale_y_log10() + geom_smooth(aes(color = bbcat), method = lm, se = FALSE)

# Plot ratio of means
ggplot(data = meanviews, aes(x = year, y = ratio)) + geom_point() + theme_minimal() + geom_smooth()
DCdensity(cleandata$year, cutpoint = 2013.99, verbose = TRUE, plot = TRUE, ext.out = FALSE, htest = TRUE)

test <- cleandata %>% group_by(year, bbcat) %>% summarise(n = n())
plot(density(cleandata$year))

# RDD Test

density(bbprops$year, bw = "nrd0", adjust = 1, kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"), weights = NULL, window = kernel, width, give.Rkern = FALSE, n = 512, from, to, cut = 3, na.rm = FALSE)
RDestimate(bbprops$bbcat_Yes ~ bbprops$year, cutpoint = cutoffscore, kernel = "rectangular")
IKbandwidth(bbprops$year, bbprops$bbcat_Yes, cutpoint = cutoffscore, bw = 1.932473, verbose = TRUE, kernel = "rectangular")


test <- lprq(logpviews,accel,h=h,tau=.5)
+         lines(fit$xx,fit$fv,lty=i)

# Full sample
summary(rq(logpviews ~ phyear + cutoff, tau = 0.5))

# Among BB categorized videos
polyintrd <- rq(logpviews ~ phyear + I(phyear^2) + phyear*cutoff + I(phyear^2)*cutoff + cutoff, subset = bareback == "Yes", tau = 0.5)
summary(polyintrd)

polyintrd <- rq(logpviews ~ phyear + I(phyear^2) + phyear*cutoff + cutoff, subset = bareback == "Yes", tau = 0.5)

polyintrd <- rq(logpviews ~ phyear + I(phyear^2) + cutoff, subset = bareback == "Yes", tau = 0.5)

# Removing the higher-order polynomials
intrd <- rq(logpviews ~ phyear*cutoff, subset = bareback == "Yes" & year > 2010, tau = 0.5)
summary(intrd)

# Remove the interaction term
cutrd <- rq(logpviews ~ phyear + cutoff, subset = bareback == "Yes" & year > 2010, tau = 0.5)
summary(cutrd)

# Compare the two smaller models 
anova(cutrd, intrd)

regplot_2 + geom_abline(intercept = median$coefficients[1], slope = median$coefficients[2], color="black", show.legend = TRUE, linetype="dashed") + 
  geom_abline(intercept = median$coefficients[1] + median$coefficients[3], slope = median$coefficients[2], color="red", show.legend = TRUE, linetype="dashed")
