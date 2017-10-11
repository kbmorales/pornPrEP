# Purpose: to analyze the data and check primary hypothesis

library(ggplot2)
library(wordcloud)
library(tidyr)
library(ggthemes)
library(kableExtra)
library(cowplot)
library(RColorBrewer)

# EDA


# Titles Analysis
bb_vids_titles <- cleandata
bb_vids_titles$bbtitle <- grepl(bb_pattern, bb_vids_titles$titles, ignore.case=TRUE)

ggplot(data = bb_vids_titles, aes(x = year, fill = bbtitle)) + theme_minimal() + geom_bar(position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(bb_vids_titles$year),max(bb_vids_titles$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position="bottom") + labs(title = "Videos with Bareback Pattern in Title", subtitle = "Uploaded by year", x="Year", y = "Proportion", fill = "Bareback")
# Not super impressive

# Tags analysis
# Tags are essentially user-generated categories for videos. I created a wordcloud out of the 200 most common:

# Wordcloud of tags
tags <- cleandata[c("tags", "year")]
tags$tags <- str_split(tags$tags, ", ")
tags <- data.frame(word = names(table(unlist(tags$tags))), freq=as.numeric(table(unlist(tags$tags)))) %>% arrange(desc(freq))
tags$word <- as.character(tags$word)
wordcloud(words = tags$word, freq = tags$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

# Time trends in 5 most popular tags
poptags <- tags[1:5,1]
poptagspat <- str_c(poptags, collapse = "|")
tags <- cleandata[c("tags", "year")]
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

ggplot(data = melttags, aes(x = year, y = value, colour = variable)) + theme_minimal() + scale_color_brewer(palette="Paired") + stat_summary(fun.y = mean, geom = "line", size = 1) + scale_x_continuous(breaks=seq(min(melttags$year),max(melttags$year),1)) + theme(axis.text.x = element_text(angle=45)) + labs(title = "Top 5 user tags on videos", subtitle = "Proportion of videos containing tag", x="Year", y = "Proportion", color="Tag")

# Checking out categories
# Check to see difference between bareback categories & bareback classification by string pattern
cats <- cleandata[c("categories", "year", "views")]
cats$bbcat <- factor(ifelse(str_detect(cats$categories, "Bareback"), 1, 0), labels = c("No", "Yes"))
cats$bbid <- cleandata$bareback

table(cats$bbcat, cats$bbid)

# Compare Number of Videos Categorized Bareback to those Identified as BB
# By count
catbbcnt.plot <- ggplot(data = catsns, aes(x = year, fill = bbcat)) + theme_minimal() + geom_bar(position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(melttags$year),max(melttags$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position="none") + labs(title = "Videos Categorized Bareback", subtitle = "Proportion Uploaded by Year", x="Year", y = "Proportion", fill = "Bareback")
isbbcnt.plot <- ggplot(data = catsns, aes(x = year, fill = bareback)) + theme_minimal() + geom_bar(position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(melttags$year),max(melttags$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position= c(0.9,1.35)) + labs(title = "Videos Containing Bareback Pattern", subtitle = "Proportion Uploaded by Year", x="Year", y = "Proportion", fill = "Bareback")

plot_grid(catbbcnt.plot, isbbcnt.plot, labels = c("A", "B"), nrow = 2, align = "v", rel_heights = c(1,1))

# By views
catbb.plot <- ggplot(data = catsns, aes(x = year, y = views, fill = bbcat)) + theme_minimal() + geom_bar(stat = "identity", position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(melttags$year),max(melttags$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position = "bottom") + labs(title = "Videos Categorized Bareback", subtitle = "Total views per year", x="Year", y = "Proportion of Views", fill = "Bareback")
legend <- get_legend(catbb.plot)
catbb.plot <- catbb.plot + theme(legend.position = "none")
# It appears that, while videos categorized as Bareback have enjoyed a tremendous boost since 2014, videos that seem to contain bareback content have had a more moderate increase.

isbb.plot <- ggplot(data = catsns, aes(x = year, y = views, fill = bareback)) + theme_minimal() + geom_bar(stat = "identity", position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(melttags$year),max(melttags$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position="none") + labs(title = "Videos Containing Bareback Pattern", subtitle = "Total views per year", x="Year", y = "Proportion of Views", fill = "Bareback")

plot_grid(catbb.plot, isbb.plot, legend, labels = c("A", "B"), nrow = 3, align = "v", rel_heights = c(4,4,1))


# Wordcloud of Categories
cats <- str_split(cleandata$categories, ", ")
cats <- data.frame(category = names(table(unlist(cats))), freq=as.numeric(table(unlist(cats)))) %>% arrange(desc(freq))
cats$category <- as.character(cats$category)
# Remove categories useless from a content perspective
cats <- cats[!str_detect(cats$category, "Gay|HD|Verified Amateurs|Virtual Reality|Exclusive|Verified Models"), ]
wordcloud(words = cats$category, freq = cats$freq, scale = c(3,.4), random.order=FALSE, rot.per=0.1, colors=brewer.pal(8, "Dark2"))


# Attempt to do a Popularity Ranking
# Popularity ranking reveals categories by the number of views generated by all videos in a given category, weighted by the number of these videos. This shows the repetition of views on videos in a given category, revealing the consistency of viewers’ requests for this content. These categories may point to content for which demand surpasses what is offered by uploaders.
poprank <- cats[,1]
for(i in seq_along(poprank)) {
  if(i == 1){
  poprankyr <- data.frame(cleandata %>% group_by(year) %>% subset(subset = str_detect(categories, poprank[i])) %>% summarise(cat = poprank[i], n = n(), mviews = mean(views)))
  }
  if(i >= 2) {
    poprankyr <- rbind(poprankyr, data.frame(cleandata %>% group_by(year) %>% subset(subset = str_detect(categories, poprank[i])) %>% summarise(cat = poprank[i], n = n(), mviews = mean(views))))
  }
}

topcats <- poprankyr %>% group_by(cat) %>% summarise(tviews = sum(mviews*n)) %>% arrange(desc(tviews))
# Bareback is the most viewed category overall

# Overall plot
ggplot(data = topcats, aes(x = cat, y = tviews)) + theme_minimal() + scale_color_brewer(palette="Paired") + geom_col() + coord_flip() + scale_x_discrete(limits=rev(topcats$cat[1:20])) + theme(legend.position = "bottom") + labs(title = "Total Views by Category", subtitle = "Top 20 Categories", x="Category", y = "Viewcount")

topcats <- as.data.frame(topcats)
popcats <- topcats[1:5,1]
popcatspat <- str_c(popcats, collapse = "|")
popranktop5 <- poprankyr[grep(popcatspat, poprankyr$cat),]

# By year plot
ggplot(data = popranktop5, aes(x = year, y = mviews)) + theme_minimal() + scale_color_brewer(palette="Paired") + geom_line(aes(color = cat)) + scale_x_continuous(breaks=seq(min(popranktop5$year),max(popranktop5$year),1)) + scale_y_log10() + theme(legend.position = "bottom") + labs(title = "Mean Views by Category per Year", subtitle = "Top 5 Categories by Overall Popularity, Mean Views Log Transformed", x="Year", y = "log(Views)", color = "Categories:")

topcatplot <- subset(poprankyr, grepl(str_c(topcats$cat[1:5], collapse = "|"), cat))

ggplot(data = topcatplot, aes(x = year, y = mviews, colour = cat)) + theme_minimal() + scale_color_brewer(palette="Paired") + geom_line() + scale_x_continuous(breaks=seq(min(melttags$year),max(melttags$year),1)) + theme(axis.text.x = element_text(angle=45), legend.position = "bottom") + labs(title = "Trends in Mean Viewcount", subtitle = "Top 5 Categories by Total Views", x="Year", y = "Viewcount", color="Category")

# Ratios of bareback to non-bareback films by year

# Videos of "Bareback" type per year
bbbyyear <- as.data.frame(prop.table(table(cleandata$year, cleandata$bareback), 1)*100)
colnames(bbbyyear) <- c("Year", "Bareback", "Percentage")
bbbyyear$Bareback <- factor(bbbyyear$Bareback, levels = c(0,1), labels = c("No", "Yes"))
ggplot(bbbyyear, aes(x=Year, y = Percentage, fill = Bareback)) + geom_col() + theme_minimal() + scale_fill_brewer(palette="Paired") + labs(title = "Percentage of Bareback Videos by Year")

# Videos by views
ggplot(cleandata, aes(x = bareback, y = views)) + theme_minimal() + scale_y_log10() + geom_violin()

# Total number of videos by bareback category
ggplot(cleandata, aes(x = year, fill = bareback)) + theme_minimal() + geom_bar(position = "dodge")

# By production status (I think this is highly suspicious)

table(cleandata$production, cleandata$bareback, cleandata$year)

# Total video views by bareback category
  # By year
  ggplot(data = cleandata, aes(year, views, fill = bareback)) + stat_summary(fun.y = sum, geom="bar", position = "dodge")
  # As percentage over a year
  ggplot(data = cleandata, aes(x = year, y = views, fill = bareback)) + geom_bar(position = "fill", stat = "identity")

# Rating Analysis
cleandata %>% group_by(bareback) %>% summarise(watched = sum(views), n = n(), mean = mean(views), mrating = mean(rating)) %>% ungroup ()


# Regression

# Penalty applied to video views in order to standardize the data. Older videos will have a larger opporunity to gather more views
cleandata$penalty = 1/(2018 - cleandata$year)
cleandata$pviews = round(cleandata$views * cleandata$penalty)

# Mean Views & Mean Penalized Views
# Remove 2009 first!
meanviews <- cleandata %>% group_by(year, bareback) %>% summarise(watched = sum(views), pwatched = sum(pviews), n = n(), mean = mean(views), pmean = mean(pviews)) %>% ungroup ()
ggplot(data = meanviews, aes(year, mean)) + geom_col(aes(fill = bareback))
ggplot(data = meanviews, aes(year, pmean)) + geom_col(aes(fill = bareback))
ggplot(aes(x = year, y = watched, colour = bareback), data = meanviews) + geom_point()

