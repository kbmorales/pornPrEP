# Purpose: to analyze the data and check primary hypothesis

# New guidelines recommend Truvada for PrEP: May 14, 2014

library(ggplot2)
library(wordcloud)
library(igraph)
library(reshape)
library(tidyr)
library(ggthemes)

# EDA

# Wordcloud of tags
tags <- all_vids
tags <- tags[c("tags", "year")]
tags$tags <- str_split(tags$tags, ", ")
tags <- data.frame(word = names(table(unlist(tags$tags))), freq=as.numeric(table(unlist(tags$tags)))) %>% arrange(desc(freq))
tags$word <- as.character(tags$word)
wordcloud(words = tags$word, freq = tags$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

# Time trends in 10 most popular tags
poptags <- tags[1:10,1]
poptagspat <- str_c(poptags, collapse = "|")
tags <- all_vids
tags <- tags[c("tags", "year")]
tags$tags <- str_extract_all(tags$tags, poptagspat)

for(i in seq_along(poptags)) {
  tags[poptags[i]] <- str_count(tags$tags, poptags[i])
}

# Plot to see the proportion of videos by year that have the top 10 most popular tags
melttags <- melt(tags[,-1], id.vars = "year")
ggplot(data = melttags, aes(x = year, y = value, colour = variable)) + theme_minimal() + scale_color_brewer(palette="Paired") + stat_summary(fun.y = mean, geom = "line", size = 1) + scale_x_continuous(breaks=seq(min(melttags$year),max(melttags$year),1)) + theme(axis.text.x = element_text(angle=45)) + labs(title = "Top 10 user tags on videos", subtitle = "Proportion of videos containing tag", x="Year", y = "Proportion", color="Tag")


# Checking out categories

cats <- str_split(all_vids$categories, ", ")
cats <- data.frame(category = names(table(unlist(cats))), freq=as.numeric(table(unlist(cats)))) %>% arrange(desc(freq))
cats$category <- as.character(cats$category)
# Remove categories useless from a content perspective
cats <- cats[!str_detect(cats$category, "Gay|HD|Verified Amateurs|Virtual Reality|Exclusive|Verified Models"), ]
wordcloud(words = cats$category, freq = cats$freq, scale = c(3,.4), random.order=FALSE, rot.per=0.1, colors=brewer.pal(8, "Dark2"))

# Remove category: solo male???
nosolos <- all_vids[- grep("Solo Male", all_vids$categories),]


# Ratios of bareback to non-bareback films by year

# Videos of "Bareback" type per year
bbbyyear <- as.data.frame(prop.table(table(all_vids$year, all_vids$bareback), 1)*100)
colnames(bbbyyear) <- c("Year", "Bareback", "Percentage")
bbbyyear$Bareback <- factor(bbbyyear$Bareback, levels = c(0,1), labels = c("No", "Yes"))
ggplot(bbbyyear, aes(x=Year, y = Percentage, fill = Bareback)) + geom_col() + theme_minimal() + scale_fill_brewer(palette="Paired") + labs(title = "Percentage of Bareback Videos by Year")

# Videos by views
ggplot(all_vids, aes(x = bareback, y = views)) + theme_minimal() + scale_y_log10() + geom_violin()

# Total number of videos by bareback category
ggplot(all_vids, aes(x = year, fill = bareback)) + theme_minimal() + geom_bar(position = "dodge")

# By production status (I think this is highly suspicious)

table(all_vids$production, all_vids$bareback, all_vids$year)

# Total video views by bareback category
  # By year
  ggplot(data = all_vids, aes(year, views, fill = bareback)) + stat_summary(fun.y = sum, geom="bar", position = "dodge")
  # As percentage over a year
  ggplot(data = all_vids, aes(x = year, y = views, fill = bareback)) + geom_bar(position = "fill", stat = "identity")

# Rating Analysis
all_vids %>% group_by(bareback) %>% summarise(watched = sum(views), n = n(), mean = mean(views), mrating = mean(rating)) %>% ungroup ()


# Regression

# Penalty applied to video views in order to standardize the data. Older videos will have a larger opporunity to gather more views
all_vids$penalty = 1/(2018 - all_vids$year)
all_vids$pviews = round(all_vids$views * all_vids$penalty)

# Mean Views & Mean Penalized Views
meanviews <- all_vids %>% group_by(year, bareback) %>% summarise(watched = sum(views), pwatched = sum(pviews), n = n(), mean = mean(views), pmean = mean(pviews)) %>% ungroup ()
meanviews <- meanviews[-1,]
ggplot(data = meanviews, aes(year, mean)) + geom_col(aes(fill = bareback))
ggplot(aes(x = year, y = watched, colour = bareback), data = meanviews) + geom_point()

