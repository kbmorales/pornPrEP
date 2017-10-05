# Purpose: to analyze the data and check primary hypothesis

# New guidelines recommend Truvada for PrEP: May 14, 2014

library(ggplot2)
library(wordcloud2)

# EDA

# Wordcloud of video tags
tags <- str_split(all_vids$tags, ", ")
tags <- data.frame(word = names(table(unlist(tags))), freq=as.numeric(table(unlist(tags)))) %>% arrange(desc(freq))
tags$word <- as.character(tags$word)
wordcloud(words = tags$word, freq = tags$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Checking out categories
cats <- str_split(all_vids$categories, ", ")
cats <- data.frame(category = names(table(unlist(cats))), freq=as.numeric(table(unlist(cats)))) %>% arrange(desc(freq))
cats$category <- as.character(cats$category)
# Remove categories useless from a content perspective
cats <- cats[!cats$category == "Gay", ]
cats <- cats[!cats$category == "HD", ]
cats <- cats[!cats$category == "Verified Amateurs", ]
cats <- cats[!cats$category == "Virtual Reality", ]
cats <- cats[!cats$category == "Exclusive", ]
cats <- cats[!cats$category == "Verified Models", ]
wordcloud(words = cats$category, freq = cats$freq, scale = c(3,.4), random.order=FALSE, rot.per=0.1, colors=brewer.pal(8, "Dark2"))

# Remove category: solo male???
nosolos <- all_vids[- grep("Solo Male", all_vids$categories),]

# Penalty applied to video views in order to standardize the data. Older videos will have a larger opporunity to gather more views
all_vids$penalty = 1/(2018 - all_vids$year)
all_vids$pviews = round(all_vids$views * all_vids$penalty)

# videos of "Bareback" category per year
table(all_vids$year, all_vids$bareback)
table(all_vids$production, all_vids$bareback)

# Videos per year
ggplot(all_vids, aes(year, views)) + geom_point()
ggplot(data = all_vids, aes(x = year)) + geom_bar()
ggplot(data = all_vids, aes(year, views)) + geom_col(aes(fill = bareback))
ggplot(data = all_vids, aes(x = year, y = views, fill = bareback)) + geom_bar(position = "fill", stat = "identity")

# Rating Analysis
all_vids %>% group_by(bareback) %>% summarise(watched = sum(views), n = n(), mean = mean(views), mrating = mean(rating)) %>% ungroup ()

# Mean Views & Mean Penalized Views
meanviews <- all_vids %>% group_by(year, bareback) %>% summarise(watched = sum(views), pwatched = sum(pviews), n = n(), mean = mean(views), pmean = mean(pviews)) %>% ungroup ()
meanviews <- meanviews[-1,]
ggplot(data = meanviews, aes(year, mean)) + geom_col(aes(fill = bareback))
ggplot(aes(x = year, y = watched, colour = bareback), data = meanviews) + geom_point()

