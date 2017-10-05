# Purpose: to analyze the data and check primary hypothesis

# New guidelines recommend Truvada for PrEP: May 14, 2014

library(ggplot2)

# Penalty applied to video views in order to standardize the data. Older videos will have a larger opporunity to gather more views
all_vids$penalty = 1/(2018 - all_vids$year)
all_vids$pviews = round(all_vids$views * all_vids$penalty)

# EDA

# videos of "Bareback" category per year
table(all_vids$year, all_vids$bareback)
table(all_vids$production, all_vids$bareback)

# Videos per year
ggplot(all_vids, aes(year, views)) + geom_point()
ggplot(data = all_vids, aes(x = year)) + geom_bar()
ggplot(data = all_vids, aes(year, views)) + geom_col(aes(fill = bareback))
ggplot(data = all_vids, aes(x = year, y = views, fill = bareback)) + geom_bar(position = "fill", stat = "identity")


meanviews <- all_vids %>% group_by(year, bareback) %>% summarise(watched = sum(views), pwatched = sum(pviews), n = n(), mean = mean(views), pmean = mean(pviews)) %>% ungroup ()
meanviews <- meanviews[-1,]
ggplot(data = meanviews, aes(year, mean)) + geom_col(aes(fill = bareback))
ggplot(aes(x = year, y = watched, colour = bareback), data = meanviews) + geom_point()

