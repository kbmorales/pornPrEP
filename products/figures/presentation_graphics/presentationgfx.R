# Presentation graphics

#
# Category Wordcloud
#

tiff('0.tiff', units = "in", width = 5, height = 5, res = 300)

wordcloud(words = cats$category, freq = cats$freq, scale = c(2.5,1), random.order=FALSE, rot.per=0.1, colors=brewer.pal(8, "Dark2"))

dev.off()

#
# Top 5 categories by upload year view proportions
#

tiff('1.tiff', units="in", width=10, height=5, res=300)

top5cats <- ggplot(data = popranktop5, aes(x = year, y = prop)) + theme_minimal() + scale_color_brewer(palette="Dark2") + geom_line(aes(color = cat), size = 2) + scale_x_continuous(breaks=seq(min(popranktop5$year),max(popranktop5$year),1)) + theme(legend.position = "bottom") + labs(title = "Proportion of Video Views per Upload Year by Category", subtitle = "Top 5 Categories by Overall Popularity", x="Year", y = "Proportion of Views", color = "Categories:")
top5cats + geom_vline(xintercept = 2013.5, colour = "red")
top5cats + geom_vline(aes(xintercept=2013.5), color = "red", linetype="dashed", size=1)

dev.off()

#
# BB vs. not: proportion of views (binned by upload year)
#

tiff('2.tiff', units="in", width=10, height=5, res=300)

catbb.plot <- ggplot(data = catsns, aes(x = year, y = views, fill = bbcat)) + theme_minimal() + geom_bar(stat = "identity", position = "fill") + scale_fill_brewer(palette="Paired") + scale_x_continuous(breaks=seq(min(catsns$year),max(catsns$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position="none") + labs(title = "Proportion of Yearly View Count", subtitle = "Comparison of Bareback Categorized Videos to Non-Bareback", x="", y = "Proportion of Views", fill = "Bareback")

catbb.plot + theme(legend.position="bottom") + geom_vline(aes(xintercept=2013.5), color = "red", linetype="dashed", size=1)

dev.off()

#
# BB vs. not: proportion of uploaded videos (binned by upload year)
#

tiff('3.tiff', units="in", width=10, height=5, res=300)

catbbcnt2.plot <- ggplot(data = catsns, aes(x = year, fill = bbcat)) + theme_minimal() + scale_fill_brewer(palette="Paired") + geom_bar(position = "fill") + scale_x_continuous(breaks=seq(min(catsns$year),max(catsns$year),1)) + theme(axis.text.x = element_text(angle=45)) + theme(legend.position= "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(title = "Proportion of Yearly Video Uploads", subtitle = "Comparison of Bareback Categorized Videos to Non-Bareback", x="Year", y = "Proportion of Video Uploads", fill = "Bareback")
catbbcnt2.plot + geom_vline(aes(xintercept=2013.5), color = "red", linetype="dashed", size=1)

dev.off()

#
# Linear regression to the mean on scatter plot (messed with this a lot to cut together legends)
#

tiff('4.tiff', units="in", width=8, height=5, res=300)

# Workable plot
regplot2 <- ggplot(cleandata, aes(x = cdyear, y = logpviews)) + theme_minimal() + theme(legend.position = "right") + scale_color_brewer(palette="Paired", labels = c("Non-Bareback", "Bareback")) + geom_point(aes(color = bbcat), position = "jitter", size = 0.5) + labs(title = "Linear Regression to the Mean", subtitle = expression(Log[10]*" Yearly View Rate of Videos by Bareback Categorization"), x="Year", y = expression(Log[10]*" Views / Year"), color = "Linear Regression")

# Add in cutoff
regplot2 <- regplot2 + geom_vline(aes(xintercept=0), color = "red", linetype="dashed", size=1)

# Add in splined regression lines
# Pre, NOT BB
regplot_test <- regplot2 + geom_segment(x = min(cdyear), y = coef(lm1)[1] + coef(lm1)[4]*min(cdyear), xend = 0, yend = coef(lm1)[1], size = 1)
# Post, NOT BB
regplot_test <- regplot_test + geom_segment(x = 0, y = coef(lm1)[1], xend = ceiling(max(cdyear)), yend = coef(lm1)[1] + coef(lm1)[5]*ceiling(max(cdyear)), size = 1, color = "black", show.legend = TRUE)
# Pre, BB
regplot_test <- regplot_test + geom_segment(x = min(cdyear), y = coef(lm1)[1] + coef(lm1)[3] + coef(lm1)[4]*min(cdyear), xend = 0, yend = coef(lm1)[1] + coef(lm1)[3], color = "darkslategray", size = 1)
# Post, BB
regplot_test + geom_segment(x = 0, y = coef(lm1)[1] + coef(lm1)[3], xend = ceiling(max(cdyear)), yend = coef(lm1)[1] + coef(lm1)[3] + coef(lm1)[5]*ceiling(max(cdyear)) + coef(lm1)[6], color = "darkslategray", size = 1)

dev.off()

# FOR X AXIS

tiff('5.tiff', units="in", width=8, height=5, res=300)

regplot <- ggplot(cleandata, aes(x = added, y = logpviews)) + theme_minimal() + theme(legend.position = "right") + scale_color_brewer(palette="Dark2") + geom_point(aes(color = bbcat), position = "jitter", size = 0.5) + labs(title = "Log Yearly View Rate of Videos by Bareback Categorization", subtitle = "Linear Regression of the Mean", x="Year", y = "log(Views / Year)", color = "Bareback")
regplot

dev.off()

#
# Quantile Regression Process
#

tiff('6.tiff', units="in", width=10, height=8, res=300)

plot(summary(rq(logpviews ~ cdyear:cutoff + nrating + cutoff*bbcat - cutoff, tau = 2:98/100)))

dev.off()
