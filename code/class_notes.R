# Notes: Sep 27
# Expository Graphs

# Acronyms: define at beginning of the paper, write entire acronym out in captions of figures / tables

library(ggplot2)

# make container for the data in the ggplot

# g is our object container for this ggplot2

# GGplot defines a canvas
g = ggplot(
  # define dataframe or tibble
  data = quakes,
  # aesthetics
  aes(x = lat, y = long, colour = stations)) +
  # geometry layers - points
  geom_point()

# Theme - check it out
?theme
# If you change text affects all text
theme(text = )
# Requires a class called element_text

g + theme(text = element_text(size = 20))

# shorthand 
tsize = function(size) element_text(size = size)

gbig = g + theme(axis.text = size(18), axis.title = tsize(20), legend.text = tsize(15), legend.title = tsize(15))

# make labels bigger

gbig = gbig + xlab("Latitude")

# add a title

gbig + ggtitle("Title") + theme(title = element_text(size = 30))

# legends

gbig2 = gbig + guides(colour = guide_colorbar(title = "Legend Title")); gbig2

# \n = new line
(title = "Legend\nTitle")

# horizontal justification
title.hjust = 0.5

# put legend inside a plot: coordinates are relative numbers from 0 - 1

gbig2 + theme(legend.position = c(0.3,0.35))

# changes x / y axis limit

ylim(c(160, max(quakes$long)))

# transparent legend fill

transparent_legend = theme(
  legend.background = element_rect(fill = "transparent"),
  legend.key = element_rect(fill = "transparent",
                            color = "transparent")
)

# title position
title.position = "right"

# manipulating after the fact

gbig2$guides$colour$title.position = "left"

# use ; to end a code line

# check out ggtheme
?ggtheme

g + theme_light

# check out ggplot2 basic code for EDA

g = ggplot(aes(y = am), data = mtcars) + 
  geom_point(position = position_jitter(height = 0.2)) + 
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se = FALSE) +
  geom_smooth(method = "loess", se = FALSE, col = "red")

g + aes(x = mpg)

gmpg = g + aes(x = mpg); gdrat = g + aes(x = drat)
gridExtra::grid.arrange(gmpg, gdrat, ncol = 2)

gmpg = gmpg + annotate(x = 30, y = 0.2, geom = "text", label = "A", size = 20)
gdrat = gdrat + annotate(x = 4.5, y = 0.2, geom = "text", label = "B", size = 20)
gridExtra::grid.arrange(gmpg,gdrat, ncol = 2)


# Class notes: Oct 2 2017
# Dimension Reduction

library(pheatmap)

# PCA stuff in two dimensions
prcomp(data)

# SVD -- scale subtracts means across rows
svd(scale(datamatrix)) # faster!

# impute missing values
impute.knn(data)
svd(data)

# set some to NA and impute and see if they look similar!!

# 10042017

# ggplots:

ggplot(stamp, aes(y = Thickness, x = 1)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(height = 0) + theme_big

boxplot(thick); 
stripchart(thick, add =TRUE, vertical=TRUE, jitter=0.1, method = "jitter", pch=19, col=2)

# Binning

par(mfrow=c(1,2))
hist(thick,col=2); hist(thick,breaks=100,col=2)

# Density plots

ggplot(stamp, aes(x = Thickness)) + 
  geom_density()

dens = density(thick); 
plot(dens, col=2)

# calculate number of modes in a univariate continuous data

nmodes <- function(y) {
  x <- diff(y)
  n <- length(x)
  sum(x[2:n] < 0  & x[1:(n - 1)] >  0)
}
nmodes(dens$y)

# Second Term

# Oct 25

# Shiny