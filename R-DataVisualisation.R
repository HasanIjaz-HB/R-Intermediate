
# 1. Read the Avocado Prices Dataset and answer Q2-Q10 according to the dataset. Avocado Dataset:
#   https://www.kaggle.com/neuromusic/avocado-prices
getwd()

fileName <- "avocado.csv"
avocado <- read.table(file=fileName, header=TRUE, sep=",")

# 2. (a) Find the average (mean) of the Total.Volume Then create an extra factor (myVolume) column by
# assigning it "low" if Total.Volume is equal to or below the mean and "high" if the Total.Volume
# is over the mean (Hint: Use ifelse() function). Transform its type to factor (b) Transform type of
# "myVolume", "type", and "region" to factor.
#part a and part b
total_mean <- mean(avocado$Total.Volume)
myVolume <- ifelse(avocado$Total.Volume <= total_mean, "low", "high")
head(avocado$Total.Volume)
head(myVolume)
myVolume <- as.factor(myVolume)
avocado <- cbind(avocado, myVolume)
avocado$type <- as.factor(avocado$type)
avocado$region <- as.factor(avocado$region)
summary(avocado[,c("type", "region", "myVolume")])

# 3. Draw a pie chart of the "type" attribute. Categories of the attribute and their percentages should be
# written on each category (Hints: Use table(), paste0() functions and the plotrix package).
install.packages("plotrix")
library(plotrix)

percentages <- round(table(avocado$type)/sum(table(avocado$type))*100,2)
percentages
per_names <- paste0(names(percentages), " ", percentages, sep="%")
per_names
pie3D(percentages, labels = per_names, explode=0.1, main="Avocado Regions")

# 4. Create a bar chart of "type" and Total.Bags. (Hints: Use aggregate() function and sub-setting).
x <- aggregate(avocado$Total.Bags, by = list(avocado$type), FUN = mean)
names(x) <- c("type", "totalBags")
barplot(x$totalBags, names.arg = x$type,
        xlab = "Type", ylab = "Total Avacado Bags")

# 5. Create histogram of Total.Volume.
hist(avocado$Total.Volume, xlab = "Total Avacado Volume")

# 6. Create the box-plot of Total.Volume, find the outliers and create a subset for outliers if exist. Print
# Total.Volume of first ten rows of the outliers subset.
boxplot(avocado$Total.Volume, xlab="Total Avacado Volume", ylab="$")

outlier_values <- boxplot.stats(avocado$Total.Volume)$out
outier_indexes <- which(avocado$Total.Volume %in% c(outlier_values))
head(outier_indexes)
priceOutliers <- avocado[outier_indexes, ]
priceOutliers[1:10, "Total.Volume"]

# 7. Create a heatmap with correlation values for X4046, X4225, and X4770.
install.packages("gplots")
library(gplots)
heatmap.2(cor(avocado[,c(5, 6, 7)]), Rowv = FALSE, Colv = FALSE, dendrogram = "none", cellnote = round(cor(avocado[,c(5, 6, 7)]),2), notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# 8. Create a scatter plot of X4770 vs. XLarge.Bags. According to categories of myVolume observations
# should be shown in different colors (Hint: Use ggplot2 package).
install.packages("ggplot2")
library(ggplot2)
ggplot(avocado, aes(y = X4770, x = XLarge.Bags, colour = myVolume)) + geom_point(alpha = 0.6)


# 9. Create detailed plots for X4046, X4225, and X4770 by using GGally package.
install.packages("GGally")
library(GGally)
ggpairs(avocado[,c(5, 6, 7)])

# 10. Create the box-plot of Total.Volume according to categories of myVolume

boxplot(avocado$Total.Volume ~ avocado$myVolume, xlab = "Volume Avocado Status", ylab = "Total Volume of a Single Avocado", horizontal = FALSE)
