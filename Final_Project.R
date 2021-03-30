## Hi Team! This is a test to see if we can use github to collaborate.
library(pacman)
library(ggplot2)

attach(data)
## checking where empty values are located.
colSums(is.na(data))
bedrooms_nooutliers <- subset(data,bedrooms <=5 & bedrooms >2)
boxplot(bedrooms_nooutliers)
summary(boxplot(bedrooms))
data$sqft_living <- data$sqft_above + data$sqft_basement
colSums(is.na(data))
ggplot(data, aes(x = sqft_living, y = bedrooms, color = bathrooms)) + geom_point()
