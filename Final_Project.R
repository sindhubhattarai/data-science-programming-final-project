## Hi Team! This is a test to see if we can use github to collaborate.
library(pacman)
library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("house_sales.csv")

## checking where empty values are located.
colSums(is.na(data))
#fill in sqft_living with basic addition of sqft_above and sqft_basement
data$sqft_living <- data$sqft_above + data$sqft_basement




data %>% fill(bedrooms, .direction = "up")
colSums(is.na(data.a))
data
boxplot(data$bathrooms)
##bathroom outlier > 4
boxplot(data$bedrooms)
##bedroom outliers <2 and >5
boxplot(data$sqft_living)
##sqft_living outliers > 4000

#checking and removing outliers
data <- subset(data, (bedrooms <=5 & bedrooms >= 2 & sqft_living < 4000))
colSums(is.na(data))

boxplot(data$sqft_living)
summary(data$bedrooms)
colSums(is.na(data))
ggplot(data, aes(x=sqft_living, y=bedrooms, color = bathrooms)) + geom_point()




colSums(is.na(data))
ggplot(data, aes(x = sqft_living, y = bedrooms, color = bathrooms)) + geom_point()
