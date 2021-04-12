## Hi Team! This is a test to see if we can use github to collaborate.
library(pacman)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)


data <- read.csv("house_sales.csv")

## checking where empty values are located.
colSums(is.na(data))
data <- drop(data$living_lot, data$living15_lot15)
#fill in sqft_living with basic addition of sqft_above and sqft_basement
data$sqft_living <- data$sqft_above + data$sqft_basement
head(data, 20)
#fill in bedrooms and bathrooms
data$bedrooms[which(is.na(data$bedrooms))] <- 2.5
data$bathrooms[which(is.na(data$bathrooms))] <- 2
data$sqft_lot[which(is.na(data$sqft_lot))] <- 6500
data <- subset (data, select = -c(living_lot, living15_lot15))
colSums(is.na(data))

#missing data is filled in.
##Moving forward we will look at the outliers of the main categories that we will be inspecting.

boxplot(data$bathrooms)
##bathroom outlier > 4
boxplot(data$bedrooms)
##bedroom outliers <2 and >5
boxplot(data$sqft_living)
##sqft_living outliers > 4000
boxplot(data$price)
##outliers > 1000000

#checking and removing outliers
data <- subset(data, (bedrooms <=5 & bedrooms >= 2 & sqft_living < 3700 & bathrooms < 3.5 & price <= 950000))
boxplot(data$bathrooms)
boxplot(data$bedrooms)
boxplot(data$sqft_living)
boxplot(data$price)

##Scatterplot built to check bedrooms vs bathrooms by year built
ggplot(data, aes(sqft_living, bathrooms, color=yr_built)) + geom_point()

##data$bathrooms <- data$bathrooms(is.na(data$bathrooms))
ggplot(data, aes(x=sqft_living, y=bedrooms, color = bathrooms)) + geom_point()
ggplot(data, aes(x = sqft_living, y = price, color = bedrooms)) + geom_point()
