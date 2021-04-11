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


?fill
fill(data$bedrooms, .direction = "up")
colSums(is.na(data))
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

##Scatterplot built to check bedrooms vs bathrooms by year built
ggplot(data, aes(sqft_living, bathrooms, color=yr_built)) + geom_point()

##Splitting the data there seems to reveal that newer homes have increasing amount of bathrooms
h2000 <- subset(data, yr_built > 1995)
nh2000 <- subset(data, yr_built <= 1995)
##data$bathrooms <- data$bathrooms(is.na(data$bathrooms))
for(i in 1:length(data$bathrooms)) {
  if(is.na(data$bathrooms[i]) & data$yr_built > 1995) {
    data$bathrooms <- '3.5'
    print('1')
  }
  else if (is.na(data$bathrooms[i]) & data$yr_built <= 1995) {
    data$bathrooms <- '2.5'
    print('0')
  }
}
colSums(is.na(data))
boxplot(data$bathrooms)

     
ggplot(h2000, aes(sqft_living, bathrooms, color=yr_built)) + geom_point()
ggplot(nh2000, aes(sqft_living, bathrooms, color=yr_built)) + geom_point()


boxplot(data$sqft_living)
summary(data$bedrooms)
colSums(is.na(data))
ggplot(data, aes(x=sqft_living, y=bedrooms, color = bathrooms)) + geom_point()




colSums(is.na(data))
ggplot(data, aes(x = sqft_living, y = bedrooms, color = bathrooms)) + geom_point()
