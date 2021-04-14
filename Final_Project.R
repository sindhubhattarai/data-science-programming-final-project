## Hi Team! This is a test to see if we can use github to collaborate.
library(pacman)
library(ggplot2)
library(dplyr)
library(tidyr)
#install.packages("tidyverse")
library(tidyverse)


data <- read.csv("house_sales.csv")

## checking where empty values are located.
colSums(is.na(data))
#data <- drop(data$living_lot , data$living15_lot15)
#fill in sqft_living with basic addition of sqft_above and sqft_basement
data$sqft_living <- data$sqft_above + data$sqft_basement
head(data, 20)
#fill in bedrooms and bathrooms
data$bedrooms[which(is.na(data$bedrooms))] <- 2.5
data$bathrooms[which(is.na(data$bathrooms))] <- 2
data$sqft_lot[which(is.na(data$sqft_lot))] <- 6500
data <- subset (data, select = -c(living_lot, living15_lot15))
colSums(is.na(data))

?fill
fill(data$bedrooms, .direction = "up")
colSums(is.na(data))
data
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
boxplot(data$grade)
##outliers >9 and less than 6

#checking and removing outliers
data <- subset(data, (bedrooms <=5 & bedrooms >= 2 & sqft_living < 4000))
colSums(is.na(data))


data <- subset(data, (bedrooms <=5 & bedrooms >= 2 & sqft_living < 3700 & bathrooms < 3.5 & price <= 950000 
                      & grade <= 9 & grade >=6 ))
boxplot(data$bathrooms)
boxplot(data$bedrooms)
boxplot(data$sqft_living)
boxplot(data$price)
boxplot(data$grade)

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

ggplot(data, aes(x = sqft_living, y = price, color = bedrooms)) + geom_point()

#testing correlation between variables

# correlation between price and sqft_living
cor_1 <- cor.test(data$price, data$sqft_living, 
                method = "pearson")
cor_1
sprintf("The correlation Index between price and sqft living is %f" , cor_1$estimate)

#Both have moderate correlation 

#correlation  Index between price and sqft_lot
cor_2 <- cor.test(data$price, data$sqft_lot, 
                  method = "pearson")
cor_2
sprintf("The correlation Index between price and sqft lot is %f" , cor_2$estimate)

#very less correlation almost 0.

# Now testing correlation between price and condition.
cor_3 <- cor.test(data$price, data$condition, 
                  method = "pearson")
cor_3
sprintf("The correlation Index between price and condition is %f" , cor_3$estimate)

#correlation between price and yr_built.
cor_4 <- cor.test(data$price, data$yr_built, 
                  method = "pearson")
cor_4
sprintf("The correlation Index between price and yr_built  is %f" , cor_4$estimate)
ggplot(data, aes(price, yr_built, color= bathrooms)) + geom_point()
ggplot(data, aes(price, yr_renovated, color= bathrooms)) + geom_point()

#Boxplot to check outliers in grade 
boxplot(data$grade)


#correlation  between price and grade
cor_5 <- cor.test(data$price, data$grade, 
                 method = "pearson")
cor_5
ggplot(data, aes(price, grade , color= bedrooms)) + geom_point()
#boxplot(data$grade)
