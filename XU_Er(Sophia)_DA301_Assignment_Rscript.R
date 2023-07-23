# Install tidyverse.
install.packages('tidyverse')

# Import the tidyverse library.
library(tidyverse) 

# Import the data set (wages_plots.csv).
turtle_sales <- read.csv(file.choose(), header=TRUE) 

# View the data frame.
View(turtle_sales) 

# View a summary of the data frame.
summary(turtle_sales)

#get rid of the columns: ranking, year, genre and publisher
turtle_sales_2 <- subset(turtle_sales, select = -c(Ranking, Year, Genre, Publisher))

#view the new data frame
View(turtle_sales_2)

#View a summary of the new data frame
summary(turtle_sales_2)

#convert Product to categorical variable
turtle_sales_3 <- mutate(turtle_sales_2,
                         Product = as.factor(Product))
view (turtle_sales_3)
summary (turtle_sales_3)

# Specify X as product, y as global sales, and turtle_sales_2 as the data source 
# (the x-axis variable is passed first, followed by the y-axis,
#  and then the source of the data is specified).
qplot(Product, Global_Sales, data=turtle_sales_3)
qplot(Product, NA_Sales, data=turtle_sales_3)
qplot(Product, EU_Sales, data=turtle_sales_3)

#create histogram for global sales
qplot(Global_Sales, data= turtle_sales_3)
#adjust the number of bins
qplot(Global_Sales, bins = 10, data=turtle_sales_3)

#create a histogram for EU_Sales
qplot(EU_Sales, data = turtle_sales_3)
#adjust the number of bins
qplot(EU_Sales, bins=10, data=turtle_sales_3)

#create a histogram for NA_Sales
qplot(NA_Sales, data = turtle_sales_3)
#adjust the number of bins
qplot(NA_Sales, bins=10, data=turtle_sales_3)

#boxplot - sales and platform
qplot(Global_Sales, Platform, data=turtle_sales_3, geom='boxplot')

qplot(EU_Sales, Platform, data=turtle_sales_3, geom='boxplot')

qplot(NA_Sales, Platform, data=turtle_sales_3, geom='boxplot')

#week 5
#determine the mean, max and min of the three sales column
#1) North America
min (turtle_sales_3$NA_Sales)
max (turtle_sales_3$NA_Sales)
mean (turtle_sales_3$NA_Sales)

#2) Europe
min (turtle_sales_3$EU_Sales)
max (turtle_sales_3$EU_Sales)
mean (turtle_sales_3$EU_Sales)

#3) Total
min (turtle_sales_3$Global_Sales)
max (turtle_sales_3$Global_Sales)
mean (turtle_sales_3$Global_Sales)

summary(turtle_sales_3)

# We need to import tidyverse of dplyr for the group_by function.
library(dplyr)
library(ggplot2)

group_by(turtle_sales_3) %>% summarise(EU_Sales=sum(EU_Sales),
                            NA_Sales=sum(NA_Sales),
                            Global_Sales = sum(Global_Sales))

df_sales <- turtle_sales_3 %>% group_by(Product) %>% 
  summarise(EU_Sales=sum(EU_Sales),
            NA_Sales=sum(NA_Sales),
            Global_Sales = sum(Global_Sales), .groups = 'drop')

View(df_sales)

summary(df_sales)
sapply (df_sales, mean)
sapply (df_sales, sd)

#determine the normality of the sales data
qqnorm(turtle_sales_3$EU_Sales, main = "The Normality of EU Sales")
qqline(turtle_sales_3$EU_Sales, col='red')

#determine the normality of the sales data
qqnorm(turtle_sales_3$NA_Sales, main = "The Normality of NA Sales")
qqline(turtle_sales_3$NA_Sales, col='red')

#determine the normality of the sales data
qqnorm(turtle_sales_3$Global_Sales, main = "The Normality of Global Sales")
qqline(turtle_sales_3$Global_Sales, col='red')

## Shapiro-Wilk test:
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(turtle_sales_3$EU_Sales)
shapiro.test(turtle_sales_3$NA_Sales)
shapiro.test(turtle_sales_3$Global_Sales)

library(moments)


# Specify the skewness and kurtosis functions.
skewness(turtle_sales_3$EU_Sales) 
skewness(turtle_sales_3$NA_Sales)
skewness(turtle_sales_3$Global_Sales)


kurtosis(turtle_sales_3$EU_Sales)
kurtosis(turtle_sales_3$NA_Sales)
kurtosis(turtle_sales_3$Global_Sales)

#determine whether there is correlation between the sales columns 
cor(turtle_sales_3$EU_Sales, turtle_sales_3$NA_Sales)
cor(turtle_sales_3$EU_Sales, turtle_sales_3$Global_Sales)
cor(turtle_sales_3$NA_Sales, turtle_sales_3$Global_Sales)

library(ggplot2)
ggplot(turtle_sales_3, aes(x = Platform, y = EU_Sales)) + geom_boxplot()

ggplot(turtle_sales_3, aes(x = Platform, y = EU_Sales)) + 
  geom_boxplot(fill = 'blue',
           col = 'black')+
  labs (title = 'EU sales by platform')+
  coord_flip()

ggplot(turtle_sales_3, aes(x = Platform, y = NA_Sales)) + 
  geom_boxplot(fill = 'purple',
               col = 'black')+
  labs (title = 'NA sales by platform')+
  coord_flip()

ggplot(turtle_sales_3, aes(x = Platform, y = Global_Sales)) + 
  geom_boxplot(fill = 'red',
               col = 'black')+
  labs (title = 'Global sales by platform')+
  coord_flip()

#Week 6
#create a simple linear regression model
#find a correlation
#first remove non-sales columns
turtle_sales_4 <- subset(turtle_sales_3, select = -c(Product, Platform))
view(turtle_sales_4)
cor(turtle_sales_4)

plot (turtle_sales_4$NA_Sales, turtle_sales_4$Global_Sales)

model1 <- lm (Global_Sales ~ NA_Sales, turtle_sales_4)
model1

summary(model1)

#plot the model 
plot(model1$residuals)

par(mfrow=c(1, 1))

# Plot the relationship with base R graphics.
plot(turtle_sales_4$NA_Sales, turtle_sales_4$Global_Sales, 
     xlab = 'North American Sales', ylab = 'Global Sales')
coefficients(model1)

# Add line-of-best-fit.
abline(coefficients(model1), col = 'red')

#Create model 2 with EU sales data
plot (turtle_sales_4$EU_Sales, turtle_sales_4$Global_Sales)
model2 <- lm (Global_Sales ~ EU_Sales, turtle_sales_4)
model2

summary(model2)
plot(model2$residuals)

plot(turtle_sales_4$EU_Sales, turtle_sales_4$Global_Sales,
     xlab = 'EU Sales',
     ylab = 'Global Sales')
coefficients(model2)
# Add line-of-best-fit.
abline(coefficients(model2), col = 'blue')

#Compare the 2 models
par(mfrow=c(2, 1))

# Plot the relationship with base R graphics.
plot(turtle_sales_4$NA_Sales, turtle_sales_4$Global_Sales)
coefficients(model1)

# Add line-of-best-fit.
abline(coefficients(model1), col = 'red')

plot(turtle_sales_4$EU_Sales, turtle_sales_4$Global_Sales)
coefficients(model2)
# Add line-of-best-fit.
abline(coefficients(model2), col = 'blue')

#check whether NA sales and EU sales are correlated
model3 <- lm (NA_Sales ~ EU_Sales, turtle_sales_4)
summary (model3)

#create a multiple linear regression model
corPlot(turtle_sales_4, cex=2)

modela = lm(Global_Sales~EU_Sales+NA_Sales, data=turtle_sales_4)

summary(modela)

#predict global sales based on provided values
turtle_sales_test <- read.csv(file.choose(), header=TRUE)
colnames(turtle_sales_test)[1] = "NA_Sales"
colnames(turtle_sales_test)[2] = "EU_Sales"

view(turtle_sales_test)
str(turtle_sales_3)
head(turtle_sales_3)

predictTest = predict (modela, newdata = turtle_sales_test,
                       interval = 'confidence')

predictTest

turtle_sales_3 %>% filter (NA_Sales == 34.02, EU_Sales == 23.80)
turtle_sales_3 %>% filter (NA_Sales == 3.93, EU_Sales == 1.56)
turtle_sales_3 %>% filter (NA_Sales == 2.73, EU_Sales == 0.65)
turtle_sales_3 %>% filter (NA_Sales == 2.26, EU_Sales == 0.97)
turtle_sales_3 %>% filter (NA_Sales == 22.08, EU_Sales == 0.52)



















