# Advertizing and MSE

# Sections not listed

# 0. Setup

library(tidyverse)
# 1. Read the advertizing data to make a tibble

adSales <- read_csv('Advertising.csv')
adSales

# Remove the counting integers in column 1
adSales <- adSales[,-1]
adSales

# 2. Make as scatterplot x = TV Budget y= Units Sold

pTV <- ggplot(adSales, aes(x = TV, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'TV Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Advertising Results') + hw
pTV

# 3. Make a plot like the ISLR text plot
# with vertical lines connecting the points
# to the regression line

# Focus on the relevant data in tibble
TVdat <- select(adSales, TV, Sales)

# Fit a linear model and
# save the results in list structure
TVmodel <- lm(Sales~TV,data = adSales)

# Extract the regression coefficient from the list structure
ab <- coef(TVmodel)

# Extract the predict values and add them to the tibble
TVdat$Pred <- predict(TVmodel)

# Make the plot

ggplot(TVdat, aes(x = TV,y=Sales)) +
  geom_abline(intercept = ab[1],slope = ab[2],
    color = "blue",size = 1.5) +
  geom_point(aes(y = Sales),
    shape = 21, fill = 'red',color = 'black', size = 3) +
  geom_segment(aes(xend = TV,y = Sales,yend = Pred)) +
  labs(x = 'TV Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Regression and Residual Lines') + hw

