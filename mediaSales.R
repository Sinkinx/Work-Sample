# Advertising and MSE


# Sections
# 0. Setup
# 1. Read the advertising data to make a tibble
# 2. Scatterplots and models y = f(x) + e
# 3. Panel based plot comparisons
# 4. The regression input variable domain
# 5. Linear Regression
# 6. The Mean Squared Error


# 0. Setup

library(tidyverse)
source('hw.R')
library(rgl)

fixLights <- function(specular = gray(c(.3,.3,0))){
  clear3d(type = "lights")
  light3d(theta = -50,phi = 40,
    viewpoint.rel = TRUE, ambient = gray(.7),
    diffuse = gray(.7),specular = specular[1])

  light3d(theta = 50,phi = 40,
    viewpoint.rel = TRUE, ambient = gray(.7),
    diffuse = gray(.7),specular = specular[2])

  light3d(theta = 0,phi = -70,
    viewpoint.rel = TRUE, ambient = gray(.7),
    diffuse = gray(.7),specular = specular[3])
}

# 1. Read the advertising data to make a tibble

adSales <- read_csv('Advertising.csv')
adSales

# Remove the counting integers in column 1
adSales <- adSales[,-1]
adSales

#
# Input variables may be called explanatory
# variables, predictor, or independent
# variables in different contexts.
#
# Looking at univariate summary statistics provide
# one way to start learning about the data.

summary(adSales)



# 2. Scatterplots and models y = f(x) + e where
#    y is Sales and x is one of the three
#    input variables
#
#    The plots below are similar to those
#    the ISLR text, Section 2.

pTV <- ggplot(adSales, aes(x = TV, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') + hw +
  labs(x = 'TV Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Advertising Results')
pTV

# In the TV budget plot above,
# the vertical spread of the points
# increases from left to right.
# Correspondingly, the variance of the
# residuals about the fitted line
# increases going left to right.
# This violates the assumption that
# the errors are identically distributed.

pRadio <- ggplot(adSales, aes(x = Radio, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Radio Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Advertising Results') + hw
pRadio

# In the Radio budget plot above, the vertical variation of points
# about the regression line is also increase as budgets get large.
# The assumption of identically distribute random errors is again
# violated.

pNews <- ggplot(adSales, aes(x = Newspaper, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
labs(x = 'Newspaper Budget in $1000s',
  y = 'Thousands of Units Sold',
  title = 'Advertising Results') + hw
pNews

# In the newspaper budget plot above, there is a different
# pattern. The vertical variation points about the
# regression line seem pretty uniform except for
# the highest budgets.
#
# What is clear is the reduced density of points after around
# $57,000 dollars. There are only two points larger than $90,000.
# For the high budget part of the plot it is hard to assess the
# local variation in thousands of units sold.
#
# People controlled the budget spending.  The reduced density
# does not direcly cause and inference problem.

# 3. Panel based plot comparisons


adSales

adSalesG <- gather(
  adSales,
  key = "Media",
  value = "Budget",
  TV:Newspaper,
  factor_key = TRUE
)

adSalesG

ggplot(adSalesG, aes(x = Budget, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Advertising Results') +
  facet_grid(Media ~ .)  + hw

# Note the media labels on the right of the
# three panels.
#
# Now the x-axes are the same and we
# clearly see that Radio has the steepest slope.
#
# The fitted blue lines suggest that $50,000 for
# Radio advertising yielded about same units
# sold as $300,000 for TV sales.
#
# We can't really know if the units sold will
# continue to increase with an increase budget
# because we don't have data.

# For comparison proposes a superposed plot
# provides an alternative to a juxtaposed panel
# plot.
#
# The superpose plot distinguished the Media
# by panel membership.  Below we use the line
# color aesthetic in the geom_smooth to
# distinguish Media.

ggplot(adSalesG, aes(x = Budget, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm', aes(color = Media)) +
  labs(x = 'Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Advertising Results') + hw


lm(Sales~TV,data = adSales)
lm(Sales~Newspaper,data = adSales)


# 4. The regression input variable domain


# 4.1  Below we produce a 3D scatterplot
#      to parts of the data domain with
#      points relative the range (min and max)
#      for each of the variables

domain <- as.matrix(adSales[, -4])
open3d(FOV = 0)  # no perspective projection
fixLights()
plot3d(
  domain,
  type = "s",
  radius = 3.2,
  col = rgb(1, .2, .2),
  aspect = TRUE
)

# The rgl plot may appear in a small
# window at a strange place on your screen,
# such along the base of the screen or behind
# the RStudio window.
#
# Once located, left click on the plot lower
# right corner and drag to change its size.
#
# Left click on the top blue bar
# and drag to move the plot.
#
# Left click in the plot and drag
# to rotate the cube.
#
# Right click and move down or up
# to zoom in or out respectively.


# 4.2  A look at 2D Radio and TV Domain

ggplot(adSales,aes(x = Radio,y = TV)) +
  geom_point(shape = 21,fill = 'red',
     color = 'black',size = 2.5) +
  labs(x = 'Radio Budget in $1000s',
       y = 'TV Budget in $1000s',
       title = 'Advertising Data') + hw

# 5.  Linear Regression

# Below, Sales is the dependent variable
# .  means all other variable are input
#    variables

adModel1 <- lm(Sales~.,data = adSales)
adModel1
summary(adModel1)

# The Multiple R-squared .897 indicates that
# the model accounts for roughly 90% of the
# Sales variability
#
# The adjusted R-squared is a little smaller and
# include a penalty for including more variables
# in a model. We can include random noise as
# variables in the model and improve the fit.
#
# Assuming the standardized residuals have
# roughly a normal distribution we make can
# statistical inferences about the model.
# The probability of the F-statistics being so
# large at random is basically 0.  The
# F-statistic compares fitting all the variables
# to fitting just the dependent variable mean.
#
# The t-statistics are based individual
# variables improving the fit with the other
# variables listed in regression output
# already in the model.  Is there
# strong evidence that the regression coefficient
# is not zero? What is the probability that
# improved fit is due random variation?  For
# TV and Radio, the probability is close to 0.
# For Newspaper, the p value of .86 suggests
# that using white noise has better chance of
# improving the fit.

# The correlation matrix shows that Newspaper
# budget is substantially correlated (.354) with
# the Radio budget.

cor(adSales[,1:3])

# In the 1 variable newspaper model, newspaper
# get some credit for high sales because it was
# high when Radio sales were high. However when
# TV and Radio are in the model the Newspaper budget
# has almost no impact on the fit.

Res_Sales <- residuals(lm(Sales~TV+Radio,adSales))
Res_News <- residuals(lm(Newspaper~TV + Radio,adSales))
ResRes <- tibble(Res_Sales, Res_News)
subtitletxt <- paste("Residuals From Regressing",
  "Sales and the Newspaper Budget on TV and Radio Budgets")

ggplot(ResRes, aes(x = Res_News, y = Res_Sales)) +
  geom_smooth(size = 2, color = 'red') +
  geom_point(shape = 21, fill = 'cyan',
    color = "black", size = 2.5) +
  labs(x = "Newspaper Budget Residuals",
     y = "Sales Residuals", title = "Adjusted Variable Plot",
     subtitle = subtitletxt) + hw

# The unique contribution of the Newspaper budget to
# model domain has no relationship to Sales residual
# from fitting a TV+Radio model.

# 5.1 Regression diagnostics plots

plot(adModel1)

# As indicated in the console, after clicking
# in the console, hit Return to see
# the next plot in the set of 4 regression,
# diagnostics plots.
#
# Residual versus fitted values plot
# To match the assumption that the model errors
# are independent and identically distributed
# normal random variables residuals centered
# roughly centers about the line y = 0.
# The red line smooth of the residuals around
# shows curvature.  This violates the model
# assumption need justify making statistical
# inference model and it coefficients.
#
# In this plot, the points numbered 131, 6 and
# 179 are low value outliers. Removing them will
# likely reduce the bend in the red line.
# We might consider deleting the points
# if we have a good reason to think that one or
# more of their value are flawed. Of course then
# we should also wonder there other flawed values
# are that don't stand out as outliers.

# When residuals are plotted against a
# variable and the smooth looks like a parabola,
# including the square of variable's values in the
# model may yield a better fitting model.
# Here any variable highly correlated to the
# fitted values will likely be helpful.
#
# Plot 2: the Normal Q-Q plot.
# We see the outliers and a thick
# left tail.  That is, points on the left are far
# below the reference line. The residuals do have
# an approximately normal distribution.
# Statistical inference (hypothesis tests and
# confidence intervals) for the model as a whole
# and for the individual terms are not justified.

# The right tail is thin. The right-side points
# are on the center-of-the-plot side of the
# reference line.  Thin tails are of less
#  concerned in linear regression.

# Plot 3, the scale-location plot
# The y-axis is the square root of the absolute
# standardized residuals. The regression residuals
#  have covariance matrix that is based on the
# design matrix. In general the correlations are
# ignorable. The variances are not.  Standardized
# residual have been divide by their estimated
# standard deviations.

# The absolute value transformation puts all the
# negative residuals on the positive side of the
# zero.
#
# The square-root transformation helps balance the
# small and large absolute residuals. The red
# smooth line should be y = 1.  The curved line
# means the residual don't have the same variance.
# Our independent identically distributed errors
# assumption fails in terms of the mean (Plot 1)
# and the variance (Plot 3)
#
# Plot 4 Standardized Residuals versus Leverage
#
# The high leverage points are on the far right.
# The influence of a depends on its leverage and
# have far it would be from the regression
# line if it were omitted.  Points 131 and 6 are
# high influence points. They have substantial
# leverage and large standardized residuals.

# 5.2 A TV and Radio model

adModel2 <- lm(Sales~TV+Radio, data = adSales)
summary(adModel2)
plot(adModel2)

# Dropping the term didn't change much.

# 5.3 Adding an interaction terms for TV and Radio

# In the R linear model syntax
# TV:Radio is an interaction term
# This multiples the TV and Radio vectors and
# includes the resulting vector in the model.
#
# TV*Radio is interpreted as TV + Radio + TV:Radio

# There are three different ways to specify the same model


# This result is the same
adModel3a <- lm(Sales~ TV+Radio + TV:Radio, adSales)
summary(adModel3a)

# Results of direct mathematical operations on
# variables need to be surrounded by I()
# This result is the same
adModel3b  <- lm(Sales~TV + Radio + I(TV*Radio), adSales)
summary(adModel3b)

# 5.4 Adding a square term

# We can square the TV budget vector and
# include it in the model

adModel4 <- lm(Sales~ TV*Radio+I(TV^2), adSales)
summary(adModel4)

# There is almost no variance left to explain.
# Is this data real or was in generated with
# two outliers included.

# The two outliers are still present on the left but
# the curvature in the residuals has been reduced.

# 5.5 Specifying a quadratic response surface

# The polym() function can be used specify
# quadratic response surface.


adModel5 <- lm(Sales~polym(TV,Radio,degree=2),adSales)
summary(adModel5)

# 6.  The Mean Squared Error
#
# The Q-Q plot discourages us claims based on test
# statistics and their p-values because the residual
# distribution does no support test statistics
# distribution assumptions.
#
# However, that does prohibit comparing the accuracy
# linear regression models to each other or to other
# models.


MSEmodel4 <- mean((adSales$Sales - fitted(adModel4))^2)
MSEmodel4

