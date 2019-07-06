library(ggplot2)
library(plm)

load("C:/Users/enemy/OneDrive - George Mason University/Desktop/GMU/STAT 463/Final/Fatalities.rda")
data(Fatalities)

# obtain the dimension and inspect the structure
dim(Fatalities)
str(Fatalities)

# Simple regression 

# define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# subset the data
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")
# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)
library(stargazer)
coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")

coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")


# plot observations and add estimated regression line for 1982-1988 data

ggplot(Fatalities,aes(beertax,fatal_rate,color=year)) + geom_point() +
    geom_smooth(method="lm",se=F,show.legend = T) +
    labs(x = 'Beer tax(in dollars)',y = 'Fatality rate(fatalities per 10000)',
         title = 'Traffic Fatality Rates and Beer Taxes 1982-1988') 


ggplot(Fatalities,aes(beertax,fatal_rate,color=state)) + geom_point() +
  geom_smooth(method="lm",se=F) +
  labs(x = 'Beer tax(in dollars)',y = 'Fatality rate(fatalities per 10000)',
       title = 'Traffic Fatality Rates and Beer Taxes 1982-1988 by state') 

# Panel Data with ??Before and After??

# compute the differences 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)

coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")
# plot the differenced data
plot(x = diff_beertax, 
     y = diff_fatal_rate, 
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes between 1982 and 1988",
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20, 
     col = "steelblue")

# add the regression line to plot
abline(fatal_diff_mod, lwd = 1.5)



# Fixed Effects Regression

fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
fatal_fe_lm_mod

# obtain demeaned data
Fatalities_demeaned <- with(Fatalities,
                            data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
                                       beertax = beertax - ave(beertax, state)))

# estimate the regression
summary(lm(fatal_rate ~ beertax - 1, data = Fatalities_demeaned))

# plot the demeaned data
plot(x = Fatalities_demeaned$beertax, 
     y = Fatalities_demeaned$fatal_rate, 
     xlab = "Change in beer tax (in dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20, 
     col = "steelblue")

abline(fatal_fe_lm_mod, lwd = 1.5)




# Regression with Time Fixed Effects

# estimate a combined time and entity fixed effects regression model

# via lm()
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities)
fatal_tefe_lm_mod
lm(formula = fatal_rate ~ beertax + state + year - 1, data = Fatalities)

#In our call of plm() we set another argument effect = “twoways” for inclusion of entity and time dummies.
fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

coeftest(fatal_tefe_mod,vcov = vcovHC, type = "HC1")

#for presentation
presentation <- subset(Fatalities,select=c(year,state,beertax,fatal_rate))
par(mfcol=c(2,2))
plot(fatal_tefe_lm_mod)
