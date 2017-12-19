# read the states data
states.data <- readRDS("states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
states.info
tail(states.info, 8)

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, 
              data=states.data) 
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model

library(dplyr)
library(ggplot2)
metro.energy <- select(states.data, metro, energy)
summary(metro.energy)
#Both metro and energy have 1 NA value.
plot(states.data$metro, states.data$energy)
plot(metro.energy)
#The scatterplot of energy vs. metro shows 4 positive outliers. The relationship
# beteween the two variables does not appear to be highly linear.
cor(metro.energy, use = "complete.obs") 
# Correlation after removing observations containing NA
# = -0.3397; shows weak negative linear correlation between variables

# Removing outliers to see if strnger correlatin can be obtained
outliers_rmvd <- filter(metro.energy, energy <= 500)
cor(outliers_rmvd, use = "complete.obs")
plot(outliers_rmvd)
#After removing the outliers, R has improved to -0.47. 
##   2. Print and interpret the model `summary'
model1<-lm(energy~metro, data=states.data)
summary(model1)
SSE = sum(model1$residuals^2) # sum of squared error=580411.5
SSE
RMSE = sqrt(SSE/(nrow(outliers_rmvd))) # root mean squared error=112.3
RMSE
#Residuals are quite large. Variable metro is significant at alpha=0.05 level with
# p value of 0.031 and t value of -2.225. Coeffiient for metro is negative. 
#Adjusted R^2 is 0.07751, indicating a poor fit model. 
##   3. `plot' the model to look for deviations from modeling assumptions
plot(model1)
# 3 large positive outliers can be seen by loking at the residuals vs. fitted plot.
#The normal Q-Q plot shows that the data is not normally distributed.
##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?
cor(states.data[3:21], use = "complete.obs")
model2<-lm(energy~metro+density+waste+toxic+green+income, data=na.omit(states.data))
summary(model2)
model3<-lm(energy~metro+toxic+green, data=na.omit(states.data))
summary(model3)
model4<-lm(energy~toxic+green, data=na.omit(states.data))
summary(model4)
plot(model4)
#model 4 is significantly better than model 1. Both toxic and green are strongly linearly
#correlated with energy. Coefficients for both are highly significant, 
#adjusted R-squared for model 4 is signifcantly higher than model 1(0.75 vs 0.08)
# Examine Data of Final Model
model4data <- na.omit(select(states.data, energy, green, toxic))
summary(model4data) # Examine subset of variables of interest

# Examine correlation between variables
plot(states.data$green, states.data$energy)
plot(states.data$toxic, states.data$energy)
# all contain some outliers, but decent linear correlation

cor(model4data, use = "complete.obs")
# all indep variables fairly highly correlated with energy (0.77 and 0.60), 
#lower correlation between indep variable (only 0.27)

# Check Quality Final Model
SSE = sum(model4$residuals^2) # sum of squared error=152582
SSE
RMSE = sqrt(SSE/(nrow(model4data))) # root mean squared error=56.4
RMSE
# SSE and RMSE much lower error than model of energy~metro

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

#Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
summary(lm(csat ~ C(region, base=4),
           data=states.data))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
cor(states.data[3:21], use = "complete.obs")
#Looking at the correlation matrix, house and senate are the variables
#that are most highly correlated with energy after toxic and green so I will add the
#interaction term house*senate to the model. 
model5<-lm(energy~toxic+green+house*senate, data=na.omit(states.data))
summary(model5)
plot(model5)
library(dplyr)
model5data <- na.omit(select(states.data, energy, green, toxic, house, senate))
SSE = sum(model5$residuals^2) # sum of squared error=152582
SSE
RMSE = sqrt(SSE/(nrow(model5data))) # root mean squared error=56.4
RMSE
#The interaction term house:senate is not significant (t value 0.34, p value 0.74).
#In addition adjusted R-squared has dropped to 0.74, compared to the original model.
model4<-lm(energy~toxic + green, data=na.omit(states.data))
summary(model4)
plot(model4)
#Model 4, the original model had adjusted R-squared of 0.75, and all its explanatory
#variables are significant. Going from model 4 to model 5, SSE decreases slightly
# from 152582 to 150591 and RMSE decreases slightly from 56.4 to 56.1. However, 
#this is not that significant of a difference given the fact that all of the
# terms in model 4 are significant, while the interaction term in model 5 is not.
#Given all this, model 4 without the interaction term is the better model. 

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

# Region contrasts
contrasts(states.data$region)

# Use different regions as reference 
summary(lm(energy ~ toxic+green+C(region, base=1), data=states.data))
summary(lm(energy ~ toxic+green+C(region, base=2), data=states.data))
summary(lm(energy ~ toxic+green+C(region, base=3), data=states.data))
summary(lm(energy ~ toxic+green+C(region, base=4), data=states.data))
#In neither of these 4 models was region a significant variable, regardless of the base.
# In all 4 models, each of the 3 region terms had a p value significantly higher than
#0.05. Adding region to our model does not improve the model.


model6<-lm(energy~toxic+green + region, data=na.omit(states.data))
summary(model6)
#Similarly to the analysis before, when we code fore region without creating separate
#bases, none of the region variables (region N.East, regionSouth, and regionMidwest)
#are significant at the alpha=0.05 level. 
plot(model6)
anova(model6)
#Since the p values in the linear regression model with the variable "region" 
#and the F value in the ANOVA test are less than 0.05, there are not
#significant differences in the 4 regions 