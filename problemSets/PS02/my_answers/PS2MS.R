#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

#Loading data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

#Recoding 'Choice' to binary
climateSupport$choice <- as.numeric(climateSupport$choice == "Supported")
#Recoding 'Countries' and 'Sanctions' to unordered factors, setting reference levels
climateSupport$countries <- relevel(factor(climateSupport$countries, ordered = FALSE), ref = "20 of 192")
climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, ordered = FALSE), ref = "None")

#Additive model 
glm_full <- glm (choice ~ countries + sanctions, 
                 data = climateSupport, 
                 family = binomial (link = "logit"))
summary(glm_full)
stargazer(glm_full)

#Calculating odds ratio of the coefficients
odds_ratios <- exp(glm_full$coefficients)
print(odds_ratios)

#Calculating percentages
percentages <- (odds_ratios-1)*100
print(percentages)

#Reduced model
glm_reduced <- glm(choice ~ 1, data = climateSupport, family = binomial(link = "logit"))

#Likelihood ratio test to determine p-value
anova(glm_full, glm_reduced, test="LRT")

#Calculating change in odds of Choice=1 if sanctions rise from 5% to 15%
change <- glm_full$coefficients["sanctions15%"] - glm_full$coefficients["sanctions5%"]
print(change)

#Calculating the odds ratio
odds_ratio1 <- exp(change)
print(odds_ratio1)

#Calculating the percentage
percentage <- (odds_ratio1-1)*100
print(percentage)

#Calculating the probability
plus <- glm_full$coefficients["(Intercept)"]+glm_full$coefficients["countries80 of 192"] #0.06369783 
probability <- 1 / (1 + exp(-plus))
print(probability)

#Checking with the predict() function
probability2 <- predict(glm_full, newdata = data.frame(countries = "80 of 192", sanctions = "None"), type = "response")
print(probability2)

#Interactive model
interactive_glm <- glm(choice ~ countries * sanctions, 
                       data = climateSupport, 
                       family = binomial(link = "logit"))
summary(interactive_glm)
stargazer(interactive_glm)

#Comparing additive and interactive models
lrt <- anova(glm_full, interactive_glm, test = "LRT")
print(lrt)

#Calculating the probability (interactive model)
probability3 <- predict(interactive_glm, newdata = data.frame(countries = "80 of 192", sanctions = "None"), type = "response")
print(probability3)

#Difference in probabilities
probability3-probability2
