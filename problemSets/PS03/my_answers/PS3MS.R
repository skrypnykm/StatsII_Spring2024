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

lapply(c("nnet", "MASS", "stargazer", "AER"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
#Loading data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

#Categorising the output variable into three categories 
gdp_data$GDPWdiff1 <- ifelse(gdp_data$GDPWdiff > 0, "positive",
                      ifelse(gdp_data$GDPWdiff == 0, "no change", "negative"))

#Relevelling so the reference category is "no change"
gdp_data$GDPWdiff1 <- relevel(factor(gdp_data$GDPWdiff1), ref = "no change")

#Estimating an UNORDERED multinomial logit model
model1 <- multinom(GDPWdiff1 ~ REG + OIL,
                   data = gdp_data)
summary(model1) #Coefficients indicate the change in log odds

#Formatting a result table
stargazer(model1, type = "latex", title = "Unordered Multinomial Model 1")

#Exponentiating to get the odd ratios and confidence intervals
exp_model1 <-exp(cbind(OR = coef(model1), confint(model1)))
exp_model1

#Formatting a result table
stargazer(exp_model1, type = "latex", title = "Unordered Multinomial Model 1 (OR)")

#Relevelling to ordered
gdp_data$GDPWdiff1 <- factor(gdp_data$GDPWdiff1, levels = c("negative", "no change", "positive", ordered = TRUE))

#Estimating an ORDERED multinomial logit model
model2 <- polr(GDPWdiff1 ~ REG + OIL,
               data = gdp_data, Hess=T)
summary(model2)

#Formatting a result table
stargazer(model2, type = "latex", title = "Ordered Multinomial Model 2")

#Exponentiating to get the odd ratios and confidence intervals
exp_model2 <-exp(cbind(OR = coef(model2), confint(model2)))
exp_model2

#####################
# Problem 2
#####################

#Loading data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

#Estimating a Poisson model
mexico_poisson <- glm(PAN.visits.06 ~ factor(competitive.district) + marginality.06 + factor(PAN.governor.06), data = mexico_elections, family = poisson)
summary(mexico_poisson) #Coefficients represent change in the log count

#Formatting a result table
stargazer(mexico_poisson, type = "latex", title = "Poisson Model")

#Exponentiating to get the odd ratios and confidence intervals
exp_mexico_poisson <-exp(cbind(OR = coef(mexico_poisson), confint(mexico_poisson)))
exp_mexico_poisson

#Formatting a result table
stargazer(exp_mexico_poisson, type = "latex", title = "Poisson Model (exp.)")

#Testing for overdispersion
dispersiontest(mexico_poisson)

#Exploratory analysis of the outcome variable: calculating a mean number of visits
mean(mexico_elections$PAN.visits.06)

#Calculating lambda (mean number of visits)
coeff <- mexico_poisson$coeff
lambda <- exp(coeff[1]+coeff[2]+coeff[4])
lambda 
