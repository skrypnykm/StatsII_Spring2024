## load required packages ## 
library(lme4)
library(MASS)
library(stargazer)
library(ggplot2)
library(RColorBrewer)
library(caret)
library(nnet)
library(brant)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load the data ##
data_part <- read.csv("data_partner.csv") # Data on China's diplomatic partnerships (cross-national data) 
data_TSCS <- read.csv("data_TSCS.csv") # Data on China's aid to Africa (TSCS data)
data_aid <- read.csv("data_aid.csv") # Data on China's aid to Africa (cross-national data)
data_visit <- read.csv("data_visit.csv") # Data on China's leadership visits (cross-national data)

#FIGURE 2 TWIST
africa <- subset(data_TSCS, africa==1 & year>1999)
africa <- cbind(africa["country"], africa["year"], africa["china_aid"], africa["production2"])
africa$china_aid <- ifelse(is.na(africa$china_aid)==T, 0, africa$china_aid)
africa <- africa[order(africa$country, africa$year), ]

Morocco <- subset(africa, country == "Morocco")
Senegal <- subset(africa, country == "Senegal")
CAR <- subset(africa, country == "Central African Republic")
Burundi <- subset(africa, country == "Burundi")
Botswana <- subset(africa, country == "Botswana")
Sudan <- subset(africa, country == "Sudan")

par(mar=c(4,3,2,3), mfrow=c(2, 3))

#Northern Africa: Morocco
plot(Morocco$china_aid/1000000 ~ Morocco$year, type="l", lwd=4, xlab="", ylab="", ylim=c(0, 500), axes=F, main="Northern Africa: MOROCCO")
axis(1, at= Morocco$year, label= Morocco$year, cex.axis=1.2)
axis(2, at=seq(0, 500, by=100), label=seq(0, 500, by=100), cex.axis=1.2)
par(new=T)
plot(Morocco$production2/1000 ~ Morocco$year, type="l", lwd=4, lty=9, xlab="", ylab="", ylim=c(0, 200), axes=F)
axis(4, at=seq(0, 200, by=50), label=seq(0, 200, by=50), cex.axis=1.2)
box()
legend("topleft", c("Chinese aid (millions)", "Oil production (KBPD)"), cex=1.3, lwd=4, lty=c(1, 9))

#Western Africa: Senegal
plot(Senegal$china_aid/1000000 ~ Senegal$year, type="l", lwd=4, xlab="", ylab="", ylim=c(0, 500), axes=F, main="Western Africa: SENEGAL")
axis(1, at= Senegal$year, label= Senegal$year, cex.axis=1.2)
axis(2, at=seq(0, 500, by=100), label=seq(0, 500, by=100), cex.axis=1.2)
par(new=T)
plot(Senegal$production2/1000 ~ Senegal$year, type="l", lwd=4, lty=9, xlab="", ylab="", ylim=c(0, 200), axes=F)
axis(4, at=seq(0, 200, by=50), label=seq(0, 200, by=50), cex.axis=1.2)
box()
legend("topleft", c("Chinese aid (millions)", "Oil production (KBPD)"), cex=1.3, lwd=4, lty=c(1, 9))

#Central Africa: Central African Republic
plot(CAR$china_aid/1000000 ~ CAR$year, type="l", lwd=4, xlab="", ylab="", ylim=c(0, 500), axes=F, main="Central Africa: CENTRAL AFRICAN REPUBLIC")
axis(1, at= CAR$year, label= CAR$year, cex.axis=1.2)
axis(2, at=seq(0, 500, by=100), label=seq(0, 500, by=100), cex.axis=1.2)
par(new=T)
plot(CAR$production2/1000 ~ CAR$year, type="l", lwd=4, lty=9, xlab="", ylab="", ylim=c(0, 200), axes=F)
axis(4, at=seq(0, 200, by=50), label=seq(0, 200, by=50), cex.axis=1.2)
box()
legend("topleft", c("Chinese aid (millions)", "Oil production (KBPD)"), cex=1.3, lwd=4, lty=c(1, 9))

#Eastern Africa: Burundi
plot(Burundi$china_aid/1000000 ~ Burundi$year, type="l", lwd=4, xlab="", ylab="", ylim=c(0, 500), axes=F, main="Eastern Africa: BURUNDI")
axis(1, at= Burundi$year, label= Burundi$year, cex.axis=1.2)
axis(2, at=seq(0, 500, by=100), label=seq(0, 500, by=100), cex.axis=1.2)
par(new=T)
plot(Burundi$production2/1000 ~ Burundi$year, type="l", lwd=4, lty=9, xlab="", ylab="", ylim=c(0, 200), axes=F)
axis(4, at=seq(0, 200, by=50), label=seq(0, 200, by=50), cex.axis=1.2)
box()
legend("topleft", c("Chinese aid (millions)", "Oil production (KBPD)"), cex=1.3, lwd=4, lty=c(1, 9))

#Southern Africa: Botswana
plot(Botswana$china_aid/1000000 ~ Botswana$year, type="l", lwd=4, xlab="", ylab="", ylim=c(0, 500), axes=F, main="Southern Africa: BOTSWANA")
axis(1, at= Botswana$year, label= Botswana$year, cex.axis=1.2)
axis(2, at=seq(0, 500, by=100), label=seq(0, 500, by=100), cex.axis=1.2)
par(new=T)
plot(Botswana$production2/1000 ~ Botswana$year, type="l", lwd=4, lty=9, xlab="", ylab="", ylim=c(0, 200), axes=F)
axis(4, at=seq(0, 200, by=50), label=seq(0, 200, by=50), cex.axis=1.2)
box()
legend("topleft", c("Chinese aid (millions)", "Oil production (KBPD)"), cex=1.3, lwd=4, lty=c(1, 9))

#Bonus: oil-producing Sudan
plot(Sudan$china_aid/1000000 ~ Sudan$year, type = "l", lwd = 4, xlab = "", ylab = "", ylim = c(0, 2500), axes = FALSE, main = "Oil Producer: SUDAN")
axis(1, at = Sudan$year, label = Sudan$year, cex.axis = 1.2)
axis(2, at = seq(0, 2500, by = 500), label = seq(0, 2500, by = 500), cex.axis = 1.2)
par(new = TRUE)
plot(Sudan$production2 ~ Sudan$year, type = "l", lwd = 4, lty = 9, xlab = "", ylab = "", ylim = c(0, max(Sudan$production2, na.rm = TRUE)), axes = FALSE)
axis(4, at = seq(0, max(Sudan$production2, na.rm = TRUE), by = 100), label = seq(0, max(Sudan$production2, na.rm = TRUE), by = 100), cex.axis = 1.2)
box()
legend("topleft", c("Chinese aid (millions)", "Oil production (KBPD)"), cex = 1.3, lwd = 4, lty = c(1, 9))

#***
#MODEL 1 TWIST: UNORDERED/ORDERED MULTINOMIAL FOR PARTNERSHIP LEVELS 

#PREPARING THE DATA 

#1
#Model 1 dropped 4 observations from the 'subset(data_part, oecd==0)' (129), 
#so there were 125 states left. After re-checking with the Table A2, and manually, 
#we know that the 4 states dropped are: Cuba, Kyrgyzstan, Somalia, South Sudan.

#Subsetting 125 non-OECD members
data_part <- subset(data_part, !(country %in% c("Cuba", "Kyrgyzstan", "Somalia", "South Sudan")))
data_part125 <- subset(data_part, oecd==0)

#2
#Creating new column for the partnership level
data_part125$partlevel <- NA

#Combining partnership levels (dummy variables) into one column 
data_part125$partlevel[data_part125$part == 1] <- "Partnership"
data_part125$partlevel[data_part125$part2 == 1] <- "Strategic Partnership"
data_part125$partlevel[data_part125$part3 == 1] <- "Comprehensive Strategic Partnership"
data_part125$partlevel[is.na(data_part125$partlevel)] <- "No Partnership"

#Recoding  'partlevel' into a categorical variable (as factor), unordered (for now), 
#with reference category being "No Partnership"
data_part125$partlevel <- relevel(factor(data_part125$partlevel),
                                  ref = "No Partnership")

#Bar plot for 'partlevel'
color_palette <- brewer.pal(9, "Reds")[6:9] #Custom color palette in dark red colours
ggplot(data_part125, aes(x = factor(partlevel, levels = c("No Partnership", "Partnership", "Strategic Partnership", "Comprehensive Strategic Partnership")))) +
  geom_bar(fill = color_palette) +
  labs(title = "Distribution of Partnership Levels among 125 Non-OECD States",
       x = "",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, vjust = -0.5))  

#ORIGINAL MODEL 1 (DV: Partnership: 0 for no partnership, 
#1 for partnership (any kind))
#Binomial logit regression. OECD countries excluded
model1 <- glm(partner~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, family=binomial, data = data_part125)
summary(model1)

#Exponentiating coefficients (to OR). 
#Checking again by adding confidence intervals;
#CI should not contain 1 for a significant result (odds either below or above 1)
exp_model1 <- exp(cbind(OR = coef(model1), confint(model1)))
print(round(exp_model1, 3))
stargazer(exp_model1, type="latex", t.auto=F); logLik(model1)

#Calculating predicted probabilities and converting it to a factor, 
#where a probability <0.05 = 0 (no partnership), >0.5 = 1 (partnership)
predprob_model1 <- factor(ifelse(predict(model1, type = "response") > 0.5, 1, 0), levels = c(0, 1))
#Converting the dependent variable into factor 
data_part125$partner <- factor(data_part125$partner)
#Confusion matrix
cm_model1 <- confusionMatrix(predprob_model1, data_part125$partner)
cm_model1 #Accuracy score 0.784

#UNORDERED MODEL. OECD countries excluded.
unordmodel <- multinom(partlevel~prod2+GDPpc2+growth2+FDI2+
                         trade.de2+trade.de2+polity2+dom2+usally2, 
                       data_part125)
summary(unordmodel) 

#Calculating p-values to estimate significance of the coefficients
coefs <- coef(unordmodel)
std_errors <- summary(unordmodel)$standard.errors
t_values <- coefs/std_errors
df <- nrow(data_part125) - length(coef(unordmodel))
p_values <- 2 * pnorm(abs(t_values), lower.tail = FALSE)
p_values < 0.05

#Exponentiating coefficients (to OR). 
#Checking again by adding confidence intervals;
#CI should not contain 1 for a significant result (odds either below or above 1)
exp_unordmodel <- exp(coef(unordmodel))
print(round(exp_unordmodel, 3))
stargazer(exp_unordmodel, type="latex", t.auto=F); logLik(unordmodel)

#Calculating predicted probabilities
predprob_unord <- predict(unordmodel, newdata = data_part125, type = "class")

#Confusion matrix
cm_unordmodel <- confusionMatrix(predprob_unord, data_part125$partlevel)
cm_unordmodel #Accuracy score 0.744

#ORDERED MODEL. OECD countries excluded.
#Recoding  'partlevel' into a categorical ORDERED variable (as factor)
data_part125$partlevel <- factor(data_part125$partlevel, 
                                 ordered = TRUE,
                                 levels = c("No Partnership", 
                                            "Partnership", 
                                            "Strategic Partnership", 
                                            "Comprehensive Strategic Partnership"))
#Fitting an ordered model
ordmodel<- polr(partlevel~prod2+GDPpc2+growth2+FDI2+
                trade.de2+trade.de2+polity2+dom2+usally2, 
                data_part125)  
summary(ordmodel)

#Calculating p-values to estimate significance of the coefficients
t_values <- coef(summary(ordmodel))[, "t value"]
df <- nrow(data_part125) - length(coef(ordmodel))
p_values <- 2 * pt(abs(t_values), df = df, lower.tail = FALSE)
p_values < 0.05

##Exponentiating coefficients (to OR). 
#Checking again by adding confidence intervals;
#CI should not contain 1 for a significant result (odds either below or above 1)
exp_ordmodel <- exp(cbind(OR = coef(ordmodel), confint(ordmodel)))
print(round(exp_ordmodel, 3))
stargazer(exp_ordmodel, type="latex", t.auto=F); logLik(ordmodel)

#Calculating predicted probabilities
predprob_ord <- predict(ordmodel, newdata = data_part125, type = "class")

#Confusion matrix
cm_ordmodel <- confusionMatrix(data = predprob_ord, reference = data_part125$partlevel)
cm_ordmodel #Accuracy score 0.752

#MODELS STATISTICS
#Residual Deviance
deviance(unordmodel)
deviance(ordmodel)
deviance(model1)

#Akaike Information Criterion (the lower value, the better)
AIC(model1)
AIC(unordmodel)
AIC(ordmodel)

#Bayesian Information Criterion (the lower value, the better)
BIC(model1)
BIC(unordmodel)
BIC(ordmodel)

#Brant test for the ordered model (testing the "proportional odds assumption")
brant(ordmodel)