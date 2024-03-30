#### R codes for replicating the empirical analysis in "China's Energy Diplomacy: Does Chinese Foreign Policy Favor Oil Producing Countries?" ### 

## load required packages ## 
library(lme4)
library(MASS)
library(stargazer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load the data ##
data_part <- read.csv("data_partner.csv") # Data on China's diplomatic partnerships (cross-national data) 
data_TSCS <- read.csv("data_TSCS.csv") # Data on China's aid to Africa (TSCS data)
data_aid <- read.csv("data_aid.csv") # Data on China's aid to Africa (cross-national data)
data_visit <- read.csv("data_visit.csv") # Data on China's leadership visits (cross-national data)

## Table 1 ##

#PARTNERSHIPS
table_partnerships <- table(subset(data_part, oecd==0)$partner, I(subset(data_part, oecd==0)$prod2>0)) #Table 1: partnerships 
table_partnerships 
#Notes. 1: China's partner; 0: Non-partner; True: Oil producer; False: Non-oil producer
rownames(table_partnerships) <- c("Non-partner", "Partner")
colnames(table_partnerships) <- c("Non-oil producer", "Oil producer")
col_sums <- colSums(table_partnerships)
table_partnerships <- cbind(table_partnerships, ColumnSums = col_sums)
table_partnerships_p <- round(prop.table(table_partnerships, margin = 2) * 100)
rownames(table_partnerships_p) <- c("Non-partner (%)", "Partner (%)")
colnames(table_partnerships_p) <- c("Non-oil producer (%)", "Oil producer (%)", "ColumnSums (%)")
table_partnerships <- cbind(table_partnerships, table_partnerships_p)
table_partnerships 

#AID
table_aid <- table(I(data_aid$aid2)>0, I(data_aid$prod2>0)) #Table 1: aid to Africa
#Notes. True (row): China's aid recipient; False (row): Non-aid recipient; True (column): Oil producer; False (column): Non-oil producer
rownames(table_aid) <- c("Non-aid recipient", "Aid recipient")
colnames(table_aid) <- c("Non-oil producer", "Oil producer")
col_sums <- colSums(table_aid)
table_aid <- cbind(table_aid, ColumnSums = col_sums)
table_aid_p <- round(prop.table(table_aid, margin = 2) * 100)
rownames(table_aid_p) <- c("Non-aid recipient (%)", "Aid recipient (%)")
colnames(table_aid_p) <- c("Non-oil producer (%)", "Oil producer (%)", "ColumnSums (%)")
table_aid <- cbind(table_aid, table_aid_p)
table_aid

#STATE VISITS
table_state_visits <- table(I(subset(data_visit, oecd==0)$visit)>0, I(subset(data_visit, oecd==0)$prod2>0)) #Table 1: state visits
#Notes. True (row): Country visited; False (row): Country not visited; True (column): Oil producer; False (column): Non-oil producer
rownames(table_state_visits) <- c("Country not visited", "Country visited")
colnames(table_state_visits) <- c("Non-oil producer", "Oil producer")
col_sums <- colSums(table_state_visits)
table_state_visits <- cbind(table_state_visits, ColumnSums = col_sums)
table_state_visits_p <- round(prop.table(table_state_visits, margin = 2) * 100)
rownames(table_state_visits_p) <- c("Country not visited (%)", "Country visited (%)")
colnames(table_state_visits_p) <- c("Non-oil producer (%)", "Oil producer (%)", "ColumnSums (%)")
table_state_visits <- cbind(table_state_visits, table_state_visits_p)
table_state_visits

table_partnerships
table_aid
table_state_visits

## Table 2 ##
model1 <- glm(partner~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, family=binomial, subset(data_part, oecd==0))  # Model 1
stargazer(model1, type="text", t.auto=F); logLik(model1); BIC(model1)

model2 <- glm(partner2~prod2+GDPpc2+growth2+FDI2+trade.de2+trade.de2+polity2+dom2+usally2, family=binomial, subset(data_part, oecd==0))  # Model 2
stargazer(model2, type="text", t.auto=F); logLik(model2); BIC(model2)

model3 <- lmer(lnaid~lnaid.1+lnprod1+lnGDPpc.1+growth.1+I(lnFDI.1/10)+trade.de.1+polity.1+lndom.1+usally.1+(1|country)+(1|year), subset(data_TSCS, oecd==0 & country!="China" & africa==1))  # Model 3
stargazer(model3, type="text", t.auto=F); logLik(model3); AIC(model3); BIC(model3)

model4 <- lm(lnaid2~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, data_aid) # Model 4
stargazer(model4, type="text", t.auto=F); logLik(model4); AIC(model4); BIC(model4)

model5 <- glm.nb(visit~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, subset(data_visit, oecd==0))  # Model 5
stargazer(model5, type="text", t.auto=F); logLik(model5); BIC(model5)

#Store the models in a list
table_2 <- list(
  model1, model2, model3, model4, model5)

#Create the table using stargazer
table_2 <- stargazer(
  table_2,
  type = "html",
  t.auto = FALSE,
  title = "Table 2",
  align = TRUE,
  header = FALSE
)

writeLines(table_2, "table2.html")

## Table 3 ##
model6 <- glm(partner~reserve2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, family=binomial, subset(data_part, oecd==0))  # Model 6
stargazer(model6, type="text", t.auto=F); logLik(model6); BIC(model6)

model7 <- glm(partner2~reserve2+GDPpc2+growth2+FDI2+trade.de2+trade.de2+polity2+dom2+usally2, family=binomial, subset(data_part, oecd==0))  # Model 7
stargazer(model7, type="text", t.auto=F); logLik(model7); BIC(model7)

model8 <- lmer(lnaid~lnaid.1+lnreserve1+lnGDPpc.1+growth.1+I(lnFDI.1/10)+trade.de.1+polity.1+lndom.1+usally.1+(1|country)+(1|year), subset(data_TSCS, oecd==0 & country!="China" & africa==1))  # Model 8
stargazer(model8, type="text", t.auto=F); logLik(model8); AIC(model8); BIC(model8)

model9 <- lm(lnaid2~reserve2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, data_aid) # Model 9
stargazer(model9, type="text", t.auto=F); logLik(model9); AIC(model9); BIC(model9)

model10 <- glm.nb(visit~reserve2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, subset(data_visit, oecd==0))  # Model 10
stargazer(model10, type="text", t.auto=F); logLik(model10); BIC(model10)

model11 <- glm.nb(day~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, subset(data_visit, oecd==0))  # Model 11
stargazer(model11, type="text", t.auto=F); logLik(model11); BIC(model11)

model12 <- glm.nb(day~reserve2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, subset(data_visit, oecd==0))  # Model 12
stargazer(model12, type="text", t.auto=F); logLik(model12); BIC(model12)

table_3 <- list(
  model6, model7, model8, model9, model10, model11, model12)

table_3 <- stargazer(
  table_3,
  type = "html",
  t.auto = FALSE,
  title = "Table 3",
  align = TRUE,
  header = FALSE
)

writeLines(table_3, "table3.html")

## Figure 1 ##
china <- c(2320, 2520, 2736, 3047, 3115, 3394, 3722, 4120, 4216, 4452, 4766, 4859, 5262, 5771, 6740, 6945, 7500, 7860, 7994, 8306, 9317, 9867, 10367, 10756) # Oil consumption in China 
partner <- c(1, 2, 3, 4, 8, 10, 11, 12, 15, 16, 20, 27, 37, 42, 42, 44, 45, 45, 46, 49, 53)
aid <- c(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 2139.329, 2337.245, 2263.108, 3358.809, 3511.384, 6038.058, 9772.522, 21791.729, 5688.162, 11030.539, 10553.253, 14588.756, 12069.544, 23181.475) 
year <- c(1993:2013)

par(mar=c(4,3,1,3), mfrow=c(1, 2))
plot(c(1990:2013), china, type="l", lwd=4, ylim=c(0, 11000), axes=F, xlab="", ylab="")
axis(1, at=c(1990:2013), label=c(1990:2013), cex.axis=1.2)
axis(2, at=c(0, 2000, 4000, 6000, 8000, 10000), label=c(0, 2000, 4000, 6000, 8000, 10000), cex.axis=1.2)
par(new=T)
plot(year, partner, type="l", lwd=4, lty=6, axes=F, xlim=c(1990, 2013), ylim=c(0, 70), xlab="", ylab="")
axis(4, at=c(0, 10, 20, 30, 40, 50, 60), label=c(0, 10, 20, 30, 40, 50, 60), cex.axis=1.2)
box()
legend("topleft", c("China's oil consumption (KBPD)", "# of China's partnerships"), cex=1.3, lwd=4, lty=c(1, 6))
mtext(side=1, "Year", line=2.5, cex=1.5)

plot(year, china[4:24], type="l", lwd=4, ylim=c(0, 11000), axes=F, xlab="", ylab="")
axis(1, at=year, label= year, cex.axis=1.2)
axis(2, at=c(0, 2000, 4000, 6000, 8000, 10000), label=c(0, 2000, 4000, 6000, 8000, 10000), cex.axis=1.2)
par(new=T)
plot(year[8:21], aid[8:21], type="l", lwd=4, lty=9, axes=F, xlab="", xlim=c(year[1], 2013), ylim=c(0, 35000), ylab="")
axis(4, at=c(0, 10000, 20000, 30000), label=c(0, 10000, 20000, 30000), cex.axis=1.2)
box()
legend("topleft", c("China's oil consumption (KBPD)", "Chinese aid to Africa (millions)"), cex=1.3, lwd=4, lty=c(1, 9))
mtext(side=1, "Year", line=2.5, cex=1.5)

## Figure 2 ##
africa <- subset(data_TSCS, africa==1 & year>1999)
africa <- cbind(africa["country"], africa["year"], africa["china_aid"], africa["production2"])
africa$china_aid <- ifelse(is.na(africa$china_aid)==T, 0, africa$china_aid)
africa <- africa[order(africa$country, africa$year), ]

Tunisia <- subset(africa, country=="Tunisia")
Nigeria <- subset(africa, country=="Nigeria")
Angola <- subset(africa, country=="Angola")
Cameroon <- subset(africa, country=="Cameroon")

par(mar=c(4,3,2,3), mfrow=c(2, 2))
plot(Angola$china_aid/1000000 ~ Angola$year, type="l", lwd=4, xlab="", ylab="", ylim=c(0, 3500), axes=F, main="Angola")
axis(1, at= Angola$year, label= Angola$year, cex.axis=1.2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), label=c(0, 1000, 2000, 3000, 4000), cex.axis=1.2)
#mtext(side=2, "Chinese aid", line=2.5, cex=1.5)
par(new=T)
plot(Angola$production2 ~ Angola$year, type="l", lwd=4, lty=9, xlab="", ylab="", ylim=c(500, 3000), axes=F)
axis(4, at=c(500, 1000, 1500, 2000, 2500, 3000), label=c(500, 1000, 1500, 2000, 2500, 3000), cex.axis=1.2)
box()
legend("topleft", c("Chinese aid (millions)", "Oil production (KBPD)"), cex=1.3, lwd=4, lty=c(1, 9))

plot(Cameroon$china_aid/1000000 ~ Cameroon$year, type="l", lwd=4, xlab="", ylab="", ylim=c(0, 1500), axes=F, main="Cameroon")
axis(1, at= Cameroon$year, label= Cameroon$year, cex.axis=1.2)
axis(2, at=c(0, 500, 1000, 1500), label=c(0, 500, 1000, 1500), cex.axis=1.2)
#mtext(side=2, "Chinese aid", line=2.5, cex=1.5)
par(new=T)
plot(Cameroon$production2 ~ Cameroon$year, type="l", lwd=4, lty=9, xlab="", ylab="", ylim=c(30, 130), axes=F)
axis(4, at=c(30, 60, 90, 120), label=c(30, 60, 90, 120), cex.axis=1.2)
box()

plot(Nigeria$china_aid/1000000 ~ Nigeria$year, type="l", lwd=4, xlab="", ylab="", ylim=c(0, 4000), axes=F, main="Nigeria")
axis(1, at= Nigeria$year, label= Nigeria$year, cex.axis=1.2)
axis(2, at=c(0, 1000, 2000, 3000, 4000), label=c(0, 1000, 2000, 3000, 4000), cex.axis=1.2)
#mtext(side=2, "Chinese aid", line=2.5, cex=1.5)
par(new=T)
plot(Nigeria$production2 ~ Nigeria$year, type="l", lwd=4, lty=9, xlab="", ylab="", ylim=c(1500, 3000), axes=F)
axis(4, at=c(1500, 2000, 2500, 3000), label=c(1500, 2000, 2500, 3000), cex.axis=1.2)
box()

plot(Tunisia$china_aid/1000000 ~ Tunisia$year, type="l", lwd=4, xlab="", ylab="", ylim=c(0, 100), axes=F, main="Tunisia")
axis(1, at=Tunisia$year, label= Tunisia$year, cex.axis=1.2)
axis(2, at=c(0, 20, 40, 60, 80, 100), label=c(0, 20, 40, 60, 80, 100), cex.axis=1.2)
#mtext(side=2, "Chinese aid", line=2.5, cex=1.5)
par(new=T)
plot(Tunisia$production2 ~ Tunisia$year, type="l", lwd=4, lty=9, xlab="", ylab="", ylim=c(40, 120), axes=F)
axis(4, at=c(40, 60, 80, 100, 120, 140), label=c(40, 60, 80, 100, 120, 140), cex.axis=1.2)
box()



