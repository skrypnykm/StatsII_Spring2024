#### R codes for replicating the empirical analysis in the Appendix for "China's Energy Diplomacy: Does Chinese Foreign Policy Favor Oil Producing Countries?" ### 

## load required packages ## 
library(lme4)
library(MASS)
library(stargazer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load the data ##
data_part <- read.csv("data_partner.csv") # Data on China's diplomatic partnerships (cross-national data) 
data_aid <- read.csv("data_aid.csv") # Data on China's aid to Africa (cross-national data)
data_visit <- read.csv("data_visit.csv") # Data on China's leadership visits (cross-national data)

## Table 1 ##
m1 <- glm(partner~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, family=binomial, data_part)  # Model 1
stargazer(m1, type="text", t.auto=F); logLik(m1); AIC(m1); BIC(m1) 

m2 <- glm(partner~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2+I(pop2/1000000)+I(log(dist))+contig+taiwan2+region2, family=binomial, subset(data_part, oecd==0))  # Model 2
stargazer(m2, type="text", t.auto=F); logLik(m2); AIC(m2); BIC(m2) 

m3 <- glm(partner2~prod2+GDPpc2+growth2+FDI2+trade.de2+trade.de2+polity2+dom2+usally2, family=binomial, data_part)  # Model 3
stargazer(m3, type="text", t.auto=F); logLik(m3); AIC(m3); BIC(m3) 

m4 <- glm(partner2~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2+I(pop2/1000000)+I(dist/1000)+contig+taiwan2+region2, family=binomial, subset(data_part, oecd==0))  # Model 4
stargazer(m4, type="text", t.auto=F); logLik(m4); AIC(m4); BIC(m4) 

m5 <- lm(lnaid2~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2+I(pop2/1000000)+I(dist/1000)+taiwan2, data_aid) # Model 5
stargazer(m5, type="text", t.auto=F); logLik(m5); AIC(m5); BIC(m5) 

m6 <- glm.nb(visit~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, data_visit)  # Model 6
stargazer(m6, type="text", t.auto=F); logLik(m6); AIC(m6); BIC(m6) 

m7 <- glm.nb(visit~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2+I(pop2/1000000)+I(dist/1000)+contig+taiwan2, subset(data_visit, oecd==0))  # Model 7
stargazer(m7, type="text", t.auto=F); logLik(m7); AIC(m7); BIC(m7) 

## Table 2 ##
## load the data ##
data_jiang <- read.csv("data_jiang.csv") # Data on Jiang-Zhu Administration (1998-2002) 
data_hu <- read.csv("data_hu.csv") # Data on Hu-Wen Administration (2003-2012)
data_xi <- read.csv("data_xi.csv") # Data on Xi-Li Administration (2013) 

# JiangZhu 1998-2002 #
m1 <- glm(partner~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, family=binomial, data_jiang)  # Model 1
stargazer(m1, type="text", t.auto=F); logLik(m1); AIC(m1); BIC(m1) 

m2 <- lm(lnaid2~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2+prod2, subset(data_jiang, africa==1)) # Model 2
stargazer(m2, type="text", t.auto=F); logLik(m2); AIC(m2); BIC(m2) 

m3 <- glm(I(totalPresPremvisits.y>0)~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, data_jiang, family=binomial)  # Model 3
stargazer(m3, type="text", t.auto=F); logLik(m3); AIC(m3); BIC(m3) 

# HuWen 2003-2012 #
m4 <- glm(partner2~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, family=binomial, data_hu)  # Model 4
stargazer(m4, type="text", t.auto=F); logLik(m4); AIC(m4); BIC(m4) 

m5 <- lm(lnaid2~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, subset(data_hu, africa==1)) # Model 5
stargazer(m5, type="text", t.auto=F); logLik(m5); AIC(m5); BIC(m5) 

m6 <- glm.nb(visit_h~prod2+GDPpc2+growth2+FDI2+trade.de2+polity2+dom2+usally2, data_hu)  # Model 6
stargazer(m6, type="text", t.auto=F); logLik(m6); AIC(m6); BIC(m6) 

# XiLi 2013#
m7 <- glm(partner2~lnprod1+lnGDPpc.1+growth.1+lnFDI.1+trade.de.1+polity.1+lndom.1+usally.1, family=binomial, data_xi)  # Model 7
stargazer(m7, type="text", t.auto=F); logLik(m7); AIC(m7); BIC(m7) 

m8 <- lm(lnaid~lnprod1+lnGDPpc.1+growth.1+lnFDI.1+trade.de.1+polity.1+lndom.1+usally.1, subset(data_xi,africa==1)) # Model 8
stargazer(m8, type="text", t.auto=F); logLik(m8); AIC(m8); BIC(m8) 

m9 <- glm(visits~lnprod1+lnGDPpc.1+growth.1+lnFDI.1+trade.de.1+polity.1+lndom.1+usally.1, data_xi, family=binomial)  # Model 9
stargazer(m9, type="text", t.auto=F); logLik(m9); AIC(m9); BIC(m9) 
