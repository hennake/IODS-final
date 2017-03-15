# IODS final assignment
# Henna Kettunen

###############################################################
#                                                             #
# DISCLAIMER:                                                 #
# It's a horrible mess here, please see the final.Rmd instead #
#                                                             #
###############################################################


setwd("L:\\Users\\Henna\\storage\\r\\iods\\IODS-final\\IODS-final")

library(tidyr)
library(dplyr)
library(ggplot2)

load("tree_data.RData")

# Summary
summary(dat)

# Histograms
library(reshape2)
mdat <- melt(dat)
ggplot(mdat, aes(value)) + geom_histogram(fill="#009E73") + facet_wrap(~ variable, scales="free")

# Scatterplot matrix
pairs(dat, cex=0.1, pch=16, col=dat$Genus)

# Correlation matrix
library(corrplot)
cor_matrix <- cor(dat[,2:ncol(dat)]) 
corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex=0.6)

# Drop correlated variables
drop <- c("leaf_toughness", "C_N", "SLA")
dat <- dat[,!colnames(dat) %in% drop]


## Multinomial logistic regression

# Divide to training and test sets
# ind <- sample(nrow(dat), nrow(dat)*0.5)
# save(ind, file="ind.RData")
load("ind.RData")
train <- dat[ind,]
test <- dat[-ind,]

# Full main effects model
library(nnet)
fit1 <- multinom(Genus ~ ., data = train)
pred1 <- predict(fit1, newdata=test)
genus <- test$Genus
tab <- table(pred=pred1, obs=genus)
tab2 <- as.data.frame(tab)
# Correct rate
cor1 <- sum(tab2$Freq[tab2$pred==tab2$obs])/nrow(test)
tab
corg <- tab2[tab2$pred==tab2$obs,c("pred","Freq")]
cor1


# Model selection
library(MASS)
step <- stepAIC(fit1, direction="backward")

# Best model according to AIC
fit2 <- multinom(Genus ~ leaf_thickness + sapwood_density + N + C13 + chlorophyll_concentration + surface_area, data = train)
pred2 <- predict(fit2, newdata=test)
pred2p <- as.data.frame(fitted(fit2))
tab3 <- table(pred=pred2, obs=genus)
tab3p <- round(prop.table(tab3, 2), 2)
tab4 <- as.data.frame(tab3)
# Correct rate
cor2 <- sum(tab4$Freq[tab4$pred==tab4$obs])/nrow(test)
cor2

# Model summary and odd ratios by exponentation
summary(fit2)
exp(coef(fit2))

# LR test
library(car)
Anova(fit2)

# 2-tailed Wald test
z <- summary(fit2)$coefficients/summary(fit2)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2


# Full model with 2-way interactions
fit3 <- multinom(Genus ~ .*., data = train)
pred1 <- predict(fit1, newdata=test)
AIC(fit1, fit2, fit3)

coef(fit2)
library(AER)
coeftest(fit2)

fit4 <- multinom(Genus ~ leaf_thickness + sapwood_density + N + chlorophyll_concentration + surface_area, data = train)
AIC(fit4)

trainp <- predict(fit2)
tabp <- table(pred=trainp, obs=train$Genus)
tabp2 <- as.data.frame(tabp)
# Correct rate
corp <- sum(tabp2$Freq[tabp2$pred==tabp2$obs])/nrow(test)
corp

# Visualization
library(visreg)
visreg(fit2, xvar="leaf_thickness", collapse=T, type="conditional")
visreg(fit2, xvar="sapwood_density", collapse=T, type="conditional")
visreg(fit2, xvar="N", collapse=T, type="conditional")
visreg(fit2, xvar="C13", collapse=T, type="conditional")
visreg(fit2, xvar="chlorophyll_concentration", collapse=T, type="conditional")
visreg(fit2, xvar="surface_area", collapse=T, type="conditional")

library(stargazer)
stargazer(fit1)
