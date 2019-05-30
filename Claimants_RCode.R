
# Multiple Logistic Regression
## Claimants Insurance Dataset
### Implementation

getwd()
claimants <- read.csv("claimants.csv")
View(claimants)
attach(claimants)
summary(claimants)
# Logistic Regression
colnames(claimants)
str(CLMSEX)
str(as.factor(CLMSEX))
logit <- glm(ATTORNEY ~ factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT) + CLMAGE + LOSS, family=binomial)
summary(logit)
library(MASS)
library(caret)
library(car)
x <- stepAIC(logit)
# Odds Ratio
exp(coef(logit))
# Confusion matrix table 
prob <- predict(logit,type=c("response"),claimants)
prob
prob <- as.data.frame(prob)
final <- cbind(prob,claimants)
dim(final)
final1 <- cbind(final,claimants)
confusion <- table(prob > 0.5,claimants$ATTORNEY)
table(prob < 0.5)
table(ATTORNEY)
confusion
# Model Accuracy 
Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy
vif(logit)
# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained


