setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# C51w21 Quiz 3 -----------------------------------------------------------

# Q1
x <- c(28.3, 22.5, 26, 24.8, 26, 23.8)

y <- c(8, 0, 9, 0, 4, 0)

fitq1 <-  glm(y ~ x, family = poisson())

summary(fitq1)
drop1(fitq1,test = "LRT")

# Q2 is T

# Q3
CuredYes = c(78, 101, 40, 54)
CuredNo = c(28, 11, 5, 6)
TRT = c("A","B", "A", "B")
Diag = c(rep("Complicated",2), rep("Uncomplicated", 2))
data.frame(Diag, TRT, CuredYes, CuredNo)


fitq3 <- glm(cbind(CuredYes,CuredNo) ~ TRT + Diag, family = binomial())

summary(fitq3)
exp(0.9161)

# Q4
1 - pchisq(2.0804,df = 1)

# Q5
exp(confint(fitq3))

# Q6 F

# Q7
x_par = 2

diag_par = 1

# AIC = 2k - 2ln(L)
# null aic


2 * 1 + 492.029

# main effect 
2 * (1 + 2 + 1) + 450.071

# two factor interaction
# 
2 * (1 + 2 + 1 + 2) + 447.556



# example 0 ---------------------------------------------------------------


## If the package is not installed then
# install.packages('NHANES')
library(tidyverse)
library(NHANES)
small.nhanes <- na.omit(NHANES[NHANES$SurveyYr=="2011_12"
                               & NHANES$Age > 17,c(1,3,4,8:11,13,25,61)])
small.nhanes <- small.nhanes %>%
  group_by(ID) %>% filter(row_number()==1)

head(small.nhanes)
glimpse(small.nhanes)

set.seed(111)
train <- small.nhanes[sample(seq_len(nrow(small.nhanes)), size = 500),]
length(unique(small.nhanes$ID))
nrow(small.nhanes)
length(which(small.nhanes$ID %in% train$ID))
test <- small.nhanes[!small.nhanes$ID %in% train$ID,]

logit.glm <- glm(SmokeNow ~ Gender + Age + Race3 + Education +
                   MaritalStatus + HHIncome + Poverty +
                   BPSysAve, data=train, family=binomial(logit))

summary(logit.glm)

# (b)
### Stepwise AIC and BIC ###
AIC.mod <- step(logit.glm, trace = 0)
summary(AIC.mod)

BIC.mod <- step(logit.glm, trace = 0, k = log(n))
summary(BIC.mod)

library(rms)
logit.mod <- lrm(SmokeNow ~ Gender + Age + Race3 + Education + MaritalStatus + HHIncome + Poverty + BPSysAve,
                 data = train, x = TRUE, y  = TRUE, model =  T)

cross.calib <- calibrate(logit.mod, method="crossvalidation", B=10)         
plot(cross.calib, las=1, xlab = "Predicted Probability")

# (c)
library(pROC)

p <- predict(logit.mod, type = "fitted")
p1 <- predict(logit.glm, type = "response")
roc_logit <- roc(train$SmokeNow ~ p)

## The True Positive Rate ##
TPR <- roc_logit$sensitivities
## The False Positive Rate ##
FPR <- 1 - roc_logit$specificities
plot(FPR, TPR, xlim = c(0,1), ylim = c(0,1), type = 'l', lty = 1, lwd = 2,col = 'red')
abline(a = 0, b = 1, lty = 2, col = 'blue')
text(0.7,0.4,label = paste("AUC = ", round(auc(roc_logit),2)))

# (d)
pred.prob <- predict(logit.mod, newdata = train, type = "fitted") 
test$pred.prob <- predict(logit.glm, newdata = test, type = "response") 
deciles <- quantile(test$pred.prob, probs = seq(0,1, by =0.1)) 
test$decile <- findInterval(test$pred.prob, deciles, rightmost.closed = T) 
pred.prob <- tapply(test$pred.prob, test$decile, mean)
obs.prob <- tapply(as.numeric(test$SmokeNow)-1, test$decile, mean)




# example 1 ---------------------------------------------------------------
library(VGAM)

gator = read.table("gator.txt",header=T)
gator$Size = factor(gator$Size)
gator$Gender = factor(gator$Gender)
totaln=sum(gator[1:16,5:9]) ## total sample size
rown=c(1:16)
for (i in 1:16) {
  rown[i]=sum(gator[i,5:9]) 
  rown
}

rown ## sample size by the profile

gator$Lake = factor(gator$Lake)

##set the ref levels so that R output matches the SAS code
##sets Hancock as the baseline level
contrasts(gator$Lake)=contr.treatment(levels(gator$Lake),base=2)
contrasts(gator$Lake)

##sets "small" as the refernce level
contrasts(gator$Size)=contr.treatment(levels(gator$Size),base=1)
contrasts(gator$Size)

##sets male as the reference level
contrasts(gator$Gender)=contr.treatment(levels(gator$Gender),base=2)
contrasts(gator$Gender)

##Fit all basline logit models with Fish as the basline level
## By default VGLM will use the last level as the baseline level for creating the logits
## to set Fish as the baseline level, specify it last in vglm call below

# intercept only
fit0=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~1, data=gator, family=multinomial)
fit0
summary(fit0)
deviance(fit0) ## gives only the deviance

# with Gender
fit1=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Gender, data=gator, family=multinomial)
summary(fit1)

# with Size
fit2=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Size, data=gator, family=multinomial)
summary(fit2)

# with Lake
fit3=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake, data=gator, family=multinomial)
summary(fit3)

# with Lake + Size
fit4 = vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size, data=gator, family=multinomial)
summary(fit4)

# with Lake + Size + Gender
fit5=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size+Gender, data=gator, family=multinomial)
summary(fit5)
exp(coefficients(fit5)) ## to get the odds and odds-ratios

# with Lake + Size + LakeXSize
fit6=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size+Lake:Size, data=gator, family=multinomial)
summary(fit6)

# saturated 
fitS = vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size+Gender+Lake:Size+Lake:Gender+Size:Gender+Lake:Size:Gender, data=gator, family=multinomial)
fitS

## Parts of Analysis of deviance
## To compare deviances of different models, for example 
deviance(fit5)-deviance(fitS)
df.residual(fit5)-df.residual(fitS)

deviance(fit4)-deviance(fit5)
df.residual(fit4)-df.residual(fit5)


## to adderess overdispersion with model from the fit4
## we usually use the Chi-Sq. statisics devided by its dfs
## here we need to compute it via first computing pearson residuals
pears.res=(fitted.values(fitS)*rown-fitted.values(fit4)*rown)^2/(fitted.values(fit4)*rown)
X2=sum(pears.res)

scaleparm=sqrt(X2/44)  ## 1.148 for fit 4
## then adjust for dispersion
summary(fit4, dispersion=scaleparm)
##or  gives the same
summary(fit4, dispersion=1.148)

## Consider collapsing over Gender
## see notes on ANGEL

### For Sections 8.2 and 8.3 in the notes?
# Baseline Categories Logit model for nominal response
# Adjacent-Categories Logit Model for nominal response

##recall that vglm uses the last level in R is the default level for creating the logits
fit.bcl = vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size,data=gator,family=multinomial) # Lake + Size

fit.acl = vglm(cbind(Invertebrate,Reptile,Bird,Other,Fish)~Lake+Size,data=gator,family=acat(rev=T)) # consult help(acat) 

deviance(fit.bcl)
deviance(fit.acl) # model fits are equivalent
junk = expand.grid(Size=levels(gator$Size),Lake=levels(gator$Lake))
(pred.bcl = cbind(junk,predict(fit.bcl,type="response",newdata=junk))) # pred. same  
(pred.acl = cbind(junk,predict(fit.acl,type="response",newdata=junk))) # for both models

t(coef(fit.bcl,matrix=T)) 
t(coef(fit.acl,matrix=T)) # coefficients are different, but related:

rev(cumsum(rev(coef(fit.acl,matrix=T)["(Intercept)",])))  #These are the alpha_j for the baseline-category logit model, derived from the adjacent-categories logit model

rev(cumsum(rev(coef(fit.acl,matrix=T)["Lakehancock",]))) # effects for Lake Hancock for bcl model, derived from acl model

rev(cumsum(rev(coef(fit.acl,matrix=T)["Lakeoklawaha",]))) # effects for Lake Oklawaha for bcl model, derived from acl model

rev(cumsum(rev(coef(fit.acl,matrix=T)["Laketrafford",]))) # effect for Lake Trafford for bcl model, derived from acl model

rev(cumsum(rev(coef(fit.acl,matrix=T)["Size<2.3",]))) # effects for small alligators for bcl model, derived from acl model

