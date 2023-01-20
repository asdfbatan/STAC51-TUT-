library(dplyr)

# Example 1
V <- gl(2,4,length = 8,labels = c("W","B"))
D <- gl(2,1,length = 8,labels = c("W","B"))
P <- gl(2,2,length = 8,labels = c("Y","N"))
# P <- relevel(P,ref = "N")
Freq <- c(53,11,414,37,0,4,16,139)
dat_1 <- data.frame(V,D,P,Freq)
dat_1

dat_1_tab <- array(Freq,dim = c(2,2,2),dimnames = list(D = c("W","B"),P = c("Y","N"), V=c("W","B")))
dat_1_tab

# 1(a) Homogeneous Association Model
Q1.fit <- glm(Freq~ D*V + D*P + P*V, family = poisson() )
summary(Q1.fit)
model.matrix(Q1.fit)


Q1.fitted <- Q1.fit$fitted.values
Q1.fitted


Q1.fitted_tab <- array(Q1.fitted,dim = c(2,2,2),dimnames = list(D = c("W","B"),P = c("Y","N"), V=c("W","B")))
Q1.fitted_tab
# 1(b)
# when V = White
(Q1.fitted[1]* Q1.fitted[4])/(Q1.fitted[2] * Q1.fitted[3])
# when V = Black
(Q1.fitted[5]* Q1.fitted[8])/(Q1.fitted[6] * Q1.fitted[7])

coef(Q1.fit)

exp(-0.8677967)


# 1(c)
Q1.fitted_tab
Q1.fitted_tab[,,1] + Q1.fitted_tab[,,2]

dat_1_tab
dat_1_tab[,,1] + dat_1_tab[,,2]

# Marginal OR
(53 * 176)/(430 * 15)

# 1(d)
summary(Q1.fit)

pchisq(q= 0.37984, df = 1, lower.tail = F)

# 1(e)
Q1e.fit <- glm(Freq~ D*V + P*V, family = poisson() )
summary(Q1e.fit)

pchisq(q= 5.394, df = 2, lower.tail = F)


library(boot)
# Standardized Pearson residuals
glm.diag(Q1e.fit)$rp

# conditional independence model may not be a very good fit.
array(glm.diag(Q1e.fit)$rp,dim = c(2,2,2),dimnames = list(D = c("W","B"),P = c("Y","N"), V=c("W","B")))

# 1(f)
anova(Q1e.fit,Q1.fit,test = "Chisq")

# fisher-exact test example
small.data <- array(data = c(17, 12, 2, 3), dim = c(2, 2),
                    dimnames = list(Treatment = c("Surgery", "Radiation"),
                                    Cancer_controlled = c("Yes","No")))
                                  

small.data
## Calculate probabilities using dhyper ##
P_n17 <- dhyper(17, 19, 15, 29)
P_n17
P_n18 <- dhyper(18, 19, 15, 29)
P_n18
P_n19 <- dhyper(19, 19, 15, 29)
P_n19
P_n17 + P_n18 + P_n19

fisher.test(small.data, alternative = "greater")



