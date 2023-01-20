# Quiz
# 1
sqrt(9 * 0.2 * 0.8)

# 2
0.75
x = 36 * 0.75
library(binom)

binom.confint(x = 27,n=36,conf.level = 0.95)






# Q1
# (a)
x <- c(17,218)
n <- c(147,646)
#prop.test(x,n) # This gives the CI with continuity correction
prop.test(x,n,conf.level=0.90,correct=F) # without continuity correction

#(b)
odds <- (x/(n-x))
(OR <- odds[1]/odds[2])

# (n11 * n22)/(n12 * n21)

(SE_OR <- sqrt(1/17 + 1/218 + 1/130 + 1/418))

# 90% CI
# log(OR) + c(-1, 1) * qnorm(0.95) * SE_OR
exp(log(OR) + c(-1, 1) * qnorm(0.95) * SE_OR)

# (c)
dat <- matrix(c(x,n-x),nrow = 2)
t_c <- chisq.test(dat, correct = F)
t_c$observed
t_c$expected
t_c

sum((t_c$observed - t_c$expected)^2/t_c$expected)


mu <- t_c$expected
G2 <- 2 * sum(dat * log(dat/mu))
G2
1 - pchisq(G2, df =1)


# Q2
gen_dat_school <- array(c(10, 100, 90, 200,
                          480, 180, 120, 20), dim = c(2, 2, 2), dimnames = list(
                            Gender = c("Male", "Female"), Accept = c("Yes", "No"),
                            School = c("Law", "Business")))

tab_law <- gen_dat_school[,,1]
tab_b <- gen_dat_school[,,2]

# a  
tab_law + tab_b
(490 * 220)/(210 * 280)

# Mantel-Haenszel Test stat
e1 <- (100 * 110)/400

s1 <- (100 * 300 * 110 * 290)/(400^2 * 399)

e2 <- (600 * 660)/800

s2 <- (600 * 200 * 660 * 140)/(800^2 * 799)

((10 - e1 + 480 - e2)/sqrt(s1 + s2))^2

 
# CMH OR
nom <-((10 * 200)/400) + ((480 * 20)/800)
de <- ((90 * 100)/400) + ((120 * 180)/800)

nom/de

mantelhaen.test(gen_dat_school, correct = F)
