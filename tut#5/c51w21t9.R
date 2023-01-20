library(binom)

# Midterm Questions -------------------------------------------------------
# Q6
### x_bar
xbar <- (0.2606555 + 0.03934445) / 2
### Margin of Error

ME <- (0.2606555 - 0.03934445) / 2

# SE
se <- ME / qnorm(.975)

# N
n = floor((xbar * (1 - xbar)) / (se ^ 2))

X = n * xbar

binom.confint(x = 6, n = 40, conf.level = 0.95)

# Q11 & 12 
DP = array(
  c(53, 11, 414, 37, 1, 4, 16, 139),
  dim = c(2, 2, 2),
  dimnames = list(
    X = c("White", "Black"),
    Y = c("Yes", "No"),
    Z = c("White", "Black")
  )
)
DP

mantelhaen.test(DP, correct=F)

# Q15
(qnorm(0.75)+37.1)/20.9
