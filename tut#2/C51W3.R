#R code for finding the MLE of pi where Y~Bin(10, pi)
# and obsered y = 3
  
par(mfrow = c(1,2))
## Using Likelihood ##
likelihood <- function(pi) { (pi^3)*((1-pi)^7) }
opt.lik <- optimize(likelihood, interval=c(0, 1), maximum=TRUE)
curve(likelihood, from=0, to=1, xlab=expression(pi), ylab=expression(l(pi)))
abline(v = opt.lik$maximum)
text(opt.lik$maximum + 0.05, 0.0014, label = round(opt.lik$maximum, 4))

## Using log-loikelihood ##      
log_likelihood <- function(pi) { 3*log(pi) + 7*log(1-pi) }
curve(log_likelihood, from=0, to=1, xlab=expression(pi), ylab=expression(L(pi)))
opt.log_lik <- optimize(log_likelihood, interval=c(0, 1), maximum=TRUE)
abline(v = opt.log_lik$maximum)
text(opt.log_lik$maximum + 0.05, -15, label = round(opt.lik$maximum, 4))

#R code for Likelihood Ratio based Confidence interval
# p 12 Aggresti
library(rootSolve)
n <- 32
y <- 23
phat <- y/n
alpha <- 0.05
f1 <- function(pi0) {
  -2*(y*log(pi0) + (n-y)*log(1-pi0)-y*log(phat)
      -(n-y)*log(1-phat)) - qchisq(1-alpha,df=1)
}
uniroot.all(f=f1, interval=c(0.000001,0.999999))
par(oma = c(1,1,0,1), bty = "n")
curve(f1, from=0, to=1, xlab=expression(pi[0]), 
      ylab=expression(paste("-2log(" ) ~ Lambda ~ paste( ") - ") ~ chi[1]^2 ~ 
                        paste( "(" )   ~ alpha ~ paste( ")" ) ))
abline(h=0, col="red")
dev.off()

# tetanus example
par(mfrow = c(1,2))
## Using Likelihood ##
likelihood <- function(mu) { exp(-2*mu)*(mu^4)/6 }
opt.lik <- optimize(likelihood, interval=c(0, 10), maximum=TRUE)
curve(likelihood, from=0, to=10, xlab=expression(mu), ylab=expression(l(mu)))
abline(v = opt.lik$maximum)

## Using log-loikelihood ##      
log_likelihood <- function(mu) {log(exp(-2*mu)*(mu^4)/6)}
curve(log_likelihood, from=0, to=10, xlab=expression(mu), ylab=expression(L(mu)))
opt.log_lik <- optimize(log_likelihood, interval=c(0, 10), maximum=TRUE)
abline(v = opt.log_lik$maximum)

dev.off()


## Binomial Power example
theta <- seq(0,1,0.01)
beta_1 <- theta^5

beta_2 <- 1 - pbinom(q = 2,size = 5,prob = theta)

plot(theta,beta_1,ylab = "power",type = "l")
lines(theta,beta_2)





