Y = matrix(c(10, 2, 2, 4), ncol=2)
dimnames(Y) = list(trt = c("test", "control"), fav = c("Yes", "No") )
Y

chisq.test(Y, correct=F)

library(vcd)
assocstats(Y)

fisher.test(Y)
round(dhyper(1:12, 12, 6, 12), 5)


round(dhyper(6:12, 12, 6, 12), 5)
round(dhyper(6:12, 12, 6, 12), 5) < round(dhyper(10, 12, 6, 12), 5)

sum(dhyper(c(6,11,12), 12, 6, 12))


library(tidyverse)
dat <- tibble(x = factor(6:12),
              y = dhyper(6:12, 12, 6, 12))


p1 <- ggplot(dat, aes(x = x, y = y,col = x, fill = x)) + 
  geom_bar(stat="identity",) + theme_bw()

p1
