
firstdigit   <- 1:9
benfordprobs <- log10(1+1/firstdigit)



NC <- read.csv("https://raw.githubusercontent.com/STAT-JET-ASU/Datasets/master/Instructor/nccounties20102016.csv")
str(NC)
summary(NC)
View(NC)

library(dplyr)
library(BenfordTests)


NC <- NC %>% mutate(firstdigit2016 = signifd(pop2016, digits=1))


print(expected)



observed <- table(NC$firstdigit2016)
n <- sum(observed)

expected <- benfordprobs * n

class(NC$firstdigit2016)

ggplot(NC,aes(x = factor(firstdigit2016))) +
  geom_bar() +
  scale_x_discrete("Number of First Digit")  


 
chisq.test(observed, p = benfordprobs)

chisq.test(observed, p = benfordprobs, simulate.p.value = TRUE)



chisq.benftest(NC$firstdigit2016)


NC <- NC %>% mutate(firstdigitmiles = signifd(area_m2, digits=1))
chisq.test(table(NC$firstdigitmiles), p = benfordprobs, simulate.p.value = TRUE)
table(NC$firstdigitmiles)



chisq.benftest(NC$firstdigitmiles)


pi1 <- pnorm(-2)
pi2 <- pnorm(-1) - pnorm(-2)
pi3 <- pnorm( 0) - pnorm(-1)


pi4 <- pnorm(+1) - pnorm( 0)
pi5 <- pnorm(+2) - pnorm(+1)
pi6 <- pnorm(+2, lower.tail=F)

nullprobs <- c(pi1, pi2, pi3, pi4, pi5, pi6)
print(nullprobs)

ANTHRO <- read.csv("https://raw.githubusercontent.com/STAT-JET-ASU/Datasets/master/Instructor/anthropometric.csv")


ANTHROM <- ANTHRO %>% 
  filter(gender == "male" & height != "NA") %>% 
  mutate(zscoreht = (height - mean(height)) / sd(height))



observed <- c(sum(ANTHRO$zscoreht <= -2),
              sum(-2 < ANTHROM$zscoreht & ANTHROM$zscoreht <= -1),
              sum(-1 < ANTHROM$zscoreht & ANTHROM$zscoreht <=  0),
              sum( 0 < ANTHROM$zscoreht & ANTHROM$zscoreht <= +1),
              sum(+1 < ANTHROM$zscoreht & ANTHROM$zscoreht <= +2),
              sum(ANTHROM$zscoreht > +2))
names(observed) <- c("less than -2sd",
                     "between -2sd and -1sd",
                     "between -1sd and 0",
                     "between 0 and +1sd",
                     "between +1sd and +2sd",
                     "greater than =2sd")
print(observed)

n <- sum(observed)

expected <- nullprobs * n
names(expected) <- c("less than -2sd",
                     "between -2sd and -1sd",
                     "between -1sd and 0",
                     "between 0 and +1sd",
                     "between +1sd and +2sd",
                     "greater than =2sd")
print(expected)

chisq.test(observed, p = nullprobs)


library(ggplot2)

ggplot(ANTHROM, aes(x = zscoreht)) +
  geom_density()

ggplot(ANTHROM, aes(x = height)) +
  geom_density()
