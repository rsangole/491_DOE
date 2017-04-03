setwd("C:/Users/Syamala.srinivasan/Google Drive/NorthWestern/DOE_SPC/DOE/LawsonExerciseRcodes")

set.seed(7638)
f <- factor(rep(c("time35","time40","time45"), each =4))
fac <- sample (f, 12)
eu <- 1:12
plan <- data.frame(loaf=eu, time=fac)
write.csv(plan, file = "Plan.csv", row.names=FALSE)
bread <- read.csv("Plan1.csv")
bread
boxplot(height~time, data=bread)
library(daewr)
mod0 <- lm(height ~ time, data=bread)
summary(mod0)
library(gmodels)
attach(bread)
fit.contrast(mod0, "time", c(1, -1,0))
fit.contrast(mod0, "time", c(1, 0,-1))
fit.contrast(mod0, "time", c(0, 1,-1))
mod1 <- aov(height ~ time, data=bread)
summary(mod1)
names(mod1)
mod1$coefficients
mod1$residuals
par(mfrow = c(1,1))
plot(mod1, which=5)
plot(mod1, which=1)
plot(mod1, which=2)
plot(residuals(mod1) ~ loaf, main="Residuals vs Exp. Unit", font.main=1, data=bread)
abline(h=0, lty = 2)
library(MASS)
bc <- boxcox(mod1)
names(bc)
lambda <- bc$x[which.max(bc$y)]
lambda
tbread <- transform(bread, theight = height^(-0.5050505))
par(mfrow = c(1,1))
boxplot(theight~time, data=tbread)
mod2 <- aov(theight~time, data = tbread)
summary(mod2)
par(mfrow = c(1,1))
plot(mod2, which=5)
plot(mod2, which=1)
plot(mod2, which=2)
plot(residuals(mod2) ~ loaf, main="Residuals vs Exp. Unit", font.main=1, data=bread)
abline(h=0, lty = 2)

with( bread, { std <-  tapply( height,time, sd)  
weights <- rep( 1/std, each = 4 ) 
mod3 <- lm( height ~ time, weights = weights, data = bread ) 
anova(mod3 )
})

library(daewr) 
rmin <- 2 # smallest number of replicates 
rmax <- 8 # largest number of replicates 
alpha <- rep(0.05, rmax - rmin +1) 
sigma <- sqrt(2.1)
nlev <- 3 
nreps <- rmin:rmax
Delta <- 3 
power <- Fpower1(alpha, nlev, nreps, Delta, sigma) 
power

contrasts(bread$time) <- contr.poly(3)
contrasts(bread$time)

mod3 <- aov(height ~ time, bread)
summary.lm(mod3)
mod3.tukey <- TukeyHSD(mod3, ordered=T)
mod3.tukey

