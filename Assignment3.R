library(tidyr)
library(tidyverse)
library(gmodels)
library(daewr)
library(effects)

data <- read_csv(file = 'Assignment3.csv')
data <- head(data, -1)
colnames(data)[1] <- 'Block'
data.long <- reshape2::melt(
    data = data,
    id.vars = 'Block',
    variable.name = 'treatment',
    value.name = 'yield'
)
data.long$Block <- as.factor(data.long$Block)
data.long$treatment <- as.factor(data.long$treatment)
str(data.long)
data.long

lattice::bwplot(Block ~ yield | treatment, data.long)
lattice::bwplot(yield ~ treatment, data.long)
lattice::bwplot(yield ~ Block, data.long)

mod0 <- aov(yield ~ treatment + Block, data = data.long)
mod0
summary(mod0)

plot(
    effect(
    term = "Block*treatment",
    mod = mod0,
    default.levels = 3
),
multiline = TRUE)

TukeyHSD(mod0)

mod1 <- aov(yield ~ treatment, data = data.long)
mod1
summary(mod1)
TukeyHSD(mod1)


mod2 <- aov(yield ~ Block, data = data.long)
mod2
summary(mod2)
TukeyHSD(mod2)


mod0$coefficients
mod0$residuals
par(mfrow = c(2,2))
plot(mod0, which=5)
plot(mod0, which=1)
plot(mod0, which=2)
plot(residuals(mod0) ~ yield,
     main="Residuals vs Exp. Unit",
     font.main=1,
     data=data.long)
abline(h=0, lty = 2)


library(MASS)
bc <- boxcox(mod0)
lambda <- bc$x[which.max(bc$y)]
lambda

data.long$yield_transformed <- data.long$yield^1.59596

mod4 <- aov(yield_transformed ~ treatment + Block, data = data.long)
mod4
summary(mod4)
par(mfrow = c(2,2))
plot(mod4, which=5)
plot(mod4, which=1)
plot(mod4, which=2)
plot(residuals(mod4) ~ yield,
     main="Residuals vs Exp. Unit",
     font.main=1,
     data=data.long)
abline(h=0, lty = 2)

with(data.long,
     {
         std=tapply(yield,treatment,sd)
         wt = rep(1/std,each=5)
         mod5=lm(yield~treatment+Block,weights = wt,data=data.long)
         anova(mod5)
         par(mfrow = c(2,2))
     plot(mod5, which=5)
         plot(mod5, which=1)
         plot(mod5, which=2)
         plot(residuals(mod5) ~ yield,
              main="Residuals vs Exp. Unit",
              font.main=1,
              data=data.long)
         abline(h=0, lty = 2)
     })
