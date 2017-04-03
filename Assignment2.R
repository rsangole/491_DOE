library(tidyr)
library(tidyverse)
library(gmodels)
library(daewr)

data <- read_csv(file = 'Assignment1.csv')[,-1]
data <- head(data,-1)

data <- gather(data = data,
       key = 'rf',
       value = 'etchrate')
data$rf <- as.numeric(data$rf)

boxplot(etchrate~rf,data,
        xlab='RF Power',
        ylab='Etch Rate')

mod0 <- lm(etchrate~rf, data=data)
summary(mod0)

mod1 <- aov(etchrate~rf, data=data)
summary(mod1)
names(mod1)
mod1$coefficients
mod1$residuals
par(mfrow = c(2,2))
plot(mod1, which=5)
plot(mod1, which=1)
plot(mod1, which=2)
plot(residuals(mod1) ~ rf,
     main="Residuals vs Exp. Unit",
     font.main=1,
     data=data)
abline(h=0, lty = 2)

data %>%
    group_by(rf) %>%
    summarise(var=var(etchrate))
