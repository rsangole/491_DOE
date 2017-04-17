library(tidyr)
library(tidyverse)
library(gmodels)
library(daewr)

data <- read_csv(file = 'Assignment2.csv')
data <- head(data,-1)

data <- reshape2::melt(data = data,id.vars='material',variable.name='temperature',value.name='batt_life')
data$material <- as.factor(data$material)
data$temperature <- as.factor(data$temperature)
str(data)
data

lattice::bwplot(batt_life~material+temperature,data)
lattice::bwplot(batt_life~temperature|material,data)
lattice::bwplot(batt_life~material|temperature,data)

mod0 <- lm(batt_life~material*temperature, data=data)
summary(mod0)

plot(effect(term="material*temperature",mod=mod0,default.levels=3),multiline=TRUE)

mod1 <- aov( batt_life~material*temperature, data = data )
model.tables( mod1, type = "means", se = T )
TukeyHSD(mod1)

