setwd("C:/Users/Syamala.srinivasan/Google Drive/NorthWestern/DOE_SPC/DOE/LawsonExerciseRcodes")

# Example 1 p. 115 
f <- factor( c(1,2,3,4) )
b1t <- sample(f,4) 
b2t <- sample(f,4) 
b3t <- sample(f,4) 
b4t <- sample(f,4) 
t <- c(b1t, b2t, b3t, b4t)
block <- factor( rep(c("carnation", "daisy", "rose", "tulip"), each=4)) 
flnum <- rep(f,4) 
plan<-data.frame(TypeFlower = block, FlowerNumber = flnum, treatment=t)
write.table(plan, file = "RCBPlan.csv", sep = ",", row.names = FALSE) 
plan 
#################################################################
# Example 2 p. 115
library(agricolae) 
treat<- c(1,2,3,4) 
outdesign <- design.rcbd(treat, 4, seed = 11) 
rcb2 <- outdesign$book 
levels(rcb2$block) <- c("carnation", "daisy", "rose", "tulip")
rcb2
##################################################################
# Example 3 p. 118 
library(daewr) 
head(drug)
str(drug)
drug
write.csv(drug,"drug.csv")

library(car) 
scatterplot(rate ~ dose | rat, data=drug,
            main="Enhanced Scatter Plot")


mod1 <- aov( rate ~ rat + dose, data = drug )
summary(mod1) 

mod0 <- aov( rate ~ dose, data = drug )
summary(mod0) 
        
# Example 4 p. 119
contrasts(drug$dose) <- contr.poly(5) 
mod2 <- aov( rate ~ rat + dose, data = drug) 
summary.aov(mod2,split = list(dose = list("Linear" = 1, "Quadratic" = 
                                            2,"Cubic" = 3, "Quartic" = 4) ) ) 
names(mod2)
mod2$fitted.values
mod2$coefficients
# Example 5 p. 119 produces Figure 4.1 
R <- do.call("cbind", split(drug$rate, drug$rat)) 
y <- apply(R, 1, mean ) 
x <- as.double( levels(drug$dose) ) 
plot( x, y, xlab = "dose", ylab = "average lever 
      press rate" ) 
xx <- seq( 0.0, 2.0, .1 ) 
rate.quad <- lm( y ~ poly( x, 2) )
lines(xx, predict( rate.quad, data.frame( x = xx) )) 
# Example 6 p. 121
library(daewr) 
bmin <- 2 
bmax <- 6 
alpha <- .05 
#sigma2 <- 0.0083487
sigma2 <- 0.04376
css <- 0.460208 
nu1 <- 5-1 
blocks <- c(bmin:bmax) 
nu2 <- (blocks - 1) * 4 
nc <- (blocks * css) / sigma2 
Power <- Fpower( alpha, nu1, nu2, nc )
Power 
data.frame(blocks, nu1,nu2, nc, Power)
##################################################################
##################################################################
##################################################################
# Example 7 p. 123 
library(daewr)
head(bha)
bha


mod3 <- aov( y ~ block + strain * treat, data = bha) 
summary(mod3)
mod3a <- aov( y ~ strain * treat, data = bha) 
summary(mod3a)
# Figure 4.2 p. 124 
with(bha, 
     (interaction.plot(treat, strain, y, type = "b", pch = c(0,2,6,15), leg.bty = 
                         "o", main = "BHA Effect for Each Strain", xlab = "BHA Treated", ylab = "average 
                       EROD"))) 
####################################################################
# Example 8 p. 126 
library(daewr)
head(rcb) 
rcb
mod4 <- aov(cdistance ~ teehgt + Error(id/teehgt), data = rcb) 
summary(mod4) 
# Example 9 p. 126 
mod4a <- aov( cdistance ~ id*teehgt, data = rcb) 
summary(mod4a) 
# Example 10 p. 127 
cellmeans <- tapply( rcb$cdistance, list(rcb$id, rcb$teehgt), mean) 
dim(cellmeans) <- NULL
teehgt <- factor( rep(c(1,2,3), each = 9) ) 
id<-factor(rep(c(1,2,3,4,5,6,7,8,9), 3) ) 
mod5 <- aov( cellmeans ~id + teehgt )
summary(mod5)
model.tables( mod5, type = "means" )$tables$teehgt 
# Example 11 p. 127
TukeyHSD( mod5, "teehgt" ) 
######################################################
# Example 12 p. 130 
library(agricolae) 
tmts <- c(1, 2, 3, 4) 
outdesign <- design.lsd( tmts, seed = 23) 
lsd <- outdesign$book
levels(lsd$row) <- c("Week 1", "Week 2", "Week 3", "Week 4") 
levels(lsd$col) <- c("Store 1", "Store 2", "Store 3", "Store 4") 
head(lsd)
lsd
# Example 13 p. 131
library(daewr) 
bioeqv
mod6 <- aov( AUC ~ Subject + Period + Treat, data = bioeqv)
summary(mod6) 
mod7 <- aov( AUC ~ Treat, data = bioeqv)
summary(mod7) 
# Example 14 p. 132 
model.tables( mod6, type = "means")$tables$Treat 
# Example 15 p. 132 
TukeyHSD( mod6, "Treat") 