library(FrF2)
library(DoE.base)
library(daewr)

design <-
    pb(
        nruns = 20,
        nfactors = 19,
        factor.names = LETTERS[1:19],
        randomize = FALSE
    )
design

y <-
    c(
        1.04,
        0.76,
        0.84,
        2.68,
        2.08,
        1.20,
        1.22,
        1.36,
        1.14,
        0.60,
        2.16,
        0.78,
        0.80,
        0.98,
        0.74,
        1.98,
        1.72,
        0.86,
        0.94,
        2.08
    )
design <- add.response(design , y)

design

mod1 <- lm(y ~ (.) ^ 2, data = design)
summary(mod1)

sort(abs(coef(mod1)),decreasing = T)[1:6]

cfs <- coef(mod1)[2:19]
names <- names(cfs)
halfnorm(
    effects = cfs,
    labs = names,
    alpha = 0.3,
    refline = F
)
