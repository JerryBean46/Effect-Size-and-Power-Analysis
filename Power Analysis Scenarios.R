setwd("I:/Teaching(Short)/Effect Sizes and Power Analysis")

## Load Libraries
library(pwr)
library(semPower)
library(lavaan)

## Two-sample proportions for Alcohol Use by Gender Analysis
two.p <- pwr.2p.test(h = .35, sig.level = .05, power = .8)
two.p
plot(two.p)

## One-sample t-test for Eating Disorder Intervention Power Analysis
one.t <- pwr.t.test(d = .3, sig.level = .05, power = .8, type = "paired", 
                    alternative = "two.sided")
one.t
plot(one.t)

## Two-sample t-test for RTR Intervention Power Analysis
two.t <- pwr.t.test(d = .5, sig.level = .05, power = .8, type = "two.sample", 
                          alternative = "two.sided")
two.t
plot(two.t)

## Regression Predicting Depression 
pwr.f2.test(u= 6, f2 = .40/(1 - .40), sig.level = .05, v=100)

pwr.f2.test(u= 5, f2 = .15, sig.level = .05, power = .8)
reg <- pwr.f2.test(u= 5, f2 = .35, sig.level = .05, power = .8)

pwr.f2.test(u= 5, f2 = .10/(1 - .10), sig.level = .05, v = 100)

## CFA
## Specify Model and determine Df using lavaan syntax
cfa.model <- 'scale = ~ Item.1 + Item.2 + Item.3 + Item.4 + Item.5 + Item.6'
semPower.getDf(cfa.model)

## Conduct CFA power analysis using RMSEA effect size
ap.cfa <- semPower.aPriori(effect = .06, effect.measure = 'RMSEA', alpha = .05, power = .80, df = 9)
summary(ap.cfa)
ap.cfa$requiredN

## SEM
## Specify Model and determine Df using lavaan syntax
sem.model <- '
  attitudes =~ sex.fool + sex.harm
  norms =~ frnd.sex + love.sex
  control =~ self.cntl + how.ref
  intention =~ int.abs + int.avoid
  intention ~ attitudes + norms + control'
semPower.getDf(sem.model)

## Conduct power analysis using RMSEA effect size
ap.sem <- semPower.aPriori(effect = .06, effect.measure = 'RMSEA', alpha = .05, power = .80, df = 14)
summary(ap.sem)
ap.sem$requiredN

## Post hoc Power analysis of the Perceived School Experience Scale CFA
## Specify Model and determine Df using lavaan syntax
cfa.model <- 'F1 = ~ Item.1 + Item.2 + Item.3 + Item.4
              F2 = ~ Item.5 + Item.6 + Item.7 + Item.8 + Item.9 + Item.10
              F3 = ~ Item.11 + Item.12 + Item.13 + Item.14'
semPower.getDf(cfa.model)

ph.dab <- semPower.postHoc(N = 386, effect = .05, effect.measure = 'RMSEA', alpha = .05, df = 74)
summary(ph.dab)
##sink()
