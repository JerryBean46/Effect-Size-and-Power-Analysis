## Set working directory
setwd("I:/Teaching(Short)/Effect Sizes and Power Analysis")

##Load Libraries
library(ggplot2)     
library(effectsize)
library(correlation)
library(vcd)
library(psych)
library(gplots)

## Pregnancy program x Consistent protection outcome
protection <- read.csv("protection.csv")

protection$group <- factor(protection$group,
                          levels = c(1,2),
                          labels = c("program", "control"))
protection$protection <- factor(protection$protection,
                           levels = c(1,2),
                           labels = c("yes", "no"))

## 2x2 Crosstabulation
options(digits = 3)
(protect <- xtabs(~  protection + group, data = protection))
(prop.table(protect, 1))
(Xsq <- chisq.test(protect))
(effectsize(Xsq, type = "phi"))
(effectsize(Xsq, type = "oddsratio"))
(effectsize(Xsq, type = "riskratio"))

## School Mental Health
mh <- read.csv("summit_mh.csv")  

##3x2 Family * Depression
(fam.dep <- xtabs(~ family + dep_risk, data = mh))

options(digits = 5)
(Xsq <- chisq.test(fam.dep))
(effectsize(Xsq, type = "cramers_V"))

## Stress Program Independent Samples t-test and effect size
stress <- read.csv("stress.csv") 
t.test(stress ~ program, data=stress)
cohens_d(stress ~ program, data=stress)

## Infant Care Self-efficacy Paired Sample t-test example
infant <- read.csv("infant.csv") 
head(infant)
attach(infant)
t.test(x=pre, y=post, paired = TRUE, alternative = "two.sided", data = infant)
cohens_d(x=pre, y=post, paired = TRUE, alternative = "two.sided", data = infant)

survey <- read.csv("survey.csv")
attach(survey)

## Depression Risk by Gender Independent Samples t-test and effect size
t.test(depression ~ gender, data=stress)
cohens_d(depression ~ gender, data=stress)

## One-way Anova of support by family
## Plot the mean of support by family
plotmeans(support ~ family, data=survey, frame = TRUE, n.label = F, xlab = "Family Composition (Parents)",
          ylab = "Support")

## ANOVA with Tukey post hoc test
anova <- aov(support ~ family, data=survey)
summary(anova)
TukeyHSD(anova)
par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(anova))

## Anova effect sizes
eta_squared(anova)
omega_squared(anova)

##Examining two continuous variables
plot(depression ~ stress, xlab = "Stress", ylab = "Depression", data = survey)
abline(lm(depression ~ stress, data = survey), col = "blue")

##Correlation Between Stress and Depression
cor.test(x = survey$stress, y = survey$depression, method = "pearson", data = survey)

##Multivariate Analysis
scores <- as.data.frame(cbind(survey$connect, survey$efficacy, survey$support, survey$press, survey$stress, 
                              survey$depression))
colnames(scores) <- c("connect", "efficacy", "support", "press", "stress", "depression") 
head(scores)  
pairs.panels(scores, main="Scatterplot Matix", data = survey)
cors <- correlation(scores)
cors
summary(cors)

## Regression Modeling
fit <- lm(depression ~ connect + efficacy + press + support + stress, data=survey)
summary(fit)
standardize_parameters(fit, method = "refit")

##Set Correlation
setCor(y=c("stress","depression"), x=c("connect","efficacy","press","support"), data=scores)

