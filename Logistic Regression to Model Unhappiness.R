# Investigating Risk Factors of Unhappiness

# Load the libraries
library(foreign)
library(dplyr)
library(plyr)
library(tidyr)
library(MASS)
library(car)
library(pROC)
library(caret)
library(ResourceSelection)

# Import the 2019 General Social Survey data
data <- read.spss("/Users/emma/Desktop/625 Project/GSS2018.sav", 
                  to.data.frame=TRUE)
dim(data) #The original 2018 GSS data have 2348 rows and 1064 columns

# Subset the original data to the study sample, containing 12 variables of 
# interest only
study <- dplyr::select(data, c("HAPPY", "WRKSTAT", "DEGREE", "MARITAL", "SEX", 
                               "AGE", "RACE", "GOD", "HEALTH", "LIFE", "SATFIN", 
                               "FEAR"))
# After omitting missing values of the study sample, the study sample now 
# consists of 1520 rows and 12 columns
study <- na.omit(study)
dim(study) 

########## Response - HAPPY ##########
# Create a binary response variable UNHAPPY
study$UNHAPPY <- factor(ifelse(study$HAPPY == "NOT TOO HAPPY", 
                               "unhappy", "happy"))
# Relevel make unhappy become the first column of a contingency table, 
# easier to interpret odds ratio
study$UNHAPPY = relevel(study$UNHAPPY, ref = "unhappy") 
# Y = 1 (UNHAPPY = "unhappy") occur 228 times
# Y = 0 (UNHAPPY = "happy") occur 1292 times
table(study$UNHAPPY)

# Write a function "OR" to calculate the sample odds ratio of unhappiness from 
# a 2 by 2 contingency table
OR <- function(exposure){
  or <- table(exposure, 
              study$UNHAPPY)[1,1] * table(exposure, study$UNHAPPY)[2,2] / (table(exposure, study$UNHAPPY)[1,2] * table(exposure, study$UNHAPPY)[2,1])
  return(or)
}

########## Explanatory - AGE ##########
# The original AGE variable consists of factorized integers
table(study$AGE,  exclude = NULL)
study$AGE <- as.numeric(as.character(study$AGE))
study$AGE[is.na(study$AGE)] <- 89 # NAs introduced by coercion because 
                                  # "89 OR OLDER" cannot be coerced into 89
quantile(study$AGE)

# Create a ordinal variable AGEG for grouped age
# Group AGE into 4 categories: <30, 30-50, 50-70 and 70+
study$AGEG <- factor(ifelse(study$AGE < 30, "<30", 
                            ifelse((30 <= study$AGE) & (study$AGE < 50), "30-50", 
                                   ifelse((50 <= study$AGE) & (study$AGE < 70), "50-70", "70+"))))
# One-way frequency table:
table(study$AGEG)
# Two-way contingency table:
table(study$AGEG, study$UNHAPPY)
# Sample conditional probabilities:
prop.table(table(study$AGEG, study$UNHAPPY), margin = 1) 

# Create a binary variable YOUNG
# YOUNG = yes if AGE < 30 and no otherwise
study$YOUNG <- factor(ifelse(study$AGE < 30, "yes", "no"))
# Relevel to make odds ratio more interpretable
study$YOUNG <- relevel(study$YOUNG, ref = "yes") 
# Two-way contingency table:
table(study$YOUNG, study$UNHAPPY)
# Sample odds ratio:
OR(study$YOUNG)

########## Explanatory - SEX ##########
# One-way frequency table:
table(study$SEX,  exclude = NULL)
# Two-way contingency table:
table(study$SEX, study$UNHAPPY)
# Sample odds ratio:
OR(study$SEX)

########## Explanatory - RACE ##########
# One-way frequency table:
table(study$RACE,  exclude = NULL)
# Two-way contingency table:
table(study$RACE, study$UNHAPPY)
# Sample conditional probabilities:
prop.table(table(study$RACE, study$UNHAPPY), margin = 1)

# Create a binary variable NONWHITE
# NONWHITE = no if race is white; NONWHITE = yes if race is black or other
study$NONWHITE <- factor(ifelse(study$RACE == "WHITE", "no", "yes"))
# Relevel to make odds ratio more interpretable
study$NONWHITE <- relevel(study$NONWHITE, ref = "yes") 
# Two-way contingency table:
table(study$NONWHITE, study$UNHAPPY)
# Sample odds ratio:
OR(study$NONWHITE)

########## Explanatory - MARITAL ##########
# One-way frequency table:
table(study$MARITAL,  exclude = NULL)
# Two-way contingency table:
table(study$MARITAL, study$UNHAPPY)
# Sample conditional probabilities:
prop.table(table(study$MARITAL, study$UNHAPPY), margin = 1) 

# Create a binary variable MARRIED
# MARRIED = yes if the subject is married and no otherwise
study$MARRIED <- factor(ifelse(study$MARITAL == "MARRIED", "yes", "no"))
# Two-way contingency table:
table(study$MARRIED, study$UNHAPPY)
# Sample odds ratio:
OR(study$MARRIED)
# Relevel to make ML estimate of the coefficient more interpretable
study$MARRIED <- relevel(study$MARRIED, ref = "yes")

########## Explanatory - DEGREE ##########
# One-way frequency table:
table(study$DEGREE,  exclude = NULL)
# Two-way contingency table:
table(study$DEGREE, study$UNHAPPY)
# Sample conditional probabilities:
prop.table(table(study$DEGREE, study$UNHAPPY), margin = 1)

# Create a binary variable HIGHEDU
# HIGHEDU = yes if the person has bachelor and above degree and no otherwise
study$HIGHEDU <- factor(ifelse(study$DEGREE == "BACHELOR" | study$DEGREE == "GRADUATE", "yes", "no"))
# Two-way contingency table:
table(study$HIGHEDU, study$UNHAPPY)
# Sample odds ratio:
OR(study$HIGHEDU)
# Relevel to make ML estimate of the coefficient more interpretable
study$HIGHEDU <- relevel(study$HIGHEDU, ref = "yes") 

########## Explanatory - WRKSTAT ##########
# One-way frequency table:
table(study$WRKSTAT,  exclude = NULL)
# Two-way contingency table:
table(study$WRKSTAT, study$UNHAPPY)
# Sample conditional probabilities:
prop.table(table(study$WRKSTAT, study$UNHAPPY), margin = 1)

# Create a binary variable UNEMP
# UNEMP = yes if the subject is unemployed or other, no otherwise
study$UNEMP <- factor(ifelse(study$WRKSTAT == "UNEMPL, LAID OFF" | study$WRKSTAT == "OTHER", "yes", "no"))
# Relevel to make odds ratio more interpretable
study$UNEMP <- relevel(study$UNEMP, ref = "yes")
# Two-way contingency table:
table(study$UNEMP, study$UNHAPPY)
# Sample odds ratio:
OR(study$UNEMP) 

########## Explanatory - SATFIN ##########
# One-way frequency table:
table(study$SATFIN,  exclude = NULL)
# Two-way contingency table:
table(study$SATFIN, study$UNHAPPY)
# Sample conditional probabilities:
prop.table(table(study$SATFIN, study$UNHAPPY), margin = 1)

# Create a binary variable UNSATFIN
# UNSATFIN = yes if the subject is unsatisfied about his or her financial situation and no otherwise
study$UNSATFIN <- factor(ifelse(study$SATFIN == "NOT AT ALL SAT", "yes", "no")) 
# Relevel to make odds ratio more interpretable
study$UNSATFIN <- relevel(study$UNSATFIN, ref = "yes") 
# Two-way contingency table:
table(study$UNSATFIN, study$UNHAPPY)
# Sample odds ratio:
OR(study$UNSATFIN) 

########## Explanatory - GOD ##########
# One-way frequency table:
table(study$GOD,  exclude = NULL)
# Sample conditional probabilities:
prop.table(table(study$GOD, study$UNHAPPY), margin = 1)

# Create a binary variable GODB
# GODB = yes if the person believes in God and no otherwise
study$GODB <- factor(ifelse(study$GOD == "KNOW GOD EXISTS", "yes", "no"))
# Two-way contingency table:
table(study$GODB,  study$UNHAPPY)    
# Sample odds ratio:
OR(study$GODB) 
# Relevel to make ML estimate of the coefficient more interpretable
study$GODB <- relevel(study$GODB, ref = "yes")

########## Explanatory - HEALTH ##########
# One-way frequency table:
table(study$HEALTH,  exclude = NULL)
# Sample conditional probabilities:
prop.table(table(study$HEALTH, study$UNHAPPY), margin = 1)

# Create a binary variable UNHEALTH 
# UNHEALTH = yes if the person has fair or poor health, no otherwise
study$UNHEALTH <- factor(ifelse(study$HEALTH == "POOR" | study$HEALTH == "FAIR", "yes", "no")) 
# Relevel to make odds ratio more interpretable
study$UNHEALTH <- relevel(study$UNHEALTH, ref = "yes") 
# Two-way contingency table:
table(study$UNHEALTH, study$UNHAPPY)
# Sample odds ratio:
OR(study$UNHEALTH) 

########## Explanatory - LIFE ##########
# One-way frequency table:
table(study$LIFE,  exclude = NULL)
# Sample conditional probabilities:
prop.table(table(study$LIFE, study$UNHAPPY), margin = 1)

# Create a binary variable UNEXCITE
# UNEXCITE = yes if the person finds life routine or dull (unexciting) and no if
# the person finds life exciting
study$UNEXCITE <- factor(ifelse(study$LIFE == "EXCITING", "no", "yes")) 
# Relevel to make odds ratio more interpretable
study$UNEXCITE <- relevel(study$UNEXCITE, ref = "yes") 
# Two-way contingency table:
table(study$UNEXCITE, study$UNHAPPY)
# Sample odds ratio:
OR(study$UNEXCITE) 

########## Explanatory - FEAR ##########
# Two-way contingency table:
table(study$FEAR, study$UNHAPPY) 
# Sample odds ratio:
OR(study$FEAR)


########## MODEL SELECTION ##########
# Relevel the response variable back because the project is interested in studying unhappiness
# study$UNHAPPY <- relevel(study$UNHAPPY, ref = "happy") 
study$UNHAPPY <- ifelse(study$HAPPY == "NOT TOO HAPPY", 1, 0)
# The main effect model:
main <- glm(UNHAPPY ~ AGEG + SEX + RACE + MARITAL + DEGREE + WRKSTAT + SATFIN + GOD + HEALTH + LIFE + FEAR, 
            data = study, family = binomial)
summary(main)
# Check for multicollinearity: all VIF are smaller than 5
vif(main)

# Next, 4 initial full models (m1, m2, m3, m4) with all main effects and all possible two-factor interactions are examined
# These 4 initial full models try different codings of the explanatory variables. 
# stepAIC function is run on each of the 4 initial full models to select 4 best models based on AIC

# First initial full model mostly considers original variables from 2018 GSS and their interactions
m1 <- glm(UNHAPPY ~ (AGEG + SEX + RACE + MARITAL + DEGREE + WRKSTAT + SATFIN + GODB + HEALTH + LIFE + FEAR)^2, 
          data = study, family = binomial)
# stepAIC(m1)
# stepAIC(m1) results in m1.model

# Second initial full model mostly considers recoded variables and their interactions
m2 <- glm(UNHAPPY ~ (YOUNG + SEX + NONWHITE + MARRIED + HIGHEDU + UNEMP + UNSATFIN + GODB + UNHEALTH + UNEXCITE + FEAR)^2, 
          data = study, family = binomial)
# stepAIC(m2)
# stepAIC(m2) results in m2.model

# Third initial full model considers a combination of original variables and recoded variables, as well as the interactions
m3 <- glm(UNHAPPY ~ (AGEG + SEX + RACE + MARRIED + HIGHEDU + UNEMP + SATFIN + GODB + HEALTH + LIFE + FEAR)^2, 
          data = study, family = binomial)
# stepAIC(m3)
# stepAIC(m3) results in m3.model

# Fourth initial full model also considers a combination of original variables and recoded variables, as well as the interactions
m4 <- glm(UNHAPPY ~ (AGEG + SEX + NONWHITE + MARRIED + HIGHEDU + UNEMP + UNSATFIN + GODB + HEALTH + LIFE + FEAR)^2, 
          data = study, family = binomial)
# stepAIC(m4)
# stepAIC(m4) results in m4.model

######### Best model resulted from stepAIC(m1) - m1.model ##########
m1.model <- glm(formula = UNHAPPY ~ SEX + RACE + MARITAL + SATFIN + GODB + HEALTH + LIFE + SEX:RACE + SATFIN:GODB, 
                family = binomial, data = study)
summary(m1.model) 
# Conduct likelihood ratio tests for effect parameters
Anova(m1.model)

# Retrieve predicted probabilities of unhappiness based on m1.model
study$prob1 <- predict(m1.model, type = c("response"))

# AUC:
(g1 <- roc(UNHAPPY ~ prob1, data = study))
# Plot the ROC curve for all possible cut-off pi_0
plot(g1, xlab = "1 - Specificity", print.auc = TRUE, main = "Model 1") 

# Set the cut-off pi_0 equal to the original proportion of Y = 1 (UNHAPPY = unhappy); pi_0 = 0.15
threshold <- table(study$UNHAPPY)[2]/nrow(study)

# Construct the confusion matrix
predicted1 <- ifelse(predict(m1.model, type="response") > threshold, 1, 0)
actual1 <- m1.model$y
conf_matrix1<-table(predicted1, actual1)
conf_matrix1

# Calculate sensitivity, specificity and Pr(correct classification)
sensitivity(conf_matrix1) 
specificity(conf_matrix1) 
(conf_matrix1[1] + conf_matrix1[4])/nrow(study)

# Hosmer and Lemeshow goodness of fit (GOF) test for ungrouped data
hoslem.test(m1.model$y, fitted(m1.model)) 

######### Best model resulted from stepAIC(m2) - m2.model ##########
m2.model <- glm(formula = UNHAPPY ~ YOUNG + NONWHITE + MARRIED + HIGHEDU + UNEMP + UNSATFIN + GODB + UNHEALTH + UNEXCITE + FEAR + YOUNG:UNEMP + NONWHITE:UNEMP + NONWHITE:GODB + MARRIED:UNSATFIN + HIGHEDU:UNEXCITE + UNEMP:UNSATFIN + UNEMP:UNEXCITE + UNSATFIN:GODB, 
                family = binomial, data = study)
summary(m2.model) 
# Conduct likelihood ratio tests for effect parameters
Anova(m2.model)

# Retrieve predicted probabilities of unhappiness based on m2.model
study$prob2 <- predict(m2.model, type = c("response"))

# AUC:
(g2 <- roc(UNHAPPY ~ prob2, data = study))
# Plot the ROC curve for all possible cut-off pi_0
plot(g2, xlab = "1 - Specificity", print.auc = TRUE, main = "Model 2") 

# Construct the confusion matrix
predicted2 <- ifelse(predict(m2.model, type="response") > threshold, 1, 0)
actual2 <- m2.model$y
conf_matrix2<-table(predicted2, actual2)
conf_matrix2

# Calculate sensitivity, specificity and Pr(correct classification)
sensitivity(conf_matrix2) 
specificity(conf_matrix2) 
(conf_matrix2[1] + conf_matrix2[4])/nrow(study)

# Hosmer and Lemeshow goodness of fit (GOF) test for ungrouped data
hoslem.test(m2.model$y, fitted(m2.model)) 

######### Best model resulted from stepAIC(m3) - m3.model ##########
m3.model <- glm(formula = UNHAPPY ~ AGEG + SEX + RACE + MARRIED + HIGHEDU + UNEMP + SATFIN + GODB + HEALTH + LIFE + FEAR + AGEG:RACE + AGEG:MARRIED + AGEG:FEAR + SEX:RACE + SEX:FEAR + MARRIED:SATFIN + MARRIED:HEALTH + MARRIED:LIFE + HIGHEDU:UNEMP + HIGHEDU:LIFE + SATFIN:GODB, 
                family = binomial, data = study)
summary(m3.model) 
# Conduct likelihood ratio tests for effect parameters
Anova(m3.model)

# Retrieve predicted probabilities of unhappiness based on m3.model
study$prob3 <- predict(m3.model, type = c("response"))

# AUC:
(g3 <- roc(UNHAPPY ~ prob3, data = study))
# Plot the ROC curve for all possible cut-off pi_0
plot(g3, xlab = "1 - Specificity", print.auc = TRUE, main = "Model 3") 

# Construct the confusion matrix
predicted3 <- ifelse(predict(m3.model, type="response") > threshold, 1, 0)
actual3 <- m3.model$y
conf_matrix3<-table(predicted3, actual3)
conf_matrix3

# Calculate sensitivity, specificity and Pr(correct classification)
sensitivity(conf_matrix3) 
specificity(conf_matrix3) 
(conf_matrix3[1] + conf_matrix3[4])/nrow(study)

# Hosmer and Lemeshow goodness of fit (GOF) test for ungrouped data
hoslem.test(m3.model$y, fitted(m3.model)) 

######### Best model resulted from stepAIC(m4) - m4.model ##########
m4.model <- glm(formula = UNHAPPY ~ AGEG + SEX + NONWHITE + MARRIED + HIGHEDU + UNEMP + UNSATFIN + GODB + HEALTH + LIFE + FEAR + AGEG:MARRIED + SEX:NONWHITE + NONWHITE:UNEMP + NONWHITE:GODB + NONWHITE:FEAR + MARRIED:UNSATFIN + MARRIED:HEALTH + MARRIED:LIFE + HIGHEDU:UNEMP + HIGHEDU:LIFE + UNEMP:UNSATFIN + UNEMP:GODB + UNSATFIN:GODB, 
                family = binomial, data = study)
summary(m4.model) 
# Conduct likelihood ratio tests for effect parameters
Anova(m4.model)

# Retrieve predicted probabilities of unhappiness based on m4.model
study$prob4 <- predict(m4.model, type = c("response"))

# AUC:
(g4 <- roc(UNHAPPY ~ prob4, data = study))
# Plot the ROC curve for all possible cut-off pi_0
plot(g4, xlab = "1 - Specificity", print.auc = TRUE, main = "Model 4") 

# Construct the confusion matrix
predicted4 <- ifelse(predict(m4.model, type="response") > threshold, 1, 0)
actual4 <- m4.model$y
conf_matrix4<-table(predicted4, actual4)
conf_matrix4

# Calculate sensitivity, specificity and Pr(correct classification)
sensitivity(conf_matrix4) 
specificity(conf_matrix4) 
(conf_matrix4[1] + conf_matrix4[4])/nrow(study)

# Hosmer and Lemeshow goodness of fit (GOF) test for ungrouped data
hoslem.test(m4.model$y, fitted(m4.model)) 

########## Grouped Data Analysis for the best model - m3.model ########## 
# Group the study data to perform residual anlaysis
# Select variables from the study data based on m3.model
G <- data.frame(ftable(xtabs(~ UNHAPPY + AGEG + SEX + RACE + MARRIED + HIGHEDU + UNEMP + SATFIN + GODB + HEALTH + LIFE + FEAR, data = study)))
G <- G[G$Freq != 0, ]
GS <- spread(G, key = UNHAPPY, value = Freq)

names(GS)[12] <- "happy"
names(GS)[13] <- "unhappy"

GS$unhappy[is.na(GS$unhappy)] <- 0
GS$happy[is.na(GS$happy)] <- 0

nrow(GS) # 1106 rows of grouped observation
head(GS) # Now the data is grouped

# Model fit for grouped data, same as m3.model
GM <- glm(formula = unhappy/(happy + unhappy) ~ AGEG + SEX + RACE + MARRIED + HIGHEDU + UNEMP + SATFIN + GODB + HEALTH + LIFE + FEAR + AGEG:RACE + AGEG:MARRIED + AGEG:FEAR + SEX:RACE + SEX:FEAR + MARRIED:SATFIN + MARRIED:HEALTH + MARRIED:LIFE + HIGHEDU:UNEMP + HIGHEDU:LIFE + SATFIN:GODB, 
          family = binomial, data = GS, weights = (happy + unhappy))
summary(GM)
Anova(GM)

GS$stdres <- rstandard(GM, type = "pearson")
lackfit <- GS[abs(GS$stdres) > 3, ]
# There are 23 lack-of-fit grouped observations
nrow(lackfit) 
# Check what are these lack-of-fit observations
View(lackfit)

# Deviance goodness-of-fit test of the Model 3 using grouped data shows that the p-value is 1
1 - pchisq(GM$deviance, GM$df.residual)

# Remove lack-of-fit observations
NOL <- GS[abs(GS$stdres) < 3, ]
NOLM <- glm(formula = unhappy/(happy + unhappy) ~ AGEG + SEX + RACE + MARRIED + HIGHEDU + UNEMP + SATFIN + GODB + HEALTH + LIFE + FEAR + AGEG:RACE + AGEG:MARRIED + AGEG:FEAR + SEX:RACE + SEX:FEAR + MARRIED:SATFIN + MARRIED:HEALTH + MARRIED:LIFE + HIGHEDU:UNEMP + HIGHEDU:LIFE + SATFIN:GODB, 
            family = binomial, data = NOL, weights = (happy + unhappy))
summary(NOLM)
library(profileModel)
confintModel(NOLM, objective = "ordinaryDeviance", method = "zoom", endpoint.tolerance = 1e-08)



xtabs(Freq ~ MARRIED + UNHAPPY + AGEG, data = G)
levels(check$UNHAPPY) <- c("happy","unhappy")
check <- data.frame(ftable(xtabs(~ UNHAPPY + AGEG +  MARRIED, data = study)))
# 3-way
xtabs(check$Freq ~ MARRIED + UNHAPPY + AGEG, data=check)
xtabs(check$Freq ~ AGEG + UNHAPPY + MARRIED , data=check)

########## Discussion ##########
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(snakecase)
theme_set(theme_sjplot())
plot_model(m3.model, type = "pred", terms = c("AGEG", "RACE"))
plot_model(m3.model, type = "pred", terms = c("SEX", "RACE"))
plot_model(m3.model, type = "pred", terms = c("MARRIED", "SATFIN"))
plot_model(m3.model, type = "pred", terms = c("MARRIED", "HEALTH"))
plot_model(m3.model, type = "pred", terms = c("MARRIED", "LIFE"))
plot_model(m3.model, type = "pred", terms = c("HIGHEDU", "LIFE"))
plot_model(m3.model, type = "pred", terms = c("SATFIN", "GODB"))
