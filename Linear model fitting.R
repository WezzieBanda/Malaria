
library(readxl)
library(lme4)

EHTdata2 <- read_excel("Wezzie-Tom/EHTdata2.xlsx")

View(EHTdata2)
spec(EHTdata2)


EHTdata2$observation <- factor(formatC(1:nrow(EHTdata2), flag="0", width=3))

#install.packages("lme4")

#install.packages("lme4", type = "source")

fit <-
  glmer(
    cbind(tot_dead, Total - tot_dead) ~
      treatment + (1 | hut_code) + (1 | code_captureur) + (1 | observation),
    family = binomial, data = EHTdata2) 

is_singular <- isSingular(fit)

# Print the result
print(is_singular)

summary(fit)
#table(EHTdata2$hut_code)
#summary(EHTdata2$hut_code)
#unique_levels <- unique(EHTdata2$hut_code)
#print(unique_levels)
#unique_levels[unique_levels == "Hut 6"]
#hut_6_occurrences <- unique_levels[unique_levels == "Hut 6"]
#print(hut_6_occurrences)

###############To see if the model is important################
fit0 <-
  glmer(
    cbind(tot_dead, Total - tot_dead) ~
      treatment + (1 | hut_code) + (1 | code_captureur),
    family = binomial, data = EHTdata2) 

summary(fit0)
anova(fit,fit0)




#Here are a couple of ways to extract parameter values from the fitted model
fit@beta #Estimates for the fixed effects (on the log-odds scale)
coef(summary(fit))["(Intercept)", "Estimate"]
coef(summary(fit))["treatmentOP", "Estimate"]

InvLogit <- function(X){
  exp(X)/(1+exp(X))
}

#Convert to mortality scale
#InvLogit(coef(summary(fit))["treatmentN1u", "Estimate"])
InvLogit(coef(summary(fit))["(Intercept)", "Estimate"])
InvLogit(coef(summary(fit))["(Intercept)", "Estimate"] + coef(summary(fit))["treatmentOP", "Estimate"])
#InvLogit(coef(summary(fit))["(Intercept)", "Estimate"]+c(-1,1)*1.96*coef(summary(fit))["(Intercept)", "Std. Error"])


InvLogit(coef(summary(fit))["(Intercept)", "Estimate"]+
           c(-1,1)*1.96*coef(summary(fit))["(Intercept)", "Std. Error"])

InvLogit(coef(summary(fit))["(Intercept)", "Estimate"] +
           coef(summary(fit))["treatmentOP", "Estimate"])






