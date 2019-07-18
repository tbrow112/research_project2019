#Taylor Brown

library(tidyverse)
library(stargazer)


################################################################
#                       CREATING A MODEL                       #
################################################################
hmda <- readRDS("hdma.RDS")
mod <- glm(denial ~ race + sex + loan_amount_000s + applicant_income_000s + census_tract_number, 
           data = hmda,
           family = "binomial")
summary(mod)

stargazer(mod, type = "text",
          title = "Logit Model of Home Purchase Loan Denials",
          covariate.labels=c("Applicant-Black", "Applicant-Asian", "Applicant-Other Race",
                             "Applicant-Race Unknown", "Applicant-Gender Unknown", "Applicant-Female",
                             "Loan Amount (1,000s)"),
          notes = "Tract fixed effects not shown.",
          omit = 9:48,
          no.space = TRUE, out = "denialmodel.txt")


# coefficient plot

library(broom)

tidymod <- tidy(mod, conf.int = TRUE)
tidymodcov <- tidymod[1:9,]
tidymodcov <- tidymodcov %>%
  mutate(term = factor(term, levels = c("(Intercept)", "sexunknown", "sexFemale",
                                        "loan_amount_000s", "applicant_income_000s",
                                        "raceunknown", "raceother", "raceasian", "raceblack")),
         term = fct_recode(term, race_black = "raceblack",
                           race_asian = "raceasian",
                           race_other = "raceother",
                           race_unknown = "raceunknown",
                           income = "applicant_income_000s",
                           loan_amount = "loan_amount_000s",
                           sex_female = "sexFemale",
                           sex_unknown = "sexunknown",
                           intercept = "(Intercept)"))
ggplot(tidymodcov, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0)

save.image("results.R")

# predicted probability plot

# create new data

newdata <- with(hmda,
                
                data.frame(race = c("white", "black", "asian", "other", "unknown"),
                           
                           applicant_income_000s=mean(applicant_income_000s, na.rm=TRUE),
                           
                           loan_amount_000s=mean(loan_amount_000s, na.rm=TRUE),
                           
                           sex = "male", census_tract_number = "0104.01"))



# generate predicted probabilites

modpp <- cbind(newdata, predict(mod, newdata = newdata, type = "response",se.fit = TRUE))

# derive confidence intervals and reorder race for plotting

modpp <-modpp %>%
  mutate(lb = fit - 1.96*se.fit,
         ub = fit + 1.96*se.fit,
         race = fct_relevel(race, "unknown", "other", "asian", "black", "white"))
# plot
ggplot(modpp, aes(x = race, y = fit)) +
  geom_point() +
  geom_linerange(aes(ymin = lb, ymax=ub)) +
  coord_flip()