# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)
library(survival)
library(broom)

df_dementia <- df_dementia %>%
  mutate(
    traj = relevel(factor(traj, levels = c("Consistently Active", "Increasingly Active", "Variably Active", "Decreasingly Active", "Consistently Inactive")), ref = "Consistently Active"),
    gender = relevel(factor(gender), ref = "men"),
    high_edu = relevel(factor(high_edu, levels = c("less than upper secondary", "upper secondary and vocat", "tertiary")), ref = "less than upper secondary"),
    lb_status = relevel(factor(lb_status, levels = c("fulltime_employed", "other")), ref = "other"),
    marital_status = relevel(factor(marital_status), ref = "other"),
    born_in_country = relevel(factor(born_in_country), ref = "in country"),
    urbanicity = relevel(factor(urbanicity), ref = "rural"),
    wealth_quartile = relevel(factor(wealth_quartile), ref = "1"),
    n_chronic = relevel(factor(n_chronic), ref = "0 chronic"),
    drink_now = relevel(factor(drink_now), ref = "0"),
    smoke_2004 = relevel(factor(smoke_2004), ref = "non smoker"),
    BMI = relevel(factor(BMI), ref = "0"),
    depression = relevel(factor(depression), ref = "0"),
    age = as.numeric(as.character(age))
  )

surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)

model_dementia_gender <- coxph(
  surv_obj ~ traj * gender + age + high_edu + lb_status + marital_status + wealth_quartile +
    drink_now + smoke_2004 + n_chronic + urbanicity + BMI + born_in_country + depression,
  data = df_dementia
)

model_results_gender <- tidy(model_dementia_gender, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    p_value_formatted = ifelse(p.value < 0.001, "<0.001", format(round(p.value, 3), nsmall = 3)),
    across(c(estimate, conf.low, conf.high), ~ round(.x, 2))
  ) %>%
  select(term, estimate, conf.low, conf.high, p_value_formatted)
