# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)
library(survival)
library(broom)

df_dementia <- df_dementia %>%
  mutate(
    traj = relevel(factor(traj, levels = c("Consistently Active", "Increasingly Active", "Variably Active", "Decreasingly Active", "Consistently Inactive")), ref = "Consistently Active"),
    gender = relevel(factor(gender), ref = "men")
  )

surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)

model_dementia_gender <- coxph(
  surv_obj ~ traj * gender + age + high_edu + lb_status + Race + marital_status + wealth_quartile +
    drink_now + smoke_2004 + n_chronic + urbanicity + BMI + us_born,
  data = df_dementia
)

model_results_gender <- tidy(model_dementia_gender, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    p_value_formatted = ifelse(p.value < 0.001, "<0.001", format(round(p.value, 3), nsmall = 3)),
    across(c(estimate, conf.low, conf.high), ~ round(., 2))
  ) %>%
  select(term, estimate, conf.low, conf.high, p_value_formatted)
