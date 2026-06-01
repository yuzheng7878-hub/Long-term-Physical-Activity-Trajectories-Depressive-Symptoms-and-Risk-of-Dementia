library(survival)
library(broom)
library(dplyr)

# Gender interaction analysis

surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)
model_dementia_gender <- coxph(surv_obj ~ traj *gender + age + high_edu + lb_status +
                          marital_status + wealth_quartile +
                          drink_now + smoke_2002 + n_chronic + race + BMI + uk_born,
                          data = df_dementia)
exp(confint(model_dementia_gender))

model_results_g <- tidy(model_dementia_gender, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    p_value_formatted = ifelse(p.value < 0.001, "<0.001",
                              format(round(p.value, 3), nsmall = 3)),
    across(c(estimate, conf.low, conf.high), ~ round(., 2))
  ) %>%
  select(term, estimate, conf.low, conf.high, p_value_formatted)
