# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)
library(survival)
library(broom)
library(nnet)

# Sensitivity analysis

df_dementia <- df_dementia %>%
  mutate(
    traj = relevel(factor(traj, levels = c("Consistently Inactive", "Decreasingly Active", "Variably Active", "Increasingly Active", "Consistently Active")), ref = "Consistently Active"),
    across(c(gender, high_edu, lb_status, marital_status, wealth_quartile, drink_now, smoke_2004, n_chronic, urbanicity, born_in_country), as.factor),
    across(c(age, event_time, BMI), as.numeric)
  )

# Inverse probability censoring weighting

df_dementia <- df_dementia %>%
  mutate(
    death_wave4 = ifelse(r4iwstat == 5, 1, ifelse(r4iwstat < 5, 0, NA)),
    death_wave5 = ifelse(r5iwstat == 5, 1, ifelse(r5iwstat < 5, 0, NA)),
    death_wave6 = ifelse(r6iwstat == 5, 1, ifelse(r6iwstat < 5, 0, NA)),
    death_wave7 = ifelse(r7iwstat == 5, 1, ifelse(r7iwstat < 5, 0, NA)),
    death_wave8 = ifelse(r8iwstat == 5, 1, ifelse(r8iwstat < 5, 0, NA)),
    death_wave9 = ifelse(r9iwstat == 5, 1, ifelse(r9iwstat < 5, 0, NA)),
    lost_wave4 = ifelse(r4iwstat %in% c(4, 7, 9), 1, ifelse(r4iwstat == 1, 0, NA)),
    lost_wave5 = ifelse(r5iwstat %in% c(4, 7, 9), 1, ifelse(r5iwstat == 1, 0, NA)),
    lost_wave6 = ifelse(r6iwstat %in% c(4, 7, 9), 1, ifelse(r6iwstat == 1, 0, NA)),
    lost_wave7 = ifelse(r7iwstat %in% c(4, 7, 9), 1, ifelse(r7iwstat == 1, 0, NA)),
    lost_wave8 = ifelse(r8iwstat %in% c(4, 7, 9), 1, ifelse(r8iwstat == 1, 0, NA)),
    lost_wave9 = ifelse(r9iwstat %in% c(4, 7, 9), 1, ifelse(r9iwstat == 1, 0, NA))
  )

for (wave in 4:9) {
  death_var <- paste0("death_wave", wave)
  lost_var <- paste0("lost_wave", wave)

  model_death <- glm(as.formula(paste(death_var, "~ age + gender + high_edu + lb_status + marital_status + wealth_quartile + drink_now + smoke_2004 + n_chronic + urbanicity + BMI + born_in_country")),
                     data = df_dementia, family = binomial, control = glm.control(maxit = 1000), na.action = na.exclude)
  model_lost <- glm(as.formula(paste(lost_var, "~ age + gender + high_edu + lb_status + marital_status + wealth_quartile + drink_now + smoke_2004 + n_chronic + urbanicity + BMI + born_in_country")),
                    data = df_dementia, family = binomial, control = glm.control(maxit = 1000), na.action = na.exclude)

  pred_death <- predict(model_death, type = "response")
  pred_lost <- predict(model_lost, type = "response")

  df_dementia[[paste0("prob_alive_inw", wave)]] <- (1 - pred_death) * (1 - pred_lost)
}

df_dementia <- df_dementia %>%
  mutate(
    event_wave = case_when(
      dementia4 == 1 ~ 4,
      dementia5 == 1 ~ 5,
      dementia6 == 1 ~ 6,
      dementia7 == 1 ~ 7,
      dementia8 == 1 ~ 8,
      dementia9 == 1 ~ 9,
      TRUE ~ 9
    ),
    ipcw_cumulative = case_when(
      event_wave == 4 ~ 1 / prob_alive_inw4,
      event_wave == 5 ~ 1 / (prob_alive_inw4 * prob_alive_inw5),
      event_wave == 6 ~ 1 / (prob_alive_inw4 * prob_alive_inw5 * prob_alive_inw6),
      event_wave == 7 ~ 1 / (prob_alive_inw4 * prob_alive_inw5 * prob_alive_inw6 * prob_alive_inw7),
      event_wave == 8 ~ 1 / (prob_alive_inw4 * prob_alive_inw5 * prob_alive_inw6 * prob_alive_inw7 * prob_alive_inw8),
      event_wave == 9 ~ 1 / (prob_alive_inw4 * prob_alive_inw5 * prob_alive_inw6 * prob_alive_inw7 * prob_alive_inw8 * prob_alive_inw9),
      TRUE ~ 1
    )
  )

df_dementia$ipcw_cumulative[is.na(df_dementia$ipcw_cumulative)] <- 1
ipcw_q <- quantile(df_dementia$ipcw_cumulative, probs = c(0.01, 0.99), na.rm = TRUE)
df_dementia$ipcw_cumulative_trunc <- pmin(pmax(df_dementia$ipcw_cumulative, ipcw_q[1]), ipcw_q[2])

model_ipcw <- coxph(
  Surv(event_time, event_status) ~ traj + age + gender + high_edu + lb_status + marital_status + wealth_quartile +
    drink_now + smoke_2004 + n_chronic + urbanicity + BMI + born_in_country,
  data = df_dementia,
  weights = ipcw_cumulative_trunc
)

model_ipcw_result <- tidy(model_ipcw, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    HR = round(estimate, 2),
    CI_low = round(conf.low, 2),
    CI_high = round(conf.high, 2),
    p_value = ifelse(p.value < 0.001, "<0.001", format(round(p.value, 3), nsmall = 3))
  ) %>%
  select(term, HR, CI_low, CI_high, p_value)

# Inverse probability treatment weighting

multinom_model <- multinom(
  traj ~ age + gender + high_edu + lb_status + marital_status + wealth_quartile + drink_now + smoke_2004 + n_chronic + urbanicity + BMI + born_in_country,
  data = df_dementia,
  maxit = 1000,
  trace = FALSE
)

predicted_probs <- fitted(multinom_model)
marginal_probs <- table(df_dementia$traj) / nrow(df_dementia)

iptw_weights <- numeric(nrow(df_dementia))
for (i in 1:nrow(df_dementia)) {
  actual_traj <- as.character(df_dementia$traj[i])
  iptw_weights[i] <- as.numeric(marginal_probs[actual_traj]) / predicted_probs[i, actual_traj]
}

df_dementia$iptw_weight <- iptw_weights
iptw_q <- quantile(iptw_weights, probs = c(0.01, 0.99), na.rm = TRUE)
df_dementia$iptw_weight_trunc <- pmin(pmax(iptw_weights, iptw_q[1]), iptw_q[2])

required_vars <- c("event_time", "event_status", "traj", "age", "gender", "high_edu", "lb_status", "marital_status",
                   "wealth_quartile", "drink_now", "smoke_2004", "n_chronic", "urbanicity", "BMI", "born_in_country", "iptw_weight_trunc")

df_iptw_clean <- na.omit(df_dementia[required_vars])

model_iptw <- coxph(
  Surv(event_time, event_status) ~ traj + age + gender + high_edu + lb_status + marital_status + wealth_quartile +
    drink_now + smoke_2004 + n_chronic + urbanicity + BMI + born_in_country,
  data = df_iptw_clean,
  weights = iptw_weight_trunc,
  robust = TRUE
)

model_iptw_result <- tidy(model_iptw, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    HR = round(estimate, 2),
    CI_low = round(conf.low, 2),
    CI_high = round(conf.high, 2),
    p_value = ifelse(p.value < 0.001, "<0.001", format(round(p.value, 3), nsmall = 3))
  ) %>%
  select(term, HR, CI_low, CI_high, p_value)
