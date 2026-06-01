# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)
library(survival)
library(broom)
library(nnet)

df_dementia <- df_dementia %>%
  mutate(
    traj = relevel(factor(traj, levels = c("Consistently Inactive", "Decreasingly Active", "Variably Active", "Increasingly Active", "Consistently Active")), ref = "Consistently Active"),
    across(c(gender, high_edu, lb_status, Race, marital_status, wealth_quartile, drink_now, smoke_2004, n_chronic, urbanicity, us_born), as.factor),
    across(c(age, event_time, BMI), as.numeric)
  )

# Inverse probability censoring weighting

df_dementia <- df_dementia %>%
  mutate(
    death_wave9 = ifelse(r9iwstat == 5, 1, ifelse(r9iwstat < 5, 0, NA)),
    death_wave10 = ifelse(r10iwstat == 5, 1, ifelse(r10iwstat < 5, 0, NA)),
    death_wave11 = ifelse(r11iwstat == 5, 1, ifelse(r11iwstat < 5, 0, NA)),
    death_wave12 = ifelse(r12iwstat == 5, 1, ifelse(r12iwstat < 5, 0, NA)),
    death_wave13 = ifelse(r13iwstat == 5, 1, ifelse(r13iwstat < 5, 0, NA)),
    death_wave14 = ifelse(r14iwstat == 5, 1, ifelse(r14iwstat < 5, 0, NA)),
    death_wave15 = ifelse(r15iwstat == 5, 1, ifelse(r15iwstat < 5, 0, NA)),
    lost_wave9 = ifelse(r9iwstat %in% c(4, 7, 9), 1, ifelse(r9iwstat == 1, 0, NA)),
    lost_wave10 = ifelse(r10iwstat %in% c(4, 7, 9), 1, ifelse(r10iwstat == 1, 0, NA)),
    lost_wave11 = ifelse(r11iwstat %in% c(4, 7, 9), 1, ifelse(r11iwstat == 1, 0, NA)),
    lost_wave12 = ifelse(r12iwstat %in% c(4, 7, 9), 1, ifelse(r12iwstat == 1, 0, NA)),
    lost_wave13 = ifelse(r13iwstat %in% c(4, 7, 9), 1, ifelse(r13iwstat == 1, 0, NA)),
    lost_wave14 = ifelse(r14iwstat %in% c(4, 7, 9), 1, ifelse(r14iwstat == 1, 0, NA)),
    lost_wave15 = ifelse(r15iwstat %in% c(4, 7, 9), 1, ifelse(r15iwstat == 1, 0, NA))
  )

for (wave in 9:15) {
  death_var <- paste0("death_wave", wave)
  lost_var <- paste0("lost_wave", wave)

  model_death <- glm(as.formula(paste(death_var, "~ age + gender + high_edu + lb_status + marital_status + drink_now + smoke_2004 + wealth_quartile + n_chronic + BMI + Race + urbanicity + us_born")),
                     data = df_dementia, family = binomial, control = glm.control(maxit = 1000), na.action = na.exclude)
  model_lost <- glm(as.formula(paste(lost_var, "~ age + gender + high_edu + lb_status + marital_status + drink_now + smoke_2004 + wealth_quartile + n_chronic + BMI + Race + urbanicity + us_born")),
                    data = df_dementia, family = binomial, control = glm.control(maxit = 1000), na.action = na.exclude)

  pred_death <- predict(model_death, type = "response")
  pred_lost <- predict(model_lost, type = "response")

  df_dementia[[paste0("prob_alive_inw", wave)]] <- (1 - pred_death) * (1 - pred_lost)
}

df_dementia <- df_dementia %>%
  mutate(
    event_wave = case_when(
      cogfunction2008_dementia == 1 ~ 9,
      cogfunction2010_dementia == 1 ~ 10,
      cogfunction2012_dementia == 1 ~ 11,
      cogfunction2014_dementia == 1 ~ 12,
      cogfunction2016_dementia == 1 ~ 13,
      cogfunction2018_dementia == 1 ~ 14,
      cogfunction2020_dementia == 1 ~ 15,
      TRUE ~ 15
    ),
    ipcw_cumulative = case_when(
      event_wave == 9 ~ 1 / prob_alive_inw9,
      event_wave == 10 ~ 1 / (prob_alive_inw9 * prob_alive_inw10),
      event_wave == 11 ~ 1 / (prob_alive_inw9 * prob_alive_inw10 * prob_alive_inw11),
      event_wave == 12 ~ 1 / (prob_alive_inw9 * prob_alive_inw10 * prob_alive_inw11 * prob_alive_inw12),
      event_wave == 13 ~ 1 / (prob_alive_inw9 * prob_alive_inw10 * prob_alive_inw11 * prob_alive_inw12 * prob_alive_inw13),
      event_wave == 14 ~ 1 / (prob_alive_inw9 * prob_alive_inw10 * prob_alive_inw11 * prob_alive_inw12 * prob_alive_inw13 * prob_alive_inw14),
      event_wave == 15 ~ 1 / (prob_alive_inw9 * prob_alive_inw10 * prob_alive_inw11 * prob_alive_inw12 * prob_alive_inw13 * prob_alive_inw14 * prob_alive_inw15),
      TRUE ~ 1
    )
  )

df_dementia$ipcw_cumulative[is.na(df_dementia$ipcw_cumulative)] <- 1
ipcw_q <- quantile(df_dementia$ipcw_cumulative, probs = c(0.01, 0.99), na.rm = TRUE)
df_dementia$ipcw_cumulative_trunc <- pmin(pmax(df_dementia$ipcw_cumulative, ipcw_q[1]), ipcw_q[2])

model_ipcw <- coxph(
  Surv(event_time, event_status) ~ traj + age + gender + high_edu + lb_status + Race + marital_status + wealth_quartile +
    drink_now + smoke_2004 + n_chronic + urbanicity + BMI + us_born,
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
  traj ~ age + gender + high_edu + lb_status + Race + marital_status + wealth_quartile + drink_now + smoke_2004 + n_chronic + urbanicity + BMI + us_born,
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

required_vars <- c("event_time", "event_status", "traj", "age", "gender", "high_edu", "lb_status", "Race", "marital_status",
                   "wealth_quartile", "drink_now", "smoke_2004", "n_chronic", "urbanicity", "us_born", "BMI", "iptw_weight_trunc")

df_iptw_clean <- na.omit(df_dementia[required_vars])

model_iptw <- coxph(
  Surv(event_time, event_status) ~ traj + age + gender + high_edu + lb_status + Race + marital_status + wealth_quartile +
    drink_now + smoke_2004 + n_chronic + urbanicity + BMI + us_born,
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
