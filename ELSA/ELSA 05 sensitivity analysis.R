library(dplyr)
library(survival)
library(broom)
library(nnet)

# Sensitivity analysis

df_dementia$traj <- factor(df_dementia$traj,
                           levels = c("Consistently Inactive",
                                      "Decreasingly Active",
                                      "Variably Active",
                                      "Increasingly Active",
                                      "Consistently Active"))
df_dementia$traj <- relevel(df_dementia$traj,ref = "Consistently Active")

cols_to_factor <- c("gender", "high_edu", "lb_status", "race",
                    "marital_status", "wealth_quartile", "drink_now", "smoke_2002",
                    "n_chronic")
df_dementia <- df_dementia %>%
  mutate(across(all_of(cols_to_factor), as.factor))

cols_to_numeric <- c("age", "event_time")
df_dementia <- df_dementia %>%
  mutate(across(all_of(cols_to_numeric), as.numeric))

####sensitivity analysis####
df_dementia <- df_dementia %>%
  mutate(
    death_wave7 = ifelse(r7iwstat == 5, 1, ifelse(r7iwstat < 5, 0, NA)),
    death_wave8 = ifelse(r8iwstat == 5, 1, ifelse(r8iwstat < 5, 0, NA)),
    death_wave9 = ifelse(r9iwstat == 5, 1, ifelse(r9iwstat < 5, 0, NA)),
    death_wave10 = ifelse(r10iwstat == 5, 1, ifelse(r10iwstat < 5, 0, NA)),

    lost_wave7 = ifelse(r7iwstat == 9 | r7iwstat == 7 | r7iwstat == 4, 1, ifelse(r7iwstat == 1, 0, NA)),
    lost_wave8 = ifelse(r8iwstat == 9 | r8iwstat == 7 | r8iwstat == 4, 1, ifelse(r8iwstat == 1, 0, NA)),
    lost_wave9 = ifelse(r9iwstat == 9 | r9iwstat == 7 | r9iwstat == 4, 1, ifelse(r9iwstat == 1, 0, NA)),
    lost_wave10 = ifelse(r10iwstat == 9 | r10iwstat == 7 | r10iwstat == 4, 1, ifelse(r10iwstat == 1, 0, NA))
  )

for (wave in 7:10) {
  death_var <- paste0("death_wave", wave)
  lost_var <- paste0("lost_wave", wave)

  model_death <- glm(as.formula(paste(death_var, "~ age + gender + high_edu + lb_status + BMI +
                 marital_status + wealth_quartile + drink_now + smoke_2002 + n_chronic + race")),
                     data = df_dementia,
                     family = binomial,
                     control = glm.control(maxit = 1000),
                     na.action = na.exclude)

  model_lost <- glm(as.formula(paste(lost_var, "~ age + gender + high_edu + lb_status + BMI +
                 marital_status + wealth_quartile + drink_now + smoke_2002 + n_chronic + race")),
                    data = df_dementia,
                    family = binomial,
                    control = glm.control(maxit = 1000),
                    na.action = na.exclude)

  pred_death <- predict(model_death, type = "response")
  pred_lost <- predict(model_lost, type = "response")

  if (length(pred_death) == nrow(df_dementia) && length(pred_lost) == nrow(df_dementia)) {
    df_dementia <- df_dementia %>%
      mutate(!!paste0("prob_alive_inw", wave) := (1 - pred_death) * (1 - pred_lost))
  } else {
    warning("Prediction length mismatch for wave", wave)
  }
}

for (wave in 7:10) {
  prob_var <- paste0("prob_alive_inw", wave)
  ipcw_var <- paste0("ipcw_inw", wave)

  df_dementia <- df_dementia %>%
    mutate(!!ipcw_var := 1 / get(prob_var))
}

df_dementia <- df_dementia %>%
  mutate(

    event_wave = case_when(
      dementia7 == 1 ~ 7,
      dementia8 == 1 ~ 8,
      dementia9 == 1 ~ 9,
      dementia10 == 1 ~ 10,
      TRUE ~ 10
    ),

    ipcw_cumulative = case_when(
      event_wave == 7 ~ 1 / prob_alive_inw7,
      event_wave == 8 ~ 1 / (prob_alive_inw7 * prob_alive_inw8),
      event_wave == 9 ~ 1 / (prob_alive_inw7 * prob_alive_inw8 * prob_alive_inw9),
      event_wave == 10 ~ 1 / (prob_alive_inw7 * prob_alive_inw8 * prob_alive_inw9* prob_alive_inw10),
      TRUE ~ 1
    )
  )

df_dementia$ipcw_cumulative[is.na(df_dementia$ipcw_cumulative)] <- 1
ipcw_q <- quantile(df_dementia$ipcw_cumulative, probs = c(0.01, 0.99), na.rm = TRUE)
df_dementia$ipcw_cumulative_trunc <- pmin(pmax(df_dementia$ipcw_cumulative, ipcw_q[1]), ipcw_q[2])

surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)

model_ipcw <- coxph(surv_obj ~ traj + age + gender + high_edu + lb_status +
                      marital_status + wealth_quartile + BMI +
                      drink_now + smoke_2002 + n_chronic + race,
                    data = df_dementia,
                    weights = ipcw_cumulative_trunc)

model_ipcw_result <- tidy(model_ipcw, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    HR         = round(estimate, 2),
    CI_low     = round(conf.low, 2),
    CI_high    = round(conf.high, 2),
    p_value    = ifelse(p.value < 0.001, "<0.001",
                        format(round(p.value, 3), nsmall = 3))
  ) %>%
  select(term, HR, CI_low, CI_high, p_value)

#iptw

df_dementia$traj <- factor(df_dementia$traj,
                           levels = c("Consistently Inactive",
                                      "Decreasingly Active",
                                      "Variably Active",
                                      "Increasingly Active",
                                      "Consistently Active"))
df_dementia$traj <- relevel(df_dementia$traj,ref = "Consistently Active")


multinom_model <- multinom(
  traj ~ age + gender + high_edu + lb_status +
    marital_status + wealth_quartile + BMI +
    drink_now + smoke_2002 + n_chronic + race,
  data = df_dementia,
  maxit = 1000,
  trace = FALSE
)

predicted_probs <- fitted(multinom_model)

marginal_probs <- table(df_dementia$traj) / nrow(df_dementia)

iptw_weights <- numeric(nrow(df_dementia))
for (i in 1:nrow(df_dementia)) {
  actual_traj <- as.numeric(df_dementia$traj[i])
  iptw_weights[i] <- marginal_probs[actual_traj] / predicted_probs[i, actual_traj]
}

df_dementia$iptw_weight <- iptw_weights


lower_cut <- quantile(iptw_weights, 0.01, na.rm = TRUE)
upper_cut <- quantile(iptw_weights, 0.99, na.rm = TRUE)

df_dementia$iptw_weight_trunc <- ifelse(
  iptw_weights < lower_cut, lower_cut,
  ifelse(iptw_weights > upper_cut, upper_cut, iptw_weights)
)


required_vars <- c("event_time", "event_status", "traj", "age", "gender",
                   "high_edu", "lb_status", "race", "marital_status",
                   "wealth_quartile", "drink_now", "smoke_2002", "BMI",
                   "n_chronic", "iptw_weight_trunc")

df_iptw_clean <- na.omit(df_dementia[required_vars])



model_iptw <- coxph(
  Surv(event_time, event_status) ~ traj + age + gender + high_edu + lb_status +
    marital_status + wealth_quartile + BMI +
    drink_now + smoke_2002 + n_chronic + race,
  data = df_iptw_clean,
  weights = iptw_weight_trunc,
  robust = TRUE
)
model_iptw_result <- tidy(model_iptw, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    HR         = round(estimate, 2),
    CI_low     = round(conf.low, 2),
    CI_high    = round(conf.high, 2),
    p_value    = ifelse(p.value < 0.001, "<0.001",
                        format(round(p.value, 3), nsmall = 3))
  ) %>%
  select(term, HR, CI_low, CI_high, p_value)
