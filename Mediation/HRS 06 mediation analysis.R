library(dplyr)
library(mice)
library(mediation)

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate

df <- df %>%
  mutate(
    has_prior_dementia = ifelse(cogfunction2008_dementia == 1, 1, 0)
  )

df_clean <- df %>% filter(has_prior_dementia == 0)

outcome_cols <- c("cogfunction2010_dementia","cogfunction2012_dementia", "cogfunction2014_dementia",
                  "cogfunction2016_dementia", "cogfunction2018_dementia","cogfunction2020_dementia")

df_clean <- df_clean %>%
  mutate(
    event_status = case_when(
      if_any(all_of(outcome_cols), ~ .x == 1) ~ 1,
      if_all(all_of(outcome_cols), is.na) ~ NA_real_,
      TRUE ~ 0
    )
  )

df_dementia <- df_clean[!is.na(df_clean$event_status),]

df_dementia$traj <- factor(df_dementia$traj,
                           levels = c("Consistently Active", "Increasingly Active",
                                      "Variably Active", "Decreasingly Active", "Consistently Inactive"))
df_dementia$Race <- factor(df_dementia$Race,levels = c("Non-Hispanic Black","Non-Hispanic White","Hispanic","Other"))
df_dementia$Race <- relevel(df_dementia$Race,ref = "Non-Hispanic White" )
df_dementia$gender <- relevel(factor(df_dementia$gender), ref = "men")
df_dementia$wealth_quartile <- relevel(factor(df_dementia$wealth_quartile), ref = "1")
df_dementia$urbanicity <- relevel(factor(df_dementia$urbanicity),ref = "rural")
df_dementia$n_chronic <- relevel(factor(df_dementia$n_chronic), ref = "0 chronic")
df_dementia$drink_now <- relevel(factor(df_dementia$drink_now), ref = "0")
df_dementia$smoke_2004 <- relevel(factor(df_dementia$smoke_2004), ref = "non smoker")
df_dementia$lb_status <- relevel(factor(df_dementia$lb_status), ref = "fulltime_employed")
df_dementia$us_born <- relevel(factor(df_dementia$us_born),ref = "0")
df_dementia$depression <- relevel(factor(df_dementia$depression),ref = "0")

vars_to_impute <- c(
  "age", "gender", "Race", "lb_status", "marital_status",
  "wealth_quartile", "drink_now", "smoke_2004", "n_chronic",
  "urbanicity", "cesd2008","depression")

df_converted <- df_dementia[, vars_to_impute]
df_converted <- df_converted %>% mutate_if(is.character, as.factor)

set.seed(9400)
mice_mod <- mice(df_converted, method = "cart", m = 1, maxit = 5, printFlag = FALSE)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df_dementia), names(imputed_data))
df_dementia[common_cols] <- imputed_data[common_cols]

df_dementia$depression_2008 <- ifelse(df_dementia$cesd2008 >= 3, 1, 0)

df_dementia$increasingly_vs_active <- ifelse(df_dementia$traj == "Increasingly Active", 1,
                                             ifelse(df_dementia$traj == "Consistently Active", 0, NA))
df_dementia$variably_vs_active <- ifelse(df_dementia$traj == "Variably Active", 1,
                                         ifelse(df_dementia$traj == "Consistently Active", 0, NA))
df_dementia$decreasingly_vs_active <- ifelse(df_dementia$traj == "Decreasingly Active", 1,
                                             ifelse(df_dementia$traj == "Consistently Active", 0, NA))
df_dementia$inactive_vs_active <- ifelse(df_dementia$traj == "Consistently Inactive", 1,
                                         ifelse(df_dementia$traj == "Consistently Active", 0, NA))

df_increasingly <- df_dementia[!is.na(df_dementia$increasingly_vs_active), ]
df_variably <- df_dementia[!is.na(df_dementia$variably_vs_active), ]
df_decreasingly <- df_dementia[!is.na(df_dementia$decreasingly_vs_active), ]
df_inactive <- df_dementia[!is.na(df_dementia$inactive_vs_active), ]

covariates <- c("age", "gender", "Race", "urbanicity", "us_born")

analyze_mediation_with_interaction <- function(data, exposure, mediator, cvs, n_boot = 50) {

  data_sub <- data[, c("event_status", exposure, mediator, cvs)]
  data_sub <- na.omit(data_sub)

  f_med <- as.formula(paste(mediator, "~", exposure, "+", paste(cvs, collapse = "+")))
  mod_m <- glm(f_med, data = data_sub, family = binomial("logit"))

  f_out_interaction <- as.formula(paste("event_status ~", exposure, "+", mediator, "+",
                                        exposure, ":", mediator, "+", paste(cvs, collapse = "+")))
  mod_y_interaction <- glm(f_out_interaction, data = data_sub, family = binomial("logit"))

  f_out <- as.formula(paste("event_status ~", exposure, "+", mediator, "+", paste(cvs, collapse = "+")))
  mod_y <- glm(f_out, data = data_sub, family = binomial("logit"))

  interaction_term <- paste0(exposure, ":", mediator)
  coef_summary <- summary(mod_y_interaction)$coefficients

  if (interaction_term %in% rownames(coef_summary)) {
    interaction_coef <- coef_summary[interaction_term, "Estimate"]
    interaction_se <- coef_summary[interaction_term, "Std. Error"]
    interaction_p <- coef_summary[interaction_term, "Pr(>|z|)"]

    interaction_ci_low <- interaction_coef - 1.96 * interaction_se
    interaction_ci_high <- interaction_coef + 1.96 * interaction_se
  } else {
    interaction_coef <- NA
    interaction_ci_low <- NA
    interaction_ci_high <- NA
    interaction_p <- NA
  }

  mod_m$call$formula <- f_med
  mod_y$call$formula <- f_out

  set.seed(1005)
  res <- mediate(mod_m, mod_y, treat = exposure, mediator = mediator,
                 boot = TRUE, sims = n_boot)
  sum_res <- summary(res)

  set.seed(1005)
  n_sim <- n_boot
  acme_int_vec <- numeric(n_sim)
  total_vec <- numeric(n_sim)
  prop_int_vec <- numeric(n_sim)
  prop_standard_vec <- numeric(n_sim)

  for(i in 1:n_sim) {
    boot_idx <- sample(1:nrow(data_sub), replace = TRUE)
    boot_data <- data_sub[boot_idx, ]

    tryCatch({
      m_boot <- glm(f_med, data = boot_data, family = binomial("logit"))
      y_boot <- glm(f_out_interaction, data = boot_data, family = binomial("logit"))
      y_boot_no_int <- glm(f_out, data = boot_data, family = binomial("logit"))

      data_x1 <- data_x0 <- boot_data
      data_x1[[exposure]] <- 1
      data_x0[[exposure]] <- 0

      pred_m1 <- predict(m_boot, newdata = data_x1, type = "response")
      pred_m0 <- predict(m_boot, newdata = data_x0, type = "response")

      data_y_calc <- data_x1
      data_y_calc[[mediator]] <- pred_m1
      y_1m1 <- mean(predict(y_boot, newdata = data_y_calc, type = "response"))

      data_y_calc[[mediator]] <- pred_m0
      y_1m0 <- mean(predict(y_boot, newdata = data_y_calc, type = "response"))

      acme_int_vec[i] <- y_1m1 - y_1m0

      y_1 <- mean(predict(y_boot_no_int, newdata = data_x1, type = "response"))
      y_0 <- mean(predict(y_boot_no_int, newdata = data_x0, type = "response"))
      total_vec[i] <- y_1 - y_0

      if(abs(total_vec[i]) > 1e-10) {
        prop_int_vec[i] <- acme_int_vec[i] / total_vec[i]
      } else {
        prop_int_vec[i] <- NA
      }

      data_y_std <- data_x1
      data_y_std[[mediator]] <- pred_m1
      y_std_1m1 <- mean(predict(y_boot_no_int, newdata = data_y_std, type = "response"))

      data_y_std[[mediator]] <- pred_m0
      y_std_1m0 <- mean(predict(y_boot_no_int, newdata = data_y_std, type = "response"))

      acme_std <- y_std_1m1 - y_std_1m0
      if(abs(total_vec[i]) > 1e-10) {
        prop_standard_vec[i] <- acme_std / total_vec[i]
      } else {
        prop_standard_vec[i] <- NA
      }

    }, error = function(e) {
      acme_int_vec[i] <<- NA
      total_vec[i] <<- NA
      prop_int_vec[i] <<- NA
      prop_standard_vec[i] <<- NA
    })
  }

  acme_int_vec <- acme_int_vec[!is.na(acme_int_vec)]
  prop_int_vec <- prop_int_vec[!is.na(prop_int_vec)]

  acme_interaction <- mean(acme_int_vec)
  acme_int_ci <- quantile(acme_int_vec, c(0.025, 0.975))
  acme_int_p <- 2 * min(mean(acme_int_vec >= 0), mean(acme_int_vec <= 0))

  prop_interaction <- mean(prop_int_vec)
  prop_int_ci <- quantile(prop_int_vec, c(0.025, 0.975))
  prop_int_p <- 2 * min(mean(prop_int_vec >= 0), mean(prop_int_vec <= 0))

  return(list(
    mediation_results = c(
      ACME = sum_res$d.avg,
      ACME_Low = sum_res$d.avg.ci[1],
      ACME_High = sum_res$d.avg.ci[2],
      ACME_P = sum_res$d.avg.p,

      ADE = sum_res$z.avg,
      ADE_Low = sum_res$z.avg.ci[1],
      ADE_High = sum_res$z.avg.ci[2],
      ADE_P = sum_res$z.avg.p,

      Total = sum_res$tau.coef,
      Total_Low = sum_res$tau.ci[1],
      Total_High = sum_res$tau.ci[2],
      Total_P = sum_res$tau.p,

      Prop_Med = sum_res$n.avg,
      Prop_Med_Low = sum_res$n.avg.ci[1],
      Prop_Med_High = sum_res$n.avg.ci[2],
      Prop_Med_P = sum_res$n.avg.p
    ),
    interaction_coef = interaction_coef,
    interaction_ci_low = interaction_ci_low,
    interaction_ci_high = interaction_ci_high,
    interaction_p = interaction_p,
    acme_interaction = acme_interaction,
    acme_int_ci_low = acme_int_ci[1],
    acme_int_ci_high = acme_int_ci[2],
    acme_int_p = acme_int_p,
    prop_interaction = prop_interaction,
    prop_int_ci_low = prop_int_ci[1],
    prop_int_ci_high = prop_int_ci[2],
    prop_int_p = prop_int_p
  ))
}

mediators_list <- c("depression_2008")

comparisons <- list(
  list(df = df_increasingly, exp = "increasingly_vs_active", label = "Increasingly vs Active"),
  list(df = df_variably, exp = "variably_vs_active", label = "Variably vs Active"),
  list(df = df_decreasingly, exp = "decreasingly_vs_active", label = "Decreasingly vs Active"),
  list(df = df_inactive, exp = "inactive_vs_active", label = "Inactive vs Active")
)

results_df <- data.frame()

for (comp in comparisons) {
  print(paste("Running:", comp$label))

  for (med in mediators_list) {
    print(paste("  - Analyzing mediator:", med))

    result <- analyze_mediation_with_interaction(comp$df, comp$exp, med, covariates, n_boot = 100)
    out <- result$mediation_results

    use_interaction <- !is.na(result$interaction_p) && result$interaction_p < 0.05

    if(use_interaction) {
      final_acme <- result$acme_interaction
      final_acme_ci_low <- result$acme_int_ci_low
      final_acme_ci_high <- result$acme_int_ci_high
      final_acme_p <- result$acme_int_p

      final_prop <- result$prop_interaction
      final_prop_ci_low <- result$prop_int_ci_low
      final_prop_ci_high <- result$prop_int_ci_high
      final_prop_p <- result$prop_int_p

      result_type <- "Interaction"
    } else {
      final_acme <- out["ACME"]
      final_acme_ci_low <- out["ACME_Low.2.5%"]
      final_acme_ci_high <- out["ACME_High.97.5%"]
      final_acme_p <- out["ACME_P"]

      final_prop <- out["Prop_Med"]
      final_prop_ci_low <- out["Prop_Med_Low.2.5%"]
      final_prop_ci_high <- out["Prop_Med_High.97.5%"]
      final_prop_p <- out["Prop_Med_P"]

      result_type <- "Standard"
    }

    row_data <- data.frame(
      Comparison = comp$label,
      Mediator = med,
      N = nrow(comp$df),
      Result_Type = result_type,

      Final_ACME = sprintf("%.4f", final_acme),
      Final_ACME_CI = paste0(sprintf("%.4f", final_acme_ci_low), ", ", sprintf("%.4f", final_acme_ci_high)),
      Final_ACME_P = sprintf("%.3f", final_acme_p),

      Final_Prop_Med = sprintf("%.2f%%", final_prop * 100),
      Final_Prop_Med_CI = paste0(sprintf("%.2f%%", final_prop_ci_low * 100), ", ",
                                 sprintf("%.2f%%", final_prop_ci_high * 100)),
      Final_Prop_Med_P = sprintf("%.3f", final_prop_p),

      Standard_ACME = sprintf("%.4f", out["ACME"]),
      Standard_ACME_CI = paste0(sprintf("%.4f", out["ACME_Low.2.5%"]), ", ", sprintf("%.4f", out["ACME_High.97.5%"])),
      Standard_ACME_P = sprintf("%.3f", out["ACME_P"]),

      ADE = sprintf("%.4f", out["ADE"]),
      ADE_CI = paste0(
        sprintf("%.4f", out["ADE_Low.2.5%"]), ", ",
        sprintf("%.4f", out["ADE_High.97.5%"])
      ),
      ADE_P = sprintf("%.3f", out["ADE_P"]),

      Total = sprintf("%.4f", out["Total"]),
      Total_CI = paste0(sprintf("%.4f", out["Total_Low.2.5%"]), ", ", sprintf("%.4f", out["Total_High.97.5%"])),
      Total_P = sprintf("%.3f", out["Total_P"]),

      Standard_Prop_Med = sprintf("%.2f%%", out["Prop_Med"] * 100),
      Standard_Prop_Med_CI = paste0(sprintf("%.2f%%", out["Prop_Med_Low.2.5%"] * 100), ", ",
                                    sprintf("%.2f%%", out["Prop_Med_High.97.5%"] * 100)),
      Standard_Prop_Med_P = sprintf("%.3f", out["Prop_Med_P"]),

      Interaction_Coef = ifelse(is.na(result$interaction_coef), "NA", sprintf("%.4f", result$interaction_coef)),
      Interaction_CI = ifelse(is.na(result$interaction_coef), "NA",
                              paste0(sprintf("%.4f", result$interaction_ci_low), ", ",
                                     sprintf("%.4f", result$interaction_ci_high))),
      Interaction_P = ifelse(is.na(result$interaction_p), "NA", sprintf("%.3f", result$interaction_p)),

      ACME_Interaction = ifelse(is.na(result$acme_interaction), "NA", sprintf("%.4f", result$acme_interaction)),
      ACME_Int_CI = ifelse(is.na(result$acme_interaction), "NA",
                           paste0(sprintf("%.4f", result$acme_int_ci_low), ", ",
                                  sprintf("%.4f", result$acme_int_ci_high))),
      ACME_Int_P = ifelse(is.na(result$acme_int_p), "NA", sprintf("%.3f", result$acme_int_p)),

      Prop_Interaction = ifelse(is.na(result$prop_interaction), "NA", sprintf("%.2f%%", result$prop_interaction * 100)),
      Prop_Int_CI = ifelse(is.na(result$prop_interaction), "NA",
                           paste0(sprintf("%.2f%%", result$prop_int_ci_low * 100), ", ",
                                  sprintf("%.2f%%", result$prop_int_ci_high * 100))),
      Prop_Int_P = ifelse(is.na(result$prop_int_p), "NA", sprintf("%.3f", result$prop_int_p))
    )

    results_df <- rbind(results_df, row_data)

  }
}

print(results_df)

simplified_results <- results_df %>%
  dplyr::select(Comparison, Mediator, N, Result_Type,
                Final_ACME, Final_ACME_CI, Final_ACME_P,
                ADE,ADE_CI, ADE_P,  # <--- Added these two columns
                Final_Prop_Med, Final_Prop_Med_CI, Final_Prop_Med_P,
                Total, Total_CI, Total_P,
                Interaction_Coef, Interaction_CI, Interaction_P)

sig_interactions <- results_df %>%
  dplyr::filter(!is.na(Interaction_P) & Interaction_P != "NA" & as.numeric(Interaction_P) < 0.05)

if(nrow(sig_interactions) > 0) {
  print("=== Significant Interactions ===")
  sig_int_summary <- sig_interactions %>%
    dplyr::select(Comparison, Mediator, Interaction_Coef, Interaction_CI, Interaction_P,
                  ACME_Interaction, ACME_Int_CI, ACME_Int_P,
                  Prop_Interaction, Prop_Int_CI, Prop_Int_P)
  print(sig_int_summary)

} else {
  print("No significant interactions found")
}

cat("\n=== Analysis Complete ===\n")
cat(sprintf("Total analyses: %d\n", nrow(results_df)))
cat(sprintf("Significant mediations: %d\n", sum(as.numeric(results_df$Final_ACME_P) < 0.05, na.rm = TRUE)))
cat(sprintf("Significant interactions: %d\n", nrow(sig_interactions)))
