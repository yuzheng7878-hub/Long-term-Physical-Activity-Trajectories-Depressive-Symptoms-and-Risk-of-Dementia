library(dplyr)
library(mice)
library(mediation)

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate

df <- df %>%
  mutate(
    has_prior_dementia = ifelse(dementia4 == 1, 1, 0)
  )

df_clean <- df %>% filter(has_prior_dementia == 0)

outcome_cols <- c("dementia5","dementia6", "dementia7",
                  "dementia8","dementia9")

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
df_dementia$traj <- relevel(df_dementia$traj,ref =  "Consistently Active")
df_dementia$high_edu <- factor(df_dementia$high_edu,
                               levels = c("less than upper secondary","upper secondary and vocat", "tertiary"))
df_dementia$high_edu <- relevel(df_dementia$high_edu, ref = "less than upper secondary")
df_dementia$lb_status <- factor(df_dementia$lb_status,levels = c("fulltime_employed","other"))
df_dementia$lb_status <- relevel(df_dementia$lb_status,ref = "other")
df_dementia$gender <- relevel(factor(df_dementia$gender), ref = "men")
df_dementia$marital_status <- relevel(factor(df_dementia$marital_status), ref = "other")
df_dementia$born_in_country <- relevel(factor(df_dementia$born_in_country), ref = "in country")
df_dementia$urbanicity <- relevel(factor(df_dementia$urbanicity),ref = "rural")
df_dementia$wealth_quartile <- relevel(factor(df_dementia$wealth_quartile), ref = "1")
df_dementia$n_chronic <- relevel(factor(df_dementia$n_chronic), ref = "0 chronic")
df_dementia$drink_now <- relevel(factor(df_dementia$drink_now), ref = "0")
df_dementia$smoke_2004 <- relevel(factor(df_dementia$smoke_2004), ref = "non smoker")
df_dementia$age <- as.numeric(as.character(df_dementia$age))
df_dementia$BMI <- relevel(factor(df_dementia$BMI),ref = "0")

vars_to_impute <- c(
  "age", "gender", "lb_status", "marital_status",
  "wealth_quartile", "drink_now", "smoke_2004", "n_chronic",
  "urbanicity", "r4eurod")

df_converted <- df_dementia[, vars_to_impute]
df_converted <- df_converted %>% mutate_if(is.character, as.factor)

cat("Starting multiple imputation (MICE)... This may take a while.\n")
set.seed(9400)
mice_mod <- mice(df_converted, method = "cart", m = 1, maxit = 5, printFlag = FALSE)
imputed_data <- complete(mice_mod)

common_cols <- intersect(names(df_dementia), names(imputed_data))
df_dementia[common_cols] <- imputed_data[common_cols]

df_dementia$depression <- ifelse(df_dementia$r4eurod >= 4, 1, 0)

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

covariates <- c("age", "gender", "urbanicity", "born_in_country")
covariates_formula <- paste(covariates, collapse = " + ")

categorical_vars_to_check <- c("gender", "urbanicity", "born_in_country")

comparisons <- list(
  list(data = df_increasingly, exp_var = "increasingly_vs_active", label = "Increasingly_vs_Active"),
  list(data = df_variably, exp_var = "variably_vs_active", label = "Variably_vs_Active"),
  list(data = df_decreasingly, exp_var = "decreasingly_vs_active", label = "Decreasingly_vs_Active"),
  list(data = df_inactive, exp_var = "inactive_vs_active", label = "Inactive_vs_Active")
)

for (comp in comparisons) {

  current_label <- comp$label
  current_data <- comp$data
  exposure_var <- comp$exp_var
  mediator_var <- "depression"
  outcome_var <- "event_status"

  cat(paste0("\n----------------------------------------\n"))
  cat(paste0("Processing: ", current_label, "\n"))

  n_boot <- 500
  ACME <- data.frame(Estimate=rep(NA, n_boot), CI_lower=rep(NA, n_boot), CI_upper=rep(NA, n_boot))
  ADE <- data.frame(Estimate=rep(NA, n_boot), CI_lower=rep(NA, n_boot), CI_upper=rep(NA, n_boot))
  Prop <- data.frame(Estimate=rep(NA, n_boot), CI_lower=rep(NA, n_boot), CI_upper=rep(NA, n_boot))

  success_count <- 0
  total_iterations <- 0

  while (success_count < n_boot) {
    total_iterations <- total_iterations + 1

    if (total_iterations %% 50 == 0) {
      cat(" Total attempts:", total_iterations, "| Successful:", success_count, "\n")
    }

    sample_size <- round(nrow(current_data) * 0.5)
    sample_idx <- sample(nrow(current_data), sample_size, replace = FALSE)
    data_boot <- current_data[sample_idx, ]

    if (var(data_boot[[outcome_var]], na.rm = TRUE) == 0) next

    if (var(data_boot[[mediator_var]], na.rm = TRUE) == 0) next

    category_check <- sapply(data_boot[categorical_vars_to_check], function(x) {
      if (is.factor(x) || is.character(x)) {
        length(unique(na.omit(x)))
      } else {
        length(unique(na.omit(x)))
      }
    })

    if (any(category_check < 2)) next

    model_fit_success <- tryCatch({

      f_med <- as.formula(paste(mediator_var, "~", exposure_var, "+", covariates_formula))
      model_m <- glm(f_med, data = data_boot, family = binomial("logit"))

      f_out <- as.formula(paste(outcome_var, "~", exposure_var, "*", mediator_var, "+", covariates_formula))
      model_y <- glm(f_out, data = data_boot, family = binomial("logit"))

      list(m = model_m, y = model_y)

    }, error = function(e) {
      return(NULL)
    })

    if (is.null(model_fit_success)) next

    if (any(is.na(coef(model_fit_success$m))) || any(is.na(coef(model_fit_success$y)))) {
      next
    }

    med_result <- tryCatch({
      mediate(
        model.m = model_fit_success$m,
        model.y = model_fit_success$y,
        treat = exposure_var,
        mediator = mediator_var,
        data = data_boot,
        sims = 100
      )
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(med_result)) next

    s <- summary(med_result)
    success_count <- success_count + 1

    ACME[success_count, ] <- c(s$d.avg, s$d.avg.ci)
    ADE[success_count, ] <- c(s$z.avg, s$z.avg.ci)
    Prop[success_count, ] <- c(s$n.avg, s$n.avg.ci)
  }

  cat("Completed:", current_label, "(500 successful iterations)\n")

  final_res <- cbind.data.frame(
    Iteration = 1:500,
    ACME_Est = ACME$Estimate, ACME_Low = ACME$CI_lower, ACME_Up = ACME$CI_upper,
    ADE_Est = ADE$Estimate, ADE_Low = ADE$CI_lower, ADE_Up = ADE$CI_upper,
    Prop_Est = Prop$Estimate, Prop_Low = Prop$CI_lower, Prop_Up = Prop$CI_upper
  )

}

cat("\nAll bootstrap analyses completed.\n")
