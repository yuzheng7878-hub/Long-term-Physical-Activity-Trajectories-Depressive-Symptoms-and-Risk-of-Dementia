library(dplyr)
library(tidyr)
library(lubridate)
library(tableone)
library(survival)
library(broom)
library(ggplot2)
library(ggalluvial)
library(knitr)
library(kableExtra)

# Data analysis

df_dementia <- df_dementia %>%
  mutate(
    event_status = case_when(
      if_any(c("dementia8", "dementia9","dementia10"), ~ .x == 1) ~ 1,
      if_all(c("dementia8", "dementia9","dementia10"), is.na) ~ NA_real_,
      TRUE ~ 0
    )
  ) %>%
  filter(!is.na(event_status))
fill_date <- function(data, year_col, month_col, new_col) {
  median_year <- median(data[[year_col]], na.rm = TRUE)

  data %>%
    mutate(
      !!new_col := case_when(
        is.na(.data[[year_col]]) & is.na(.data[[month_col]]) ~ as.Date(NA),
        is.na(.data[[year_col]]) ~ make_date(median_year, .data[[month_col]], 1),
        is.na(.data[[month_col]]) ~ make_date(.data[[year_col]], 6, 15), # Default to June
        TRUE ~ make_date(.data[[year_col]], .data[[month_col]], 15)
      )
    )
}

df_dementia <- df_dementia %>%
  fill_date("r7iwindy", "r7iwindm", "r7date") %>%
  fill_date("r8iwindy", "r8iwindm", "r8date") %>%
  fill_date("r9iwindy", "r9iwindm", "r9date") %>%
  fill_date("r10iwindy", "r10iwindm", "r10date")

df_dementia <- df_dementia %>%
  mutate(
    min_r10date = min(r10date, na.rm = TRUE),
    median_r10date = median(r10date, na.rm = TRUE),

    event_date = case_when(
      event_status == 0 & !is.na(r10date) ~ r10date,
      event_status == 0 & is.na(r10date) & !is.na(r9date) ~ r9date,
      event_status == 0 & is.na(r10date) & is.na(r9date) & !is.na(r8date) ~ r8date,
      event_status == 0 & is.na(r10date) & is.na(r8date) ~ median_r10date,

      event_status == 1 & dementia8 == 1 ~ r7date + floor((r8date - r7date)/2),

      event_status == 1 & (is.na(dementia8) | dementia8 == 0) & dementia9 == 1 & !is.na(r8date)
          ~ r8date + floor((r9date - r8date)/2),

      event_status == 1 & (is.na(dementia8) | dementia8 == 0) & dementia9 == 1 & is.na(r8date)
          ~ r7date + floor((r9date - r7date)/2),

      event_status == 1 & (is.na(dementia8) | dementia8 == 0) & (is.na(dementia9) | dementia9 == 0) & dementia10 == 1 & !is.na(r9date)
          ~ r9date + floor((r10date - r9date)/2),

      event_status == 1 & (is.na(dementia8) | dementia8 == 0) & (is.na(dementia9) | dementia9 == 0) & dementia10 == 1 & is.na(r9date) & !is.na(r8date)
          ~ r8date + floor((r10date - r8date)/2),

      event_status == 1 & (is.na(dementia8) | dementia8 == 0) & (is.na(dementia9) | dementia9 == 0) & dementia10 == 1 & is.na(r9date) & is.na(r8date)
          ~ r7date + floor((r10date - r7date)/2),

      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(-min_r10date, -median_r10date)

df_dementia <- df_dementia %>%
  mutate(
    event_time = as.numeric(difftime(event_date, r7date, units = "days")) / 30.44
  )

########exposure: construction of trajectories of pa ######

df_long <- df_dementia %>%
  dplyr::select(hhidpn, pa_cat_5, pa_cat_6, pa_cat_7, traj) %>%
  pivot_longer(cols = starts_with("pa_cat_"),
               names_to = "wave",
               values_to = "pa") %>%
  mutate(
    wave = recode(wave,
                  "pa_cat_5" = "wave5",
                  "pa_cat_6" = "wave6",
                  "pa_cat_7" = "wave7"),
    pa = factor(pa, levels = c("Inactive", "Active"), ordered = TRUE)
  )

traj_colors <- c(
  "Consistently Inactive"  = "#d73027",
  "Decreasingly Active"    = "#fc8d59",
  "Variably Active"        = "#fee08b",
  "Increasingly Active"    = "#91bfdb",
  "Consistently Active"    = "#1a9850"
)

legend_order <- c(
  "Consistently Inactive",
  "Decreasingly Active",
  "Variably Active",
  "Increasingly Active",
  "Consistently Active"
)

p_sankey <- ggplot(
  df_long,
  aes(x = wave, stratum = pa, alluvium = hhidpn, fill = traj, label = pa)
) +
  geom_stratum(width = 0.3, color = "grey40", fill = NA) +
  geom_alluvium(alpha = 0.6) +
  geom_text(stat = "stratum", size = 4) +
  scale_fill_manual(
    values = traj_colors,
    breaks = legend_order,
    na.value = NA,
    drop = TRUE
  ) +
  labs(x = "Year", y = "Sample size",
       title = "Physical Activity Trajectories in ELSA (wave5-wave7)",
       fill = "Trajectory Group") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"))

#reference group
df_dementia$traj <- factor(df_dementia$traj,
                           levels = c("Consistently Active", "Increasingly Active",
                                      "Variably Active", "Decreasingly Active", "Consistently Inactive"))
df_dementia$traj <- relevel(df_dementia$traj,ref = "Consistently Active")
df_dementia$high_edu <- factor(df_dementia$high_edu,
                               levels = c("less than upper secondary","upper secondary and vocat", "tertiary"))
df_dementia$high_edu <- relevel(df_dementia$high_edu, ref = "less than upper secondary")
df_dementia$lb_status <- factor(df_dementia$lb_status,levels = c("fulltime_employed","other"))
df_dementia$lb_status <- relevel(df_dementia$lb_status,ref = "other")
df_dementia$race <- factor(df_dementia$race,levels = c("white","non-white"))
df_dementia$uk_born <- relevel(factor(df_dementia$uk_born), ref = "uk")
df_dementia$gender <- relevel(factor(df_dementia$gender), ref = "men")
df_dementia$marital_status <- relevel(factor(df_dementia$marital_status), ref = "other")
df_dementia$wealth_quartile <- relevel(factor(df_dementia$wealth_quartile), ref = "1")
df_dementia$n_chronic <- relevel(factor(df_dementia$n_chronic), ref = "0 chronic")
df_dementia$drink_now <- relevel(factor(df_dementia$drink_now), ref = "0")
df_dementia$smoke_2002 <- relevel(factor(df_dementia$smoke_2002), ref = "non smoker")
df_dementia$depression <- relevel(factor(df_dementia$depression), ref = "0")
df_dementia$depression_w7 <- relevel(factor(df_dementia$depression_w7), ref = "0")
df_dementia$age <- as.numeric(as.character(df_dementia$age))
df_dementia$BMI <- relevel(factor(df_dementia$BMI), ref = "0")

df_dementia$traj <- factor(
  df_dementia$traj,
  levels = c("Consistently Active", "Increasingly Active",
             "Variably Active", "Decreasingly Active",
             "Consistently Inactive")
)

tbl1 <- CreateTableOne(
  vars = c("age", "gender","high_edu", "lb_status", "marital_status",
                "drink_now", "smoke_2002", "wealth_quartile", "n_chronic", "BMI","uk_born", "race", "depression","depression_w7"),
  strata = "traj",
  data = df_dementia,
  test = TRUE
)

  tbl1,
  showAllLevels = TRUE,
  catDigits = 2,
  contDigits = 2,
  pDigits = 3,
  nonnormal = c("age"),
  printToggle = FALSE
) %>%
  knitr::kable(
    caption = "Descriptive characteristics across physical activity trajectory groups at Baseline 2010",
    booktabs = TRUE,
    linesep = ''
  ) %>%
  kableExtra::kable_styling(latex_options = "hold_position")

tbl1_mat <- print(
  tbl1,
  showAllLevels = TRUE,
  nonnormal = c("age"),
  printToggle = FALSE
)

####table - baseline 2010
t1 = CreateTableOne(vars = c( "age", "gender","high_edu", "lb_status", "marital_status",
                "drink_now", "smoke_2002", "wealth_quartile", "n_chronic", "BMI","uk_born", "race", "depression","depression_w7","traj"),
                    data=df_dementia)
  knitr::kable(caption = "Descriptive charateristics at Baseline 2010",
               booktabs = T, linesep = '') %>%
  kable_styling(latex_options = "hold_position")

####model
surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)
model_dementia <- coxph(surv_obj ~ traj + age + gender + high_edu + lb_status +
                          marital_status + wealth_quartile +
                          drink_now + smoke_2002 + n_chronic + race + BMI + uk_born,
                        data = df_dementia)
exp(confint(model_dementia))

model_dementia_unadjusted <- coxph(surv_obj ~ traj, data = df_dementia)
exp(confint(model_dementia_unadjusted))

model_results <- tidy(model_dementia, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    p_value_formatted = ifelse(p.value < 0.001, "<0.001",
                              format(round(p.value, 3), nsmall = 3)),
    across(c(estimate, conf.low, conf.high), ~ round(., 2))
  ) %>%
  select(term, estimate, conf.low, conf.high, p_value_formatted)

coef_table <- tidy(model_dementia,
                   conf.int = TRUE,
                   exponentiate = TRUE)

hr_traj <- coef_table$estimate[
  coef_table$term == "trajConsistently Inactive"
]

hr_age <- coef_table$estimate[
  coef_table$term == "age"
]

log(hr_traj) / log(hr_age)

#forest plot
model_results <- broom::tidy(model_dementia, conf.int = TRUE)

target_var <- model_results %>%
  filter(grepl("traj", term)) %>%
  mutate(
    hr = exp(estimate),
    hr_low = exp(conf.low),
    hr_high = exp(conf.high)
  ) %>%
  select(term, hr, hr_low, hr_high, p.value)

ref_row <- data.frame(
  term = "trajConsistently Active (ref)",
  hr = 1,
  hr_low = 1,
  hr_high = 1,
  p.value = NA
)
target_var <- bind_rows(ref_row, target_var)

target_var <- target_var %>%
  mutate(term = gsub("traj", "", term))

target_var <- target_var %>%
  mutate(
    term = case_when(
      term == "Consistently Active (ref)" ~ "Consistently Active (ref)",
      term == "Consistently Inactive"     ~ "Consistently Inactive",
      term == "Decreasingly Active"       ~ "Decreasingly Active",
      term == "Variably Active"            ~ "Variably Active",
      term == "Increasingly Active"       ~ "Increasingly Active",
      TRUE ~ term
    )
  ) %>%
  mutate(
    term = factor(
      term,
      levels = rev(c(
        "Consistently Active (ref)",
        "Increasingly Active",
        "Variably Active",
        "Decreasingly Active",
        "Consistently Inactive"
      ))
    )
  )

forest_plot <- ggplot(target_var, aes(x = hr, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  geom_errorbarh(aes(xmin = hr_low, xmax = hr_high),
                 height = 0.2, color = "black", size = 0.8) +
  geom_point(aes(size = 2), color = "black") +
  scale_x_log10(
    breaks = c(0.3,1,1.5,2),
    labels = c(0.3,1,1.5,2),
    limits = c(0.3,3)
  ) +
  labs(
    x = "Hazard Ratio (95% CI)",
    y = "Physical activity trajectories",
    title = "Association between physical activity trajectories and dementia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 13),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "none"
  )

tab <- table(df_dementia$traj, df_dementia$event_status)
addmargins(tab, 2)
