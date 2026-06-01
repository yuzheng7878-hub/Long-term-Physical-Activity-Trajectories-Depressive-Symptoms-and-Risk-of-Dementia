# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(tableone)
library(kableExtra)
library(survival)
library(broom)

df <- df %>%
  mutate(
    event_status = case_when(
      if_any(matches("^cogfunction.*_dementia$"), ~ .x == 1) ~ 1,
      if_all(matches("^cogfunction.*_dementia$"), is.na) ~ NA_real_,
      TRUE ~ 0
    )
  )

df_dementia <- df %>% filter(!is.na(event_status))

df_dementia <- df_dementia %>%
  mutate(
    across(c(r9iwmid, r10iwmid, r11iwmid, r12iwmid, r13iwmid, r14iwmid, r15iwmid, death_date),
           ~ as.Date(.x, origin = "1960-01-01")),
    min_r15iwmid = min(r15iwmid, na.rm = TRUE),
    median_r15iwmid = median(r15iwmid, na.rm = TRUE),
    event_date = case_when(
      event_status == 0 & !is.na(r15iwmid) ~ r15iwmid,
      event_status == 0 & !is.na(death_date) & death_date < min_r15iwmid ~ death_date,
      event_status == 0 & is.na(r15iwmid) ~ median_r15iwmid,
      event_status == 0 ~ as.Date(NA),
      event_status == 1 & cogfunction2010_dementia == 1 ~ r9iwmid + floor((r10iwmid - r9iwmid) / 2),
      event_status == 1 & (is.na(cogfunction2010_dementia) | cogfunction2010_dementia == 0) & cogfunction2012_dementia == 1 ~ case_when(
        !is.na(r10iwmid) ~ r10iwmid + floor((r11iwmid - r10iwmid) / 2),
        TRUE ~ r9iwmid + floor((r11iwmid - r9iwmid) / 2)
      ),
      event_status == 1 & (is.na(cogfunction2012_dementia) | cogfunction2012_dementia == 0) & cogfunction2014_dementia == 1 ~ case_when(
        !is.na(r11iwmid) ~ r11iwmid + floor((r12iwmid - r11iwmid) / 2),
        !is.na(r10iwmid) ~ r10iwmid + floor((r12iwmid - r10iwmid) / 2),
        TRUE ~ r9iwmid + floor((r12iwmid - r9iwmid) / 2)
      ),
      event_status == 1 & (is.na(cogfunction2014_dementia) | cogfunction2014_dementia == 0) & cogfunction2016_dementia == 1 ~ case_when(
        !is.na(r12iwmid) ~ r12iwmid + floor((r13iwmid - r12iwmid) / 2),
        !is.na(r11iwmid) ~ r11iwmid + floor((r13iwmid - r11iwmid) / 2),
        !is.na(r10iwmid) ~ r10iwmid + floor((r13iwmid - r10iwmid) / 2),
        TRUE ~ r9iwmid + floor((r13iwmid - r9iwmid) / 2)
      ),
      event_status == 1 & (is.na(cogfunction2016_dementia) | cogfunction2016_dementia == 0) & cogfunction2018_dementia == 1 ~ case_when(
        !is.na(r13iwmid) ~ r13iwmid + floor((r14iwmid - r13iwmid) / 2),
        !is.na(r12iwmid) ~ r12iwmid + floor((r14iwmid - r12iwmid) / 2),
        !is.na(r11iwmid) ~ r11iwmid + floor((r14iwmid - r11iwmid) / 2),
        !is.na(r10iwmid) ~ r10iwmid + floor((r14iwmid - r10iwmid) / 2),
        TRUE ~ r9iwmid + floor((r14iwmid - r9iwmid) / 2)
      ),
      event_status == 1 & (is.na(cogfunction2018_dementia) | cogfunction2018_dementia == 0) & cogfunction2020_dementia == 1 ~ case_when(
        !is.na(r14iwmid) ~ r14iwmid + floor((r15iwmid - r14iwmid) / 2),
        !is.na(r13iwmid) ~ r13iwmid + floor((r15iwmid - r13iwmid) / 2),
        !is.na(r12iwmid) ~ r12iwmid + floor((r15iwmid - r12iwmid) / 2),
        !is.na(r11iwmid) ~ r11iwmid + floor((r15iwmid - r11iwmid) / 2),
        !is.na(r10iwmid) ~ r10iwmid + floor((r15iwmid - r10iwmid) / 2),
        TRUE ~ r9iwmid + floor((r15iwmid - r9iwmid) / 2)
      ),
      TRUE ~ as.Date(NA)
    ),
    event_time = as.numeric(difftime(event_date, r9iwmid, units = "days")) / 30.44
  ) %>%
  select(-min_r15iwmid, -median_r15iwmid)

df_long <- df_dementia %>%
  select(hhidpn, pa_cat_7, pa_cat_8, pa_cat_9, traj) %>%
  pivot_longer(cols = starts_with("pa_cat_"), names_to = "wave", values_to = "pa") %>%
  mutate(
    wave = recode(wave, "pa_cat_7" = "wave7", "pa_cat_8" = "wave8", "pa_cat_9" = "wave9"),
    pa = factor(pa, levels = c("Inactive", "Active"))
  )

traj_colors <- c(
  "Consistently Inactive" = "#d73027",
  "Decreasingly Active" = "#fc8d59",
  "Variably Active" = "#fee08b",
  "Increasingly Active" = "#91bfdb",
  "Consistently Active" = "#1a9850"
)

legend_order <- c("Consistently Inactive", "Decreasingly Active", "Variably Active", "Increasingly Active", "Consistently Active")

p_sankey <- ggplot(df_long, aes(x = wave, stratum = pa, alluvium = hhidpn, fill = traj, label = pa)) +
  geom_stratum(width = 0.3, color = "grey40", fill = NA) +
  geom_alluvium(alpha = 0.6) +
  geom_text(stat = "stratum", size = 4) +
  scale_fill_manual(values = traj_colors, breaks = legend_order) +
  labs(x = "Wave", y = "Sample size", title = "Physical Activity Trajectories in HRS", fill = "Trajectory Group") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"))

df_dementia <- df_dementia %>%
  mutate(
    traj = relevel(factor(traj, levels = c("Consistently Active", "Increasingly Active", "Variably Active", "Decreasingly Active", "Consistently Inactive")), ref = "Consistently Active"),
    high_edu = relevel(factor(high_edu, levels = c("less than upper secondary", "upper secondary and vocat", "tertiary")), ref = "less than upper secondary"),
    lb_status = relevel(factor(lb_status, levels = c("fulltime_employed", "other")), ref = "other"),
    Race = relevel(factor(Race, levels = c("Non-Hispanic Black", "Non-Hispanic White", "Hispanic", "Other")), ref = "Non-Hispanic White"),
    gender = relevel(factor(gender), ref = "men"),
    marital_status = relevel(factor(marital_status), ref = "other"),
    urbanicity = relevel(factor(urbanicity), ref = "rural"),
    wealth_quartile = relevel(factor(wealth_quartile), ref = "1"),
    n_chronic = relevel(factor(n_chronic), ref = "0 chronic"),
    drink_now = relevel(factor(drink_now), ref = "0"),
    smoke_2004 = relevel(factor(smoke_2004), ref = "non smoker"),
    BMI = relevel(factor(BMI), ref = "0"),
    us_born = relevel(factor(us_born), ref = "0"),
    depression = relevel(factor(depression), ref = "0"),
    depression_2008 = relevel(factor(depression_2008), ref = "0")
  )

tbl1 <- CreateTableOne(
  vars = c("age", "gender", "high_edu", "lb_status", "marital_status", "drink_now", "smoke_2004",
           "wealth_quartile", "n_chronic", "BMI", "urbanicity", "us_born", "Race", "depression", "depression_2008"),
  strata = "traj",
  data = df_dementia,
  test = TRUE
)

tbl1_table <- print(tbl1, showAllLevels = TRUE, catDigits = 2, contDigits = 2,
                    pDigits = 3, nonnormal = c("age"), printToggle = FALSE)

surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)

model_dementia_unadjusted <- coxph(surv_obj ~ traj, data = df_dementia)

model_dementia <- coxph(
  surv_obj ~ traj + age + gender + high_edu + lb_status + Race + marital_status + wealth_quartile +
    drink_now + smoke_2004 + n_chronic + urbanicity + BMI + us_born,
  data = df_dementia
)

model_results <- tidy(model_dementia, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    p_value_formatted = ifelse(p.value < 0.001, "<0.001", format(round(p.value, 3), nsmall = 3)),
    across(c(estimate, conf.low, conf.high), ~ round(., 2))
  ) %>%
  select(term, estimate, conf.low, conf.high, p_value_formatted)

coef_table <- tidy(model_dementia, conf.int = TRUE, exponentiate = TRUE)
hr_traj <- coef_table$estimate[coef_table$term == "trajConsistently Inactive"]
hr_age <- coef_table$estimate[coef_table$term == "age"]
age_equivalent <- log(hr_traj) / log(hr_age)

model_results_for_plot <- tidy(model_dementia, conf.int = TRUE) %>%
  filter(grepl("traj", term)) %>%
  mutate(hr = exp(estimate), hr_low = exp(conf.low), hr_high = exp(conf.high)) %>%
  select(term, hr, hr_low, hr_high, p.value)

ref_row <- data.frame(term = "trajConsistently Active (ref)", hr = 1, hr_low = 1, hr_high = 1, p.value = NA)
target_var <- bind_rows(ref_row, model_results_for_plot) %>%
  mutate(
    term = gsub("traj", "", term),
    term = factor(term,
                  levels = rev(c("Consistently Active (ref)", "Increasingly Active", "Variably Active", "Decreasingly Active", "Consistently Inactive")))
  )

forest_plot <- ggplot(target_var, aes(x = hr, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  geom_errorbarh(aes(xmin = hr_low, xmax = hr_high), height = 0.2, color = "black", linewidth = 0.8) +
  geom_point(size = 2, color = "black") +
  scale_x_log10(breaks = c(0.3, 1, 1.5, 2), labels = c(0.3, 1, 1.5, 2), limits = c(0.3, 3)) +
  labs(x = "Hazard Ratio (95% CI)", y = "Physical activity trajectories",
       title = "Association between physical activity trajectories and dementia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), legend.position = "none")

model_dementia_tv <- coxph(
  Surv(event_time, event_status) ~ traj + age + gender + high_edu + lb_status + Race + marital_status + wealth_quartile +
    drink_now + smoke_2004 + n_chronic + urbanicity + BMI + us_born + traj:log(event_time),
  data = df_dementia
)
