# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggalluvial)
library(tableone)
library(kableExtra)
library(survival)
library(broom)

df_dementia <- df_dementia %>%
  mutate(
    event_status = case_when(
      if_any(c("dementia5","dementia6","dementia7","dementia8","dementia9"), ~ .x == 1) ~ 1,
      if_all(c("dementia5","dementia6","dementia7","dementia8","dementia9"), is.na) ~ NA_real_,
      TRUE ~ 0
    )
  ) %>%
  # drop rows do not have any dementia records (n = 7893)
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
  fill_date("r4iwy", "r4iwm", "r4date") %>%
  fill_date("r5iwy", "r5iwm", "r5date") %>%
  fill_date("r6iwy", "r6iwm", "r6date") %>%
  fill_date("r7iwy", "r7iwm", "r7date") %>%
  fill_date("r8iwy", "r8iwm", "r8date")%>%
  fill_date("r9iwy", "r9iwm", "r9date")

df_dementia <- df_dementia %>%
  mutate(
    min_r9date = min(r9date, na.rm = TRUE),
    median_r9date = median(r9date, na.rm = TRUE),

    event_date = case_when(
      # ============================================================
      # 0. Censored Cases
      # ============================================================
      # 优先级 1: 活到了 Wave 9
      event_status == 0 & !is.na(r9date) ~ as.Date(r9date),

      # 优先级 2: 没参加 Wave 9，回溯到 Wave 8
      event_status == 0 & is.na(r9date) & !is.na(r8date) ~ as.Date(r8date),

      # 优先级 3: W9/W8 都没参加，回溯到 Wave 7
      event_status == 0 & is.na(r9date) & is.na(r8date) & !is.na(r7date) ~ as.Date(r7date),

      # 优先级 4: W9/W8/W7 都没参加，回溯到 Wave 7
      event_status == 0 & is.na(r9date) & is.na(r8date) & !is.na(r7date) & !is.na(r6date) ~ as.Date(r6date),

      # 优先级 4: W9/W8/W7/W6 都没参加，回溯到 Wave 5
      event_status == 0 & is.na(r9date) & is.na(r8date) & is.na(r7date) & is.na(r6date) & !is.na(r5date) ~ as.Date(r5date),

      # 优先级 5: 极端填补（W9/W8/W7/W6/W5 全缺），用 Wave 9 中位日期
      event_status == 0 ~ as.Date(median_r9date),

      # ============================================================
      # 1. Event in Detected at Wave 5 (Interval: Wave 4 -> Wave 5)
      # ============================================================
      event_status == 1 & dementia5 == 1 ~ r4date + floor((r5date - r4date)/2),

      # ============================================================
      # 2. Event Detected at Wave 6 (Interval: ? -> Wave 6)
      # ============================================================
      event_status == 1 & (is.na(dementia5) | dementia5 == 0) & dementia6 == 1 ~ case_when(
        !is.na(r5date) ~ r5date + floor((r6date - r5date)/2),
        TRUE             ~ r4date + floor((r6date - r4date)/2)
      ),

      # ============================================================
      # 3. Event Detected at Wave 7 (Interval: last available wave -> Wave 7)
      # ============================================================
      event_status == 1 & (is.na(dementia5) | dementia5 == 0) &
                          (is.na(dementia6) | dementia6 == 0) & dementia7 == 1 ~ case_when(
        !is.na(r6date) ~ r6date + floor((r7date - r6date)/2),
        !is.na(r5date) ~ r5date + floor((r7date - r5date)/2),
        TRUE             ~ r4date + floor((r7date - r4date)/2)
      ),

      # ============================================================
      # 5. Event Detected at Wave 8 (Interval: last available wave -> Wave 8)
      # ============================================================
      event_status == 1 & (is.na(dementia5) | dementia5 == 0) &
                          (is.na(dementia6) | dementia6 == 0) &
                          (is.na(dementia7) | dementia7 == 0) & dementia8 == 1 ~ case_when(
        !is.na(r7date) ~ r7date + floor((r8date - r7date)/2),
        !is.na(r6date) ~ r6date + floor((r8date - r6date)/2),
        !is.na(r5date) ~ r5date + floor((r8date - r5date)/2),
        TRUE             ~ r4date + floor((r8date - r4date)/2)
      ),

       # ============================================================
      # 5. Event Detected at Wave 9 (Interval: last available wave -> Wave 9)
      # ============================================================
      event_status == 1 & (is.na(dementia5) | dementia5 == 0) &
                          (is.na(dementia6) | dementia6 == 0) &
                          (is.na(dementia7) | dementia7 == 0) &
                          (is.na(dementia8) | dementia8 == 0) & dementia9 == 1 ~ case_when(
        !is.na(r8date) ~ r8date + floor((r9date - r8date)/2),
        !is.na(r7date) ~ r7date + floor((r9date - r7date)/2),
        !is.na(r6date) ~ r6date + floor((r9date - r6date)/2),
        !is.na(r5date) ~ r5date + floor((r9date - r5date)/2),
        TRUE             ~ r4date + floor((r9date - r4date)/2)
      ),

      TRUE ~ as.Date(NA)
    )
  ) %>%
  dplyr::select(-min_r9date, -median_r9date)

df_dementia <- df_dementia %>%
  mutate(
    event_time = as.numeric(difftime(event_date, r4date, units = "days")) / 30.44
  )
########exposure: construction of trajectories of pa ######

# 长格式，并加上 trajectory
df_long <- df_dementia %>%
  select(hhidpn, pa_cat_1, pa_cat_2, pa_cat_4, traj) %>%
  pivot_longer(cols = starts_with("pa_cat_"),
               names_to = "wave",
               values_to = "pa") %>%
  mutate(
    wave = recode(wave,
                  "pa_cat_1" = "wave1",
                  "pa_cat_2" = "wave2",
                  "pa_cat_4" = "wave4"),
    pa = factor(pa, levels = c("Inactive","Active"), ordered = TRUE)
  )

# 颜色映射：按照活动水平从好到差
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

# 绘制桑基图
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
       title = "Physical Activity Trajectories in SHARE (wave1-wave4)",
       fill = "Trajectory Group") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_sankey)
#reference group
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
df_dementia$depression <- relevel(factor(df_dementia$depression),ref = "0")
df_dementia$depression_w4 <- relevel(factor(df_dementia$depression_w4),ref = "0")
# 先设定轨迹顺序
df_dementia$traj <- factor(
  df_dementia$traj,
  levels = c("Consistently Active", "Increasingly Active",
             "Variably Active", "Decreasingly Active",
             "Consistently Inactive")
)

# 创建 Table 1（按轨迹分组）
tbl1 <- CreateTableOne(
  vars = c("age", "gender", "high_edu", "lb_status", "marital_status",
      "drink_now", "smoke_2004", "wealth_quartile", "n_chronic",
      "BMI", "urbanicity", "born_in_country", "depression","depression_w4"),
  strata = "traj",                 # 👈 关键！
  data = df_dementia,
  test = TRUE                      # 👈 显示p值
)

# 输出表格
print(
  tbl1,
  showAllLevels = TRUE,            # 显示每个类别
  catDigits = 2,                   # 分类变量百分比2位小数
  contDigits = 2,                  # 连续变量2位小数
  pDigits = 3,                     # p值3位小数
  nonnormal = c("age"),            # 年龄用非参数
  printToggle = FALSE
) %>%
  knitr::kable(
    caption = "Descriptive characteristics across physical activity trajectory groups at Baseline 2004",
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
t1 = CreateTableOne(vars = c( "age", "gender", "high_edu", "lb_status", "marital_status",
      "drink_now", "smoke_2004", "wealth_quartile", "n_chronic",
      "BMI", "urbanicity", "born_in_country", "depression","depression_w4","traj"),
                    data=df_dementia)
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE, nonnormal = c("height2010","age")) %>%
  knitr::kable(caption = "Descriptive charateristics at Baseline 2010",
               booktabs = T, linesep = '') %>%
  kable_styling(latex_options = "hold_position")
####model

surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)
model_dementia <- coxph(surv_obj ~ traj + age + gender + high_edu + lb_status +
                          marital_status + wealth_quartile +
                          drink_now + smoke_2004 + n_chronic + urbanicity + BMI + born_in_country,
                        data = df_dementia)
summary(model_dementia)
exp(confint(model_dementia))

model_dementia_unadjusted <- coxph(surv_obj ~ traj, data = df_dementia)
summary(model_dementia_unadjusted)
exp(confint(model_dementia_unadjusted))

# 获取整洁的模型结果
model_results <- tidy(model_dementia, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    p_value_formatted = ifelse(p.value < 0.001, "<0.001",
                              format(round(p.value, 3), nsmall = 3)),
    across(c(estimate, conf.low, conf.high), ~ round(., 2))
  ) %>%
  select(term, estimate, conf.low, conf.high, p_value_formatted)

# 查看结果
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
#extract data
