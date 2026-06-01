# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)
library(mice)

####preparing the df####
##define sample size; n = 11043

df <- df_h[(df_h$inw1 == 1 &df_h$inw2 == 1 & df_h$inw4 == 1),]
##exclude age >= 50; n =  10672
df <- df[which(df$age >= 50), ]
##delete na in education; n = 10448
df <- subset(df,
            !(is.na(vigo_act1) |is.na(vigo_act2) | is.na(vigo_act4) |
              is.na(mode_act1) |is.na(mode_act2) | is.na(mode_act4)))
#### construct the exposure: physical activity ####
#2.>1 per week, 3.1 per week, 4.1-3 per mon, or 5.never
## construct the exposure: physical activity ##
# 重新定义活动变量分类
df <- df %>%
  mutate(
    # Wave 1: 重新分类剧烈和中等活动
    vig_cat_1 = case_when(
      vigo_act1 %in% c( 2) ~ ">=2",      # 1.Every day, 2.>1 per week → >=2
      vigo_act1 == 3 ~ "=1",               # 3.1 per week → =1
      vigo_act1 %in% c(4, 5) ~ "<1",       # 4.1-3 per mon, 5.never → <1
      TRUE ~ NA_character_
    ),
    mod_cat_1 = case_when(
      mode_act1 %in% c( 2) ~ ">=2",      # 1.Every day, 2.>1 per week → >=2
      mode_act1 == 3 ~ "=1",               # 3.1 per week → =1
      mode_act1 %in% c(4, 5) ~ "<1",       # 4.1-3 per mon, 5.never → <1
      TRUE ~ NA_character_
    ),

    # Wave 2
    vig_cat_2 = case_when(
      vigo_act2 %in% c( 2) ~ ">=2",
      vigo_act2 == 3 ~ "=1",
      vigo_act2 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    ),
    mod_cat_2 = case_when(
      mode_act2 %in% c( 2) ~ ">=2",
      mode_act2 == 3 ~ "=1",
      mode_act2 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    ),

    # Wave 4
    vig_cat_4 = case_when(
      vigo_act4 %in% c( 2) ~ ">=2",
      vigo_act4 == 3 ~ "=1",
      vigo_act4 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    ),
    mod_cat_4 = case_when(
      mode_act4 %in% c( 2) ~ ">=2",
      mode_act4 == 3 ~ "=1",
      mode_act4 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    )
  )

# 根据v+m组合定义活动状态
df <- df %>%
  mutate(
    # Wave 1活动状态
    pa_cat_1 = case_when(
      (vig_cat_1 == "=1" & mod_cat_1 == "<1") |
      (vig_cat_1 == "<1" & mod_cat_1 == "=1") |
      (vig_cat_1 == "<1" & mod_cat_1 == "<1") ~ "Inactive",
      TRUE ~ "Active"  # 其他所有情况都为Active
    ),

    # Wave 2活动状态
    pa_cat_2 = case_when(
      (vig_cat_2 == "=1" & mod_cat_2 == "<1") |
      (vig_cat_2 == "<1" & mod_cat_2 == "=1") |
      (vig_cat_2 == "<1" & mod_cat_2 == "<1") ~ "Inactive",
      TRUE ~ "Active"
    ),

    # Wave 4活动状态
    pa_cat_4 = case_when(
      (vig_cat_4 == "=1" & mod_cat_4 == "<1") |
      (vig_cat_4 == "<1" & mod_cat_4 == "=1") |
      (vig_cat_4 == "<1" & mod_cat_4 == "<1") ~ "Inactive",
      TRUE ~ "Active"
    )
  )

# 轨迹定义保持不变
df <- df %>% mutate(
  traj = case_when(
    # Consistently Active
    pa_cat_1 == "Active" & pa_cat_2 == "Active" & pa_cat_4 == "Active" ~ "Consistently Active",

    # Increasingly
    pa_cat_1 == "Inactive" & pa_cat_2 == "Inactive" & pa_cat_4 == "Active" ~ "Increasingly Active",
    pa_cat_1 == "Inactive" & pa_cat_2 == "Active" & pa_cat_4 == "Active" ~ "Increasingly Active",

    # Decreasingly
    pa_cat_1 == "Active" & pa_cat_2 == "Active" & pa_cat_4 == "Inactive" ~ "Decreasingly Active",
    pa_cat_1 == "Active" & pa_cat_2 == "Inactive" & pa_cat_4 == "Inactive" ~ "Decreasingly Active",

    # Consistently Inactive
    pa_cat_1 == "Inactive" & pa_cat_2 == "Inactive" & pa_cat_4 == "Inactive" ~ "Consistently Inactive",

    # Fluctuating
    pa_cat_1 == "Active" & pa_cat_2 == "Inactive" & pa_cat_4 == "Active" ~ "Variably Active",
    pa_cat_1 == "Inactive" & pa_cat_2 == "Active" & pa_cat_4 == "Inactive" ~ "Variably Active",

    TRUE ~ NA_character_
  )
) %>%
  mutate(traj = factor(traj,
                       levels = c("Consistently Inactive",
                                  "Decreasingly Active",
                                  "Variably Active",
                                  "Increasingly Active",
                                  "Consistently Active")))

# 查看轨迹分布

####outcome: dementia
####construct the outcome: dementia####
#n=5249
df <- df %>%
  mutate(
    cog4 = r4tr20 + r4ser7,
    cog5 = r5tr20 + r5ser7,
    cog6 = r6tr20 + r6ser7,
    cog7 = r7tr20 + r7ser7,
    cog8 = r8tr20 + r8ser7,
    cog9 = r9tr20 + r9ser7
  )

# Function to calculate dementia status for a given cognition variable
classify_dementia <- function(cog_scores, edu_levels) {
  # Initialize dementia vector with NAs
  dementia <- rep(NA_real_, length(cog_scores))  # <-- Changed to NA_real_

  # Get unique education levels (excluding NA)
  edu_unique <- unique(edu_levels[!is.na(edu_levels)])

  # For each education level, calculate cutoff and classify
  for (edu_level in edu_unique) {
    # Find indices for this education level
    idx <- which(edu_levels == edu_level & !is.na(edu_levels))

    # Get cognition scores for this education level
    cog_subset <- cog_scores[idx]

    # Calculate mean and SD (excluding NAs)
    mean_cog <- mean(cog_subset, na.rm = TRUE)
    sd_cog <- sd(cog_subset, na.rm = TRUE)

    # Calculate cutoff
    cutoff <- mean_cog - 1.5 * sd_cog

    # Classify individuals in this education level
    for (i in idx) {
      if (!is.na(cog_scores[i])) {
        dementia[i] <- ifelse(cog_scores[i] < cutoff, 1, 0)
      }
    }
  }

  return(dementia)
}

# Apply classification for each wave
df$dementia4 <- classify_dementia(df$cog4, df$H_edu)
df$dementia5 <- classify_dementia(df$cog5, df$H_edu)
df$dementia6 <- classify_dementia(df$cog6, df$H_edu)
df$dementia7 <- classify_dementia(df$cog7, df$H_edu)
df$dementia8 <- classify_dementia(df$cog8, df$H_edu)
df$dementia9 <- classify_dementia(df$cog9, df$H_edu)

##exclude NA for outcomes dementia;
selected_columns <- c("cog4")
df <- df %>% filter(!is.na(cog4))

###exclude baseline dementia n = 9120
df <- df %>% filter(dementia4 != 1)
####covariates####
#recode variabels
#gender: 1 is men; 2 is women

df$gender <- ifelse(df$gender==1, "men", "women")
df$gender <- factor(df$gender)

#recode education: 1.less than upper secondary; 2.upper secondary and vocat; 3.tertiary
df <- df %>%
  mutate(high_edu = case_when(
    H_edu == 1 ~ "less than upper secondary",
    H_edu == 2 ~ "upper secondary and vocat",
    H_edu == 3 ~ "tertiary",
    TRUE ~ NA_character_
  ))

#marital status: 1.married; 3.registered partnership; 4.separated; 5.divorced; 7.widowed; 8.never married
df$marital_status<- ifelse(df$marital_status==1|df$marital_status==3, "married/partnered", "other")

#wealth
#equivalized_wealth
df$people_living_with <- as.numeric(as.character(df$people_living_with))
df$equivalized_wealth <- df$household_wealth/sqrt(df$people_living_with)
df$equivalized_wealth <- round(df$equivalized_wealth/1000, 2)

df <- df %>%
  mutate(wealth_quartile = ntile(equivalized_wealth, 4)) %>%
  mutate(wealth_quartile = factor(wealth_quartile,
                                  levels = 1:4,
                                  labels = c("1", "2", "3", "4")))

#urbanicity
df <- df %>%
  mutate(urbanicity = case_when(
    urbanicity == 0 ~ "urban",
    urbanicity == 1 ~ "rural",
    TRUE ~ NA_character_
  ))

df$urbanicity <- factor(df$urbanicity)

# Born in Country of Interview
df$born_in_country <- ifelse(df$born_in_country == 1,"in country", ifelse(df$born_in_country == 0, "out of country", df$born_in_country))

#labour force status
#1.employed or self-employed, 3.Unemployed, 5.Partly retired
#6.permanently sick or disab,Disabled, 8.homemaker
df$lb_status <- ifelse(!is.na(df$lb_status) & df$lb_status == 1, "fulltime_employed",
                       ifelse(!is.na(df$lb_status), "other", NA))

#bmi
#BMI 1=obesity, 0=none
df$BMI <- ifelse(df$BMI >= 28, 1, 0)

#drink
#df <- df %>% rename(drink_ever = drink_now)
#table(df$drink_ever, exclude = NULL) #1=Yes, 0=none
df$drink_now <- ifelse(df$drink_ever == 0, 0, 1)

#smoke status
table(df$smoke_now, exclude = NULL) #1=Yes, 0=none
table(df$smoke_ever, exclude = NULL) #Cont

df$smoke_2004 <-ifelse(df$smoke_now == 1, "current smoker",
                       ifelse(df$smoke_now == 0 &
                                df$smoke_ever == 1, "past smoker", "non smoker"))
df$smoke_2004 <- factor(df$smoke_2004, levels = c("non smoker", "past smoker", "current smoker"))

#n_chronicdf <- df %>%
df <- df %>%
mutate(
    n_chronic = r1arthre + r1cancre + r1hibpe + r1diabe + r1lunge + r1hearte + r1stroke
  )

  df <- df %>%
  mutate(n_chronic = case_when(
    n_chronic <= 1 ~ "0 chronic",
    n_chronic >= 2 ~ "chronic",
    TRUE ~ NA_character_
  ))

#depression
df <- df %>%
  mutate(depression_w4 = ifelse(r4eurod >= 4, 1, 0))

df$depression <- ifelse(df$r1depress == 0, 0, 1)

 #==========================================
 #check missingness
 #==========================================
variables <- c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2004","n_chronic","BMI","urbanicity","depression_w4","depression","pa_cat_1","pa_cat_2","pa_cat_4","traj","dementia4","dementia5","dementia6","dementia7","dementia8","dementia9")
df_ungroup <- df %>% ungroup()

 # 然后计算缺失比例
 missing_percent <- data.frame(
   variable = variables,
   missing_prop = sapply(df_ungroup[variable], function(x) round(mean(is.na(x))*100, 2))
 ) %>%
   arrange(desc(missing_prop))

 print(missing_percent)
variables <- c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2004","n_chronic","BMI","urbanicity","depression_w4","depression","pa_cat_1","pa_cat_2","pa_cat_4","traj","dementia4","dementia5","dementia6","dementia7","dementia8","dementia9")
#missingness check
missing_summary <- sapply(df[variables], function(x) {
  percent_missing <- mean(is.na(x)) * 100
  return(round(percent_missing, 1))
})

missing_df <- data.frame(
  Variable = names(missing_summary),
  Missing_Percent = missing_summary
) %>%
  arrange(desc(Missing_Percent))

variables <- c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2004","n_chronic","BMI","urbanicity","depression_w4","depression")
df_complete <- df[complete.cases(df[, variables]), ]

####imputation####
variables <- c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2004","n_chronic","BMI","urbanicity","depression_w4","depression")

#covariates imputation
df_converted <- df[, c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2004","n_chronic","BMI","urbanicity", "depression_w4","depression")]
classes <- sapply(df_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_converted <- df_converted %>%
  mutate_if(names(.) %in% c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2004","n_chronic","BMI","urbanicity","depression_w4","depression"), as.factor)
set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]
