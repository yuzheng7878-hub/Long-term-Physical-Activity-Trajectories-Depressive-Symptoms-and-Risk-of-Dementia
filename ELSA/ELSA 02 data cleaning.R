library(dplyr)
library(tidyr)
library(mice)

# Data cleaning

df <- df_h[(df_h$inw5 == 1 &df_h$inw6 == 1 & df_h$inw7 == 1),]

df <- df[which(df$age >= 50), ]

df <- subset(df,
            !(is.na(vigo_act5) |is.na(vigo_act6) | is.na(vigo_act7) |
              is.na(mode_act5) |is.na(mode_act6) | is.na(mode_act7)))

df <- df %>%
  mutate(

    vig_cat_5 = case_when(
      vigo_act5 %in% c( 2) ~ ">=2",
      vigo_act5 == 3 ~ "=1",
      vigo_act5 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    ),
    mod_cat_5 = case_when(
      mode_act5 %in% c( 2) ~ ">=2",
      mode_act5 == 3 ~ "=1",
      mode_act5 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    ),

    vig_cat_6 = case_when(
      vigo_act6 %in% c( 2) ~ ">=2",
      vigo_act6 == 3 ~ "=1",
      vigo_act6 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    ),
    mod_cat_6 = case_when(
      mode_act6 %in% c( 2) ~ ">=2",
      mode_act6 == 3 ~ "=1",
      mode_act6 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    ),

    vig_cat_7 = case_when(
      vigo_act7 %in% c( 2) ~ ">=2",
      vigo_act7 == 3 ~ "=1",
      vigo_act7 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    ),
    mod_cat_7 = case_when(
      mode_act7 %in% c( 2) ~ ">=2",
      mode_act7 == 3 ~ "=1",
      mode_act7 %in% c(4, 5) ~ "<1",
      TRUE ~ NA_character_
    )
  )

df <- df %>%
  mutate(

    pa_cat_5 = case_when(
      (vig_cat_5 == "=1" & mod_cat_5 == "<1") |
      (vig_cat_5 == "<1" & mod_cat_5 == "=1") |
      (vig_cat_5 == "<1" & mod_cat_5 == "<1") ~ "Inactive",
      TRUE ~ "Active"
    ),

    pa_cat_6 = case_when(
      (vig_cat_6 == "=1" & mod_cat_6 == "<1") |
      (vig_cat_6 == "<1" & mod_cat_6 == "=1") |
      (vig_cat_6 == "<1" & mod_cat_6 == "<1") ~ "Inactive",
      TRUE ~ "Active"
    ),

    pa_cat_7 = case_when(
      (vig_cat_7 == "=1" & mod_cat_7 == "<1") |
      (vig_cat_7 == "<1" & mod_cat_7 == "=1") |
      (vig_cat_7 == "<1" & mod_cat_7 == "<1") ~ "Inactive",
      TRUE ~ "Active"
    )
  )

df <- df %>% mutate(
  traj = case_when(
    pa_cat_5 == "Active" & pa_cat_6 == "Active" & pa_cat_7 == "Active" ~ "Consistently Active",

    pa_cat_5 == "Inactive" & pa_cat_6 == "Inactive" & pa_cat_7 == "Active" ~ "Increasingly Active",
    pa_cat_5 == "Inactive" & pa_cat_6 == "Active" & pa_cat_7 == "Active" ~ "Increasingly Active",

    pa_cat_5 == "Active" & pa_cat_6 == "Active" & pa_cat_7 == "Inactive" ~ "Decreasingly Active",
    pa_cat_5 == "Active" & pa_cat_6 == "Inactive" & pa_cat_7 == "Inactive" ~ "Decreasingly Active",

    pa_cat_5 == "Inactive" & pa_cat_6 == "Inactive" & pa_cat_7 == "Inactive" ~ "Consistently Inactive",

    pa_cat_5 == "Active" & pa_cat_6 == "Inactive" & pa_cat_7 == "Active" ~ "Variably Active",
    pa_cat_5 == "Inactive" & pa_cat_6 == "Active" & pa_cat_7 == "Inactive" ~ "Variably Active",

    TRUE ~ NA_character_
  )
) %>%
  mutate(traj = factor(traj,
                       levels = c("Consistently Inactive",
                                  "Decreasingly Active",
                                  "Variably Active",
                                  "Increasingly Active",
                                  "Consistently Active")))

df <- df %>%
  ungroup() %>%
  mutate(
    cog7 = r7tr20 + r7ser7,
    cog8 = r8tr20 + r8ser7,
    cog9 = r9tr20 + r9ser7,
    cog10 = r10tr20 + r10ser7
  )

classify_dementia <- function(cog_scores, edu_levels) {
  dementia <- rep(NA_real_, length(cog_scores))  # <-- Changed to NA_real_

  edu_unique <- unique(edu_levels[!is.na(edu_levels)])

  for (edu_level in edu_unique) {
    idx <- which(edu_levels == edu_level & !is.na(edu_levels))

    cog_subset <- cog_scores[idx]

    mean_cog <- mean(cog_subset, na.rm = TRUE)
    sd_cog <- sd(cog_subset, na.rm = TRUE)

    cutoff <- mean_cog - 1.5 * sd_cog

    for (i in idx) {
      if (!is.na(cog_scores[i])) {
        dementia[i] <- ifelse(cog_scores[i] < cutoff, 1, 0)
      }
    }
  }

  return(dementia)
}

df$dementia7 <- classify_dementia(df$cog7, df$H_edu)
df$dementia8 <- classify_dementia(df$cog8, df$H_edu)
df$dementia9 <- classify_dementia(df$cog9, df$H_edu)
df$dementia10 <- classify_dementia(df$cog10, df$H_edu)

selected_columns <- c("cog7")
df <- df %>% filter(!is.na(cog7))

df <- df %>% filter(dementia7 != 1)

####covariates####
df$gender <- ifelse(df$gender==1, "men", "women")
df$gender <- factor(df$gender)

df <- df %>%
  mutate(high_edu = case_when(
    H_edu == 1 ~ "less than upper secondary",
    H_edu == 2 ~ "upper secondary and vocat",
    H_edu == 3 ~ "tertiary",
    TRUE ~ NA_character_
  ))

 df$marital_status<- ifelse(df$marital_status==1|df$marital_status==3, "married/partnered", "other")

df$people_living_with <- as.numeric(as.character(df$people_living_with))
df$equivalized_wealth <- df$household_wealth/sqrt(df$people_living_with)
df$equivalized_wealth <- round(df$equivalized_wealth/1000, 2)
df <- df %>%
  mutate(wealth_quartile = ntile(equivalized_wealth, 4)) %>%
  mutate(wealth_quartile = factor(wealth_quartile,
                                  levels = 1:4,
                                  labels = c("1", "2", "3", "4")))

df$race <- ifelse(df$race == 1, "white", ifelse(df$race == 4, "non-white", df$race))

df$uk_born <- ifelse(df$rabplace == 1,"uk", ifelse(df$rabplace == 11, "outside-uk", df$rabplace))

#table(df$lb_status)
df$lb_status <- ifelse(!is.na(df$lb_status) & df$lb_status == 1, "fulltime_employed",
                       ifelse(!is.na(df$lb_status), "other", NA))

#table(df$drink_ever, exclude = NULL) #1=Yes, 0=none
df$drink_now <- ifelse(df$drink_frq == 0, 0, 1)

df$smoke_2002 <-ifelse(df$smoke_now == 1, "current smoker",
                       ifelse(df$smoke_now == 0 &
                                df$smoke_ever == 1, "past smoker", "non smoker"))
df$smoke_2002 <- factor(df$smoke_2002, levels = c("non smoker", "past smoker", "current smoker"))

df <- df %>%
  mutate(n_chronic = case_when(
    n_chronic <= 1 ~ "0 chronic",
    n_chronic >= 2 ~ "chronic",
    TRUE ~ NA_character_
  ))

df <- df %>%
  mutate(depression_w7 = ifelse(r7cesd >= 3, 1, 0))

df <- df %>%
  mutate(depression = case_when(
    r5depres == 1 ~ "1",
    r5depres >= 0 ~ "0",
    TRUE ~ NA_character_
  ))

df$BMI <- ifelse(df$r4mbmi >= 28, 1, 0)

 df_ungroup <- df %>% ungroup()

 missing_percent <- data.frame(
   variable = colnames(df_ungroup),
   missing_prop = sapply(df_ungroup, function(x) round(mean(is.na(x))*100, 2))
 ) %>%
   arrange(desc(missing_prop))

variables <- c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2002","n_chronic","race","uk_born","pa_cat_5","pa_cat_6","pa_cat_7","traj","r4mbmi","depression","depression_w7","dementia7","dementia8","dementia9","dementia10")
missing_summary <- sapply(df[variables], function(x) {
  percent_missing <- mean(is.na(x)) * 100
  return(round(percent_missing, 1))
})

missing_df <- data.frame(
  Variable = names(missing_summary),
  Missing_Percent = missing_summary
) %>%
  arrange(desc(Missing_Percent))

variables <- c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2002","n_chronic","uk_born","race","depression","depression_w7","BMI")
df_complete <- df[complete.cases(df[, variables]), ]

####imputation####
variables <- c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2002","n_chronic","uk_born","race","depression","depression_w7","BMI")

#covariates imputation
df_converted <- df[, c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2002","n_chronic","uk_born","race","depression","depression_w7","BMI")]
classes <- sapply(df_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_converted <- df_converted %>%
  mutate_if(names(.) %in% c("age","gender","high_edu","lb_status","marital_status","wealth_quartile","drink_now","smoke_2002","n_chronic","uk_born","race","depression","BMI"), as.factor)
set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]
