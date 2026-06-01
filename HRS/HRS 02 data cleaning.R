# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)
library(mice)

df <- df_h[(df_h$inw7 == 1 & df_h$inw8 == 1 & df_h$inw9 == 1), ]
df <- df[which(df$age >= 50), ]
df <- subset(df,
             !(is.na(vigo_act7) | is.na(vigo_act8) | is.na(vigo_act9) |
               is.na(mode_act7) | is.na(mode_act8) | is.na(mode_act9)))

df <- df %>%
  mutate(
    vig_cat_7 = case_when(vigo_act7 %in% c(1, 2) ~ ">=2", vigo_act7 == 3 ~ "=1", vigo_act7 %in% c(4, 5) ~ "<1", TRUE ~ NA_character_),
    mod_cat_7 = case_when(mode_act7 %in% c(1, 2) ~ ">=2", mode_act7 == 3 ~ "=1", mode_act7 %in% c(4, 5) ~ "<1", TRUE ~ NA_character_),
    vig_cat_8 = case_when(vigo_act8 %in% c(1, 2) ~ ">=2", vigo_act8 == 3 ~ "=1", vigo_act8 %in% c(4, 5) ~ "<1", TRUE ~ NA_character_),
    mod_cat_8 = case_when(mode_act8 %in% c(1, 2) ~ ">=2", mode_act8 == 3 ~ "=1", mode_act8 %in% c(4, 5) ~ "<1", TRUE ~ NA_character_),
    vig_cat_9 = case_when(vigo_act9 %in% c(1, 2) ~ ">=2", vigo_act9 == 3 ~ "=1", vigo_act9 %in% c(4, 5) ~ "<1", TRUE ~ NA_character_),
    mod_cat_9 = case_when(mode_act9 %in% c(1, 2) ~ ">=2", mode_act9 == 3 ~ "=1", mode_act9 %in% c(4, 5) ~ "<1", TRUE ~ NA_character_)
  ) %>%
  mutate(
    pa_cat_7 = case_when((vig_cat_7 == "=1" & mod_cat_7 == "<1") | (vig_cat_7 == "<1" & mod_cat_7 == "=1") | (vig_cat_7 == "<1" & mod_cat_7 == "<1") ~ "Inactive", TRUE ~ "Active"),
    pa_cat_8 = case_when((vig_cat_8 == "=1" & mod_cat_8 == "<1") | (vig_cat_8 == "<1" & mod_cat_8 == "=1") | (vig_cat_8 == "<1" & mod_cat_8 == "<1") ~ "Inactive", TRUE ~ "Active"),
    pa_cat_9 = case_when((vig_cat_9 == "=1" & mod_cat_9 == "<1") | (vig_cat_9 == "<1" & mod_cat_9 == "=1") | (vig_cat_9 == "<1" & mod_cat_9 == "<1") ~ "Inactive", TRUE ~ "Active")
  )

df <- df %>%
  mutate(
    traj = case_when(
      pa_cat_7 == "Active" & pa_cat_8 == "Active" & pa_cat_9 == "Active" ~ "Consistently Active",
      pa_cat_7 == "Inactive" & pa_cat_8 == "Inactive" & pa_cat_9 == "Active" ~ "Increasingly Active",
      pa_cat_7 == "Inactive" & pa_cat_8 == "Active" & pa_cat_9 == "Active" ~ "Increasingly Active",
      pa_cat_7 == "Active" & pa_cat_8 == "Active" & pa_cat_9 == "Inactive" ~ "Decreasingly Active",
      pa_cat_7 == "Active" & pa_cat_8 == "Inactive" & pa_cat_9 == "Inactive" ~ "Decreasingly Active",
      pa_cat_7 == "Inactive" & pa_cat_8 == "Inactive" & pa_cat_9 == "Inactive" ~ "Consistently Inactive",
      pa_cat_7 == "Active" & pa_cat_8 == "Inactive" & pa_cat_9 == "Active" ~ "Variably Active",
      pa_cat_7 == "Inactive" & pa_cat_8 == "Active" & pa_cat_9 == "Inactive" ~ "Variably Active",
      TRUE ~ NA_character_
    ),
    traj = factor(traj,
                  levels = c("Consistently Inactive", "Decreasingly Active", "Variably Active", "Increasingly Active", "Consistently Active"))
  )

df <- df %>% filter(!is.na(cogfunction2008))

for (col in c("cogfunction2008", "cogfunction2010", "cogfunction2012", "cogfunction2014", "cogfunction2016", "cogfunction2018", "cogfunction2020")) {
  df[[paste0(col, "_dementia")]] <- ifelse(is.na(df[[col]]), NA, ifelse(df[[col]] == 3, 1, 0))
}

df <- df[df$cogfunction2008_dementia != 1, ]

df <- df %>%
  mutate(
    gender = factor(ifelse(gender == 1, "men", "women")),
    high_edu = case_when(H_edu == 1 ~ "less than upper secondary", H_edu == 2 ~ "upper secondary and vocat", H_edu == 3 ~ "tertiary", TRUE ~ NA_character_),
    marital_status = ifelse(marital_status %in% c(1, 3), "married/partnered", "other"),
    people_living_with = as.numeric(as.character(people_living_with)),
    equivalized_wealth = round(household_wealth / sqrt(people_living_with) / 1000, 2),
    wealth_quartile = factor(ntile(equivalized_wealth, 4), levels = 1:4, labels = c("1", "2", "3", "4")),
    Race = case_when(
      race == 1 & hispanic == 0 ~ "Non-Hispanic White",
      race == 2 & hispanic == 0 ~ "Non-Hispanic Black",
      hispanic == 1 ~ "Hispanic",
      TRUE ~ "Other"
    ),
    urbanicity = factor(case_when(urbanicity == 0 ~ "urban", urbanicity == 1 ~ "rural", TRUE ~ NA_character_)),
    lb_status = ifelse(!is.na(lb_status) & lb_status == 1, "fulltime_employed", ifelse(!is.na(lb_status), "other", NA)),
    BMI = ifelse(BMI_r >= 28, 1, 0),
    drink_now = ifelse(drink_frq == 0, 0, 1),
    smoke_2004 = factor(ifelse(smoke_now == 1, "current smoker", ifelse(smoke_now == 0 & smoke_ever == 1, "past smoker", "non smoker")),
                        levels = c("non smoker", "past smoker", "current smoker")),
    n_chronic = case_when(n_chronic <= 1 ~ "0 chronic", n_chronic >= 2 ~ "chronic", TRUE ~ NA_character_),
    us_born = ifelse(us_born == 11, 0, 1),
    depression_2008 = factor(ifelse(cesd2008 >= 3, 1, 0)),
    depression = factor(ifelse(r7depres == 0, 0, 1))
  )

variables <- c("age", "gender", "high_edu", "lb_status", "marital_status", "wealth_quartile",
               "drink_now", "smoke_2004", "n_chronic", "BMI", "urbanicity", "Race",
               "depression", "depression_2008", "us_born")

df_complete <- df[complete.cases(df[, variables]), ]

df_converted <- df[, variables] %>% mutate(across(everything(), as.factor))
set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m = 1, maxit = 5)
imputed_data <- complete(mice_mod)

common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]
