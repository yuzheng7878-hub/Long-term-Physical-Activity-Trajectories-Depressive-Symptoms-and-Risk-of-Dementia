# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)

subdat_r <- subset(data_r,
                   select = c(hhid, hhidpn, inw7, inw8, inw9, inw10,
                              r8iwstat, r9iwstat, r10iwstat, r11iwstat, r12iwstat, r13iwstat, r14iwstat, r15iwstat,
                              r7wtresp, r8wtresp, r9wtresp, r10wtresp, r11wtresp, r12wtresp, r13wtresp, r14wtresp, r15wtresp,
                              r7iwmid, r8iwmid, r9iwmid, r10iwmid, r11iwmid, r12iwmid, r13iwmid, r14iwmid, r15iwmid,
                              raddate, rabyear, r7agey_e, ragender, raracem, rahispan,
                              h7atotb, h7hhres, r7lbrf, r7mstath, r7conde, r7tr20,
                              r7smokev, r7smoken, r7drink, r7drinkd,
                              r7vgactx, r8vgactx, r9vgactx,
                              r7mdactx, r8mdactx, r9mdactx,
                              r7ltactx, r8ltactx, r9ltactx,
                              rabplace, r7depres, r7cesd, r8cesd, r9cesd,
                              r10cesd, r11cesd, r12cesd, r13cesd, r14cesd, r15cesd,
                              r7bmi))

subdat_h <- subset(data_h,
                   select = c(hhid, hhidpn,
                              r10orient, r11orient, r12orient, r13orient,
                              h7rural, r7mbmi, r7mheight, r7mweight,
                              raeducl, ramomeducl, radadeducl))

data01 <- left_join(subdat_r, subdat_h, by = c("hhid", "hhidpn"))

names(data01) <- c(
  "hhid", "hhidpn",
  "inw7", "inw8", "inw9", "inw10",
  "r8iwstat", "r9iwstat", "r10iwstat", "r11iwstat", "r12iwstat", "r13iwstat", "r14iwstat", "r15iwstat",
  "weight7", "weight8", "weight9", "weight10", "weight11", "weight12", "weight13", "weight14", "weight15",
  "r7iwmid", "r8iwmid", "r9iwmid", "r10iwmid", "r11iwmid", "r12iwmid", "r13iwmid", "r14iwmid", "r15iwmid",
  "death_date", "birth_year", "age", "gender", "race", "hispanic",
  "household_wealth", "people_living_with", "lb_status", "marital_status",
  "n_chronic", "word_recall", "smoke_ever", "smoke_now", "drink_ever", "drink_frq",
  "vigo_act7", "vigo_act8", "vigo_act9",
  "mode_act7", "mode_act8", "mode_act9",
  "light_act7", "light_act8", "light_act9",
  "us_born", "r7depres", "cesd2004", "cesd2006", "cesd2008", "cesd2010", "cesd2012",
  "cesd2014", "cesd2016", "cesd2018", "cesd2020", "BMI_r",
  "orient10", "orient11", "orient12", "orient13",
  "urbanicity", "BMI_h", "height", "weight", "H_edu", "H_edu_m", "H_edu_f")

base_date <- as.Date("1960-01-01")
for (w in 7:15) {
  date_var <- paste0("r", w, "iwmid_date")
  year_var <- paste0("r", w, "iwmid_year")
  month_var <- paste0("r", w, "iwmid_month")
  decimal_var <- paste0("riwmid_w", w - 7)
  source_var <- paste0("r", w, "iwmid")

  data01[[date_var]] <- base_date + data01[[source_var]]
  data01[[year_var]] <- as.numeric(format(data01[[date_var]], "%Y"))
  data01[[month_var]] <- as.numeric(format(data01[[date_var]], "%m"))
  data01[[decimal_var]] <- data01[[year_var]] + data01[[month_var]] / 12
}

subdat_score <- subset(data_r,
                       select = c(hhid, hhidpn,
                                  r9tr20, r10tr20, r11tr20, r12tr20, r13tr20, r14tr20p, r14tr20w, r15tr20p, r15tr20w,
                                  r9ser7, r10ser7, r11ser7, r12ser7, r13ser7, r14ser7p, r14ser7w, r15ser7p, r15ser7w,
                                  r10bwc20, r11bwc20, r12bwc20, r13bwc20, r14bwc20p,
                                  r10imrc, r11imrc, r12imrc, r13imrc, r14imrcp,
                                  r10dlrc, r11dlrc, r12dlrc, r13dlrc, r14dlrcp,
                                  r10cog27, r11cog27, r12cog27, r13cog27, r14cog27,
                                  raracem, rahispan,
                                  r10iwbeg, r11iwbeg, r12iwbeg, r13iwbeg, r14iwbeg,
                                  radyear, radmonth, raddate))

df_cognition$hhid <- sub("^0", "", df_cognition$hhid)
df_cognition$hhidpn <- paste(as.character(df_cognition$hhid), as.character(df_cognition$pn), sep = "")

data01$hhidpn <- as.numeric(data01$hhidpn)
df_cognition$hhidpn <- as.numeric(df_cognition$hhidpn)

df_dementia <- left_join(
  data01,
  df_cognition %>%
    select(hhidpn,
           starts_with("cogfunction"),
           starts_with("ser7_imp"),
           starts_with("imrc_imp"),
           starts_with("dlrc_imp")),
  by = "hhidpn")

df_dementia <- left_join(df_dementia, subdat_score, by = "hhidpn")
