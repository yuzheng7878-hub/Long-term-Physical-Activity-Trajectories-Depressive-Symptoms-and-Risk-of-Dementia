# This script is intended for code sharing.
# Data import and output file paths are intentionally omitted.

library(dplyr)

df <- subset(data_r,select = c(
  # id household id
  hhid,mergeid,

  # Interview Status wave 1 - wave 9
  r1iwstat, r2iwstat, r3iwstat, r4iwstat, r5iwstat,
  r6iwstat, r7iwstat, r8iwstat,r9iwstat,

  # Wave Status w1-w9
  inw1,inw2,inw4,inw5,inw6,inw7,inw8,inw9,

  # person-level analysis weight
  r1wtresp,r2wtresp,r4wtresp,r5wtresp,r6wtresp,r7wtresp,r8wtresp,r9wtresp,
  ral12wtrsp,ral23wtrsp,ral34wtrsp,ral45wtrsp,ral56wtrsp,ral67wtrsp,ral78wtrsp,ral89wtrsp,ral19wtrsp,

  # interview dates year/month
  r1iwy,r2iwy,r4iwy,r5iwy,r6iwy,r7iwy,r8iwy,r9iwy,# interview year
  r1iwm,r2iwm,r4iwm,r5iwm,r6iwm,r7iwm,r8iwm,r9iwm,# interview month

  # demographics
  rabyear,radyear,r1agey,ragender,rabcountry,

  # socioeconomic
  h1atotb, #HHold total wealth
  hh1hhresp, #Number of Household Respondents
  r1lbrf_s,#labour force status
  r1mstath,# marital status
  raeducl,#education level
  ramomeducl,#mother harmonized education level
  radadeducl,#father harmonized education level
  h1rural,

  #exposure
  r1vgactx,r2vgactx,r4vgactx,r5vgactx,r6vgactx,r7vgactx,r8vgactx,
  r1mdactx,r2mdactx,r4mdactx,r5mdactx,r6mdactx,r7mdactx,r8mdactx,

  #health related
  r1smokev,r1smoken,r1drinkxw,

  # n chronic
  r1arthre, #w1 r ever had arthritis
  r1cancre, #w1 r ever had cancer
  r1hibpe, #w1 r Ever had high blood pressure
  r1diabe, #w1 r ever had diabetes
  r1lunge, #w1 r ever had lung disease
  r1hearte, #w1 r ever had heart problem
  r1stroke, #w1 r ever had stroke
  r1bmi,#body mass
  country,#country

  # Outcome
  r4tr20,r5tr20,r6tr20,r7tr20,r8tr20,r9tr20,
  r4ser7,r5ser7,r6ser7,r7ser7,r8ser7,r9ser7,

  # Depression
  r4eurod,r5eurod,r6eurod,r7eurod,r8eurod,r1depress
))
names(df) <- c(
  # id household id
  "hhid","hhidpn",

  # Interview Status wave 1 - wave 9
  "r1iwstat","r2iwstat","r3iwstat","r4iwstat","r5iwstat","r6iwstat","r7iwstat","r8iwstat","r9iwstat",

  # Wave Status w1-w9
  "inw1","inw2","inw4","inw5","inw6","inw7","inw8","inw9",

  # person-level analysis weight
  "weight1","weight2","weight4","weight5","weight6","weight7","weight8","weight9",
  "weight1_l","weight2_l","weight3_l","weight4_l","weight5_l","weight6_l","weight7_l","weight8_l","weight9_l",

  # interview dates year/month
  "r1iwy","r2iwy","r4iwy","r5iwy","r6iwy","r7iwy","r8iwy","r9iwy",
  "r1iwm","r2iwm","r4iwm","r5iwm","r6iwm","r7iwm","r8iwm","r9iwm",

  # demographics
  "birth_year","death_date","age","gender","born_in_country",

  # wealth
  "household_wealth", "people_living_with",

  # labour force status
  "lb_status",

  # marital
  "marital_status",

  # education
  "H_edu", "H_edu_m","H_edu_f","urbanicity",

  # physical activity
  "vigo_act1","vigo_act2","vigo_act4","vigo_act5","vigo_act6","vigo_act7","vigo_act8",
  "mode_act1","mode_act2","mode_act4","mode_act5","mode_act6","mode_act7","mode_act8",

   # smoking
  "smoke_ever","smoke_now","drink_ever",

  # chonic
  "r1arthre","r1cancre","r1hibpe","r1diabe","r1lunge","r1hearte","r1stroke",

  #BMI
  "BMI","country",

  #outcome
  "r4tr20","r5tr20","r6tr20","r7tr20","r8tr20","r9tr20",
  "r4ser7","r5ser7","r6ser7","r7ser7","r8ser7","r9ser7",

  #mediator depression
  "r4eurod","r5eurod","r6eurod","r7eurod","r8eurod","r1depress")

#interview time
df$r1iwindm <- df$r1iwy+df$r1iwm/12
df$r2iwindm <- df$r2iwy+df$r2iwm/12
df$r4iwindm <- df$r4iwy+df$r4iwm/12
df$r5iwindm <- df$r5iwy+df$r5iwm/12
df$r6iwindm <- df$r6iwy+df$r6iwm/12
df$r7iwindm <- df$r7iwy+df$r7iwm/12
df$r8iwindm <- df$r8iwy+df$r8iwm/12
