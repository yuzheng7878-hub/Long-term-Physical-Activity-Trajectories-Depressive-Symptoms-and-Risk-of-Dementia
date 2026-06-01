library(dplyr)

# Data extraction

#####data extraction####

df <- subset(data,select = c(idauniqc,r1iwstat, r2iwstat, r3iwstat, r4iwstat, r5iwstat,
                             r6iwstat, r7iwstat, r8iwstat, r9iwstat,r10iwstat,
                             r5lwtresp,r6lwtresp,r7lwtresp,r8lwtresp,r9lwtresp,r10lwtresp,
                             r5iwm,r6iwm,r7iwm,r8iwm,r9iwm,r10iwm, #individual interview month
                             r5iwy,r6iwy,r7iwy,r8iwy,r9iwy,r10iwy, #individual interview year
                             rabyear,r5agey,ragender,raracem,rabplace,
                             h5atotb, #HHold total wealth
                             hh5hhresp, #respondents in household
                             r5lbrf_e,#labour force status
                             r5mstath,raeducl,ramomeduage,radadeduage,rabcountry,
                             #exposure
                             r5vgactx_e,r5mdactx_e,r5ltactx_e,
                             r6vgactx_e,r6mdactx_e,r6ltactx_e,
                             r7vgactx_e,r7mdactx_e,r7ltactx_e,
                             r5smokev,r5smoken,
                             r5drinkwn_e,
                             r5arthre, #w5 r ever had arthritis
                             r5cancre, #w5 r ever had cancer
                             r5hibpe, #w5 r Ever had high blood pressure
                             r5diabe, #w5 r ever had diabetes
                             r5lunge, #w5 r ever had lung disease
                             r5hearte, #w5 r ever had heart problem
                             r5stroke, #w5 r ever had stroke
                             r5psyche, #w5 r ever had psych problem
                             r5cesd,r6cesd,r7cesd,r8cesd,r9cesd,r5depres,
                             r5sleepr,
                             r5socyr,
                             r7orient,r7tr20,r7ser7,
                             r8orient,r8tr20,r8ser7,
                             r9orient,r9tr20,r9ser7,
                             r10orient,r10tr20,r10ser7,
                             inw5,inw6,inw7,
                             r6mheight,r6mweight,
                             r4mheight,r4mweight,
                             r4mbmi,r6mbmi

))

names(df) <- c("hhidpn","r1iwstat","r2iwstat","r3iwstat","r4iwstat","r5iwstat","r6iwstat","r7iwstat","r8iwstat","r9iwstat","r10iwstat",
               "weight5","weight6","weight7","weight8","weight9","weight10",
               "r5iwindm","r6iwindm","r7iwindm","r8iwindm","r9iwindm","r10iwindm",
               "r5iwindy","r6iwindy","r7iwindy","r8iwindy","r9iwindy","r10iwindy",
               "birth_year","age", "gender", "race","rabplace", "household_wealth", "people_living_with",
               "lb_status","marital_status","H_edu", "H_edu_m","H_edu_f","urbanicity",
               "vigo_act5","mode_act5","light_act5","vigo_act6","mode_act6","light_act6","vigo_act7","mode_act7","light_act7",
               "smoke_ever","smoke_now","drink_frq","r5arthre","r5cancre","r5hibpe","r5diabe","r5lunge","r5hearte","r5stroke","r5psyche",
               "r5cesd","r6cesd","r7cesd","r8cesd","r9cesd","r5depres","sleep","sco_act",
               "orient7","r7tr20","r7ser7","orient8","r8tr20","r8ser7","orient9","r9tr20","r9ser7","r10orient","r10tr20","r10ser7",
               "inw5","inw6","inw7","height_m","weight_kg", "height_m4","weight_kg4","r4mbmi","r6mbmi")

df$n_chronic <- rowSums(df[, 55:62], na.rm = T)
