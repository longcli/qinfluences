# 2018-01-12
# Analysis of potential influences on ITBQ
# Cliff Long
# Janis Vollmer


# LOAD PACKAGES ###############################################################

library(here)
library(tidyverse)
library(janitor)
library(readxl)
library(ISOweek)
library(lubridate)
library(stringr)
library(data.table)  # long to 'wide' with more than one value variable


# FUNCTIONS ###################################################################



# LOAD DATA ###################################################################


# add facility info -----------------------------------------------------------

d_facility0 <- read_excel(path = here::here('data/facility.xlsx'), sheet = 'Sheet1')

d_facility <- d_facility0 %>%
  clean_names() %>%
  mutate(fac_num = str_pad(number, width = 3, side = 'left', pad = '0')) %>%
  filter(business_unit != 'Medco', active == 'Y')


# adding a comment


# DATA SPINE ==================================================================

# create dataframe with all weeks in 2017 by all facilities

d_fw <- d_facility %>% 
  select(business_unit, fac_num) %>% 
  arrange(business_unit, fac_num)


dim(d_fw)
with(d_fw, table(business_unit))


df_days <- data.frame(daysinyear = seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by = 1))

df_days <- df_days %>% 
  mutate(yearwk = ISOweek(daysinyear), 
         ISOyear = isoyear(daysinyear)) %>%
  filter(ISOyear == '2017')


# expand.grid(c("a", "b", "c"),  c("e", "f"))

df_spine_facbyweek <- expand.grid(yearwk = unique(df_days$yearwk),  
                                  fac_num = unique(d_facility$fac_num), 
                                  stringsAsFactors = FALSE)


# get just Common Platform facilities

df_spine_facbyweek_CP <- df_spine_facbyweek %>% 
  left_join(d_facility[, c('fac_num', 'business_unit')], by = 'fac_num') %>% 
  filter(business_unit %in% c("Business Essentials", "Facility Essentials", "Combined (BE/FE)"))


unique(d_facility$business_unit)
# 59 facilities
# 52 ISO weeks
# expect 3068 rows

with(df_spine_facbyweek, table(yearwk, fac_num))

dim(df_spine_facbyweek)
dim(df_spine_facbyweek_CP)




# load itbq data --------------------------------------------------------------

# p1 <- here::here('weekly_itbq.csv')

d_itbq <- read.csv(here::here('data/weekly_itbq.csv'), header = TRUE, stringsAsFactors = FALSE)

d_itbq <- clean_names(d_itbq)

glimpse(d_itbq)

d_itbq <- d_itbq %>% 
  mutate(yearwk = ISOweek(as.Date(date, format = "%Y-%m-%d")), 
         fac_num = str_pad(facility_number, side = 'left', width = 3, pad = 0))



# load upc scan data ----------------------------------------------------------

d_upc <- read.csv(here::here('data/weekly_upc_out.csv'), header = TRUE, stringsAsFactors = FALSE)

d_upc <- clean_names(d_upc)

glimpse(d_upc)

d_upc <- d_upc %>% mutate(yearwk = ISOweek(as.Date(day_date, format = "%m/%d/%Y")))

d_upc_week <- d_upc %>% 
  mutate(fac_num = str_pad(fac_num, side = 'left', width = 3, pad = 0),
         iso_year = substr(yearwk, start = 1, stop = 4)) %>% 
  group_by(yearwk, iso_year, zone, fac_num) %>% 
  # tally by strata
  summarize(upc_picks_sum = sum(upc_picks),
            related_sum = sum(total_upc_related_picks),
            prop_upc = upc_picks_sum / related_sum) %>%
  #drop sum columns before spread
  select(-c(upc_picks_sum, related_sum)) %>% 
  #spread zones
  spread(zone, prop_upc) %>% 
  clean_names()


glimpse(d_upc_week)
names(d_upc_week)



# SPTQ 2018.xlsx ##############################################################

# trans mode data =============================================================

# load SPC data by week -------------------------------------------------------
fname1 <- 'data/SPTQ 2018.xlsx'
fsheet1 <- 'SPC'

d_spc_week <- read_excel(here::here(fname1), fsheet1)
names(d_spc_week)

d_spc_week <- d_spc_week %>% 
  clean_names() %>% 
  mutate(yearwk = ISOweek(eff_dt))

glimpse(d_spc_week)



# load LTL data by week -------------------------------------------------------
fsheet2 <- 'LTL'

d_ltl_week <- read_excel(here::here(fname1), fsheet2)
names(d_spc_week)

d_ltl_week <- d_ltl_week %>% 
  clean_names() %>% 
  mutate(yearwk = ISOweek(eff_dt))

glimpse(d_ltl_week)



# load ADOT data by week ------------------------------------------------------
fsheet3 <- 'Adot'

d_adot_week <- read_excel(here::here(fname1), fsheet3)
names(d_adot_week)

d_adot_week <- d_adot_week %>% 
  clean_names() %>% 
  mutate(yearwk = ISOweek(eff_dt))

glimpse(d_adot_week)



# load TOTAL data by week -----------------------------------------------------
fsheet4 <- 'Total'

d_total_week <- read_excel(here::here(fname1), fsheet4)
names(d_spc_week)

d_total_week <- d_total_week %>% 
  clean_names() %>%
  mutate(yearwk = ISOweek(eff_dt))

glimpse(d_total_week)



# take a look -----------------------------------------------------------------

names(d_spc_week)
names(d_ltl_week)
names(d_adot_week)
names(d_total_week)


dim(d_spc_week)
dim(d_ltl_week)
dim(d_adot_week)
dim(d_total_week)



# rename prior to merge ----------

names(d_spc_week) <- c("eff_dt", "fac_num", "shp_ln_ct_spc", "shp_ctn_ct_spc", "yearwk")
names(d_ltl_week) <- c("eff_dt", "fac_num", "shp_ln_ct_ltl", "shp_ctn_ct_ltl", "yearwk")
names(d_adot_week) <- c("eff_dt", "fac_num", "shp_ln_ct_adot", "shp_ctn_ct_adot", "yearwk")
names(d_total_week) <- c("eff_dt", "fac_num", "shp_ln_ct_total", "shp_sal_amt_total", "shp_ctn_ct_total", "yearwk")

names(df_spine_facbyweek)



# join using dplyr 'full_join' ------------------------------------------------


d_join_carrcls <- df_spine_facbyweek_CP %>% 
  left_join(d_total_week, by = c('yearwk', 'fac_num'))  %>%
  left_join(d_adot_week, by = c('yearwk', 'fac_num')) %>%
  left_join(d_ltl_week, by = c('yearwk', 'fac_num')) %>%
  left_join(d_spc_week, by = c('yearwk', 'fac_num'))


dim(df_spine_facbyweek_CP)
dim(d_join_carrcls)

with(d_join_carrcls, table(fac_num, yearwk))

names(d_join_carrcls)



colnames(d_join_carrcls)[colnames(d_join_carrcls) == "eff_dt.x"] <- "eff_dt"


# remove columns eff_dt.y, eff_dt.x.x, eff_dt.y.y
dropcols <- c("eff_dt.y", "eff_dt.x.x", "eff_dt.y.y")

d_join_carrcls <- d_join_carrcls[ , !(names(d_join_carrcls) %in% dropcols)]



# add proportions of ship lines by carrier class ------------------------------

d_prop_carrcls <- d_join_carrcls %>% 
  group_by(yearwk, fac_num) %>% 
  mutate(prop_shp_ln_totcalc = sum(shp_ln_ct_adot + shp_ln_ct_ltl + shp_ln_ct_spc) / shp_ln_ct_total, 
         prop_shp_ln_adot = shp_ln_ct_adot / shp_ln_ct_total,
         prop_shp_ln_ltl = shp_ln_ct_ltl / shp_ln_ct_total,
         prop_shp_ln_spc = shp_ln_ct_spc / shp_ln_ct_total,
         # now the same for ctn
         prop_shp_ctn_totcalc = sum(shp_ctn_ct_adot + shp_ctn_ct_ltl + shp_ctn_ct_spc) / shp_ctn_ct_total, 
         prop_shp_ctn_adot = shp_ctn_ct_adot / shp_ctn_ct_total,
         prop_shp_ctn_ltl = shp_ln_ct_ltl / shp_ctn_ct_total,
         prop_shp_ctn_spc = shp_ln_ct_spc / shp_ctn_ct_total) %>%
  #drop unnecessary columns
  select(c("fac_num", "yearwk", "prop_shp_ln_totcalc", "prop_shp_ln_adot", 
           "prop_shp_ln_ltl", "prop_shp_ln_spc", "prop_shp_ctn_totcalc", 
           "prop_shp_ctn_adot", "prop_shp_ctn_ltl", "prop_shp_ctn_spc"))


names(d_prop_carrcls)
dim(d_prop_carrcls)
dim(d_join_carrcls)
dim(df_spine_facbyweek_CP)


# merge with itbq -------------------------------------------------------------

glimpse(d_itbq)

names(d_itbq)

with(d_itbq, table(business_unit_name, fac_num))


# prune the columns ----------

# filter(business_unit %in% c("Business Essentials", "Facility Essentials", "Combined (BE/FE)"))

d_prop_itbq_CP <- d_itbq %>% 
  filter(business_unit_name %in% c("Business Essentials", "Facility Essentials", "Combined (BE/FE)")) %>% 
  select(c("business_unit_name", "fac_num", "facility_abbreviation", "region", 
           "iso_year", "iso_week", "yearwk", "fillable_lines", 
           "itbprop", "whseprop", "dmgprop", "mmprop"))


names(d_prop_itbq_CP)
names(d_prop_carrcls)

dim(d_prop_itbq_CP)
dim(d_prop_carrcls)



# join -----------------------

# make sure that date scope matches
range(d_prop_carrcls$yearwk)
range(d_prop_itbq_CP$yearwk)

# > range(d_prop_carrcls$yearwk)
# [1] "2017-W01" "2017-W52"
# > range(d_prop_itbq$yearwk)
# [1] "2014-W01" "2017-W52"

glimpse(d_prop_itbq)

# limit itbq data to 2017 ISO weeks
d_prop_itbq_CP <- d_prop_itbq_CP %>% filter(iso_year == 2017)


d_join_itbq_carrcls <- d_prop_carrcls %>% 
  left_join(d_prop_itbq_CP, by = c('yearwk', 'fac_num')) 


# REMOVE ORS NASCO


dim(df_spine_facbyweek_CP)
dim(d_prop_carrcls)
dim(d_prop_itbq)
dim(d_join_itbq_carrcls)


names(d_join_itbq_carrcls)



# merge with upc scan outbound ------------------------------------------------

glimpse(d_upc_week)

names(d_upc_week)


# make sure that date scope matches
range(d_prop_carrcls$yearwk)
range(d_upc_week$yearwk)

# > range(d_prop_carrcls$yearwk)
# [1] "2017-W01" "2017-W52"
# > range(d_upc_week$yearwk)
# [1] "2014-W01" "2017-W52"

glimpse(d_upc_week)

# limit itbq data to 2017 ISO weeks
d_upc_week <- d_upc_week %>% filter(iso_year == 2017)


d_everything <- d_join_itbq_carrcls %>% 
  left_join(d_upc_week, by = c('yearwk', 'fac_num')) %>% 
  select(-c(iso_year.y)) %>% 
  filter(fac_num != '030')


names(d_everything)

dim(d_everything)
dim(df_spine_facbyweek_CP[df_spine_facbyweek_CP$fac_num != '030',])



# NOT WORKING !!
#d_everything <- rename(iso_year = iso_year.x) 

colnames(d_everything)[colnames(d_everything) == "iso_year.x"] <- "iso_year"
colnames(d_everything)[colnames(d_everything) == "bulk"] <- "upc_bulk"
colnames(d_everything)[colnames(d_everything) == "shelf"] <- "upc_shelf"
colnames(d_everything)[colnames(d_everything) == "business_unit_name"] <- "bu"


glimpse(d_everything)



# add item category info ------------------------------------------------------
# from SPTQ 2018.xlsx

# category 1 ----------

d_cat1 <- read_excel(path = here::here('data/SPTQ 2018.xlsx'), sheet = 'Cat1')

d_cat1 <- d_cat1 %>% 
  clean_names() %>% 
  mutate(yearwk = ISOweek(eff_dt)) %>%
  arrange(fil_fac_num, yearwk, ctgry_1_dsc)


# create proportion for each category within fac and yearwk
# https://stackoverflow.com/questions/29549731/finding-percentage-in-a-sub-group-using-group-by-and-summarise

d_cat1 <- d_cat1 %>% 
  group_by(fil_fac_num, yearwk) %>%
  # creates a total for all cats within each fac, yearwk
  mutate(shp_ln_TOT = sum(shp_ln_ct),
         shp_ctn_TOT = sum(shp_ctn_ct)) %>%
  group_by(ctgry_1_dsc, add = TRUE) %>%
  mutate(prop_ln = shp_ln_ct/shp_ln_TOT,
         prop_ctn = shp_ctn_ct/shp_ctn_TOT) %>%
  select(c(yearwk, fil_fac_num, ctgry_1_dsc, prop_ln, prop_ctn))


# spread after category with data.table

# https://stackoverflow.com/questions/30592094/r-spreading-multiple-columns-with-tidyr
# library(data.table)

d_cat1w <- dcast(setDT(d_cat1), fil_fac_num + yearwk ~ ctgry_1_dsc, value.var = c("prop_ln", "prop_ctn"))



# modify column names with 'prop' with prefix 'cat1'

d_cat1w <- d_cat1w %>% 
  clean_names() %>% 
  setNames(paste0('cat1_', names(.)))


colnames(d_cat1w)[colnames(d_cat1w) == "cat1_fil_fac_num"] <- "fac_num"
colnames(d_cat1w)[colnames(d_cat1w) == "cat1_yearwk"] <- "yearwk"

names(d_cat1w)

glimpse(d_cat1w)

dim(d_cat1w)
dim(df_spine_facbyweek_CP)


# category 2 ----------

d_cat2 <- read_excel(path = here::here('data/SPTQ 2018.xlsx'), sheet = 'Cat2')

d_cat2 <- d_cat2 %>% 
  clean_names() %>% 
  mutate(yearwk = ISOweek(eff_dt)) %>%
  arrange(fil_fac_num, yearwk, ctgry_2_dsc)

# create proportion for each category within fac and yearwk
  
d_cat2 <- d_cat2 %>% 
  group_by(fil_fac_num, yearwk) %>%
  # creates a total for all cats within each fac, yearwk
  mutate(shp_ln_TOT = sum(shp_ln_ct),
         shp_ctn_TOT = sum(shp_ctn_ct)) %>%
  group_by(ctgry_2_dsc, add = TRUE) %>%
  mutate(prop_ln = shp_ln_ct/shp_ln_TOT,
         prop_ctn = shp_ctn_ct/shp_ctn_TOT) %>%
  select(c(yearwk, fil_fac_num, ctgry_2_dsc, prop_ln, prop_ctn))  

  
# spread after category with data.table
  
d_cat2w <- dcast(setDT(d_cat2), fil_fac_num + yearwk ~ ctgry_2_dsc, value.var = c("prop_ln", "prop_ctn"))

  
  
# modify column names with 'prop' with prefix 'cat2'

d_cat2w <- d_cat2w %>% 
  clean_names() %>% 
  setNames(paste0('cat2_', names(.)))


colnames(d_cat2w)[colnames(d_cat2w) == "cat2_fil_fac_num"] <- "fac_num"
colnames(d_cat2w)[colnames(d_cat2w) == "cat2_yearwk"] <- "yearwk"

names(d_cat2w)

glimpse(d_cat2w)


dim(d_cat1w)
dim(d_cat2w)
dim(df_spine_facbyweek_CP)


  
# category 3 ----------  
  
d_cat3 <- read_excel(path = here::here('data/SPTQ 2018.xlsx'), sheet = 'Cat3')

d_cat3 <- d_cat3 %>% 
  clean_names() %>% 
  mutate(yearwk = ISOweek(eff_dt)) %>% 
  arrange(fil_fac_num, yearwk, ctgry_3_dsc)

# create proportion for each category within fac and yearwk

d_cat3 <- d_cat3 %>% 
  group_by(fil_fac_num, yearwk) %>%
  # creates a total for all cats within each fac, yearwk
  mutate(shp_ln_TOT = sum(shp_ln_ct),
         shp_ctn_TOT = sum(shp_ctn_ct)) %>%
  group_by(ctgry_3_dsc, add = TRUE) %>%
  mutate(prop_ln = shp_ln_ct/shp_ln_TOT,
         prop_ctn = shp_ctn_ct/shp_ctn_TOT) %>%
  select(c(yearwk, fil_fac_num, ctgry_3_dsc, prop_ln, prop_ctn))  


# spread after category with data.table

d_cat3w <- dcast(setDT(d_cat3), fil_fac_num + yearwk ~ ctgry_3_dsc, value.var = c("prop_ln", "prop_ctn"))


# modify column names with 'prop' with prefix 'cat3'

d_cat3w <- d_cat3w %>% 
  clean_names() %>% 
  setNames(paste0('cat3_', names(.)))


colnames(d_cat3w)[colnames(d_cat3w) == "cat3_fil_fac_num"] <- "fac_num"
colnames(d_cat3w)[colnames(d_cat3w) == "cat3_yearwk"] <- "yearwk"

names(d_cat3w)

glimpse(d_cat3w)

dim(d_cat3w)
dim(df_spine_facbyweek_CP)



# join CAT tables to d_everything ---------------------------------------------

glimpse(d_everything)


# earlier - removed fac_num 030 for now


d_everything1 <- d_everything %>% 
  left_join(d_cat1w, by = c('yearwk', 'fac_num')) %>% 
  left_join(d_cat2w, by = c('yearwk', 'fac_num')) %>% 
  left_join(d_cat3w, by = c('yearwk', 'fac_num')) 


glimpse(d_everything1)


dim(df_spine_facbyweek_CP[df_spine_facbyweek_CP$fac_num != '030', ])
dim(d_everything)
dim(d_everything1)


  
# join to d_everything1 -------------------------------------------------------

d_everything2 <- d_everything1 %>%
  left_join(d_facility, by = 'fac_num') %>%
  select(-c(region.x, bu), region = region.y, bunit = business_unit) %>%
  select(-c(number, abbreviation))


dim(d_everything2)


# add prefixes to vars
# "x_" for explanatory
# "y_" for response related to ITBQ metrics


# minor adjustment
colnames(d_everything2)[colnames(d_everything2) == "facility_abbreviation"] <- "fac_abbr"
colnames(d_everything2)[colnames(d_everything2) == "name"] <- "fac_name"



# load label reprint data #####################################################

# load raw data ---------------------------------------------------------------

d_reprint0 <- read_excel(path = here::here('data/labelreprint_2017.xlsx'), sheet = 'data')

d_reprint <- d_reprint0 %>% 
  clean_names() %>%
  mutate(yearwk = ISOweek(mod_dt_tm),
         pos1 = 1 + regexpr(",", key_data),
         pos2 = nchar(key_data),
         reason = substr(key_data, pos1, pos2))


# aggregate counts by week and facility ---------------------------------------

d_reprintagg <- d_reprint %>%
  group_by(yearwk, fac_num) %>% 
  summarize(ctreprints = n())


# get total carton counts by fac and week and combine

names(d_total_week)

tmp_ctnct <- d_total_week %>% select('fil_fac_num', 'yearwk', 'shp_ctn_ct_total')

colnames(tmp_ctnct)[colnames(tmp_ctnct) == "fil_fac_num"] <- "fac_num"

glimpse(tmp_ctnct)

with(d_total_week, table(fil_fac_num, yearwk))



d_reprintagg1 <- df_spine_facbyweek_CP %>% 
  filter(fac_num != '030') %>% 
  left_join(d_reprintagg, by = 'yearwk')


with(d_reprintagg1, table(yearwk, fac_num))


names(tmp_ctnct) 
names(d_reprintagg1)


d_reprintagg2 <- d_reprintagg1 %>% left_join(tmp_ctnct, by = c('fac_num', 'yearwk'))



# load Inventory Accuracy #####################################################

d_invacc0 <- read_excel(path = here::here('data/inventory_accuracy_by_week.xlsx'), sheet = 'Sheet1')

d_invacc <- d_invacc0 %>% 
  clean_names() %>% 
  mutate(fac_num = str_pad(fac_num, width = 3, side = 'left', pad = "0")) %>% 
  mutate(yearwk = ISOweek(as.Date(paste(yearwk, 1),"%Y-%U %u"))) %>% 
  arrange(fac_num, yearwk, counts,counts_off_nvl, rc_var_0_nvl, inv_accuracy) %>% 
  select(fac_num, yearwk, inv_accuracy)



names(d_invacc)


# merge with d_invacc ---------------------------------------------------------

d_everything3 <- d_everything2 %>% 
  left_join(d_invacc, by = c('yearwk', 'fac_num')) 


dim(d_everything3)
dim(d_everything2)
dim(d_invacc)
dim(df_spine_facbyweek_CP)

names(d_everything3)

sum(is.na(d_everything3$inv_accuracy))

with(d_everything3, cor(x = inv_accuracy, y = mmprop, use = "pairwise.complete.obs"))



with(d_everything3, pairs(inv_accuracy ~ mmprop, na.action = "na.omit"))





# final prep before store and analyze #########################################

d_everything_z <- d_everything3


# classify columns
vars_f <- c("fac_num", "fac_name", "fac_abbr", "bunit", "region", "active", "common_platform", "pinnacle_group")
vars_y <- c("itbprop", "dmgprop", "whseprop", "mmprop")
vars_x <- names(d_everything_z)[!( (names(d_everything_z) %in% vars_f) | (names(d_everything_z) %in% vars_y) )] 


# add column prefixes
names(d_everything_z)[names(d_everything_z) %in% vars_f] = paste0("f_", names(d_everything_z)[names(d_everything_z) %in% vars_f])
names(d_everything_z)[names(d_everything_z) %in% vars_y] = paste0("y_", names(d_everything_z)[names(d_everything_z) %in% vars_y])
names(d_everything_z)[names(d_everything_z) %in% vars_x] = paste0("x_", names(d_everything_z)[names(d_everything_z) %in% vars_x])



# check the results
dim(df_spine_facbyweek_CP[df_spine_facbyweek_CP$fac_num != '030', ])
dim(d_everything)
dim(d_everything1)
dim(d_everything2)
dim(d_everything3)
dim(d_everything_z)


glimpse(d_everything_z)
names(d_everything_z)






# SAVE AND LOAD ###############################################################

# save as .rda
save(d_everything_z, file = 'itbq_integrated_data.rda')


# save as .rds so can load locally if need to restart R
saveRDS(d_everything_z, file = 'itbq_integrated_data.rds')


# load back into environment
# d_getcopy <- readRDS(file = 'joined_data.rds')
# d_everything2 <- d_getcopy



# END DATA PREP ###############################################################

# NEXT - GO TO DATA EXPLORE ###################################################

# END CODE ####################################################################