#Coded by: Brian Buh
#Started on: 10.01.2022
#Last Updated: 11.01.2022

library(tidyverse)
library(haven)
library(lubridate)

###########################################################################
# Creating long dataset all waves -----------------------------------------
###########################################################################

#All hhresp and indresp files waves 1-18 plus xwave from the BSPS must be loaded

# -------------------------------------------------------------------------
# Household datasets ------------------------------------------------------
# -------------------------------------------------------------------------

#Selecting variables and give them wave specific names
hhvar <- c("hid", "hidp",
           "fihhmngrs_dv", "fihhmb", "hhneti", "hhyneti", "hhnetde", "hhnetde2", "bhcinda", "loctax", #These are the income variables
           "hcost", "xphsg", "xphsn", "rent", "rentg_bh", "tenure_dv", "hsownd_bh", "hsroom", "hstype") #These are housing cost variables

w1hhvar <- paste0('ba_', hhvar)

w2hhvar <- paste0('bb_', hhvar)

w3hhvar <- paste0('bc_', hhvar)

w4hhvar <- paste0('bd_', hhvar)

w5hhvar <- paste0('be_', hhvar)

w6hhvar <- paste0('bf_', hhvar)

w7hhvar <- paste0('bg_', hhvar)

w8hhvar <- paste0('bh_', hhvar)

w9hhvar <- paste0('bi_', hhvar)

w10hhvar <- paste0('bj_', hhvar)

w11hhvar <- paste0('bk_', hhvar)

w12hhvar <- paste0('bl_', hhvar)

w13hhvar <- paste0('bm_', hhvar)

w14hhvar <- paste0('bn_', hhvar)

w15hhvar <- paste0('bo_', hhvar)

w16hhvar <- paste0('bp_', hhvar)

w17hhvar <- paste0('bq_', hhvar)

w18hhvar <- paste0('br_', hhvar)

#Preparing the variables for merging; extracting from each wave
ba_hh <- ba_hhresp %>% 
  dplyr::select(w1hhvar) %>% 
  rename_with(~ hhvar[which(w1hhvar == .x)], .cols = w1hhvar) %>% 
  mutate(wave = 1)

bb_hh <- bb_hhresp %>% 
  dplyr::select(w2hhvar) %>% 
  rename_with(~ hhvar[which(w2hhvar == .x)], .cols = w2hhvar) %>% 
  mutate(wave = 2)

bc_hh <- bc_hhresp %>% 
  dplyr::select(w3hhvar) %>% 
  rename_with(~ hhvar[which(w3hhvar == .x)], .cols = w3hhvar) %>% 
  mutate(wave = 3)

bd_hh <- bd_hhresp %>% 
  dplyr::select(w4hhvar) %>% 
  rename_with(~ hhvar[which(w4hhvar == .x)], .cols = w4hhvar) %>% 
  mutate(wave = 4)

be_hh <- be_hhresp %>% 
  dplyr::select(w5hhvar) %>% 
  rename_with(~ hhvar[which(w5hhvar == .x)], .cols = w5hhvar) %>% 
  mutate(wave = 5)

bf_hh <- bf_hhresp %>% 
  dplyr::select(w6hhvar) %>% 
  rename_with(~ hhvar[which(w6hhvar == .x)], .cols = w6hhvar) %>% 
  mutate(wave = 6)

bg_hh <- bg_hhresp %>% 
  dplyr::select(w7hhvar) %>% 
  rename_with(~ hhvar[which(w7hhvar == .x)], .cols = w7hhvar) %>% 
  mutate(wave = 7)

bh_hh <- bh_hhresp %>% 
  dplyr::select(w8hhvar) %>% 
  rename_with(~ hhvar[which(w8hhvar == .x)], .cols = w8hhvar) %>% 
  mutate(wave = 8)

bi_hh <- bi_hhresp %>% 
  dplyr::select(w9hhvar) %>% 
  rename_with(~ hhvar[which(w9hhvar == .x)], .cols = w9hhvar) %>% 
  mutate(wave = 9)

bj_hh <- bj_hhresp %>% 
  dplyr::select(w10hhvar) %>% 
  rename_with(~ hhvar[which(w10hhvar == .x)], .cols = w10hhvar) %>% 
  mutate(wave = 10)

bk_hh <- bk_hhresp %>% 
  dplyr::select(w11hhvar) %>% 
  rename_with(~ hhvar[which(w11hhvar == .x)], .cols = w11hhvar) %>% 
  mutate(wave = 11)

bl_hh <- bl_hhresp %>% 
  dplyr::select(w12hhvar) %>% 
  rename_with(~ hhvar[which(w12hhvar == .x)], .cols = w12hhvar) %>% 
  mutate(wave = 12)

bm_hh <- bm_hhresp %>% 
  dplyr::select(w13hhvar) %>% 
  rename_with(~ hhvar[which(w13hhvar == .x)], .cols = w13hhvar) %>% 
  mutate(wave = 13)

bn_hh <- bn_hhresp %>% 
  dplyr::select(w14hhvar) %>% 
  rename_with(~ hhvar[which(w14hhvar == .x)], .cols = w14hhvar) %>% 
  mutate(wave = 14)

bo_hh <- bo_hhresp %>% 
  dplyr::select(w15hhvar) %>% 
  rename_with(~ hhvar[which(w15hhvar == .x)], .cols = w15hhvar) %>% 
  mutate(wave = 15)

bp_hh <- bp_hhresp %>% 
  dplyr::select(w16hhvar) %>% 
  rename_with(~ hhvar[which(w16hhvar == .x)], .cols = w16hhvar) %>% 
  mutate(wave = 16)

bq_hh <- bq_hhresp %>% 
  dplyr::select(w17hhvar) %>% 
  rename_with(~ hhvar[which(w17hhvar == .x)], .cols = w17hhvar) %>% 
  mutate(wave = 17)

br_hh <- br_hhresp %>% 
  dplyr::select(w18hhvar) %>% 
  rename_with(~ hhvar[which(w18hhvar == .x)], .cols = w18hhvar) %>% 
  mutate(wave = 18)

#bind each wave specific dataframe together
hh_bhps <-
  bind_rows(ba_hh, bb_hh) %>%
  bind_rows(., bc_hh) %>%
  bind_rows(., bd_hh) %>%
  bind_rows(., be_hh) %>%
  bind_rows(., bf_hh) %>%
  bind_rows(., bg_hh) %>%
  bind_rows(., bh_hh) %>%
  bind_rows(., bi_hh) %>%
  bind_rows(., bj_hh) %>% 
  bind_rows(., bk_hh) %>%
  bind_rows(., bl_hh) %>%
  bind_rows(., bm_hh) %>%
  bind_rows(., bn_hh) %>%
  bind_rows(., bo_hh) %>%
  bind_rows(., bp_hh) %>%
  bind_rows(., bq_hh) %>%
  bind_rows(., br_hh) %>% 
  relocate("wave", .after = "hidp") %>%
  ungroup() #Note: arrange doesn't work here since it appears that each "hid" is unique to each wave

saveRDS(hh_bhps, file = "hh_bhps.rds")

# -------------------------------------------------------------------------
# Individual datasets -----------------------------------------------------
# -------------------------------------------------------------------------

ba_indresp %>% count(ba_age_dv)

#Selecting variables and give them wave specific names
indvar <- c("hid", "hidp", "hhorig", "sex", "birthm", "birthy", "age_dv", "istrtdatm", "istrtdaty", #basic individual variables
           "paynty", "paynti", #These are the income variables
           "xpchcf", "xpchc", "f135", #These are childcare cost variables
           "f139") #housing benefit
            #Missing are any of the variables about SES or Education

#Some variables are not included in wave 1: "birthm", "birthy", "istrtdaty"
indvar1 <- c("hid", "hidp", "hhorig", "sex", "age_dv", "istrtdatm", #basic individual variables
            "paynty", "paynti", #These are the income variables
            "xpchcf", "xpchc", "f135", #These are childcare cost variables
            "f139") #housing benefit
#Missing are any of the variables about SES or Education
           

w1indvar <- paste0('ba_', indvar1)

w2indvar <- paste0('bb_', indvar)

w3indvar <- paste0('bc_', indvar)

w4indvar <- paste0('bd_', indvar)

w5indvar <- paste0('be_', indvar)

w6indvar <- paste0('bf_', indvar)

w7indvar <- paste0('bg_', indvar)

w8indvar <- paste0('bh_', indvar)

w9indvar <- paste0('bi_', indvar)

w10indvar <- paste0('bj_', indvar)

w11indvar <- paste0('bk_', indvar)

w12indvar <- paste0('bl_', indvar)

w13indvar <- paste0('bm_', indvar)

w14indvar <- paste0('bn_', indvar)

w15indvar <- paste0('bo_', indvar)

w16indvar <- paste0('bp_', indvar)

w17indvar <- paste0('bq_', indvar)

w18indvar <- paste0('br_', indvar)

#Preparing the variables for merging; extracting from each wave
ba_ind <- ba_indresp %>% 
  dplyr::select("pid", w1indvar) %>% 
  rename_with(~ indvar[which(w1indvar == .x)], .cols = w1indvar) %>% 
  mutate(wave = 1) %>% 
  mutate(birthm = NA) %>% 
  mutate(birthy = NA) %>% 
  mutate(istrtdaty = NA)


bb_ind <- bb_indresp %>% 
  dplyr::select("pid", w2indvar) %>% 
  rename_with(~ indvar[which(w2indvar == .x)], .cols = w2indvar) %>% 
  mutate(wave = 2)

bc_ind <- bc_indresp %>% 
  dplyr::select("pid", w3indvar) %>% 
  rename_with(~ indvar[which(w3indvar == .x)], .cols = w3indvar) %>% 
  mutate(wave = 3)

bd_ind <- bd_indresp %>% 
  dplyr::select("pid", w4indvar) %>% 
  rename_with(~ indvar[which(w4indvar == .x)], .cols = w4indvar) %>% 
  mutate(wave = 4)

be_ind <- be_indresp %>% 
  dplyr::select("pid", w5indvar) %>% 
  rename_with(~ indvar[which(w5indvar == .x)], .cols = w5indvar) %>% 
  mutate(wave = 5)

bf_ind <- bf_indresp %>% 
  dplyr::select("pid", w6indvar) %>% 
  rename_with(~ indvar[which(w6indvar == .x)], .cols = w6indvar) %>% 
  mutate(wave = 6)

bg_ind <- bg_indresp %>% 
  dplyr::select("pid", w7indvar) %>% 
  rename_with(~ indvar[which(w7indvar == .x)], .cols = w7indvar) %>% 
  mutate(wave = 7)

bh_ind <- bh_indresp %>% 
  dplyr::select("pid", w8indvar) %>% 
  rename_with(~ indvar[which(w8indvar == .x)], .cols = w8indvar) %>% 
  mutate(wave = 8)

bi_ind <- bi_indresp %>% 
  dplyr::select("pid", w9indvar) %>% 
  rename_with(~ indvar[which(w9indvar == .x)], .cols = w9indvar) %>% 
  mutate(wave = 9)

bj_ind <- bj_indresp %>% 
  dplyr::select("pid", w10indvar) %>% 
  rename_with(~ indvar[which(w10indvar == .x)], .cols = w10indvar) %>% 
  mutate(wave = 10)

bk_ind <- bk_indresp %>% 
  dplyr::select("pid", w11indvar) %>% 
  rename_with(~ indvar[which(w11indvar == .x)], .cols = w11indvar) %>% 
  mutate(wave = 11)

bl_ind <- bl_indresp %>% 
  dplyr::select("pid", w12indvar) %>% 
  rename_with(~ indvar[which(w12indvar == .x)], .cols = w12indvar) %>% 
  mutate(wave = 12)

bm_ind <- bm_indresp %>% 
  dplyr::select("pid", w13indvar) %>% 
  rename_with(~ indvar[which(w13indvar == .x)], .cols = w13indvar) %>% 
  mutate(wave = 13)

bn_ind <- bn_indresp %>% 
  dplyr::select("pid", w14indvar) %>% 
  rename_with(~ indvar[which(w14indvar == .x)], .cols = w14indvar) %>% 
  mutate(wave = 14)

bo_ind <- bo_indresp %>% 
  dplyr::select("pid", w15indvar) %>% 
  rename_with(~ indvar[which(w15indvar == .x)], .cols = w15indvar) %>% 
  mutate(wave = 15)

bp_ind <- bp_indresp %>% 
  dplyr::select("pid", w16indvar) %>% 
  rename_with(~ indvar[which(w16indvar == .x)], .cols = w16indvar) %>% 
  mutate(wave = 16)

bq_ind <- bq_indresp %>% 
  dplyr::select("pid", w17indvar) %>% 
  rename_with(~ indvar[which(w17indvar == .x)], .cols = w17indvar) %>% 
  mutate(wave = 17)

br_ind <- br_indresp %>% 
  dplyr::select("pid", w18indvar) %>% 
  rename_with(~ indvar[which(w18indvar == .x)], .cols = w18indvar) %>% 
  mutate(wave = 18)

#bind each wave specific dataframe together
ind_bhps <-
  bind_rows(ba_ind, bb_ind) %>%
  bind_rows(., bc_ind) %>%
  bind_rows(., bd_ind) %>%
  bind_rows(., be_ind) %>%
  bind_rows(., bf_ind) %>%
  bind_rows(., bg_ind) %>%
  bind_rows(., bh_ind) %>%
  bind_rows(., bi_ind) %>%
  bind_rows(., bj_ind) %>% 
  bind_rows(., bk_ind) %>%
  bind_rows(., bl_ind) %>%
  bind_rows(., bm_ind) %>%
  bind_rows(., bn_ind) %>%
  bind_rows(., bo_ind) %>%
  bind_rows(., bp_ind) %>%
  bind_rows(., bq_ind) %>%
  bind_rows(., br_ind) %>% 
  relocate("wave", .after = "pid") %>%
  ungroup() %>%  #Note: arrange doesn't work here since it appears that each "hid" is unique to each wave
  arrange(pid, wave)

saveRDS(ind_bhps, file = "ind_bhps.rds")

# -------------------------------------------------------------------------
# xwave variables ---------------------------------------------------------
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Combined dataset --------------------------------------------------------
# -------------------------------------------------------------------------

indhhbhps <- left_join(ind_bhps, hh_bhps, by= c("hid", "hidp","wave")) %>% 
  arrange(pid, wave)

indhhbhps_test <- left_join(ind_bhps, hh_bhps, by= c("hidp", "wave"))
#The result is the same if you merge using "hid" or "hidp"


saveRDS(indhhbhps, file = "indhhbhps.rds")



