#Coded by: Brian Buh
#Started on: 10.01.2022
#Last Updated: 14.01.2022

library(tidyverse)
library(haven)
library(lubridate)

###########################################################################
# Data quality analysis ---------------------------------------------------
###########################################################################

# Use the DF "indhhbhps"

# -------------------------------------------------------------------------
# Question 1: Income data quality -----------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
#Testing income data from BHPS/UKHLS files versus UKDA3909

## First Weekly income "hhneti"
ba_hhinc <- ba_hhresp %>% select(ba_hid, ba_hhneti)
ba_hhinc2 <- ba_weekinc %>% 
  select(ahid, ahhneti) %>% 
  rename("ba_hid" = "ahid")
  # %>% rename("ba_hhneti" = "ahhneti")

weekinctest <- left_join(ba_hhinc, ba_hhinc2, by = "ba_hid") %>% 
  mutate(ba_hhneti = ifelse(ba_hhneti == -9, NA, ba_hhneti)) %>% 
  mutate(diff = ba_hhneti - ahhneti)
summary(weekinctest$diff) #There is no difference in the net weekly income measures, 685 NA (both sets)

## Second annual income "hhyneti
ba_hhinc3 <- ba_hhresp %>% select(ba_hid, ba_hhyneti)
ba_hhinc4 <- ba_yearinc %>% 
  select(ahid, ahhyneti) %>% 
  rename("ba_hid" = "ahid")

yearinctest <- left_join(ba_hhinc3, ba_hhinc4, by = "ba_hid") %>% 
  mutate(ba_hhyneti = ifelse(ba_hhyneti == -9, NA, ba_hhyneti)) %>% 
  mutate(diff = ba_hhyneti - ahhyneti)
summary(yearinctest$diff) #There are some non-0 numbers
yearinctest2 <- yearinctest %>% filter(diff != 0) #This amounts to 11 observations which would need to removed

yearinctest %>% count(is.na(ahhyneti)) #There are only 6 NA in the full data set, but 685 in the UKDA3909 data set

#In conclusion, it is appropriate to use the BHPS release for net income variables. Generally, it is wise to stick to the 
#current (weekly) measures as they includes local (council) tax which is a significant expense. The data managers of the BHPS
#use a different system to impute the annual income than Jenkins & Levy meaning less NA than the 3909 set, however, this leaves certain
#measures on shakier ground than using the weekly measures. 
#My childcare costs are also measured on a weekly level, while housing costs are measured at a monthly level.

# -------------------------------------------------------------------------
#I wanna compare all 18 waves of yearly data from BHPS and UKDA3909 as the annual info in much less in the latter
#I use the year3909 bound DF and the BHPS yearly data

year3909cut <- year3909 %>% 
  select(hid, wave, hhyneti)

#DF inccut created just below

annualcomp <- 
  left_join(inccut, year3909cut, by = c("hid", "wave")) %>% 
  rename("hhynetibhps" = "hhyneti.x") %>% 
  rename("hhyneti3909" = "hhyneti.y") %>% 
  mutate(annualdiff = hhynetibhps - hhyneti3909)

countdiff <- annualcomp %>% count(is.na(hhynetibhps), is.na(hhyneti3909))
#There are 32377 missing in the 3909 DF that are available in the BHPS data, an additional 5803 NA in both
summary(annualcomp$annualdiff)
annualoutlier <- annualcomp %>% filter(annualdiff != 0) 
    # %>% distinct(hid, .keep_all = TRUE)
#There are 193 observations that do not match however these are clustered in 103 HH
summary(annualoutlier$annualdiff)

annualoutlier %>% 
  ggplot(aes(x= annualdiff)) +
  geom_histogram()
#It is a relatively normal distribution around 0 although slightly more negative

# -------------------------------------------------------------------------
#I have included the following income variables:

## Household
# "fihhmngrs_dv" - gross household income: month before interview
# "fihhmb" - HH benefit income month before interview
# "hhneti" - household net income
# "hhyneti" - household annual net income
# "hhnetde" - household current net income equivalent (McClements)
# "hhnetde2" - household current net income equivalent (OECD)
# "bhcinda" - Average inflation over the reference period (Sept 1st - August 31st)

##Individual
# "paynty" - Usual monthly net pay: Sept this year
# "paynti" - Imputation flag

income <- indhhbhps %>% 
  select(pid, wave, hid, sex, age_dv, fihhmngrs_dv, fihhmb, hhneti, hhyneti, hhnetde, hhnetde2, bhcinda, paynty, paynti, loctax)

#For joining later
inccut <- income %>% select(pid, hid, wave, hhneti, hhyneti) %>% 
  mutate(hhneti = ifelse(hhneti == -9, NA, hhneti)) %>% 
  mutate(hhyneti = ifelse(hhyneti == -9, NA, hhyneti))

summary(income$age_dv)
summary(income$fihhmngrs_dv) #There are no NA, there are 5706 missing values
summary(income$fihhmb)
summary(income$hhneti) #There are no NA, there can be negative values, there are 38180 missing values
summary(income$hhyneti) #There are no NA, there are 5803 missing values
summary(income$hhnetde) #There are no NA
summary(income$hhnetde2) #There are no NA
summary(income$bhcinda) #There are no NA

summary(income$paynty) #There are no NA, 109109 missing values, missing values at -9, -8, or -7
income %>% filter(hhneti == -9 | hhneti == -7 | hhneti == -8) %>% count(hhneti) #There are no -7 or -8, only -9
  #There are 38180 NA

#

inc2 <- income %>% filter(fihhmngrs_dv == -9) #If this is missing, all income data for the observation is missing
inc3 <- income %>% filter(hhneti > 0) 
summary(inc3$hhneti)
inc3_test <- income %>% filter(hhneti == -9) #-9 is the value for a missing value
inc4 <- income %>% filter(hhyneti == -9) #Much less missing annual than monthly data, however if this is missing almost all other income data is missing (like fihhmngrs_dv)
inc5 <- income %>% filter(paynty <= -7) #Missing values can be -9, -8 or -7
inc5 <- income %>% filter(paynty < 0)

#Plot the distribution of weekly net hh income over time
inc3 %>%
  ggplot(aes(x = wave, y = hhneti)) +
  geom_point()


#"hhneti" should equal (hhyneti/365)*7 + loctax
weekyearcomp <- income %>% 
  select(pid, wave, age_dv, hhneti, hhyneti, loctax) %>% 
  mutate(hhneti = ifelse(hhneti == -9, NA, hhneti)) %>% 
  mutate(hhyneti = ifelse(hhyneti == -9, NA, hhyneti)) %>% 
  mutate(loctax = ifelse(loctax == -9, NA, loctax)) %>% 
  mutate(yeartoweek = (hhyneti/365)*7) %>% 
  mutate(diff = yeartoweek-hhneti) %>% 
  mutate(weekminusloctax = yeartoweek-loctax)
summary(weekyearcomp$diff)

weekyearcomp2 <- weekyearcomp %>% filter(!is.na(diff))
sd(weekyearcomp2$diff)


#Histogram of the differences between the self-calculated weekly net hh income and hhneti
weekyearcomp %>% 
  filter(diff > -151.6, diff < 123.3) %>% #This is only keeping values within 1 SD of the mean
  ggplot(aes(x=diff)) +
  geom_histogram()





# -------------------------------------------------------------------------
# Question 2: Houseing cost quality ---------------------------------------
# -------------------------------------------------------------------------

#I have included the following housing cost variables:

## Household
# "hcost" - gross housing costs
# "xphsg" - gross monthly housing costs 
# "xphsn" - net monthly housing costs (net of gov't subsidies: "net monthly mortgage or rest. For renters this include partial
                                    # of complete housing benefits. Variable is zero for housing that is rent free or owned outright')
# "rent" - Net amount of last rent payment
# "rentg_bh" - gross rent including housing benefit
# "tenure_dv" - housing tenure
# "hsownd_bh" - house owned or rented
# "hsroom" - number of rooms in accommodation
# "hstype" - type of accommodation

## Individual
# "f139" - Income: housing benefit


hc <- indhhbhps %>% 
  select(pid, wave, hid, sex, age_dv, hcost, xphsg, xphsn, rent, rentg_bh, tenure_dv, hsownd_bh, hsroom, hstype)

hc1 <- hc %>% 
  select(pid, wave, age_dv, sex, xphsn, loctax) %>% 
  mutate(xphsn = ifelse(xphsn == -9, NA, xphsn)) %>% 
  mutate(yearhc = (xphsn * 12)) %>% 
  mutate(dayhc = (yearhc/365)) %>% 
  mutate(weekhc = (dayhc*7)) %>% 
  left_join(., inccut, by = c("pid", "wave")) %>% 
  mutate(hcratio = weekhc/hhneti) %>% 
  mutate(hcprop = (hhneti-weekhc)/hhneti)
summary(hc1$hcratio)

#Plot the distribution of weekly housing costs over time
hc1 %>%
  ggplot(aes(x = wave, y = weekhc)) +
  geom_jitter()

#Plot the distribution of the ratio of weekly housing cost to weekly income
hc1 %>%
  ggplot(aes(x = wave, y = hcratio)) +
  geom_jitter()
#There is something going on with crazy ratios

hc2 <- hc1 %>% 
  filter(hcratio >= 1) #There are 1812 observations where the weekly housing costs is larger than the weekly income
hc3 <- hc1 %>% 
  filter(hcratio <= 1) %>% 
  filter(hcratio >= 0)
summary(hc3$hcratio)
hc3 %>%
  ggplot(aes(x = wave, y = hcratio)) +
  geom_jitter()





# -------------------------------------------------------------------------
# Question 3: Childcare cost quality --------------------------------------
# -------------------------------------------------------------------------


#Only Individual, no household
# "xpchcf"- childcare: free or paid for
  # Negative numbers are missing (116), inapplicable (209818), proxy (6115), refusal(1), or don't know(19)
  # 1 = childcare free
  # 2 = Childcare paid
  # 0 & 39 = not sure yet (9885)
# "xpchc" - weekly childcare costs
# "f135" - Income: child benefit
  # 35 = received benefit


cc <- indhhbhps %>% 
  select(pid, wave, hid, sex, age_dv, xpchcf, xpchc, f135)

#How many observations are from inapplicable respondents
cc1 <- cc %>% filter(xpchcf >= 0) # 22927 observations where there it is possible there is a need for childcare
#Remove the mysterious 0 & 39 responses to see paid versus unpaid childcare
cc2 <- cc %>% filter(xpchcf > 0, xpchcf < 3) %>% #Applicable for childcare question (13402)
  mutate(xpchc = ifelse(xpchc < 0, NA, xpchc))
cc2 %>% count(xpchcf) # 7321 free, 5721 paid
summary(cc2$xpchc) #7523 observations pay nothing or don't know
#Look at paid childcare costs overtime
cc2 %>%  #Shows the distribution of childcare costs over the waves; unsurprisingly it grows over time
  ggplot(aes(x = wave, y = xpchc)) +
  geom_jitter()

#See what my proposed ratio looks like
cc3 <- left_join(cc2, inccut, by  = c("pid", "wave")) %>% 
  mutate(xpchc = ifelse(xpchcf == 1, 0, xpchc)) %>% 
  mutate(ccratio = xpchc/hhneti)
summary(cc3$ccratio) #1790 observations are NA
cc3 %>%
  ggplot(aes(x = wave, y = ccratio)) +
  geom_jitter()
summary(cc3$ccratio) #1790 observations are NA
cc3 %>%
  ggplot(aes(x = hhneti, y = xpchc)) +
  geom_point()



