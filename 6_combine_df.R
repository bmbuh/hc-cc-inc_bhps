#Coded by: Brian Buh
#Started on: 14.01.2022
#Last Updated: 18.01.2022

library(tidyverse)
library(haven)
library(lubridate)

###########################################################################
# Combining fertility and inc/hc/cc dataframes ----------------------------
###########################################################################

#This section creates the DF "childdoblong"
# -------------------------------------------------------------------------
#Editing the DF "fertbhps" to extract child birth dates ----------------------
# -------------------------------------------------------------------------

extmonth <- fertbhps %>% 
  select(pid, child1m, child1yr, child2m, child2yr, child3m, child3yr) %>% 
  unite(child1date, child1m, child1yr, sep = "-", remove = FALSE) %>% 
  unite(child2date, child2m, child2yr, sep = "-", remove = FALSE) %>% 
  unite(child3date, child3m, child3yr, sep = "-", remove = FALSE) %>% 
  select(pid, child1date, child2date, child3date) %>% 
  pivot_longer(cols = c('child1date', 'child2date', 'child3date'), names_to = "parity", values_to = "date") %>% 
  mutate(parity = recode(parity,
                         "child1date" = "1",
                         "child2date" = "2",
                         "child3date" = "3")) %>% 
  separate(date, into = c("month", "year"), sep = "-", convert = TRUE)


childdoblong <- fertbhps %>% 
  select(pid, totalchild, child1yr, child2yr, child3yr) %>% 
  pivot_longer(cols = c('child1yr', 'child2yr', 'child3yr'), names_to = "parity", values_to = "year") %>% 
  mutate(parity = ifelse(is.na(year), NA, parity )) %>% 
  mutate(parity = recode(parity,
                      "child1yr" = "1",
                      "child2yr" = "2",
                      "child3yr" = "3")) %>% 
  left_join(., extmonth, by = c("pid", "parity", "year")) %>% 
  mutate(dummywave = year)
  
#This RDS is a logn format of the child birth dates and total number of children
saveRDS(childdoblong, "childdoblong.rds")


testbirth <- childdoblong %>% 
  filter(year >=1991)

# -------------------------------------------------------------------------  
#Start with the combined hh and individual BHPS dataframe "indhhbhps" -----
# -------------------------------------------------------------------------

#Step 1 - account for missing intermediate waves
##Note - first attempt lost 3910 birth events because of births happening in intermediate waves
##The creation of the "dummywave" variable allows for the general year in which each wave took place to match with the year of birth
missingwave <- indhhbhps %>% 
  complete(nesting(pid), wave = seq(min(wave), max(wave), 1L)) %>% 
  mutate(istrtdaty = ifelse(wave == 1, 1991, istrtdaty)) %>% 
  mutate(dummywave = 1990 + wave) 

#Step 2 - add births to DF with all waves
missingwave2 <- missingwave %>% 
  left_join(., childdoblong, by = c("pid", "dummywave")) %>% 
  select(pid, wave, totalchild, parity, year, month) %>% 
  filter(!is.na(year)) #Removes waves without a birth event

#Step 3- Re-add birth events to the full DF
missingwave3 <- missingwave %>% 
  left_join(., missingwave2, by = c("pid", "wave")) %>% 
  filter(!is.na(hid | year)) #This selects on observations with a) interview occured or b) birth event occured

#Step 4 - evaluate success at this point
missingwave4 <- missingwave3 %>% 
  filter(!is.na(year)) %>% 
  select(pid, wave, hid, year, month) %>% 
  mutate(noinfo = ifelse(is.na(hid), 1, 0))

test4 %>% count(noinfo)

#Step 5 - 
#Needed for Step 5 below
totalchild <- fertbhps %>% 
  select(pid, totalchild)

indhhbhps2 <- missingwave3 %>% 
  select(-totalchild) %>% 
  left_join(., totalchild, by = "pid") %>% 
  mutate(event = ifelse(!is.na(year), 1, 0)) %>% 
  mutate(istrtdatm = ifelse(istrtdatm <=0, 9, istrtdatm)) %>% #There are several missing month start dates (especially in W1), however the process starts each year in Sept., so unknown months are set to september
  mutate(mntest = month - istrtdatm) %>% 
  mutate(movewave = ifelse(mntest < 0, 1, 0)) %>% 
  group_by(pid) %>% 
  mutate(rownum = row_number()) %>% 
  mutate(rownumminus1 = rownum - 1) %>% 
  fill(movewave, .direction = "downup") %>% 
  ungroup

indhhbhps2 %>% count(event)

#Step 6 - Place missing wave birth events in the available interview wave before they were born
wavena <- indhhbhps2 %>% 
  filter(is.na(hid)) %>% 
  select(pid, rownumminus1, month, year, event) %>% 
  rename("rownum" = "rownumminus1") %>%
  rename("month2" = "month") %>%
  rename("year2" = "year") %>% 
  rename("event2" = "event")

#Running test to observe data
##Note: 2556 births occur in missing observations before the first interview
##This additional step has added 1354 events that were otherwise missing
test <- wavena %>% filter(rownum == 0)
test2 <- indhhbhps2 %>% filter(rownumminus1 == 0)
wavena %>% count(event2)
wavena %>% count(rownum)

#Step 7 -Readding births from NA waves shift up 1 row
indhhbhps3 <- indhhbhps2 %>% 
  left_join(., wavena, by =  c("pid", "rownum")) %>% 
  mutate(month = ifelse(!is.na(month2), month2, month)) %>% #Add the year and month to the correct column
  mutate(year = ifelse(!is.na(year2), year2, year)) %>% 
  mutate(event = ifelse(!is.na(year), 1, 0)) %>% 
  mutate(impt = ifelse(!is.na(year2), 1, 0)) %>% #This shows me which waves had a none wave event added
  select(-month2, -year2)  %>% 
  group_by(pid) %>% 
  fill(parity, .direction = "up") %>% 
  ungroup() 

#Step 8 - Filtering out rows with NA from missing survey data
indhhbhps4 <- indhhbhps3 %>% 
  filter(!is.na(sex)) %>% #I have tried using different variables as filters, but sex is most uniformly important
  select(-event, -event2) %>% 
  mutate(event = ifelse(!is.na(year), 1, 0))

indhhbhps4 %>% count(event) #This gives me a total of 6856 events occurring in the DF
indhhbhps4 %>% count(impt) #this tells me that I have imputed 250 events from waves that would have been otherwise dropped

#Step 9 - Move events to the previous waves when the event occurred before the interview
##Look at data to see scope of the issue
test <- indhhbhps4 %>% 
  select(pid, wave, year, month, mntest, movewave, rownum, rownumminus1) %>% 
  filter(movewave == 1)

#Same process as Step 6
wavemismatch <- indhhbhps4 %>% 
  select(pid, wave, istrtdatm, istrtdaty, month, year, mntest, movewave) %>% 
  group_by(pid) %>% 
  mutate(rownum = row_number()) %>% 
  mutate(rownumminus1 = rownum - 1) %>% 
  ungroup() %>% 
  filter(movewave == 1) %>% 
  filter(!is.na(year)) %>% 
  rename("month2" = "month") %>%
  rename("year2" = "year") %>% 
  select(pid, rownum, rownumminus1, month2, year2) %>% 
  rename("forremoval" = "rownum") %>% 
  rename("rownum" = "rownumminus1")

wavemismatch2 <-  wavemismatch %>% 
  select(pid, rownum, month2, year2)

#This DF is used to create an indicator of which observations had a child dob that needed to be shifted up
wavemismatch3 <- wavemismatch %>% 
  select(pid, forremoval) %>% 
  mutate(clearmy = 1) %>% 
  rename("rownum" = "forremoval")

# Step 10 - Add back in shifted dates, replace the NA in the year and month with new shifted dates, remove old observations and variables
indhhbhps5 <- indhhbhps4 %>% 
  left_join(., wavemismatch, by = c("pid", "rownum")) %>% 
  mutate(month = ifelse(!is.na(month2), month2, month)) %>% #Add the year and month to the correct column
  mutate(year = ifelse(!is.na(year2), year2, year)) %>% 
  mutate(impt2 = ifelse(!is.na(year2), 1, 0)) %>% 
  left_join(., wavemismatch3, by = c("pid", "rownum")) %>% 
  mutate(clearmy = ifelse(is.na(clearmy), 0, clearmy)) %>% 
  mutate(month = ifelse(clearmy == 1, NA, month)) %>% #Removes data from months that ave been shifted
  mutate(year = ifelse(clearmy == 1, NA, year)) %>% #Removes data from years that ave been shifted
  select(-dummywave, -mntest, -movewave, -rownum, -rownumminus1, -event, -forremoval, -month2, -year2, -clearmy) %>% #Housekeeping
  mutate(event = ifelse(!is.na(year), 1, 0)) %>% 
  mutate(parity = ifelse(is.na(year), NA, parity)) %>% 
  group_by(pid) %>% 
  fill(parity, .direction = "down") %>% 
  ungroup() %>% 
  mutate(parity = ifelse(totalchild == 0, 0, parity))

indhhbhps5 %>% count(event) #I have a total of 6436 events after all the transformations

test <- indhhbhps5 %>% distinct(pid)

##To create an indicator for which respondents have at least 1 event during the observation period
paritycheck <- indhhbhps5 %>% 
  group_by(pid) %>% 
  summarise(obsevent = sum(event)) %>% 
  ungroup()

paritycheck %>% count(obsevent) #There are 4302 unique individuals who experience at least 1 childbirth

# Step 11 - add in the parity check to remove observations where individuals have children before observation period
indhhbhps6 <- indhhbhps5 %>% 
  mutate(parchk = ifelse(is.na(parity), 1, 0)) %>% 
  group_by(pid) %>% 
  fill(parity, .direction = "up") %>% 
  ungroup() %>% 
  mutate(parity = as.numeric(parity)) %>% 
  mutate(parity = ifelse(parchk == 1, (parity - 1), parity)) %>% 
  left_join(., paritycheck, by = "pid")


# Step 12 - Look at sample of individual who have children, but no events during observation period
indhhbhps7 <- indhhbhps6 %>% 
  filter(is.na(parity)) %>% 
  mutate(age_dv = ifelse(age_dv >100 | age_dv < 0, NA, age_dv)) #Oddly quite a few recording mistakes in the age_dv variable
summary(indhhbhps7$age_dv)  #It is clear that most of these individuals are old (mean age 55.78)

# Step 13 - Remove observations from individuals with children but no observed events
indhhbhps8 <- indhhbhps6 %>% 
  filter(!is.na(parity)) %>% 
  mutate(age_dv = ifelse(age_dv >100 | age_dv < 0, NA, age_dv))
indhhbhps8 %>% count(event) #importantly no loss of events
summary(indhhbhps8$age_dv)

# Step 14 - Use the imputations from "test2' below
indhhbhps9 <- indhhbhps8 %>% 
  mutate(sex = ifelse(sex <= 0, NA, sex)) %>% #There are NA in the sex column for individuals who have it previously recorded
  group_by(pid) %>% 
  fill(sex, .direction = "downup") %>% 
  mutate(rownum = row_number()) %>% 
  mutate(totobs = length(rownum)) %>% 
  ungroup() %>% 
  filter(!is.na(sex)) %>% 
  fill(birthy, .direction = "up") %>% #birthy and birthm can be imputed up as they don't change
  fill(birthm, .direction = "up") %>% 
  unite(intdate, c(istrtdatm, istrtdaty), sep = "-", remove = FALSE) %>% #combines the mnth & yr var to make int dates
  mutate(intdate = parse_date_time(intdate, "my")) %>% 
  unite(dob, c(birthm, birthy), sep = "-", remove = FALSE) %>% #combines the mnth & yr var to make int dates
  mutate(dob = parse_date_time(dob, "my")) %>% 
  mutate(age = interval(dob, intdate) / years(1)) %>%  #This variable gives me an exact age at time of interview (non-rounded)
  mutate(ageyr = trunc(age)) %>%  #Removes the values after decimals 
  filter(!is.na(age), age >=16) %>% #It is clear that NA and U16 observations are not valid here
  mutate(agegrp = ifelse(age <= 15, 1, ifelse(age >=16 & age <= 29, 2, ifelse(age >= 30 & age <= 45, 3, 4))))

# Step 15 - Make dummies to indicate waves before events
lag1 <- indhhbhps9 %>% 
  select(pid, rownum, event) %>% 
  mutate(rownum = rownum-1) %>% 
  rename("lag1dummy" = "event")

lag2 <- indhhbhps9 %>% 
  select(pid, rownum, event) %>% 
  mutate(rownum = rownum-2) %>% 
  rename("lag2dummy" = "event")

lag3 <- indhhbhps9 %>% 
  select(pid, rownum, event) %>% 
  mutate(rownum = rownum-3) %>% 
  rename("lag3dummy" = "event")

indhhbhps10 <- indhhbhps9 %>% 
  left_join(., lag1, by = c("pid", "rownum")) %>% 
  left_join(., lag2, by = c("pid", "rownum")) %>% 
  left_join(., lag3, by = c("pid", "rownum")) %>% 
  mutate(lag1dummy = ifelse(is.na(lag1dummy), 0, lag1dummy)) %>% 
  mutate(lag2dummy = ifelse(is.na(lag2dummy), 0, lag2dummy)) %>% 
  mutate(lag3dummy = ifelse(is.na(lag3dummy), 0, lag3dummy))

lagtest <-  indhhbhps10 %>% 
  select(pid, event, lag1dummy, lag2dummy, lag3dummy, parity)

# Step 16 - Add dummies for combination of parity observed
# New categorical variable parity.cat
## 0 = None, 1 = Only First, 2 = Only Second, 3 = Only Third
## 4 = First and Second, 5 = Second and Third, 6 = First, Second and Third

indhhbhps11 <- indhhbhps10 %>% 
  group_by(pid) %>% 
  distinct(parity) %>% 
  filter(parity != 0) %>% 
  add_count() %>% 
  mutate(parity2 = ifelse(n == 2 & parity == 1, 2, parity)) %>% 
  ungroup() %>% 
  rename("n_event" = "n") %>% 
  select(pid, n_event)

indhhbhps11 %>% count(n_event)

birthseq.cat<- indhhbhps11 %>% 
  group_by(pid) %>% 
  summarise(parity2 = sum(parity2)) %>% 
  ungroup() %>% 
  rename("parity.cat" = "parity2")

birthseq.cat %>% count(parity.cat)

indhhbhps12 <- indhhbhps10 %>% 
  left_join(., birthseq.cat, by = "pid") %>% 
  mutate(parity.cat = ifelse(is.na(parity.cat), 0, parity.cat)) %>% 
  left_join(., indhhbhps11, by = "pid") %>% 
  mutate(n_event = ifelse(is.na(n_event), 0, n_event)) %>% 
  distinct(pid, wave, .keep_all = TRUE)

test <- indhhbhps12 %>%
  group_by(pid) %>% 
  distinct(n_event, .keep_all = TRUE) %>% 
  ungroup()
test %>% count(n_event, parity.cat)

#I had a doubling of observation issues - DELETE this later
indhhbhps13 <- indhhbhps12 %>%  
   distinct(pid, wave, .keep_all = TRUE)
 
  
#The final RDS will be saved under the friendlier name incfert
saveRDS(indhhbhps12, "incfert.rds")

###########################################################################
# DF Testing --------------------------------------------------------------
############################################################################

#Note: this testing was done using the DF created in indhhbhps8 and then imputations found were add as indhhbhps9
test <- incfert %>% 
  distinct(pid, .keep_all = TRUE) %>% 
  count(totalchild) #Parity total: 0 = 14880, 1 = 1339, 2 = 1890, 3 = 878, 4+ = 195

#There appears to be a lot of 0 parity individuals, why?
test2 <- incfert %>% 
  # filter(totalchild == 0) %>% #Uncomment this line for test3
  select(pid, wave, sex, age_dv, birthm, birthy, istrtdatm, istrtdaty, totalchild) %>% 
  mutate(sex = ifelse(sex <= 0, NA, sex)) %>% #There are NA in the sex column for individuals who have it previously recorded
  group_by(pid) %>% 
  fill(sex, .direction = "downup") %>% 
  mutate(rownum = row_number()) %>% 
  mutate(totobs = length(rownum)) %>% 
  ungroup() %>% 
  filter(!is.na(sex)) %>% 
  fill(birthy, .direction = "up") %>% #birthy and birthm can be imputed up as they don't change
  fill(birthm, .direction = "up") %>% 
  unite(intdate, c(istrtdatm, istrtdaty), sep = "-", remove = FALSE) %>% #combines the mnth & yr var to make int dates
  mutate(intdate = parse_date_time(intdate, "my")) %>% 
  unite(dob, c(birthm, birthy), sep = "-", remove = FALSE) %>% #combines the mnth & yr var to make int dates
  mutate(dob = parse_date_time(dob, "my")) %>% 
  mutate(age = interval(dob, intdate) / years(1)) %>%  #This variable gives me an exact age at time of interview (non-rounded)
  mutate(ageyr = trunc(age)) %>%  #Removes the values after decimals 
  filter(!is.na(age), age >=16) #It is clear that NA and U16 observations are not valid here
  

#Warning: these tests were done by in steps. "test2" was update through the testing rounds for simplicity  
test2 %>% count(sex) #There are still 63 NA (meaning no wave had a sex registered)
test3 <- test2 %>% filter(is.na(sex)) #All these observations come from waves 16-18 and are mostly younger individuals. Removed from "test2"

#Testing for NA in the "age_dv" variable
test2 %>% count(is.na(age_dv)) #There are 4235 observations with a missing age; 2677 come from Parity 0

test4 <- test2 %>% 
  filter(is.na(age_dv)) #In all cases where the age_dv is NA the birthy is NA
summary(test4$totobs) #There observations are NOT dropable, the age must be imputed (tested via number of observations)
test4 %>% count(wave) #All NA come from Wave 1 (except 1)


test5 <- test2 %>% count(ageyr)
test6 <- test2 %>% 
  mutate(agegrp = ifelse(age <= 15, 1, ifelse(age >=16 & age <= 29, 2, ifelse(age >= 30 & age <= 45, 3, 4)))) %>% 
  count(agegrp, totalchild) #See where my distibution of ages are: I can clear drop U16 and NA

table <- incfert %>% count(event, rownum)
table2 <- incfert %>% distinct(pid, .keep_all = TRUE) %>%  count(n_event, parity.cat)
  
