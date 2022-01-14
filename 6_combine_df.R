#Coded by: Brian Buh
#Started on: 14.01.2022
#Last Updated: 

library(tidyverse)
library(haven)
library(lubridate)

###########################################################################
# Combining fertility and inc/hc/cc dataframes ----------------------------
###########################################################################


# -------------------------------------------------------------------------
#Editing the DF "combo" to extract child birth dates ----------------------
# -------------------------------------------------------------------------

extmonth <- combo %>% 
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


childdoblong <- combo %>% 
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

#Needed for Step 5 below
totalchild <- combo %>% 
  select(pid, totalchild)

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
  filter(!is.na(sex)) %>% #I have tried using different variables as filters, but sex is most uniformally important
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

#To create an indicator for which respondents have at least 1 event during the observation period
paritycheck <- indhhbhps6 %>% 
  group_by(pid) %>% 
  summarise(obsevent = sum(event)) %>% 
  ungroup()

paritycheck %>% count(obsevent) #There are 4302 uniquie individuals who experience at least 1 childbirth

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

#The final RDS will be saved under the friendlier name incfert
saveRDS(indhhbhps8, "incfert.rds")







  
