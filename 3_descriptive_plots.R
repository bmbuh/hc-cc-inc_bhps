#Coded by: Brian Buh
#Started on: 03.01.2022
#Last Updated: 05.01.2022

library(tidyverse)
library(haven)
library(lubridate)
library(cowplot)


# Plots - Descriptive Stats ------------------------------------------------


# Plot 1- Count of Completed Fertility - 1991 -2008 -----------------------


# The goal of this plot is to show the distribution of completed fertility in the sample.
# Therefore, we need to consider who is included in the sample.
# It must be women who were is the risk set during the observation period.
# This means women older than 16 and younger than 40 in 1991.
# Step 1 is extracting the completed fertility from the fert_his DF
## Create variables for age of parent: agelastint, age91, agefb, agesb, agetb
# Step 2 is merging this DF with the xwavedata from the BHPS
# Step 3 is cutting the sample to only include the women who would be in the final sample
## This is the tricky step. Until I know how I am going to measure my explanatory variable, it is hard to know where to cut.
### Cut 1 - Remove 'totalchild' NA
### Cut 2 - Women only
### Cut 3 - Only women who could have given birth starting in wave 1 (Age 16-40)
### Cut 4 - Remove women who we cannot guarantee their fertility career was complete (What age do I use, right now I have 40)
#### I could consider using the measure of fertility intentions available in waves 2, 8, 11,l 12, 13, and 17
### Cut ? (Missing, Children born before available income data)
# Step 4 Descriptive Stats
## DS1 is to count the number of complete fertility, age at first birth, age in 1991, last interview
# Step 5 Histogram


#Step 1
totfert <- fert_his %>% 
  select(pp, hh1, sex, totalchild, parentbirth_m, parentbirth_yr, child1yr, child2yr, child3yr, last_intvyr) %>% 
  rename("pid" = "pp") %>% 
  # filter(last_intvyr >= 2008) %>% 
  mutate(agelastint = last_intvyr - parentbirth_yr) %>% 
  mutate(age91 = 1991 - parentbirth_yr) %>% 
  mutate(agefb = child1yr - parentbirth_yr) %>% #Age at first birth
  mutate(agesb = child2yr - parentbirth_yr) %>% #Age at second birth
  mutate(agetb = child3yr - parentbirth_yr) #Age at third birth
  # filter(agelastint >= 40) %>%
  # filter(child1yr >= 1991)


#Step 2
combo <- bhpsxwave %>% 
  select(pid, birthy, hhorig_bh, fwintvd_dv_bh, lwintvd_dv_bh, ba_hid, dcsedw_dv_bh) %>% 
  left_join(. , totfert, by = "pid") 

#Step 3
compfert <-  combo %>%
  filter(!is.na(totalchild)) %>% #Cut1
  # filter(sex == 2) %>% #Cut2
  filter(age91 <= 40 & age91 >= 16) %>% #Cut3 
  filter(agelastint >=40) %>% #Cut4
  # filter(fwintvd_dv_bh == 1) %>%
  filter(child2yr >= 1991 | is.na(child2yr)) %>% 
  filter(hhorig_bh == 1)
# %>%
  # filter(lwintvd_dv_bh >= 10)

#Step 4
agefb <- compfert %>% count(agefb)
age91 <- compfert %>% count(age91)
compfert %>% count(totalchild, sex)

compfert %>% count(lwintvd_dv_bh)

#Step 5 Histogram of Distribution of Completed Fertility
compfert %>%
  mutate(sex = as.character(sex)) %>% 
  mutate(totalchild2 = as.character(totalchild)) %>% 
  mutate(sex = recode(sex,
                         "1" = "Men",
                         "2" = "Women")) %>% 
  ggplot(aes(x = totalchild, color = sex)) +
  geom_histogram(binwidth = 1, fill = "white", alpha = 0.5, position  = "identity") +
  facet_wrap(~sex) +
  # annotate("text", x=.15, y=5000, size = 6, label= "First Births are concentrated here") +
  # annotate("text", x=.25, y=2000, size = 6, label= "0 = No Jobless Spells") +
  # annotate("text", x=.8, y=1200, size = 6, label= "1 = Completely Jobless") +
  scale_color_manual(values = c("#5a189a", "#2a9d8f")) +
  theme_minimal()+
  theme(legend.position = c(.9,.8), plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15, vjust=-1), axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(color = "Sex") +
  ggtitle("Distribution of Completed Fertility", subtitle =  "BHPS") +
  xlab("Number of Births, Completed Fertility") +
  ylab("Count") +
  ggsave("paper2_compfert_hist_03-01-22.png", dpi = 300)




# Plot 2 - Age of different births ----------------------------------------

# Step 1 - Use DF Combo created in Steps 1 & 2 in Plot 1
# Step 2 - Remove births that happened before first interview
## Indicator created here, removal done in steps 4 & 5
# Step 3 - Look at cross-tab of ages
## There are 68 observations in which the child's birth year is registered at "1899"
### This is also true for some 2nd and 3rd births, but not all
# Step 4 - Remove birthdate that have been recorded as 1899
## Dates with 1899 are transformed to NA
### Warning, this data set works for the desired plot, but would having important missing births is used for completed fertility
# Step 5 - Transform the dates to a long format in order to use facetwrap
# Step 6 - Make plot


#Step 2
combo2 <- combo %>% 
  mutate(testfb = ifelse(child1yr < 1991, 0, 1)) %>% 
  mutate(testsb = ifelse(child2yr < 1991, 0, 1)) %>% 
  mutate(testtb = ifelse(child3yr < 1991, 0, 1))

#Step 3
child1yr <- combo2 %>% count(child1yr)
agefb <- combo2 %>% count(agefb, sex)
#???? Why are there negative numbers
agefbtest <- combo2 %>% filter(agefb < 2)
#Clearly some births are recorded as the year 1899
agefbtest2 <- combo2 %>% filter(agefb >= 14 & agefb <= 20) %>% count(child1yr, parentbirth_yr)
#The second test shows that is is just these 1899 births that are a problem
agesb <- combo2 %>% count(agesb, sex) #Age second birth
agetb <- combo2 %>% count(agetb, sex) #Age third birth
#Similar issue with birthdates not recorded correctly

#Step 4 & 5
combo3 <- combo2 %>% 
  filter(!is.na(sex)) %>% #NA in sex are also NA in fertility history
  mutate(agefb = ifelse(agefb < 2, NA, agefb)) %>% 
  mutate(agesb = ifelse(agesb < 2, NA, agesb)) %>% 
  mutate(agetb = ifelse(agetb < 2, NA, agetb)) %>% 
  mutate(agefb = ifelse(testfb == 0, NA, agefb)) %>% #Removes FB ages before first interview
  mutate(agesb = ifelse(testsb == 0, NA, agesb)) %>% #Removes SB ages before first interview
  mutate(agetb = ifelse(testtb == 0, NA, agetb)) %>% #Removes TB ages before first interview
  select(pid, sex, agefb, agesb, agetb) %>% 
  pivot_longer(cols = c('agefb', 'agesb', 'agetb'), names_to = "birth", values_to = "age")

agedist <- combo3 %>% count(age)

#Step 6
birthlabs <- c("1st Birth", "2nd Birth", "3rd Birth")
names(birthlabs) <- c("agefb", "agesb", "agetb")

birthage<- combo3 %>% 
  mutate(sex = as.character(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ggplot(aes(x = sex, y = age, color = sex)) +
  geom_violin() +
  coord_flip() +
  stat_summary(fun.data = "mean_sdl", mult=1,
               geom = "pointrange") +
  geom_hline(yintercept = 30, linetype = "dotted", size = 1) +
  facet_wrap(~birth, labeller = labeller(birth = birthlabs)) +
  # scale_color_manual(values = c("#5a189a", "#2a9d8f")) +
  theme_minimal()+
  theme(legend.position = "none",
        # legend.position = c(.85,.88), 
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15, vjust=-1), axis.title.y = element_text(size = 15), 
        # legend.key.size = unit(1, 'cm'),
        # legend.title = element_text(size = 15),
        # legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(color = "Sex") +
  ggtitle("Age at Birth Parity") +
  xlab("") +
  ylab("Age") +
  ggsave("paper2_birthage_violin_05-01-22.png", dpi = 300)







