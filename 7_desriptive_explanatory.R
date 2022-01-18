#Coded by: Brian Buh
#Started on: 17.01.2022
#Last Updated: 18.01.2022

library(tidyverse)
library(haven)
library(lubridate)

#Use DF incfert

###########################################################################
# Descriptives of the dependent variables ---------------------------------
###########################################################################


# Step 1 - look at the age of different births in the current sample
agebirth <- incfert %>% 
  select(pid, ageyr, sex, event, totalchild) %>% 
  count(ageyr, sex, event) %>% 
  filter(event == 1) #Only 41 births occur after the age of 45 (only 1 of which is from a woman)

# Step 2 - Renew age at each birth event figure
birthlabs <- c("1st Birth", "2nd Birth", "3rd Birth")
names(birthlabs) <- c("1", "2", "3")

incfert %>% 
  mutate(sex = as.character(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  filter(event == 1) %>% 
  ggplot(aes(x = sex, y = age, color = sex)) +
  geom_violin() +
  coord_flip() +
  stat_summary(fun.data = "mean_sdl", mult=1,
               geom = "pointrange") +
  geom_hline(yintercept = 30, linetype = "dotted", size = 1) +
  facet_wrap(~parity, labeller = labeller(parity = birthlabs)) +
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
  ggsave("paper2_birthage_violin_script7_17-01-22.png", dpi = 300)

##It is clear that many observations occur to individuals who are no long in the reproductive age. I will remove all observations about the age 45
agebirth2 <- incfert %>% 
  filter(ageyr <= 45)

agebirth2 %>% distinct(pid) %>% count(totalchild)

# Step 3 - Data testing for mean and median number of observations
agebirth3 <- agebirth2 %>% 
  summarize(medianobs = median(rownum), meanobs = mean(rownum))

# Step 4 - Distribution of number of observations
agebirth2 %>% count(rownum)

agebirth4 <- agebirth2 %>% #Remove repeated observations and keep only final
  arrange(pid, desc(rownum)) %>% 
  group_by(pid) %>% 
  mutate(revrownum = row_number()) %>% 
  ungroup() %>% 
  filter(revrownum == 1)

agebirth4 %>% 
  ggplot(aes(x = rownum)) +
  geom_histogram(binwidth = 1, fill = "black", col = "grey", position  = "identity") +
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
  # labs(color = "Sex") +
  ggtitle("Observations per Respondent") +
  xlab("Number of Observations") +
  ylab("Count") +
  ggsave("paper2_number_obs_histogram_17-01-22.png", dpi = 300)

# Step 5 - Average starting wave and finishing wave
##Finishing wave
agebirth5 <- agebirth2 %>% #Remove repeated observations and keep only final
  arrange(pid, desc(rownum)) %>% 
  group_by(pid) %>% 
  mutate(revrownum = row_number()) %>% 
  ungroup() %>% 
  filter(revrownum == 1)
agebirth5 %>% count(wave)
agebirth5 %>% summarise(meanlw = mean(wave), medianlw = median(wave))


agebirth5 %>% 
  ggplot(aes(x = wave)) +
  geom_histogram(binwidth = 1, fill = "black", col = "grey", position  = "identity") +
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
  # labs(color = "Sex") +
  ggtitle("Last Observed Wave") +
  xlab("Last Wave Number") +
  ylab("Count") +
  ggsave("paper2_last_wave_histogram_17-01-22.png", dpi = 300)

##Starting Wave
agebirth6 <- agebirth2 %>% #Remove repeated observations and keep only final
  filter(rownum == 1)
agebirth6 %>% count(wave)
agebirth6 %>% summarise(meanfw = mean(wave), medianfw = median(wave))

agebirth6 %>% 
  ggplot(aes(x = wave)) +
  geom_histogram(binwidth = 1, fill = "black", col = "grey", position  = "identity") +
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
  # labs(color = "Sex") +
  ggtitle("First Observed Wave") +
  xlab("First Wave Number") +
  ylab("Count") +
  ggsave("paper2_first_wave_histogram_17-01-22.png", dpi = 300)

###########################################################################
# Compute the median income/hc/cc by age and parity -----------------------
###########################################################################

# ------------------------------------------------------------------------
# Income -----------------------------------------------------------------
# ------------------------------------------------------------------------

#Look at how many waves are NA by respondent
## I create variables for waves NA, Number of NA per respondent, number of total observations, percentage of obs NA, and categorical number of observation
medianinc <- incfert %>% 
  select(pid, wave, rownum, age, ageyr, sex, hhneti, hhyneti, parity, event, lag1dummy, lag2dummy, lag3dummy, totalchild) %>% 
  filter(ageyr <= 45) %>% 
  mutate(weekinc = ifelse(hhneti == -9, NA, hhneti)) %>% 
  group_by(pid) %>% 
  add_count(is.na(weekinc)) %>% 
  rename("weekincna" = "is.na(weekinc)") %>% 
  add_count() %>% 
  rename("numobs" = "nn") %>% 
  ungroup() %>% 
  mutate(perweekincna = ifelse(weekincna == TRUE, n/numobs, NA)) %>% 
  group_by(pid) %>% 
  fill(perweekincna, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(perweekincna = ifelse(is.na(perweekincna), 0, perweekincna)) %>% 
  mutate(numobs.cat = ifelse(numobs == 1, "One", ifelse(numobs >= 2 & numobs <= 6, "Low",
                                                        ifelse(numobs >= 7 & numobs <= 12, "Medium", "High")))) %>% 
  mutate(numobs.cat = fct_relevel(numobs.cat, c("High", "Medium", "Low", "One"))) %>% 
  mutate(hhyneti = ifelse (hhyneti == -9, NA, hhyneti)) %>% 
  mutate(weekinc.imp = (hhyneti/365)*7)

#Test for number of NA
test <- medianinc %>% filter(is.na(weekinc)) #There are 15234 NA in the weekly hh inc variable or 15.2%
medianinc %>% filter(weekincna == TRUE) %>% count(n) #Gives a table of how many NA there are by respondent
test2 <- medianinc %>% filter(is.na(weekinc.imp)) #On the other hand, simply using the available annual income reduces the NA to 2225 or 2.3% of observations
test2 %>% count(n) #Gives a table of how many NA there are by respondent


summary(medianinc$perweekincna)

#Make a plot to show the relationship between the number of respondents with missing income data and number of total observations (3909 Data)
medianinc %>% 
  ggplot(aes(x = perweekincna, fill = numobs.cat))+
  geom_histogram(binwidth = .05) +
  theme_minimal()+
  theme(legend.position = c(.85,.80), 
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15, vjust=-1), axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Number of observations") +
  ggtitle("Frequency of Missing Weekly Income Data") +
  xlab("Percentage of Observations Missing") +
  ylab("Count") +
  ggsave("paper2_weeklyinc_missing_number_observations_17-01-22.png", dpi = 300)

##Test of NA in observations of event and 1, 2 and 3 waves prior

### Observation wave 0
naevent0 <- medianinc %>% 
  filter(event == 1)
naevent0 %>% count(is.na(weekinc)) #There are 817 NA the observation of the event or 12.8%
naevent0 %>% count(is.na(weekinc.imp)) #There are 153 NA the observation of the event or 2.4%

### Observation wave 1
naevent1 <- medianinc %>% 
  filter(lag1dummy == 1) #5339 events have an observation 1 wave before
naevent1 %>% count(is.na(weekinc)) #There are 610 NA the observation of the event or 11.4%
naevent1 %>% count(is.na(weekinc.imp)) #There are 92 NA the observation of the event or 1.7%

### Observation wave 2
naevent2 <- medianinc %>% 
  filter(lag2dummy == 1) #4471 events have an observation 1 wave before
naevent2 %>% count(is.na(weekinc)) #There are 478 NA the observation of the event or 10.7%
naevent2 %>% count(is.na(weekinc.imp)) #There are 56 NA the observation of the event or 1.3%

### Observation wave 3
naevent3 <- medianinc %>% 
  filter(lag3dummy == 1) #3631 events have an observation 1 wave before (or 56.8% of original events)
naevent3 %>% count(is.na(weekinc)) #There are 372 NA the observation of the event or 10.2%
naevent3 %>% count(is.na(weekinc.imp)) #There are 92 NA the observation of the event or 1.0%

table <- medianinc %>% filter(event == 1) 


# %>% count(event, rownum)

# ------------------------------------------------------------------------
# Housing Costs  ---------------------------------------------------------
# ------------------------------------------------------------------------

medianhc <- incfert %>% 
  select(pid, wave, age, ageyr, sex, xphsn, parity, event, lag1dummy, lag2dummy, lag3dummy, totalchild) %>% 
  filter(ageyr <= 45) %>% 
  mutate(monthhc = ifelse(xphsn == -9, NA, xphsn)) %>% 
  group_by(pid) %>% 
  add_count(is.na(monthhc)) %>% 
  rename("monthhcna" = "is.na(monthhc)") %>% 
  add_count() %>% 
  rename("numobs" = "nn") %>% 
  ungroup() %>% 
  mutate(permonthhcna = ifelse(monthhcna == TRUE, n/numobs, NA)) %>% 
  group_by(pid) %>% 
  fill(permonthhcna, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(permonthhcna = ifelse(is.na(permonthhcna), 0, permonthhcna)) %>% 
  mutate(numobs.cat = ifelse(numobs == 1, "One", ifelse(numobs >= 2 & numobs <= 6, "Low",
                                                        ifelse(numobs >= 7 & numobs <= 12, "Medium", "High")))) %>% 
  mutate(numobs.cat = fct_relevel(numobs.cat, c("High", "Medium", "Low", "One")))

summary(medianhc$permonthhcna)

#Make a plot to show the relationship between the number of respondents with missing housing cost data and number of total observations
medianhc %>% 
  ggplot(aes(x = permonthhcna, fill = numobs.cat))+
  geom_histogram(binwidth = .05) +
  theme_minimal()+
  theme(legend.position = c(.85,.80), 
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15, vjust=-1), axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Number of Obs.") +
  ggtitle("Frequency of Missing Monthly Housing Cost Data") +
  xlab("Percentage of Observations Missing") +
  ylab("Count") +
  ggsave("paper2_monthlyhc_missing_number_observations_17-01-22.png", dpi = 300)

### Observation wave 0
naevent0hc <- medianhc %>% 
  filter(event == 1)
naevent0hc %>% count(is.na(monthhc)) #There are 158 NA the observation of the event or 2.5%

### Observation wave 1
naevent1hc <- medianhc %>% 
  filter(lag1dummy == 1) #5339 events have an observation 1 wave before
naevent1hc %>% count(is.na(monthhc)) #There are 130 NA the observation of the event or 2.4%

### Observation wave 2
naevent2hc <- medianhc %>% 
  filter(lag2dummy == 1) #4471 events have an observation 1 wave before
naevent2hc %>% count(is.na(monthhc)) #There are 90 NA the observation of the event or 2.0%

### Observation wave 3
naevent3hc <- medianhc %>% 
  filter(lag3dummy == 1) #3631 events have an observation 1 wave before (or 56.8% of original events)
naevent3hc %>% count(is.na(monthhc)) #There are 61 NA the observation of the event or 1.7%


medianhc %>% filter(event == 1)
# ------------------------------------------------------------------------
# Childcare Costs  ---------------------------------------------------------
# ------------------------------------------------------------------------

mediancc <- incfert %>% 
  select(pid, wave, age, ageyr, sex, xpchcf, xpchc, parity, event, lag1dummy, lag2dummy, lag3dummy, totalchild) %>% 
  filter(ageyr <= 45) %>% 
  mutate(weekcc = ifelse(xpchc < 0, NA, xpchc)) %>% 
  mutate(weekcc = ifelse(xpchcf <= 1 | xpchcf > 2, 0, weekcc)) %>% #There are 140 NA for observations where people report that they pay but amount unknown
  group_by(pid) %>% 
  add_count(is.na(weekcc)) %>% 
  rename("weekccna" = "is.na(weekcc)") %>% 
  add_count() %>% 
  rename("numobs" = "nn") %>% 
  ungroup() %>% 
  mutate(perweekccna = ifelse(weekccna == TRUE, n/numobs, NA)) %>% 
  group_by(pid) %>% 
  fill(perweekccna, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(perweekccna = ifelse(is.na(perweekccna), 0, perweekccna)) %>% 
  mutate(numobs.cat = ifelse(numobs == 1, "One", ifelse(numobs >= 2 & numobs <= 6, "Low",
                                                        ifelse(numobs >= 7 & numobs <= 12, "Medium", "High")))) %>% 
  mutate(numobs.cat = fct_relevel(numobs.cat, c("High", "Medium", "Low", "One")))

summary(mediancc$perweekccna)

#Make a plot to show the relationship between the number of respondents with missing housing cost data and number of total observations
mediancc %>% 
  ggplot(aes(x = perweekccna, fill = numobs.cat))+
  geom_histogram(binwidth = .05) +
  theme_minimal()+
  theme(legend.position = c(.85,.80), 
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15, vjust=-1), axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Number of Obs.") +
  ggtitle("Frequency of Missing Monthly Housing Cost Data") +
  xlab("Percentage of Observations Missing") +
  ylab("Count") +
  ggsave("paper2_weekly_cc_missing_number_observations_17-01-22.png", dpi = 300)

### Observation wave 0
naevent0cc <- mediancc %>% 
  filter(event == 1)
naevent0cc %>% count(is.na(weekcc)) #There are 33 NA the observation of the event or 0.5%

### Observation wave 1
naevent1cc <- mediancc %>% 
  filter(lag1dummy == 1) #5339 events have an observation 1 wave before
naevent1cc %>% count(is.na(weekcc)) #There are 21 NA the observation of the event or 0.4%

### Observation wave 2
naevent2cc <- mediancc %>% 
  filter(lag2dummy == 1) #4471 events have an observation 1 wave before
naevent2cc %>% count(is.na(weekcc)) #There are 20 NA the observation of the event or 0.4%

### Observation wave 3
naevent3cc <- mediancc %>% 
  filter(lag3dummy == 1) #3631 events have an observation 1 wave before (or 56.8% of original events)
naevent3cc %>% count(is.na(weekcc)) #There are 5 NA the observation of the event or 0.1%

