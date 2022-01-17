#Coded by: Brian Buh
#Started on: 17.01.2022
#Last Updated: 

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
# Income ------------------------------------------------------------------
# ------------------------------------------------------------------------

#Look at how many waves are NA by respondent
## I create variables for waves NA, Number of NA per respondent, number of total observations, percentage of obs NA, and categorical number of observation
medianinc <- incfert %>% 
  select(pid, wave, age, ageyr, sex, hhneti, hhyneti, parity, event, totalchild) %>% 
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
  mutate(numobs.cat = fct_relevel(numobs.cat, c("High", "Medium", "Low", "One")))


summary(medianinc$perweekincna)

#Make a plot to show the relationship between the number of respondents with missing income data and number of total observations
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

medianinc %>% filter(weekincna == TRUE) %>% count(n)
  

#Test for number of NA
test <- medianinc %>% filter(is.na(weekinc)) #There are 15234 NA in the weekly hh inc variable or 15.2%

medianinc2 <- medianinc %>% 
  filter(!is.na(weekinc)) %>% 
  mutate()
  
  
  #  group_by(ageyr) %>% 
  summarise(medianinc = median(weekinc), meaninc = mean(weekinc))
