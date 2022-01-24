#Coded by: Brian Buh
#Started on: 19.01.2022
#Last Updated: 24.01.2022


library(tidyverse)
library(haven)
library(lubridate)

#Use DF incfert

###########################################################################
# Testing Weekly HH Net Income for Event Bias -----------------------------
###########################################################################

# -------------------------------------------------------------------------
# Testing distribution of variation between hhneti and hhyneti ------------
# -------------------------------------------------------------------------

weekinc <- incfert %>% 
  select(pid, wave, rownum, ageyr, event, parity, parity.cat, n_event, totalchild,
         lag1dummy, lag2dummy, lag3dummy, hhneti, hhyneti) %>% 
  filter(ageyr <= 45) %>% 
  mutate(weekinc = ifelse(hhneti == -9, NA, hhneti)) %>% 
  mutate(hhyneti = ifelse (hhyneti == -9, NA, hhyneti)) %>% 
  mutate(weekinc.imp = (hhyneti/365)*7) %>% 
  mutate(weekinc.diff = weekinc - weekinc.imp) #builds a variable to see the difference between the two measures


##Comparing the distribution
summary(weekinc$weekinc.diff)
##Median = 10.89, Mean = 26.85

##Make a distribution
##Outliers are so large the figure is not readable, only display within 1 SD
weekinc2 <- weekinc %>% filter(!is.na(weekinc.diff)) #SD cannot be calculated with NA
sd(weekinc2$weekinc.diff) #SD = 153.8
weekinc %>% 
  filter(weekinc.diff > -127.22, weekinc.diff < 180.38) %>% #This is only keeping values within 1 SD of the mean
  ggplot(aes(x=weekinc.diff)) +
  geom_histogram()
#The distribution is very close to normal, telling me that there is no issue for using one or the other


# Testing for distribution at time of events ------------------------------
weekinc3 <-  weekinc %>% 
  filter(event == 1) #"event" can be replaced with "lag*dummy" to see the distributions at different lags
summary(weekinc3$weekinc.diff)
##Median = 5.79, Mean = 15.98
weekinc4 <- weekinc3 %>% filter(!is.na(weekinc.diff)) #SD cannot be calculated with NA
sd(weekinc4$weekinc.diff) #SD = 217.8
weekinc3 %>% 
  filter(weekinc.diff > -201.8, weekinc.diff < 233.8) %>% #This is only keeping values within 1 SD of the mean
  ggplot(aes(x=weekinc.diff)) +
  geom_histogram()
#Again the distribution is normal centered close to 0
#Lagged years are even more normally distributed

###########################################################################
# Age specific explanatory variables --------------------------------------
###########################################################################

agespec <- incfert %>% 
  select(pid, wave, rownum, sex, ageyr, event, parity, parity.cat, n_event, totalchild,
         lag1dummy, lag2dummy, lag3dummy, hhneti, hhyneti, loctax, xpchcf, xpchc, xphsn) %>% 
  filter(ageyr <= 45, ageyr >= 20) %>% 
  mutate(totalchild2 = ifelse(totalchild >= 4, "4+", totalchild)) %>% 
  mutate(weekinc = ifelse(hhneti == -9, NA, hhneti)) %>% 
  mutate(hhyneti = ifelse (hhyneti == -9, NA, hhyneti)) %>% 
  mutate(weekinc.imp = ((hhyneti/365)*7)-loctax) %>% #Transforms annual to current and removes local tax
  mutate(monthhc = ifelse(xphsn == -9, NA, xphsn)) %>% 
  mutate(weekcc = ifelse(xpchc < 0, NA, xpchc)) %>% 
  mutate(weekcc = ifelse(xpchcf <= 1 | xpchcf > 2, 0, weekcc)) %>% 
  mutate(ccfop = ifelse(xpchcf == 2, "paid", ifelse(xpchcf == 1, "free", "not used")))

# -------------------------------------------------------------------------
# HH Net Income------------------------------------------------------------
# -------------------------------------------------------------------------

#I will use the weekly income derived as there are less NA
agespecinc <- agespec %>% 
  filter(!is.na(weekinc.imp)) %>% 
  group_by(sex, ageyr, totalchild2) %>% 
  summarise(agespecmean = mean(weekinc.imp), agespecsd = sd(weekinc.imp), IQR(weekinc.imp)) %>% 
  mutate(low = agespecmean - agespecsd) %>% 
  mutate(high = agespecmean + agespecsd)

agespecinc %>% 
  filter(totalchild2 != "4+") %>% 
  mutate(sex = as.character(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ggplot(aes(x = ageyr, y = agespecmean, color = totalchild2)) +
  geom_line() +
  facet_wrap(~sex) +
  scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45)) +
  scale_color_manual(values = c("#264653", "#2A9D8F", "#D5A220", "#E76F51")) +
  theme_minimal()+
  theme(
        legend.position = c(.63,.84),
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust=-1), axis.title.y = element_text(size = 10), 
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        strip.text.x = element_text(size = 10)) +
  theme(aspect.ratio = 1) +
  labs(color = "Total Child(ren)") +
  ggtitle("Age Specific Net Household Income") +
  xlab("Age") +
  ylab("Weekly Net HH Income (£)") +
  ggsave("paper2_age_specific_income_totalchild_19-01-22.png", dpi = 300)
  

# -------------------------------------------------------------------------
# Housing Costs -----------------------------------------------------------
# -------------------------------------------------------------------------

#I will use the weekly income derived as there are less NA
agespechc <- agespec %>% 
  filter(!is.na(monthhc)) %>% 
  group_by(sex, ageyr, totalchild2) %>% 
  summarise(agespecmean = mean(monthhc), agespecsd = sd(monthhc), IQR(monthhc)) %>% 
  mutate(low = agespecmean - agespecsd) %>% 
  mutate(high = agespecmean + agespecsd)

agespechc %>% 
  filter(totalchild2 != "4+") %>% 
  mutate(sex = as.character(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ggplot(aes(x = ageyr, y = agespecmean, color = totalchild2)) +
  geom_line() +
  facet_wrap(~sex) +
  scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45)) +
  scale_y_continuous(breaks = c(200, 250, 300, 350, 400, 450)) +
  scale_color_manual(values = c("#264653", "#2A9D8F", "#D5A220", "#E76F51")) +
  theme_minimal()+
  theme(
    legend.position = c(.63,.84),
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10, vjust=-1), axis.title.y = element_text(size = 10), 
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10),
    strip.text.x = element_text(size = 10)) +
  theme(aspect.ratio = 1) +
  labs(color = "Total Child(ren)") +
  ggtitle("Age Specific Net Housing Costs") +
  xlab("Age") +
  ylab("Monthly Net Housing Costs (£)") +
  ggsave("paper2_age_specific_housing_totalchild_19-01-22.png", dpi = 300)

# -------------------------------------------------------------------------
# Childcare Costs ---------------------------------------------------------
# -------------------------------------------------------------------------
# After some initial study it is clear that the variable "weekcc" is low in total count
# !!!In fact, only 22 men have a non-zero!!! - This analysis won't work for me
agespeccc <- agespec %>% 
  filter(!is.na(weekcc)) %>%
  select (pid, sex, ageyr, parity, totalchild2, xpchcf, ccfop, weekcc) %>% 
  arrange(sex, ageyr, totalchild2) %>% 
  filter(sex == 2) #Remove men, see lower code block
agespeccc %>% count(xpchcf)

# Attempt to make a pie chart
# agespecccpie <- agespeccc %>% 
#   group_by(ccfop) %>% 
#   count(ccfop) %>% 
#   arrange(desc(ccfop)) %>%
#   mutate(prop = n / sum(n) *100) %>%
#   mutate(ypos = cumsum(prop)- 0.5*prop ) %>% 
#   mutate(ccfop = fct_relevel(ccfop, c("not used", "free", "paid")))
# mutate(numobs.cat = fct_relevel(numobs.cat, c("High", "Medium", "Low", "One")))


agespecccpie %>% 
  ggplot(aes(x="", y=n, fill=ccfop)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_minimal() + 
  # theme(legend.position="none") +
  # geom_text(aes(y = ypos, label = ccfop), color = "white", size=6) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Distribution Childcare Use") +
  labs(fill = "Childcare Type") +
  xlab("") +
  ylab("") +
  ggsave("paper2_childcare_pie_free_paid_19-01-22.png", dpi = 300)

agespecccmen <- agespec %>% 
  filter(!is.na(weekcc), weekcc != 0) %>%
  select (pid, sex, ageyr,xpchcf, totalchild2, weekcc) %>% 
  arrange(sex, ageyr, totalchild2) %>% 
  filter(sex == 1)

# There are 3827 valid observations for women
## Natural this increases by age
agespecccwomen <- agespec %>% 
  filter(!is.na(weekcc), weekcc != 0) %>%
  select (pid, sex, ageyr, parity, totalchild2, xpchcf, ccfop, weekcc) %>% 
  arrange(sex, ageyr, totalchild2) %>% 
  filter(sex == 2)
summary(agespecccwomen$weekcc)
## Comment line 2 to get this measure
# agespecccwomen %>% count(ccfop)

agespecccwomen %>% 
  # mutate(parity.chr = as.character(parity)) %>% 
  ggplot(aes(x = ageyr)) +
  geom_histogram(binwidth = 1, fill = "#264653", col = "grey", position  = "identity") +
  scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45)) +
  scale_y_continuous(breaks = c(50, 150, 250)) +
  theme_minimal()+
  theme(
    # legend.position = c(.63,.84),
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 12, vjust=-1), axis.title.y = element_text(size = 12), 
    # legend.key.size = unit(0.3, 'cm'),
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 8),
    axis.text = element_text(size = 12),
    strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  # labs(color = "Total Child(ren)") +
  ggtitle("Distribution of Women that Pay for Childcare") +
  xlab("Age") +
  ylab("") +
  ggsave("paper2_childcare_women_histrogram_19-01-22.png", dpi = 300)

#This DF looks at the average cost by age for those who use paid childcare
agespeccc2 <- agespeccc %>% 
  group_by(ageyr) %>% 
  summarise(agespecmean = mean(weekcc), agespecsd = sd(weekcc), IQR(weekcc)) %>%
  mutate(low = agespecmean - agespecsd) %>% 
  mutate(high = agespecmean + agespecsd)

