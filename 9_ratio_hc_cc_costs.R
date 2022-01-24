#Coded by: Brian Buh
#Started on: 24.01.2022
#Last Updated: 


library(tidyverse)
library(haven)
library(lubridate)


#Use the dataframe incfert

###########################################################################
# Create and test ratios of housing and childcare costs -------------------
###########################################################################

# Step 1 - create a DF; remove individuals younger than 20 and older than 45
incfert.cut <- incfert %>% 
  select(pid, wave, rownum, sex, age, ageyr, event, parity, parity.cat, n_event, totalchild,
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

#Step 2 - Look at counts of most important variables
incfert.cut %>% count(event) #6057
incfert.cut %>% count(totalchild2)
incfert.cut %>% distinct(pid, .keep_all = TRUE) %>% count(totalchild2) #6887 Men, 6273 Women
incfert.cut %>% distinct(pid) #13160
incfert.cut %>% distinct(pid, .keep_all = TRUE) %>% count(sex) #6887 Men, 6273 Women
incfert.cut %>% count(sex)
incfert.cut %>% count(is.na(weekinc.imp)) #1971 NA
incfert.cut %>% count(is.na(monthhc)) #1919 NA
incfert.cut %>% count(is.na(weekcc), sex) #139 NA
## How many of the NAs in HC/CC are due to NA in Income
incfert.cut %>% filter(is.na(weekinc.imp)) %>% count(is.na(monthhc)) #1144 NA in both Inc/HC, 827 unaccounted
incfert.cut %>% filter(is.na(weekinc.imp)) %>% count(is.na(weekcc)) #None are accounted for (so 139 NA)
incfert.cut %>% count(ageyr) #We see a gradual decrease with age with 45 being about 1/3 of 20
table(incfert.cut$ageyr)

## Histogram of the age distribution
incfert.cut %>% 
  mutate(sex = as.character(sex)) %>%
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>%
  ggplot(aes(x = ageyr, fill = sex)) + 
  geom_histogram(binwidth = 1, col = "grey") +
  scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45)) +
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
  ggtitle("Distribution of Age") +
  xlab("Age") +
  ylab("") +
  ggsave("paper2_count_age_histrogram_24-01-22.png", dpi = 300)

# fill = "#264653"

# Step 3 - create ratios
ratios <- incfert.cut %>% 
  mutate(ccratio = weekcc/weekinc.imp) %>% 
  # convert monthly to weekly housing costs
  mutate(weekhc = ((monthhc*12)/365)*7) %>% 
  mutate(hcratio = weekhc/weekinc.imp)

# Step 4 - Looking at stats of new variables
summary(ratios$ccratio)
test <- ratios %>% filter(ccratio != 0, sex == 2) #3827 Observations
test2 <- test %>% filter(ccratio >= 0 & ccratio <= 1) #Remove outliers not in the normal range (18 Obs.)
summary(ratios$weekhc)
summary(ratios$hcratio)
test3 <- ratios %>% filter(!is.na(hcratio)) #77358 Observations (3.5% are NA)
test4 <- test3 %>% filter(hcratio >= 0 & hcratio <= 1) #75656 Observations (2.2% are outside the expected range)
test5 <- test4 %>% filter(hcratio == 0) %>% #11964 observations pay nothing for housing
  count(ageyr, totalchild2)

##Histogram ratio childcare
test2 %>% 
  ggplot(aes(x = ccratio)) +
  geom_histogram(fill = "#264653", col = "grey", position  = "identity") +
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
  ggtitle("Distribution of Ratio of Childcare to Weekly Net HH Inc") +
  xlab("Ratio of Childcare Costs") +
  ylab("") +
  ggsave("paper2_ratio_childcare_histrogram_24-01-22.png", dpi = 300)
#The histrogram shows that the ratios are positively skewed (Looking good!)

##Histogram ratio housing costs
test4 %>% 
  ggplot(aes(x = hcratio)) +
  geom_histogram(fill = "#264653", col = "grey", position  = "identity") +
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
  ggtitle("Distribution of Ratio of Housing to Weekly Net HH Inc") +
  xlab("Ratio of Housing Costs") +
  ylab("") +
  ggsave("paper2_ratio_housing_costs_histrogram_24-01-22.png", dpi = 300)
#It ends with an "Zero-inflated" positive skew (as expected!)

# -----------------------------------------------------------------------------
# Step 5 - Making Age, Sex and Parity Specific Costs -------------------------
# -----------------------------------------------------------------------------
##First, Childcare
agespecratiocc <- test2 %>% 
  group_by(ageyr, totalchild2) %>% 
  summarise(meancc = mean(ccratio))

agespecratiocc %>% 
  filter(totalchild2 != "4+", totalchild2 != 0) %>% 
  # mutate(sex = as.character(sex)) %>% 
  # mutate(sex = recode(sex,
  #                     "1" = "Men",
  #                     "2" = "Women")) %>% 
  ggplot(aes(x = ageyr, y = meancc, color = totalchild2)) +
  geom_smooth(size = 2) +
  # facet_wrap(~sex) +
  scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45)) +
  # scale_y_continuous(breaks = c(200, 250, 300, 350, 400, 450)) +
  # "#264653", 
  scale_color_manual(values = c("#2A9D8F", "#D5A220", "#E76F51")) +
  theme_minimal()+
  theme(
    legend.position = c(.85,.9),
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 12, vjust=-1), axis.title.y = element_text(size = 12), 
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 12),
    strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  labs(color = "Total Child(ren)") +
  ggtitle("Age Specific Ratio of Childcare Costs to Net HH Inc") +
  xlab("Age") +
  ylab("Ratio of Weekly Childcare Costs") +
  ggsave("paper2_age_specific_ratiocc_totalchild_24-01-22.png", dpi = 300)



##Second, Housing
agespecratiohc <- test4 %>% 
  group_by(ageyr, sex, totalchild2) %>% 
  summarise(meancc = mean(hcratio))

agespecratiohc %>% 
  filter(totalchild2 != "4+") %>% 
  mutate(sex = as.character(sex)) %>%
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>%
  ggplot(aes(x = ageyr, y = meancc, color = totalchild2)) +
  geom_smooth(size = 1.5) +
  facet_wrap(~sex) +
  scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45)) +
  # scale_y_continuous(breaks = c(200, 250, 300, 350, 400, 450)) +
  scale_color_manual(values = c("#264653", "#2A9D8F", "#D5A220", "#E76F51")) +
  theme_minimal()+
  theme(
    legend.position = c(.77,.25),
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 12, vjust=-1), axis.title.y = element_text(size = 12), 
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 12),
    strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  labs(color = "Total Child(ren)") +
  ggtitle("Age Specific Ratio of Housing Costs to Net HH Inc") +
  xlab("Age") +
  ylab("Ratio of Weekly Housing Costs") +
  ggsave("paper2_age_specific_ratiohc_totalchild_24-01-22.png", dpi = 300)
