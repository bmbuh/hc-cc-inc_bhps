#Coded by: Brian Buh
#Started on: 10.12.2021
#Last Updated:


library(tidyverse)
library(haven)
library(lubridate)

test <- ba_hhresp %>% 
  select(ba_hhneti, ba_hhnetde, ba_hhnetde2)

test2 <- br_hhresp %>% 
  select(br_hhneti, br_hhnetde, br_hhnetde2)



