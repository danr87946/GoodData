library(data.table)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(magrittr)
library(DBI)

options(scipen = 999)

store <- fread("../inputs/dependencies/store_numbers.csv") %>% 
  rename(`Store Num` = Store)

my_db <- src_sqlite("/tlogs/OOS_DB", create = FALSE) # create = TRUE creates a new database

Current  <- tbl(my_db,"OOS") %>% 
  collect() %>% 
  mutate(Date = as_date(Date)) %>% 
  group_by(UPC, `Store Name`, Date) %>% 
  summarize(OOS_Scan = n_distinct(UPC)) %>% 
  left_join(store) %>% 
  ungroup() %>% 
  select(-`Store Name`) 

write.csv(Current, "files/oos.csv", row.names = F)
