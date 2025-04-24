library(data.table)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(magrittr)
library(DBI)

options(scipen = 999)

store <- fread("../inputs/dependencies/store_numbers.csv") %>% 
  rename(`Store Num` = Store)

my_db <- src_sqlite("/tlogs/Shrink_DB", create = FALSE) # create = TRUE creates a new database

Current  <- tbl(my_db,"Shrink") %>% 
  collect() %>% 
  mutate(Date = as_date(Date)) %>% 
  filter(Reason != "Transfer" & Reason != "Sample") %>% 
  group_by(UPC, `Store Name`, Reason, Date) %>% 
  summarize(Shrink = sum(Shrink)) %>% 
  left_join(store) %>% 
  ungroup() %>% 
  select(-`Store Name`)


write.csv(Current, "files/Shrink.csv", row.names = F)
