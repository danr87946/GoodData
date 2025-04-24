library(rmarkdown)
library(tidyverse)
library(data.table)
library(lubridate)
options(scipen = 999)

my_db <- src_sqlite("/tlogs/Sales_DB", create = FALSE)

store_totals <- tbl(my_db,"store_totals" ) %>%
  collect() %>% 
  mutate(Date = as_date(Date)) %>% 
  group_by(Store, Date) %>% 
  summarize(StoreTransactions = sum(Transactions, na.rm = T), StoreSales = sum(Sales, na.rm = T), StoreItems = sum(Items, na.rm = T)) %>% 
  filter(!(is.na(StoreTransactions) & is.na(StoreSales))) %>% 
  mutate(StoreTransactions = ifelse(is.na(StoreTransactions),0,StoreTransactions)) %>% 
  mutate(StoreItems = ifelse(is.na(StoreItems),0,StoreItems))

write.csv(store_totals, "files/store_trans_gd.csv", row.names = F)
