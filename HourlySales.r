library(odbc)
library(DBI)
library(tidyverse)
library(hms)
library(lubridate)
library(scales)
library(data.table)

start.date <- as.numeric(mdy("1/1/2020"))

my_db <- src_sqlite("/tlogs/TLog_DB", create = FALSE)

store <- fread("/scripts/inputs/dependencies/store_numbers.csv") %>% 
  rename(`Store Num` = Store)

Hourly <- tbl(my_db,"SaleTotal" ) %>%
  filter(Date >= start.date) %>%
  collect() %>% 
  filter(Time >= 70000 & Time <= 220000) %>% 
  mutate(Time = as_hms(floor(Time/10000)*3600)) %>% 
  mutate(Date = as_date(Date)) %>% 
  group_by(`Store Name`, Date, Time) %>% 
  summarise(`Hourly Sales` = sum(PositiveSaleAMT)) %>% 
  left_join(store) %>% 
  ungroup() %>% 
  select(-`Store Name`)

write.csv(Hourly, "files/hourlysales.csv", row.names = F)
