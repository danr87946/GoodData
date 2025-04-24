library(data.table)
library(tidyverse)
library(lubridate) 
library(magrittr)
library(odbc)
library(DBI)

options(scipen = 999)

store <- fread("/scripts/inputs/dependencies/store_numbers.csv") %>% 
  rename(`Store Num` = Store,
         Store = `Store Name`)

my_db <- src_sqlite("/tlogs/Sales_DB", create = FALSE) # create = TRUE creates a new database

Depts  <- tbl(my_db,"dept_totals") %>%
  collect() %>%
  mutate(`Sub-Department` = str_to_title(Department)) %>% 
  mutate(`Sub-Department` = ifelse(`Sub-Department` == "Prepack Salads", "PrePack Salads", `Sub-Department`)) %>% 
  mutate(Date = as.Date(Date, origin = "1970-01-01")) %>%
  group_by(Store, `Sub-Department`, Date, Source) %>% 
  summarize(Sales = sum(Sales), Transactions = sum(Transactions), Items = sum(Items)) %>% 
  mutate(Transactions = ifelse(is.na(Transactions),0,Transactions),
         Items = ifelse(is.na(Items),0,Items)) %>% 
  left_join(store) %>% 
  ungroup() %>% 
  select(-Store) %>% 
  mutate(Source = case_when(Source == "Internet" ~ "Internet",
                            Source == "Mercato" ~ "Mercato",
                            Source == "eGrowcery" ~ "eGrowcery",
                            Source == "LOC" ~ "Registers",
                            Source == "Accounting" ~ "Registers",
                            TRUE ~ "Registers")) %>% 
  select(Date, Items, Sales, Source, `Store Num`, `Sub-Department`, Transactions)

write.csv(Depts, "files/sales.csv", row.names = F)

