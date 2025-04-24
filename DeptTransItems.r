library(data.table)
library(tidyverse)
library(lubridate) 
library(janitor)
library(magrittr)
library(odbc)
library(DBI)

options(scipen = 999)

dept <- fread("/scripts/inputs/dependencies/depts_groups.csv") %>%
  mutate(Group = ifelse(Group == "Frozen", "Grocery", Group)) %>%
  mutate(Group = ifelse(Group == "Cheese", "Spec Cheese", Group)) %>%
  mutate(Group = ifelse(Group == "Misc", "Coffee", Group)) %>%
  mutate(Group = ifelse(Group == "Baskets", "Gift Basket", Group)) %>%
  mutate(Group = ifelse(Group == "Prep", "Prep Foods", Group)) %>%
  mutate(Group = ifelse(`Department Name` == "Home Goods", "Home Goods", Group)) %>%
  mutate(`Department Name` = Group) %>%
  select(Department, `Department Name`)


my_db <- src_sqlite("/tlogs/TLog_DB", create = FALSE)

start.date <- as.numeric(mdy("1-1-2024"))
end.date <- as.numeric(today())

Orders <- tbl(my_db,"TenderRecord" ) %>%
  filter(Date >= start.date & Date <= end.date ) %>%
  collect() %>% 
  unite(ID, `Store Name`, Date, TransactionID) %>% 
  select(ID) 

UPCRecord <- tbl(my_db,"UPCRecord") %>%
  filter(RecordCode != "V" & RecordCode != ".") %>%
  filter(Date >= start.date & Date <= end.date ) %>%
  collect() %>%
  unite(ID, `Store Name`, Date, TransactionID, remove = FALSE) %>% 
  filter(ID %in% Orders$ID) %>% 
  select(`Store Name`, Date, SoldPrice, Department, TransactionID) %>%
  mutate(`Store Name` = factor(`Store Name`, c("Dearborn", "Berkley", "Livonia", "Plymouth"))) %>%
  mutate(Date = as.Date(Date, origin = "1970-01-01")) 

t <- UPCRecord %>%
  left_join(dept) %>% 
  group_by(Store = `Store Name`, Date, `Department Name`) %>% 
  summarise(Depttransactions = n_distinct(TransactionID), Deptitems = n(), Deptsales = sum(SoldPrice))

write.csv(t, "files/Trans2024.csv", row.names = F)  

gc()

t <- list("files/Trans2017.csv",
          "files/Trans2018.csv",
          "files/Trans2019.csv",
          "files/Trans2020.csv",
          "files/Trans2021.csv",
          "files/Trans2022.csv",
          "files/Trans2023.csv",
          "files/Trans2024.csv") %>%
  lapply(fread) %>%
  bind_rows %>%
  mutate(`Department Name` = stringr::str_to_title(`Department Name`))  ## just fixing my screw up

write.csv(t, "files/Dept_Trans_Fin.csv", row.names = F)
