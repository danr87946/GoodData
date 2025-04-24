library(data.table)
library(tidyverse)
library(lubridate) 
library(janitor)
library(magrittr)
library(odbc)
library(DBI)

options(scipen = 999)

my_db <- src_sqlite("/tlogs/TLog_DB", create = FALSE)

store <- fread("/scripts/inputs/dependencies/store_numbers.csv") %>% 
  rename(`Store Num` = Store)

start.date <- as.numeric(mdy("01-1-2024"))
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
  mutate(Quantity = ifelse(ScaleWeight == 0, Quantity, ScaleWeight/100)) %>% 
  #mutate(Weight = ScaleWeight/100) %>% 
  select(`Store Name`, Date, SoldPrice, UPC, Quantity, TransactionID) %>%
  mutate(`Store Name` = factor(`Store Name`, c("Dearborn", "Berkley", "Livonia", "Plymouth"))) %>%
  mutate(Date = as.Date(Date, origin = "1970-01-01")) 

t <- UPCRecord %>%
  #mutate(UPC = ifelse(floor(UPC / 100000000) == 298, UPC + 100000000, UPC)) %>%
  group_by(UPC,`Store Name`, Date) %>%
  summarize(Quantity = sum(Quantity), Sales = sum(SoldPrice)) %>% 
  left_join(store) %>%
  ungroup() %>% 
  select(-`Store Name`)

write.csv(t, "files/Movement2024.csv", row.names = F)  

gc()

ev <- fread("files/ItemEvents.csv")

t <- list("files/Movement2017.csv",
          "files/Movement2018.csv",
          "files/Movement2019.csv",
          "files/Movement2020.csv",
          "files/Movement2021.csv",
          "files/Movement2022.csv",
          "files/Movement2023.csv",
          "files/Movement2024.csv") %>%   
  lapply(fread) %>% 
  bind_rows %>% 
  left_join(ev)

write.csv(t, "files/Movement.csv", row.names = F)  

zip::zip("Movement.zip", "Movement.csv", root = "files")  
