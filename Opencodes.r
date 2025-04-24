library(data.table)
library(tidyverse)
library(lubridate) 
library(janitor)
library(magrittr)
library(odbc)
library(DBI)
library(scales)

options(scipen = 999)

my_db <- src_sqlite("/tlogs/TLog_DB", create = FALSE)

store <- fread("/scripts/inputs/dependencies/store_numbers.csv") %>% 
  rename(`Store Num` = Store)

start.date <- as.numeric(today()-30)
end.date <- as.numeric(today())

## Database Connection to LOC ##
con <- dbConnect(odbc::odbc(), "LOC", Database = "STORESQL", uid = "drodriguez", pwd = "ksiry_92")

## LOC DB field names ##
Fields <- fread("/scripts/database/files/Fields.csv") 

priceinfo <- dbReadTable(con, "PRICE_TAB")
colnames(priceinfo) <- make.unique(Fields$DESCRIPTOR[match(colnames(priceinfo), Fields$FIELD, nomatch=0)] )
priceinfo %<>% filter(`Target Identifier` == "PAL") %>% 
  mutate(UPC = as.numeric(`UPC code`)) %>%
  filter(Price == 0 | is.na(Price))

iteminfo <- dbReadTable(con, "OBJ_TAB")
colnames(iteminfo) <- make.unique(Fields$DESCRIPTOR[match(colnames(iteminfo), Fields$FIELD, nomatch=0)] )
iteminfo %<>% select(`UPC code`, `Brand description`,`Size description`, `Expanded description`)

posinfo <- dbReadTable(con, "POS_TAB")
colnames(posinfo) <- make.unique(Fields$DESCRIPTOR[match(colnames(posinfo), Fields$FIELD, nomatch=0)] )
posinfo %<>% select(`UPC code`, Department = `Sub-Department code`) 

itemlist <- iteminfo %>% 
  left_join(posinfo) %>% 
  mutate(UPC = as.numeric(`UPC code`)) %>% 
  rename(Brand = `Brand description`,
         `Long Description` = `Expanded description`) %>% 
  mutate(`Long Description` =  iconv(`Long Description`, "UTF-8", "UTF-8", sub='')) %>% 
  mutate(Brand =  iconv(Brand, "UTF-8", "UTF-8", sub='')) %>% 
  replace(., is.na(.), "")  %>% 
  mutate(Description = trimws(paste(Brand, `Long Description`, sep = " "))) %>% 
  select(UPC, Description) 


Orders <- tbl(my_db,"TenderRecord" ) %>%
  filter(Date >= start.date & Date <= end.date ) %>%
  collect() %>% 
  unite(ID, `Store Name`, Date, TransactionID) %>% 
  select(ID) 

SaleHeader <- tbl(my_db,"TransHead" ) %>%
  filter(Date >= start.date & Date <= end.date ) %>%
  collect() %>% 
  mutate(TransactionID = as.numeric(TransactionID), Cashier = `User short name`) %>% 
  select(TransactionID, Cashier)

UPCRecord <- tbl(my_db,"UPCRecord") %>%
  filter(Date >= start.date & Date <= end.date ) %>%
  collect() %>%
  filter(UPC == 0 | is.na(as.numeric(UPC)) | UPC %in% priceinfo$UPC) %>% 
  unite(ID, `Store Name`, Date, TransactionID, remove = FALSE) %>% 
  filter(ID %in% Orders$ID) %>% 
  left_join(SaleHeader) %>% 
  mutate(Quantity = ifelse(ScaleWeight == 0, Quantity, ScaleWeight/100)) %>% 
  mutate(OpenPrice = dollar(OriginalPrice/100)) %>% 
  select(`Store Name`, Date, OpenSales = SoldPrice, UPC, OpenQty = Quantity, Cashier, Department, OpenPrice) %>%
  mutate(`Store Name` = factor(`Store Name`, c("Dearborn", "Berkley", "Livonia", "Plymouth"))) %>%
  mutate(Date = as.Date(Date, origin = "1970-01-01")) 

deptgrp <- fread("/scripts/inputs/dependencies/depts_groups.csv")



OpenData <- UPCRecord %>% 
  left_join(deptgrp) %>% 
  mutate(`Sub-Department` = `Department Name`) %>% 
  left_join(itemlist) %>% 
  mutate(OpenDescription = ifelse(is.na(Description), paste(`Sub-Department`, " Open Department", sep = ""), Description)) %>% 
  select(-UPC, -`Department Name`, -Group, -Department, -Description)

write.csv(OpenData, "files/OpenData.csv", row.names = F)  
