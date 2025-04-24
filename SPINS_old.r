library(data.table)
library(tidyverse)
library(lubridate) 
library(magrittr)
library(odbc)
library(DBI)

options(scipen = 999)

## Database Connection to LOC ##
con <- dbConnect(odbc::odbc(), "LOC", Database = "STORESQL", uid = "drodriguez", pwd = "ksiry_92")

## LOC DB field names ##
Fields <- fread("/scripts/database/files/Fields.csv") 

iteminfo <- dbReadTable(con, "OBJ_TAB")
colnames(iteminfo) <- make.unique(Fields$DESCRIPTOR[match(colnames(iteminfo), Fields$FIELD, nomatch=0)] )
iteminfo %<>% mutate(UPC = as.numeric(`UPC code`)) %>% select(UPC)

spins <- fread("files/GoodData.csv") %>% 
  mutate(UPC = ifelse(`UPC EAN13` >= 1000000000000,
                      `UPC EAN13`,
                      as.numeric(str_replace_all(UPC, "-", "")))) %>% 
  filter(UPC %in% iteminfo$UPC) %>% 
  select(UPC, SPINSdollars = Dollars, SPINSunits = Units, SPINSarp = `ARP, Non-Promo`,
         SPINSavgDollars = `Average Weekly Dollars Per Store Selling Per Item`,
         SPINSavgUnits = `Average Weekly Units Per Store Selling Per Item`)


write.csv(spins, "files/SPINS.csv", row.names = F)  

