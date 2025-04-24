library(tidyverse)
library(lubridate)
library(DBI)
library(data.table)

options(scipen = 999)

con <- dbConnect(odbc::odbc(), "LOC", Database = "STORESQL", uid = "drodriguez", pwd = "ksiry_92")

store <- fread("../inputs/dependencies/store_numbers.csv") %>% 
  rename(`Store Num` = Store,
         Store = `Store Name`)

## LOC DB field names ##
Fields <- fread("/scripts/database/files/Fields.csv") 

vend_costs <- dbReadTable(con, "COST_TAB")
colnames(vend_costs) <- make.unique(Fields$DESCRIPTOR[match(colnames(vend_costs), Fields$FIELD, nomatch=0)] )

vcdata <- vend_costs %>% 
  mutate(UPC = as.numeric(`UPC code`)) %>% 
  filter(!(`Vendor id` %in% c(20,210,198,201,448) & `Last change date` < ymd(today()-weeks(9)))) %>%
  mutate(`Unit Cost` = `Base cost` / `Case size`) %>% 
  filter(`Unit Cost` != 0) %>% 
  arrange(UPC, `Unit Cost`) %>% 
  group_by(UPC) %>% 
  filter(`Unit Cost` == min(`Unit Cost`)) %>%
  filter(1:n() == 1) %>% 
  select(UPC, Pack = `Case size`) %>% 
  mutate(Pack = ifelse(is.na(Pack),1,Pack))


my_db <- src_sqlite("/tlogs/OrderScans_DB", create = FALSE)

bs_data <- tbl(my_db,"BS_Data") %>%
  collect() %>% 
  rename(BackstockScan = Quantity) %>% 
  filter(BackstockScan < 10000) %>% 
  mutate(Wk = floor_date(as_date(Date), "week", week_start = 2)) %>% 
  group_by(Store, UPC, Wk) %>% 
  summarize(BackstockScan = sum(BackstockScan)) %>% 
  left_join(vcdata) %>% 
  mutate(BackstockQty = BackstockScan * Pack)

scan_data <- tbl(my_db,"Order_Data") %>%
  collect() %>% 
  distinct() %>% 
  rename(ScanQty = Quantity) %>% 
  filter(ScanQty < 10000) %>% 
  mutate(Wk = floor_date(as_date(Date), "week", week_start = 2)+5) %>% 
  group_by(Store, UPC, Wk) %>% 
  filter(Date == max(Date)) %>% 
  select(-Date) %>% 
  full_join(bs_data) %>% 
  mutate(BackstockQty = ifelse(is.na(BackstockQty),0,BackstockQty),
         ScanQty = ifelse(is.na(ScanQty),0,ScanQty)) %>% 
  mutate(ScanQty = sum(BackstockQty + ScanQty)) %>% 
  select(-BackstockQty, -BackstockScan, -Pack) %>% 
  left_join(store) %>% 
  ungroup() %>% 
  select(-Store) %>% 
  filter(!is.na(Wk))

write.csv(scan_data, "files/OrderScans.csv", row.names = F)  
