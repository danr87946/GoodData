library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)
library(odbc)
library(DBI)
options(scipen = 999)

## LOC DB field names ##
Fields <- fread("/scripts/database/files/Fields.csv") 

con <- dbConnect(odbc::odbc(), "LOC", Database = "STORESQL", uid = "drodriguez", pwd = "ksiry_92")
depts <- dbReadTable(con, "RPT_ITM_D")
colnames(depts) <- make.unique(Fields$DESCRIPTOR[match(colnames(depts), Fields$FIELD, nomatch=0)] )

ItemEvents <- depts %>% 
  mutate(UPC = as.numeric(`UPC code`)) %>% 
  mutate(Date = as_date(`Date end`)) %>% 
  mutate(`Store Num` = as.numeric(`Terminal store`)) %>% 
  filter(`Totalizer number` == 3610 | `Totalizer number` == 3620 | `Totalizer number` == 3630) %>% 
  mutate(Event = case_when(`Totalizer number` == 3610 ~ "Sale",
                           `Totalizer number` == 3620 ~ "TPR",
                           `Totalizer number` == 3630 ~ "INS",
                           TRUE ~ "Reg")) %>% 
  group_by(Date, UPC, `Store Num`) %>% 
  filter(`Totalizer number` == min(`Totalizer number`)) %>% 
  select(Date, Event, UPC, `Store Num`) 

write.csv(ItemEvents, "files/ItemEvents.csv", row.names = F)    
