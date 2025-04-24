library(data.table)
library(tidyverse)
library(lubridate) 
library(magrittr)
library(odbc)
library(DBI)

options(scipen = 999)

#my_db <- src_sqlite("/tlogs/TLog_DB", create = FALSE)

dept <- fread("/scripts/inputs/dependencies/depts_groups.csv") %>% 
  mutate(Department = ifelse(Group == "Frozen", "Grocery", Group)) %>% 
  mutate(Department = ifelse(`Department Name` == "Home Goods", "Home Goods", Department)) %>%
  select(Department, `Sub-Department` = `Department Name`)

write.csv(dept, "files/dept.csv", row.names = F)

## Database Connection to LOC ##
con <- dbConnect(odbc::odbc(), "LOC", Database = "STORESQL", uid = "drodriguez", pwd = "ksiry_92")

## LOC DB field names ##
Fields <- fread("/scripts/database/files/Fields.csv") 

catinfo <- dbReadTable(con, "CAT_TAB")
colnames(catinfo) <- make.unique(Fields$DESCRIPTOR[match(colnames(catinfo), Fields$FIELD, nomatch=0)] )
catinfo %<>% select(`Category code`, `Category descrptor`) %>% 
  mutate(Main_Code = floor(`Category code`/10000)*10000) 

catinfo %<>% 
  inner_join(catinfo, by = c("Category code" = "Main_Code"), suffix = c("_p", "_c")) %>%
  select(`Category code` = `Category code_c`, Category = `Category descrptor_p`, `Sub-Category` = `Category descrptor_c`)

cats <- catinfo %>% 
  mutate(Category = str_to_title(Category),
         `Sub-Category` = str_to_title(`Sub-Category`)) %>% 
  select(-`Category code`)

write.csv(cats, "files/cats.csv", row.names = F)

store <- fread("../inputs/dependencies/store_numbers.csv") %>% 
  mutate(`Store Order` = as.numeric(Store)) %>% 
  rename(`Store Num` = Store,
         Store = `Store Name`)

write.csv(store, "files/store.csv", row.names = F)

iteminfo <- dbReadTable(con, "OBJ_TAB")
colnames(iteminfo) <- make.unique(Fields$DESCRIPTOR[match(colnames(iteminfo), Fields$FIELD, nomatch=0)] )
iteminfo %<>% select(`UPC code`, `Brand description`, `Category code`, `Size description`, `Expanded description`)

posinfo <- dbReadTable(con, "POS_TAB")
colnames(posinfo) <- make.unique(Fields$DESCRIPTOR[match(colnames(posinfo), Fields$FIELD, nomatch=0)] )
posinfo %<>% select(`UPC code`, Department = `Sub-Department code`, `Status code`) 

likeinfo <- dbReadTable(con, "LIKE_TAB")
colnames(likeinfo) <- make.unique(Fields$DESCRIPTOR[match(colnames(likeinfo), Fields$FIELD, nomatch=0)] )
likeinfo %<>% rename(`Link Code` = `Like code description`)

deptgrp <- fread("/scripts/inputs/dependencies/depts_groups.csv")

## Vendor Cost info ##
vend_costs <- dbReadTable(con, "COST_TAB")
colnames(vend_costs) <- make.unique(Fields$DESCRIPTOR[match(colnames(vend_costs), Fields$FIELD, nomatch=0)] )

vend_list <-  dbReadTable(con, "VENDOR_TAB")
colnames(vend_list) <- make.unique(Fields$DESCRIPTOR[match(colnames(vend_list), Fields$FIELD, nomatch=0)] )
vend_list %<>% select(`Vendor id`, `Vendor name`)

vcdata <- vend_costs %>% 
  mutate(UPC = as.numeric(`UPC code`)) %>% 
  left_join(vend_list) %>% 
  filter(!is.na(`Vendor name`)) %>% 
  filter(!(`Vendor id` %in% c(20,210,198,201,448) & `Last change date` < ymd(today()-weeks(9)))) %>%
  mutate(`Unit Cost` = `Base cost` / `Case size`) %>% 
  select(`Vendor Name` = `Vendor name`, UPC, `Unit Cost`, `Cost Date` = `Last change date`, Pack = `Case size`) %>% 
  filter(`Unit Cost` != 0) %>% 
  mutate(`Vendor Name` = case_when(`Vendor Name` == "BOARSHEAD/MI DELI PROVISIONS" ~ "BOARSHEAD",
                                   `Vendor Name` == "FRITO LAY/PEPSI" ~ "FRITO LAY",
                                   `Vendor Name` == "EYASMEEN/SAJOUNA" ~ "EYASMEEN",
                                   `Vendor Name` == "EDY/NESTLE" ~ "EDY&NESTLE",
                                   TRUE ~ `Vendor Name`)) %>% 
  arrange(UPC, `Unit Cost`) %>% 
  group_by(UPC) %>% 
  filter(`Unit Cost` == min(`Unit Cost`)) %>%
  filter(1:n() == 1) %>% 
  mutate(`Unit Cost` = round(`Unit Cost`,2),
         `Vendor Name` = str_to_title(`Vendor Name`),
         `Cost Date` = format(`Cost Date`, "%m-%d-%Y")) 

priceinfo <- dbReadTable(con, "PRICE_TAB")
colnames(priceinfo) <- make.unique(Fields$DESCRIPTOR[match(colnames(priceinfo), Fields$FIELD, nomatch=0)] )
priceinfo %<>% filter(`Target Identifier` == "PAL") %>% 
  mutate(UPC = as.numeric(`UPC code`)) %>% 
  mutate(Price = round(Price/`Price qty`,2)) %>% 
  mutate(Price = ifelse(is.na(Price), 0, Price)) %>% 
  select(UPC, Price, `Like code`) 

spins <- fread("files/GoodData.csv") %>% 
  mutate(UPC = ifelse(`UPC EAN13` >= 1000000000000,
                      `UPC EAN13`,
                      as.numeric(str_replace_all(UPC, "-", "")))) %>% 
  filter(UPC %in% as.numeric(iteminfo$UPC)) %>% 
  select(UPC, SPINSdollars = Dollars, SPINSunits = Units, SPINSarp = `ARP, Non-Promo`,
         SPINSavgDollars = `Average Weekly Dollars Per Store Selling Per Item`,
         SPINSavgUnits = `Average Weekly Units Per Store Selling Per Item`)

iteminfo <- iteminfo %>% 
  left_join(posinfo) %>% 
  left_join(catinfo) %>% 
  left_join(deptgrp) %>%
  mutate(UPC = as.numeric(`UPC code`)) %>% 
  rename(Brand = `Brand description`,
         Size = `Size description`,
         `Long Description` = `Expanded description`) %>% 
  mutate(Description =  iconv(`Long Description`, "UTF-8", "UTF-8", sub='')) %>% 
  mutate(Size =  iconv(Size, "UTF-8", "UTF-8", sub='')) %>% 
  mutate(Brand =  iconv(Brand, "UTF-8", "UTF-8", sub='')) %>% 
  mutate(`Sub-Category` = str_to_title(`Sub-Category`)) %>% 
  mutate(Status = case_when(`Status code` == 1 ~ "Discontinued",
                            `Status code` == 2 ~ "Seasonal",
                            TRUE ~ "Active")) %>% 
  select(UPC, `Sub-Category`, Brand, Description, Size, `Sub-Department` = `Department Name`, Status) %>% 
  left_join(vcdata) %>% 
  left_join(priceinfo) %>% 
  left_join(likeinfo) %>% 
  left_join(spins) %>% 
  mutate(`Unit Cost` = ifelse(`Unit Cost` == 0, "", `Unit Cost`),
         Price = ifelse(Price == 0, "", Price)) %>% 

  replace(., is.na(.), "") 

itemlist <- iteminfo %>% 
  select(UPC, `Sub-Category`, Brand, Description, Size, `Sub-Department`, `Vendor Name`, `Cost Date`, Pack, `Link Code`, Status)

write.csv(itemlist, "files/ItemList.csv", row.names = F)  

itemlist2 <- iteminfo %>% 
  select(UPC, `Unit Cost`, Price, SPINSdollars, SPINSunits, SPINSarp, SPINSavgDollars, SPINSavgUnits)

write.csv(itemlist2, "files/ItemList2.csv", row.names = F)  

old_cost_list <- fread("cost_db/cost_list.csv") %>% 
  mutate(UPC = as.numeric(UPC)) %>% 
  mutate(`Unit Cost` = as.numeric(`Unit Cost`)) %>% 
  mutate(`Cost Date` = mdy(`Cost Date`))

last_old_cost <- old_cost_list %>% 
  group_by(UPC) %>% 
  filter(`Cost Date` == max(`Cost Date`)) 

cost_list <- iteminfo %>% 
  select(UPC, `Vendor Name`, `Cost Date`, `Unit Cost`) %>% 
  mutate(`Cost Date` = mdy(`Cost Date`)) %>% 
  mutate(`Unit Cost` = as.numeric(`Unit Cost`)) %>% 
  filter(!is.na(`Unit Cost`) & !is.na(`Cost Date`)) %>% 
  anti_join(last_old_cost) %>% 
  bind_rows(old_cost_list)
  
write.csv(cost_list, "cost_db/cost_list.csv", row.names = F)
