library(tidyverse)
library(data.table)
library(lubridate)
library(fpp2)
library(ggplot2)
library(prophet)
library(scales)
library(janitor)
library(timetk)
library(tidyquant)
library(tidyverse)
library(data.table)
library(xgboost)
library(caret) 
my_db <- src_sqlite("/tlogs/Sales_DB", create = FALSE)


store <- fread("/scripts/inputs/dependencies/store_numbers.csv") %>% 
  rename(`Store Num` = Store)
     
deptgrp <- fread("/scripts/inputs/dependencies/depts_groups.csv") %>% 
  mutate(Department = toupper(`Department Name`),
         Group = case_when(`Department Name` == "Bistro" ~ "MISC",           # Separate Bistro from MISC Group
                           `Department Name` == "Coffee Beans" ~ "MISC",
                           `Department Name` == "Coffee Bar" ~ "MISC",
                           Group == "Frozen" ~ "Grocery",
                           Group == "Bar Service" ~ "MISC",# Separate Coffee Beans from MISC Group
                           `Department Name` == "Liquor" ~ "MISC",
                           TRUE ~ Group)) %>% 
  select(-`Department Name`) 

source("Forecasts/Holidays.r")

#### model ####

pred <- iris[0,0]

wkstart <- floor_date(today(), unit = "week") 

full.data <- tbl(my_db,"dept_totals" ) %>%
  collect()  %>% 
  left_join(deptgrp) %>% 
  mutate(Date = as.Date(Date, origin = "1970-01-01")) %>% 
  filter(Date >= "2017-1-1" & Date < wkstart) %>% 
  group_by(Date, Store, Group) %>% 
  summarize(Sales = sum(Sales)) %>% 
  filter(!is.na(Group))

for (str in unique(full.data$Store)) {
  for (grp in unique(full.data$Group)) {
    t <- full.data %>% 
      filter(str == Store & grp == Group) %>% 
      group_by(ds = Date) %>% 
      summarize(y = sum(Sales))
    
    if (str == "Plymouth") {
      m <- prophet(seasonality.mode = 'multiplicative', holidays = holidays_ply, changepoint.prior.scale = 1, changepoint.range = .9) ##, changepoint.range = .9) ### see how this affects things
    } else if (str == "Berkley") {
      m <- prophet(seasonality.mode = 'multiplicative', holidays = holidays_brk, changepoint.prior.scale = 1, changepoint.range = .9) ##, changepoint.range = .9)
    } else {
      m <- prophet(seasonality.mode = 'multiplicative', holidays = holidays, changepoint.prior.scale = 1, changepoint.range = .9) ##, changepoint.range = .9)
    }
    
    m <- fit.prophet(m, t)
    
    future <- make_future_dataframe(m, periods = 182)
    forecast <- predict(m, future) %>% mutate(`Store Name` = str) %>% mutate(Department = grp)  
    
    pred <- forecast %>% 
      filter(ds > (wkstart + 6 )) %>% 
      bind_rows(pred)
  }
}  


dpt <- fread("files/dept.csv") %>% 
  group_by(Department) %>% 
  filter(`Sub-Department` == first(`Sub-Department`))

StorePreds <- pred %>% 
  left_join(store) %>% 
  mutate(DeptPredictedSales = round(yhat,2),
         PredictedLow = round(yhat_lower,2),
         PredictedHigh = round(yhat_upper,2)) %>% 
  select(Date = ds, DeptPredictedSales, `Store Num`, Department) %>% 
  left_join(dpt) %>% 
  mutate(Department = `Sub-Department`) %>% 
  select(-`Sub-Department`)

DeptPreds <- fread("files/DeptPreds.csv") %>% 
  filter(Date < wkstart + 7) %>% 
  mutate(Date = as_date(Date)) %>% 
  bind_rows(StorePreds) %>% 
  filter(Date != mdy("11/28/2024") &
           Date != mdy("12/25/2024") &
           Date != mdy("1/1/2025") &
           Date != mdy("4/20/2025")) # need to figure this one out for the future


write.csv(DeptPreds, "files/DeptPreds.csv", row.names = F)


