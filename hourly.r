library(data.table)
library(tidyverse)
library(lubridate) 
library(hms)
library(plotly)
library(janitor)
library(magrittr)
library(odbc)
library(DBI)

options(scipen = 999)

my_db <- src_sqlite("/tlogs/TLog_DB", create = FALSE)
start.date <- as.numeric(mdy("2-12-2023"))
end.date <- as.numeric(mdy("2-25-2023"))

t_ord <- tbl(my_db,"SaleTotal" ) %>%
  filter(Date >= start.date & Date <= end.date ) %>%
  collect() %>% 
  mutate(Time = as_hms(floor(Time/10000)*3600 + (floor(Time/100) %% 100 * 60))) %>% 
  mutate(Date = as_date(Date)) %>% 
  mutate(Time = floor_date(as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"), "15 minutes")) %>%  # "hour")) %>% 
  mutate(Time = force_tz(Time, "UTC")) %>% 
  group_by(`Store Name`, Time) %>% 
  summarise(Sales = sum(PositiveSaleAMT)) %>% 
  ungroup() 

`Store Name` <- as.factor(c("Dearborn", "Berkley", "Livonia", "Plymouth"))
Time <-  seq(ymd_hm('2023-02-12 00:00'),ymd_hm('2023-02-25 23:45'), by = '15 mins')
all_comb <- tidyr::crossing(`Store Name`, Time) %>% 
  left_join(t_ord)
#write.csv(Orders, "Hourly.csv", row.names = F)
t_ord <- all_comb %>% 
  mutate(TimeRange = interval(Time, Time + minutes(15))) # minutes(60))) 

t_lab <- fread("files/labor.csv") %>% 
  #filter(`Dist Department Desc` == "Mgmt Front End" | `Dist Department Desc` == "Cashiers") %>% 
  filter(`Dist Department Desc` != "Office") %>% 
  filter(!grepl("Director", `Allocation`)) %>% 
  filter(!grepl("Production", `Allocation`)) %>% 
  filter(EarnHours > 0) %>% 
  filter(EarnCode != "PTO") %>% 
  mutate(Start = round_date(ymd_hm(InPunchTime),  "15 minutes"),  # "hour")) 
         End = round_date(ymd_hm(OutPunchTime),  "15 minutes")) %>%  # "hour")) 
  mutate(TimeRange = interval(Start, End))
  
alldata <- iris[0,0]
for (store in c("Dearborn", "Berkley", "Livonia", "Plymouth")) {
  Orders <- t_ord %>% filter(`Store Name` == store)
  Orders$Count <- NA
  labor <- t_lab %>% filter(`Dist Location Desc` == store) 
  for (i in 1:nrow(Orders)) {
    t <- Orders[i,2]
    Orders[i,5] <- sum(t$Time %within% labor$TimeRange)
  }
  Orders$Sal <- NA
  labor <- t_lab %>% filter(`Dist Location Desc` == store) %>% filter(`Pay Class` != "HRL")
  for (i in 1:nrow(Orders)) {
    t <- Orders[i,2]
    Orders[i,6] <- sum(t$Time %within% labor$TimeRange)
  }
  Orders$Hrl <- NA
  labor <- t_lab %>% filter(`Dist Location Desc` == store)  %>% filter(`Pay Class` == "HRL")
  for (i in 1:nrow(Orders)) {
    t <- Orders[i,2]
    Orders[i,7] <- sum(t$Time %within% labor$TimeRange)
  }
  
  Orders$`Store Name` <- store
  alldata <- alldata %>% bind_rows(Orders)
}

alldata %<>% mutate(Date = as.Date(Time), Time = as_hms(Time))
write.csv(alldata, "files/Hourly.csv", row.names = F)


# t <- alldata %>% filter(wday(Date) == 7)
