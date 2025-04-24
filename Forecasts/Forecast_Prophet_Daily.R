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

wkstart <- floor_date(today(), unit = "week")

full.data <- tbl(my_db,"dept_totals" ) %>%
  collect()  %>% 
  left_join(deptgrp) %>% 
  mutate(Date = as.Date(Date, origin = "1970-01-01")) %>% 
  filter(Date >= "2017-1-1" & Date < wkstart - 14) %>% 
  filter(Store == "Berkley" & Group == "Bakery") %>% 
  group_by(Date) %>% 
  summarize(Sales = sum(Sales))

source("Forecasts/Holidays.r")

#### model ####
t <- full.data %>% 
  group_by(ds = Date) %>% 
  summarize(y = sum(Sales))


m <- prophet(seasonality.mode = 'multiplicative', holidays = holidays, changepoint.prior.scale = 1)
m <- fit.prophet(m, t)
    
future <- make_future_dataframe(m, periods = 72)
    
forecast <- predict(m, future)

actuals_tbl <- tbl(my_db,"dept_totals" ) %>%
  collect()  %>% 
  left_join(deptgrp) %>% 
  mutate(Date = as.Date(Date, origin = "1970-01-01")) %>% 
  filter(Date >= wkstart - 14 & Date < wkstart ) %>% 
  filter(Store == "Berkley" & Group == "Bakery") %>% 
  group_by(Date) %>% 
  summarize(Sales = sum(Sales))

pred <- forecast %>% 
  mutate(Date = as_date(ds)) %>% 
  filter(Date >= wkstart - 200) %>% 
  rename(value = yhat)

full.data %>%
  filter(Date >= wkstart - 200) %>% 
  ggplot(aes(Date, Sales)) +
  # Training data
  geom_line(color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]]) +
  # Predictions
  geom_line(aes(y = value), color = palette_light()[[2]], data = pred) +
  geom_point(aes(y = value), color = palette_light()[[2]], data = pred) +
  # Actuals
  geom_line(color = palette_light()[[3]], data = actuals_tbl) +
  geom_point(color = palette_light()[[3]], data = actuals_tbl) +
  # Aesthetics
  theme_tq() +
  labs(title = "Beer Sales Forecast: Time Series Machine Learning",
       subtitle = "Using basic multivariate linear regression can yield accurate results")

    
dyplot.prophet(m, forecast)

prophet_plot_components(m,forecast)
    
error_tbl <- left_join(actuals_tbl, pred) %>%
  rename(actual = Sales, pred = value) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl

test_residuals <- error_tbl$error
test_error_pct <- error_tbl$error_pct * 100 # Percentage error

me   <- mean(test_residuals, na.rm=TRUE)
rmse <- mean(test_residuals^2, na.rm=TRUE)^0.5
mae  <- mean(abs(test_residuals), na.rm=TRUE)
mape <- mean(abs(test_error_pct), na.rm=TRUE)
mpe  <- mean(test_error_pct, na.rm=TRUE)

tibble(me, rmse, mae, mape, mpe) %>% glimpse()

