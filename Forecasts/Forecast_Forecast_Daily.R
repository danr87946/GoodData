library(timetk)
library(tidyquant)
library(tidyverse)
library(data.table)
library(forecast)
library(sweep)

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
  filter(Date >= "2017-1-1" & Date < wkstart - 42) %>% 
  filter(Store == "Berkley" & Group == "Deli") %>% 
  #mutate(Date = floor_date(Date, unit = "week")) %>% 
  #filter(year(Date) != 2020 & year(Date) != 2021) %>% 
  group_by(Date) %>% 
  summarize(Sales = sum(Sales))


full.data %>% 
  ggplot(aes(Date, Sales)) +
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  theme_tq() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Berkley Grocery Sales: 2017 through now")

full.data_ts <- tk_ts(full.data, start = 2013, frequency = 52)

has_timetk_idx(full.data_ts)

fit_arima <- auto.arima(full.data_ts)

fit_arima

sw_tidy(fit_arima)

sw_glance(fit_arima) %>% glimpse()

# sw_augment - get model residuals
sw_augment(fit_arima, timetk_idx = TRUE)

# Plotting residuals
sw_augment(fit_arima, timetk_idx = TRUE) %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") + 
  labs(title = "Residual diagnostic") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_tq()

# Forecast next 12 months
fcast_arima <- forecast(fit_arima, h = 42)

# sw_sweep - tidies forecast output
fcast_tbl <- sw_sweep(fcast_arima, timetk_idx = TRUE)

fcast_tbl

actuals_tbl <- tbl(my_db,"dept_totals" ) %>%
  collect()  %>% 
  left_join(deptgrp) %>% 
  mutate(Date = as.Date(Date, origin = "1970-01-01")) %>% 
  filter(Date >= wkstart - 42 & Date < wkstart ) %>% 
  filter(Store == "Berkley" & Group == "Deli") %>%  
  #mutate(Date = floor_date(Date, unit = "week")) %>% 
  group_by(Date) %>% 
  summarize(Sales = sum(Sales))

# Visualize the forecast with ggplot
fcast_tbl %>%
  filter(index >= wkstart - 200) %>% 
  ggplot(aes(x = index, y = Sales, color = key)) +
  # 95% CI
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  # 80% CI
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  # Prediction
  geom_line() +
  geom_point() +
  # Actuals
  geom_line(aes(x = Date, y = Sales), color = palette_light()[[1]], data = actuals_tbl) +
  geom_point(aes(x = Date, y = Sales), color = palette_light()[[1]], data = actuals_tbl) +
  # Aesthetics
  labs(title = "Grocery Sales Forecast: ARIMA", x = "", y = "Dollars",
       subtitle = "sw_sweep tidies the auto.arima() forecast output") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()

# Investigate test error 
error_tbl <- left_join(actuals_tbl, fcast_tbl, by = c("Date" = "index")) %>%
  rename(actual = Sales.x, pred = Sales.y) %>%
  select(Date, actual, pred) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl

# Calculate test error metrics
test_residuals <- error_tbl$error
test_error_pct <- error_tbl$error_pct * 100 # Percentage error

me   <- mean(test_residuals, na.rm=TRUE)
rmse <- mean(test_residuals^2, na.rm=TRUE)^0.5
mae  <- mean(abs(test_residuals), na.rm=TRUE)
mape <- mean(abs(test_error_pct), na.rm=TRUE)
mpe  <- mean(test_error_pct, na.rm=TRUE)

tibble(me, rmse, mae, mape, mpe) %>% glimpse()
