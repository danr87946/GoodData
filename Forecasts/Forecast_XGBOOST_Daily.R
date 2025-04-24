library(timetk)
library(tidyquant)
library(tidyverse)
library(data.table)
library(xgboost)
library(caret) 
## remotes::install_version("caret", version = '6.0-89', repos = "http://cran.us.r-project.org") ## had to install an older version because of errors

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
  group_by(Date) %>% 
  summarize(Sales = sum(Sales)) 

tail(full.data)

extended_data <- full.data %>% 
  rbind(tibble::tibble(Date = seq(from = as_date("2024-04-28"),
                                  by = "day", length.out = 60), 
                       Sales = rep(NA, 60)))

tail(extended_data)

extended_data %>% 
  ggplot(aes(Date, Sales)) +
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 52, size = 1) +
  theme_tq() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Berkley Grocery Sales: 2017 through now")

extended_data %>%
  tk_index() %>%
  tk_get_timeseries_summary() %>%
  glimpse()

full.data_aug <- extended_data %>%
  tk_augment_timeseries_signature()

full.data_aug

train <- full.data_aug[1:nrow(full.data), ] # initial data

pred <- full.data_aug[(nrow(full.data) + 1):nrow(extended_data), ] # extended time index

x_train <- xgb.DMatrix(as.matrix(train %>%
                                   select(year,year.iso,half,quarter,month,month.xts,day,wday,wday.xts,mday,qday,yday,mweek,week,week.iso,week2,week3,week4,mday7)))


x_pred <- as.matrix(pred %>% select(year,year.iso,half,quarter,month,month.xts,day,wday,wday.xts,mday,qday,yday,mweek,week,week.iso,week2,week3,week4,mday7))

y_train <- train$Sales

xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = 0.1, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))

xgb_model <- train(
  x_train, y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1
)

xgb_model$results
xgb_model$bestTune
xgb_model$pred

xgb_pred <- xgb_model %>% predict(x_pred)

pred[,2] <- xgb_pred
pred <- pred %>% rename(value = Sales)

actuals_tbl <- tbl(my_db,"dept_totals" ) %>%
  collect()  %>% 
  left_join(deptgrp) %>% 
  mutate(Date = as.Date(Date, origin = "1970-01-01")) %>% 
  filter(Date >= wkstart - 42 & Date < wkstart ) %>% 
  filter(Store == "Berkley" & Group == "Deli") %>% 
  #mutate(Date = floor_date(Date, unit = "week")) %>% 
  group_by(Date) %>% 
  summarize(Sales = sum(Sales))


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
  geom_line(color = palette_light()[[1]], data = actuals_tbl) +
  geom_point(color = palette_light()[[1]], data = actuals_tbl) +
  # Aesthetics
  theme_tq() +
  labs(title = "Beer Sales Forecast: Time Series Machine Learning",
       subtitle = "Using basic multivariate linear regression can yield accurate results")


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
