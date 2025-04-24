library(timetk)
library(tidyquant)
library(tidyverse)
library(data.table)

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
  #filter(Store == "Dearborn" & Group == "Meat") %>% 
  #mutate(Date = floor_date(Date, unit = "week")) %>% 
  filter(year(Date) != 2020 & year(Date) != 2021) %>% 
  group_by(Date) %>% 
  summarize(Sales = sum(Sales))

full.data %>% 
  ggplot(aes(Date, Sales)) +
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 52, size = 1) +
  theme_tq() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Berkley Grocery Sales: 2017 through now")

full.data %>%
  tk_index() %>%
  tk_get_timeseries_summary() %>%
  glimpse()

full.data_aug <- full.data %>%
  tk_augment_timeseries_signature() %>% 
  mutate(moms = ifelse(Date %in% c("2017-05-14", "2016-05-08", "2015-05-10", "2014-05-11",
                                   "2018-05-13", "2019-05-12", "2020-05-10", "2021-05-09",
                                   "2022-05-08", "2023-05-14", "2024-05-12"),1,0)) %>% 
  mutate(mem = ifelse(Date %in% c("2017-05-29", "2016-05-30", "2015-05-25", "2014-05-26",
                                  "2018-05-28", "2019-05-27", "2020-05-25", "2021-05-31",
                                  "2022-05-30", "2023-05-29", "2024-05-27"),1,0))

full.data_aug

fit_lm <- lm(Sales ~ ., data = select(full.data_aug, -c(Date, diff)))

debug_contr_error <- function (dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      } 
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  } else {
    ## step 1
    dat <- stats::na.omit(dat)
  }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}

debug_contr_error(full.data_aug)

fit_lm <- lm(Sales ~ ., data = select(full.data_aug, -c(Date, diff)))

summary(fit_lm)


full.data_idx <- full.data %>%
  tk_index()

tail(full.data_idx)

future_full_idx <- full.data_idx %>%
  tk_make_future_timeseries(n_future = 150)

future_full_idx

new_data_full_tbl <- future_full_idx %>%
  tk_get_timeseries_signature() %>% 
  mutate(moms = ifelse(index %in% c("2017-05-14", "2016-05-08", "2015-05-10", "2014-05-11",
                                   "2018-05-13", "2019-05-12", "2020-05-10", "2021-05-09",
                                   "2022-05-08", "2023-05-14", "2024-05-12"),1,0)) %>% 
  mutate(mem = ifelse(index %in% c("2017-05-29", "2016-05-30", "2015-05-25", "2014-05-26",
                                  "2018-05-28", "2019-05-27", "2020-05-25", "2021-05-31",
                                  "2022-05-30", "2023-05-29", "2024-05-27"),1,0))


new_data_full_tbl

# Make predictions
pred_full <- predict(fit_lm, newdata = select(new_data_full_tbl, -c(index, diff)))

predictions_full_tbl <- tibble(
  Date  = future_full_idx,
  value = pred_full
)

predictions_full_tbl

actuals_tbl <- tbl(my_db,"dept_totals" ) %>%
  collect()  %>% 
  left_join(deptgrp) %>% 
  mutate(Date = as.Date(Date, origin = "1970-01-01")) %>% 
  filter(Date >= wkstart - 42 & Date < wkstart ) %>% 
  #filter(Store == "Dearborn" & Group == "Meat") %>% 
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
  geom_line(aes(y = value), color = palette_light()[[2]], data = predictions_full_tbl) +
  geom_point(aes(y = value), color = palette_light()[[2]], data = predictions_full_tbl) +
  # Actuals
  geom_line(color = palette_light()[[1]], data = actuals_tbl) +
  geom_point(color = palette_light()[[1]], data = actuals_tbl) +
  # Aesthetics
  theme_tq() +
  labs(title = "Beer Sales Forecast: Time Series Machine Learning",
       subtitle = "Using basic multivariate linear regression can yield accurate results")


error_tbl <- left_join(actuals_tbl, predictions_full_tbl) %>%
  rename(actual = Sales, pred = value) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl %>% mutate(dotw = wday(Date)) %>% group_by(dotw) %>% summarize(sum(error))

test_residuals <- error_tbl$error
test_error_pct <- error_tbl$error_pct * 100 # Percentage error

me   <- mean(test_residuals, na.rm=TRUE)
rmse <- mean(test_residuals^2, na.rm=TRUE)^0.5
mae  <- mean(abs(test_residuals), na.rm=TRUE)
mape <- mean(abs(test_error_pct), na.rm=TRUE)
mpe  <- mean(test_error_pct, na.rm=TRUE)

tibble(me, rmse, mae, mape, mpe) %>% glimpse()
