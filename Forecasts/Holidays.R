#### Holidays ####
xmas <- data.frame(
  holiday = "xmas",
  ds = as.Date(
    c(
      "2018-12-23",
      "2017-12-23",
      "2016-12-23",
      "2015-12-23",
      "2014-12-23",
      "2019-12-23",
      "2020-12-23",
      "2021-12-23",
      "2022-12-23",
      "2023-12-23",
      "2024-12-23",
      "2025-12-23"
    )
  ),
  lower_window = -3,
  upper_window = 1
)

newe <- data.frame(
  holiday = "newe",
  ds = as.Date(
    c(
      "2018-12-31",
      "2017-12-31",
      "2016-12-31",
      "2015-12-31",
      "2014-12-31",
      "2019-12-31",
      "2020-12-31",
      "2021-12-31",
      "2022-12-31",
      "2023-12-31",
      "2024-12-31",
      "2025-12-31"
    )
  ),
  lower_window = 0,
  upper_window = 0
)

thx <- data.frame(
  holiday = "thx",
  ds = as.Date(
    c(
      "2018-11-21",
      "2017-11-22",
      "2016-11-23",
      "2015-11-25",
      "2014-11-26",
      "2019-11-27",
      "2020-11-25",
      "2021-11-24",
      "2022-11-23",
      "2023-11-22",
      "2024-11-27",
      "2025-11-26"
    )
  ),
  lower_window = -1,
  upper_window = 0
)

vday <- data.frame(
  holiday = "vday",
  ds = as.Date(
    c(
      "2017-02-14",
      "2016-02-14",
      "2015-02-14",
      "2014-02-14",
      "2018-02-14",
      "2019-02-14",
      "2020-02-14",
      "2021-02-14",
      "2022-02-14",
      "2023-02-14",
      "2024-02-14",
      "2025-02-14"
    )
  ),
  lower_window = 0,
  upper_window = 0
)

east <- data.frame(
  holiday = "east",
  ds = as.Date(
    c(
      "2018-03-31",
      "2017-04-15",
      "2015-03-26",
      "2015-04-04",
      "2014-04-19",
      "2019-04-20",
      "2020-04-11",
      "2021-04-03",
      "2022-04-16",
      "2023-04-08",
      "2024-03-30",
      "2025-04-20"
    )
  ),
  lower_window = -3,
  upper_window = 0
)

mday <- data.frame(
  holiday = "mday",
  ds = as.Date(
    c(
      "2017-05-14",
      "2016-05-08",
      "2015-05-10",
      "2014-05-11",
      "2018-05-13",
      "2019-05-12",
      "2020-05-10",
      "2021-05-09",
      "2022-05-08",
      "2023-05-14",
      "2024-05-12",
      "2025-05-11"
    )
  ),
  lower_window = -1,
  upper_window = 0
)

memday <- data.frame(
  holiday = "memday",
  ds = as.Date(
    c(
      "2017-05-29",
      "2016-05-30",
      "2015-05-25",
      "2014-05-26",
      "2018-05-28",
      "2019-05-27",
      "2020-05-25",
      "2021-05-31",
      "2022-05-30",
      "2023-05-29",
      "2024-05-27",
      "2025-05-26"
    )
  ),
  lower_window = -2,
  upper_window = 0
)

dads <- data.frame(
  holiday = "fathers",
  ds = as.Date(
    c(
      "2014-06-15",
      "2015-06-21",
      "2016-06-19",
      "2017-06-18",
      "2018-06-17",
      "2019-06-16",
      "2020-06-21",
      "2021-06-20",
      "2022-06-19",
      "2023-06-18",
      "2024-06-16",
      "2025-06-15"
    )
  ),
  lower_window = -1,
  upper_window = 0
)

frth <- data.frame(
  holiday = "frth",
  ds = as.Date(
    c(
      "2017-07-03",
      "2016-07-03",
      "2015-07-03",
      "2014-07-03",
      "2018-07-03",
      "2019-07-03",
      "2020-07-03",
      "2021-07-03",
      "2022-07-03",
      "2023-07-03",
      "2024-07-03",
      "2025-07-03"
    )
  ),
  lower_window = -2,
  upper_window = 0
)

lday <- data.frame(
  holiday = "lday",
  ds = as.Date(
    c(
      "2017-09-04",
      "2016-09-05",
      "2015-09-07",
      "2014-09-01",
      "2018-09-03",
      "2019-09-02",
      "2020-09-07",
      "2021-09-06",
      "2022-09-05",
      "2023-09-04",
      "2024-09-02",
      "2025-09-01"
    )
  ),
  lower_window = -2,
  upper_window = 0
)

bpan <- data.frame(
  holiday = "bpan",
  ds = as.Date("2020-03-12"),
  lower_window = 0,
  upper_window = 110
)

artinpark <- data.frame(
  holiday = "artp",
  ds = as.Date(
    c("2016-07-09",
      "2017-07-08",
      "2018-07-14",
      "2019-07-13",
      "2021-07-10",
      "2022-07-09",
      "2023-07-08",
      "2024-07-13"
    )
  ),
  lower_window = -1,
  upper_window = 1
)


fallfst <- data.frame(
  holiday = "fallfst",
  ds = as.Date(
    c("2016-09-10",
      "2017-09-09",
      "2018-09-08",
      "2019-09-07",
      "2021-09-11",
      "2022-09-10",
      "2023-09-09",
      "2024-09-07"
    )
  ),
  lower_window = -1,
  upper_window = 1
)


drmcrse <- data.frame(
  holiday = "drmc",
  ds = as.Date(
    c("2016-08-20",
      "2017-08-19",
      "2018-08-18",
      "2019-08-17",
      "2020-08-15",
      "2021-08-21",
      "2022-08-20",
      "2023-08-19",
      "2024-08-17"
    )
  ),
  lower_window = 0,
  upper_window = 0
)

holidays <- bind_rows(xmas, thx, vday, east, newe, frth, lday, bpan, mday, memday, dads)
holidays_ply <- bind_rows(xmas, thx, vday, east, newe, frth, lday, bpan, mday, memday, dads, artinpark, fallfst)
holidays_brk <- bind_rows(xmas, thx, vday, east, newe, frth, lday, bpan, mday, memday, dads, drmcrse)
