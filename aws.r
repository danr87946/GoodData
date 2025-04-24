
library(aws.s3)
library(lubridate)
# bucketlist()

put_object(
  file = "files/sales.csv",
  object = "sales.csv",
  bucket = "wbmktdata"
)

# put_object(
#   file = "files/store.csv", 
#   object = "store.csv", 
#   bucket = "wbmktdata"
# )

put_object(
  file = "files/cats.csv", 
  object = "cats.csv", 
  bucket = "wbmktdata"
)

# put_object(
#   file = "files/dept.csv", 
#   object = "dept.csv", 
#   bucket = "wbmktdata"
# )

put_object(
  file = "files/ItemList.csv", 
  object = "itemlist.csv", 
  bucket = "wbmktdata"
)

put_object(
  file = "files/ItemList2.csv", 
  object = "itemlist2.csv", 
  bucket = "wbmktdata"
)

put_object(
  file = "files/Shrink.csv", 
  object = "shrink.csv", 
  bucket = "wbmktdata"
)

put_object(
  file = "files/Movement.zip", 
  object = "movement.zip", 
  bucket = "wbmktdata",
  multipart = TRUE
)

put_object(
  file = "files/hourlysales.csv", 
  object = "hourlysales.csv", 
  bucket = "wbmktdata"
)

if (wday(today()) == 3) {
  put_object(
    file = "files/OrderScans.csv",
    object = "orderscans.csv",
    bucket = "wbmktdata",
    multipart = TRUE
  )
}

if (wday(today()) %in% c(3,5,7)) {
  put_object(
    file = "files/oos.csv",
    object = "oos.csv",
    bucket = "wbmktdata"
  )
}


put_object(
  file = "files/OpenData.csv", 
  object = "opendata.csv", 
  bucket = "wbmktdata"
)

# put_object(
#   file = "files/GP_Data.csv",
#   object = "test.csv",
#   bucket = "wbmktdata",
#   multipart = TRUE
# )

if (wday(today()) == 1) {
  
  source("Forecasts/Forecast_Prophet_Daily_StrDB.r")
  
  source("Forecasts/Forecast_Prophet_Daily_DeptDB.r")
  
  Sys.sleep(60)
  
  put_object(file = "files/StorePreds.csv",
             object = "storepreds.csv",
             bucket = "wbmktdata")
  
  put_object(file = "files/DeptPreds.csv",
             object = "deptpreds.csv",
             bucket = "wbmktdata")
}


# put_object(
#   file = "files/ItemEvents.csv",
#   object = "itemevents.csv",
#   bucket = "wbmktdata"
# )

put_object(
  file = "/scripts/craftcms/internet_sales_db.csv", 
  object = "intgooddata.csv", 
  bucket = "wbmktdata"
)
