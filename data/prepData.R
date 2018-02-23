# See here for tutorial and data source:
# http://neondataskills.org/R/time-series-plot-ggplot/
# Data downloaded 23/02/18

library(dplyr)
library(fpp2)

if(!file.exists("dailymet.csv")){
  path1 <- tempfile(fileext = ".zip")
  download.file("https://ndownloader.figshare.com/files/3701572", 
                "NEON-DS-Met-Time-Series.zip", 
                quiet = FALSE, 
                mode = "wb", cacheOK = TRUE,
                extra = getOption("download.file.extra"))
  unzip(zipfile = "NEON-DS-Met-Time-Series.zip",exdir=".",unzip="internal")
  
}
dailymet <- read.csv(
  file="NEON-DS-Met-Time-Series/HARV/FisherTower-Met/Met_HARV_Daily_2009_2011.csv",
  stringsAsFactors = FALSE) %>% select(date,airt,jd)

write.csv(dailymet,"dailymet.csv",row.names=FALSE)

dailymet$date <- as.Date(dailymet$date)
dailymet$year <- year(dailymet$date)
dailymet$month<- month(dailymet$date)

dailyts <- ts(dailymet$airt, start = c(2009, 1), frequency = 365)
dailysnaive <- snaive(dailyts)
date <- seq(as.Date("2012-01-01"), length = 730, by = "days")
dsn_pred <- as.data.frame(as.numeric(dailysnaive$mean))
dsn_pred <- cbind(date,dsn_pred) 
colnames(dsn_pred) <- c("date","airt")
dsn_pred <- mutate(dsn_pred,status="predicted")
observed <- select(dailymet,date,airt) %>% 
  mutate(status="observed")
dsn_pred <- rbind(dsn_pred,observed)
rm(dailysnaive,observed)
write.csv(dsn_pred,"dsn_pred.csv",row.names=FALSE)