# set working directory and load files

# install.packages(c("pacman", "dplyr", "tidyr", "lubridate", "ggplot2"))

library(pacman)
pacman::p_load (dplyr, tidyr, lubridate, ggplot2)

getwd()
setwd("E:/Myotis_vivesi/DDMT")
dir()

# preDDMT

readLines("Mviv17_33_ACC_Call.csv", 3)
PreDDMT <- read.csv("Mviv17_33_ACC_Call.csv")

str(PreDDMT)

names(PreDDMT)[2] <- "Event"
names(PreDDMT)

# convert datetime to right format
# str(PreDDMT$datetime)
# options(digits.secs = 3)
# PreDDMT$datetime <- as_datetime(PreDDMT$datetime)
# PreDDMT$datetime <- ymd_hms(PreDDMT$datetime)
# PreDDMT$datetime <- as.POSIXct(strptime(PreDDMT$datetime, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))

# exclude columns not needed
# names(PreDDMT)
# PreDDMT_clean <- PreDDMT[-c(1, 6:12)]
# PreDDMT_clean <- PreDDMT[c(2:5, 13:16)]
# PreDDMT_clean <- within(PreDDMT, rm(X.1, GYRX, GYRY, GYRZ, MAGX, MAGY, MAGZ, time))
# PreDDMT_clean <- subset(PreDDMT, select = -c(X.1, GYRX, GYRY, GYRZ, MAGX, MAGY, MAGZ, time))
# PreDDMT_clean <- subset(PreDDMT, select = c(Event, ACCX, ACCY, ACCZ, datetime, call, buzz, almost_buzz))
# names(PreDDMT_clean)
# rm(PreDDMT_clean)


# postDDMT

readLines("Mviv17_33_ACC_Call_outputDDMT.csv", 3)
PostDDMT <- read.csv("Mviv17_33_ACC_Call_outputDDMT.csv", sep = ";")

str(PostDDMT)

names(PostDDMT)[1] <- "Event"
names(PostDDMT)[6] <- "Time"
names(PostDDMT)

# merge postDDMT splits for Mviv17_49

# PostDDMT <- rbind(PostDDMT1, PostDDMT2)
# PostDDMT <- PostDDMT[order(PostDDMT$Event), ]
# PostDDMT <- PostDDMT %>% distinct(Event, .keep_all = TRUE)
# PostDDMT <- PostDDMT %>% unique(Event) ??
# PostDDMT <- unique(PostDDMT$Event) ??


# merge pre & post DDMT

names(PreDDMT)
names(PostDDMT)
ALL_DDMT <- merge(PreDDMT, PostDDMT, by.x = "Event", by.y = "Event")
names(ALL_DDMT)
ALL_DDMT_clean <- ALL_DDMT[-c(2:12, 20, 21)]
names(ALL_DDMT_clean)


getwd()
write.csv(ALL_DDMT_clean, file = "Mviv17_33_metrics.csv", row.names = FALSE)


readLines("Mviv17_33_metrics.csv", 4)
Metrics <- read.csv("Mviv17_33_metrics.csv")
str(Metrics)

rm(list = ls())

