# extract acc from audio times

library(pacman)
p_load(lubridate, tidyverse, dplyr, timetools, data.table)
op <- options(digits.secs=3)

# get search times from audio files
search_files <- list.files("E:/Myotis_vivesi/data/Audio/Mviv17/Mviv17_60/Search", 
                         include.dirs = FALSE, pattern = "Mviv*")
search_times <- data.frame(filename = search_files)
file <- search_files[1]

get_time <- function(file, year = 2017, ...){
    if(nchar(file) > 30){
      split <- strsplit(file, split = "_")
      month <- as.numeric(split[[1]][4])
      day <- as.numeric(split[[1]][5])
      hour <- as.numeric(split[[1]][6])
      min <- as.numeric(split[[1]][7])
      sec <- as.numeric(split[[1]][8])
      part <- as.numeric(substr(split[[1]][length(split[[1]])], 1, 
                                nchar(split[[1]][length(split[[1]])])-4))
        
      start <- ymd(paste0(year,"-",month, "-", day)) + hours(hour) + minutes(min) +
        seconds(sec) + seconds(part*0.5)
      return(as.character(start))  
    }
}

get_time(search_files[1])

# set start and end times for each audio snippet
search_times$start <- ymd_hms(sapply(X = search_files, FUN = get_time)) - 7*3600
search_times$end <- search_times$start + 0.5

search_times <- na.omit(search_times)

search_times$start %>% unique %>% length

search_times <- search_times[order(search_times$start),]

# read in acc data
acc <- read.csv("E:/Myotis_vivesi/17_60/Mviv17_60_metrics.csv")

names(acc)
acc <- acc[-c(3:7)]

names(acc)[14] <- "Pitch"
names(acc)[15] <- "Roll"

acc$datetime <- ymd_hms(acc$datetime)


# add search index to acc data frame: for each search phase call find the matching acc and add an index
acc$search_idx <- NA

for(i in 1:nrow(search_times)){
  start <- which.min(abs(search_times$start[i] - acc$datetime))
  end <- which.min(abs(search_times$end[i] - acc$datetime))
  acc$search_idx[start:end] <- i
}

hist(table(acc$search_idx))

# # summarize acc data by the search audio index
search_acc <- acc %>% group_by(search_idx) %>% 
  summarise(count = n(),
            accx = mean(ACCX), accy = mean(ACCY), accz = mean(ACCZ),
            maxX = max(ACCX), maxY = max(ACCY), maxZ = max(ACCZ),
            minX = min(ACCX), minY = min(ACCY), minZ = min(ACCZ),
            diffX = diff(range(ACCX)), diffY = diff(range(ACCY)), diffZ = diff(range(ACCZ)), 
            stx = mean(stACCX), sty = mean(stACCY), stz = mean(stACCZ),
            maxstX = max(stACCX), maxstY = max(stACCY), maxstZ = max(stACCZ),
            minstX = min(stACCX), minstY = min(stACCY), minstZ = min(stACCZ),
            diffstX = diff(range(stACCX)), diffstY = diff(range(stACCY)), diffstZ = diff(range(stACCZ)),
            dX = mean(dACCX), dY = mean(dACCY), dZ = mean(dACCZ),
            maxdX = max(dACCX), maxdY = max(dACCY), maxdZ = max(dACCZ),
            mindX = min(dACCX), mindY = min(dACCY), mindZ = min(dACCZ),
            diffdX = diff(range(dACCX)), diffdY = diff(range(dACCY)), diffdZ = diff(range(dACCZ)),
            vedba = mean(VeDBA), vesba = mean(VeSBA),
            maxVeDBA = max(VeDBA), maxVeSBA = max(VeSBA),
            minVeDBA = min(VeDBA), minVeSBA = min(VeSBA),
            diffVeDBA = diff(range(VeDBA)), diffVeSBA = diff(range(VeSBA)),
            pitch = mean(Pitch), maxPitch = max(Pitch), minPitch = min(Pitch), diffPitch = diff(range(Pitch)),
            roll = mean(Roll), maxRoll = max(Roll), minRoll = min(Roll), diffRoll = diff(range(roll)))

head(search_acc, 10)

search_acc$behav = "search" # add behavior

table(search_acc$count)

# only keep counts with 20 & 21

search_acc_clean <- search_acc[search_acc$count > 19 & search_acc$count < 22, ]

table(search_acc_clean$count)

# should also only keep rows with stACC -1 to 1

write.csv(search_acc_clean, "E:/Myotis_vivesi/17_60/Mviv17_60_search_acc.csv", row.names = FALSE)
