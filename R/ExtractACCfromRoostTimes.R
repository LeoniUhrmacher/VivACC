# extract acc from audio times

library(pacman)
p_load(lubridate, tidyverse, dplyr, timetools, data.table)
op <- options(digits.secs=3)

# get roost times from audio files
roost_files <- list.files("E:/Myotis_vivesi/data/Audio/Mviv17/Mviv17_60/Roost", 
                         include.dirs = FALSE, pattern = "Mviv*")
roost_times <- data.frame(filename = roost_files)
file <- roost_files[1]

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

get_time(roost_files[1])

# set start and end times for each audio snippet
roost_times$start <- ymd_hms(sapply(X = roost_files, FUN = get_time)) - 7*3600
roost_times$end <- roost_times$start + 0.5

# read in acc data
acc <- fread("E:/Myotis_vivesi/17_60/Mviv17_60_metrics.csv")

# find acc data that matches roost times
acc$roost_idx <- NA

# add roost index to acc data frame
for(i in 1:nrow(roost_times)){
  start <- which.min(abs(roost_times$start[i] - acc$datetime))
  end <- which.min(abs(roost_times$end[i] - acc$datetime))
  acc$roost_idx[start:end] <- i
}


# summarize acc data by the roost audio index
roost_acc <- acc %>% group_by(roost_idx) %>% 
  summarise(ACCX = mean(Acc_x), ACCY = mean(Acc_y), ACCZ = mean(Acc_z),
            maxACCX = max(Acc_x), maxACCY = max(Acc_y), maxACCZ = max(Acc_z),
            minACCX = min(Acc_x), minACCY = min(Acc_y), minACCZ = min(Acc_z),
            ACCXsm = mean(Acc_x.sm), ACCYsm = mean(Acc_y.sm), ACCZsm = mean(Acc_z.sm),
            maxACCXsm = max(Acc_x.sm), maxACCYsm = max(Acc_y.sm), maxACCZsm = max(Acc_z.sm),
            minACCXsm = min(Acc_x.sm), minACCYsm = min(Acc_y.sm), minACCZsm = min(Acc_z.sm),
            VeDBA = mean(VeDBA), VeSBA = mean(VeSBA),
            maxVeDBA = max(VeDBA), maxVeSBA = max(VeSBA),
            minVeDBA = min(VeDBA), minVeSBA = min(VeSBA),
            VeDBAsm = mean(VeDBA.smoothed), VeSBAsm = mean(VeSBA.smoothed))
roost_acc$behav = "roost" # add behavior

write.csv(roost_acc, "E:/Myotis_vivesi/17_60/Mviv17_60_roost_acc.csv", row.names = FALSE)

# repeat this for each behavior type