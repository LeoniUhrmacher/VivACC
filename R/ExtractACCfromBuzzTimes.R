# extract acc from audio times

library(pacman)
p_load(lubridate, tidyverse, dplyr, timetools, data.table)
op <- options(digits.secs=3)

# get buzz times from audio files
buzz_files <- list.files("E:/Myotis_vivesi/data/Audio/Mviv17/Mviv17_60/Buzz", 
                         include.dirs = FALSE, pattern = "Mviv*")
buzz_times <- data.frame(filename = buzz_files)
file <- buzz_files[1]

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

get_time(buzz_files[1])

# set start and end times for each audio snippet
buzz_times$start <- ymd_hms(sapply(X = buzz_files, FUN = get_time)) - 7*3600
buzz_times$end <- buzz_times$start + 0.5

# read in acc data
acc <- fread("E:/Myotis_vivesi/DDMT/Mviv17_60_metrics.csv")

# find acc data that matches buzz times
acc$buzz_idx <- NA

i = 1 # it stopped here weirdly, so you might need to manually tell the for loop to start from 
# 1789:nrow below

# add buzz index to acc data frame
for(i in 1:nrow(buzz_times)){
  start <- which.min(abs(buzz_times$start[i] - acc$datetime))
  end <- which.min(abs(buzz_times$end[i] - acc$datetime))
  acc$buzz_idx[start:end] <- i
}


# summarize acc data by the buzz audio index
buzz_acc <- acc %>% group_by(buzz_idx) %>% 
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
buzz_acc$behav = "buzz" # add behavior

write.csv(acc, "E:/Myotis_vivesi/DDMT/Mviv17_60_ACC_buzz_idx.csv", row.names = FALSE)
write.csv(buzz_acc, "E:/Myotis_vivesi/DDMT/Mviv17_60_buzz_acc.csv", row.names = FALSE)

# repeat this for each behavior type

# then combine all the 
final_acc <- rbind(buzz_acc, #commute_acc, #search_acc, #roost_acc)