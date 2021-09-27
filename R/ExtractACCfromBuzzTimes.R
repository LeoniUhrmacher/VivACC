# extract acc from audio times

library(pacman)
p_load(lubridate, tidyverse, dplyr, timetools, data.table)
op <- options(digits.secs = 3)


# read in true_buzz_offset and calculate true_buzz_ends
true_buzz <- read.csv("E:/Myotis_vivesi/17_60/Mviv17_60_truebuzz_offset.csv")

true_buzz$start_clip <- ymd_hms(true_buzz$start_clip)

#calculate end of buzz in each clip
true_buzz$buzz_end <- NA
true_buzz$buzz_end <- true_buzz$start_clip + true_buzz$segment*0.5 + true_buzz$offset

true_buzz$local_buzz_end <- NA
true_buzz$local_buzz_end <- true_buzz$buzz_end - 7*3600

true_buzz <- true_buzz[order(true_buzz$local_buzz_end),]

write.csv(true_buzz, "E:/Myotis_vivesi/17_60/Mviv17_60_true_buzz_end.csv", row.names = FALSE)


# read in true_buzz_end file
true_buzz_end <- read.csv("E:/Myotis_vivesi/17_60/Mviv17_60_true_buzz_end.csv")

#remove NAs in true_buzz
#na.omit for just removing all NA's. complete.cases allows partial selection by including only certain columns of the data frame
true_buzz_end <- na.omit(true_buzz_end)

true_buzz_end$local_buzz_end <- ymd_hms(true_buzz_end$local_buzz_end)

true_buzz_end$local_buzz_end %>% unique %>% length

# read in acc data
acc <- read.csv("E:/Myotis_vivesi/17_60/Mviv17_60_metrics.csv")
names(acc)
acc <- acc[-c(3:7)]

names(acc)[14] <- "Pitch"
names(acc)[15] <- "Roll"

acc$datetime <- ymd_hms(acc$datetime)


# add buzz index to acc data frame: for each true_buzz find the matching acc and add an index
acc$buzz_idx <- NA

i = 1034

for(i in 1:nrow(true_buzz_end)){
  buzz_end <- acc$datetime[which.min(abs(true_buzz_end$local_buzz_end[i] - acc$datetime))]
  start <- which.min(abs((buzz_end - 0.25) - acc$datetime))
  end <- which.min(abs((buzz_end + 0.25) - acc$datetime))
  acc$buzz_idx[start:end] <- i
}

hist(table(acc$buzz_idx))

# summarize acc data by the buzz audio index (over 0.5 seconds)
buzz_acc <- acc %>% group_by(buzz_idx) %>% 
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

head(buzz_acc, 10)

buzz_acc$behav = "buzz" # add behavior

table(buzz_acc$count)

# only keep counts of 21

buzz_acc_clean <- buzz_acc[buzz_acc$count == 21, ]

table(buzz_acc_clean$count)

# should also only keep rows with stACC -1 to 1

write.csv(buzz_acc_clean, "E:/Myotis_vivesi/17_60/Mviv17_60_buzz_acc.csv", row.names = FALSE)
