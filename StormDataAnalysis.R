library(tidyr)
library(lubridate)
library(ggplot2)

if (!(exists('storm_data') && is.data.frame(get('storm_data')))){
  storm_data <- read.csv('./repdata_data_StormData.csv')
}

data <- storm_data
data$BGN_DATE <- mdy(substr(data$BGN_DATE, 1, nchar(data$BGN_DATE)-8))

columns <- c('BGN_DATE', 'EVTYPE', 'FATALITIES', 'INJURIES', 'CROPDMG', 'PROPDMG')
cols <- which(names(storm_data) %in% columns)
data <- storm_data[, cols]

data$EVTYPE <- toupper(data$EVTYPE)

fatalities <- aggregate(FATALITIES ~ EVTYPE, data, sum)
fatalities <- subset(fatalities, FATALITIES != 0)
fatalities <- fatalities[order(-fatalities$FATALITIES),]

injuries <- aggregate(INJURIES ~ EVTYPE, data, sum)
injuries <- subset(injuries, INJURIES != 0)
injuries <- injuries[order(-injuries$INJURIES),]

crop_damage <- aggregate(CROPDMG ~ EVTYPE, data, sum)
names(crop_damage)[2] <- 'DAMAGE'
crop_damage <- subset(crop_damage, DAMAGE != 0)
crop_damage <- crop_damage[order(-crop_damage$DAMAGE),]

prop_damage <- aggregate(PROPDMG ~ EVTYPE, data, sum)
names(prop_damage)[2] <- 'DAMAGE'
prop_damage <- subset(prop_damage, DAMAGE != 0)
prop_damage <- prop_damage[order(-prop_damage$DAMAGE),]

total_damage <- rbind(crop_damage, prop_damage)
total_damage <- total_damage[order(-total_damage$DAMAGE),]

head_total_damage = head(total_damage,5)
ggplot(head_total_damage, aes(EVTYPE)) + geom_bar()






