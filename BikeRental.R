citibike <- read.csv("citibike.csv")

#Males travel longer distances, but rent for shorter amounts of time
library("geosphere")
tapply(citibike$tripduration, citibike$gender, mean, na.rm = TRUE)[-1]
citibike$distance = distHaversine(citibike[ ,c("start.station.longitude", "start.station.latitude")],citibike[ ,c("end.station.longitude", "end.station.latitude")])
tapply(citibike$distance, citibike$gender, mean, na.rm = TRUE)[-1]

#adding weekdays
citibike$weekday <- factor(weekdays(as.Date(citibike$starttime, format = "%m/%d/%Y")), levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#reformatting time column
as.POSIXlt(citibike$starttime, format = "%m/%d/%Y %H:%M")
citibike$hourtime <- format(as.POSIXct(strptime(citibike$starttime,"%d/%m/%Y %H:%M",tz="")) ,format = "%H")
citibike$hourtime <- as.integer(citibike$hourtime)



#Data Visualization
citibike$weekend <- ifelse(citibike$weekday == "Monday" | citibike$weekday == "Tuesday" |citibike$weekday == "Wednesday" |citibike$weekday == "Thursday" |citibike$weekday == "Friday", "Weekday", "Weekend")

citibike1 <- subset(citibike, !is.na(hourtime))

citibike1$timeOfDay <- ifelse(citibike1$hourtime >= 04 & citibike1$hourtime < 12, "Morning",ifelse(citibike1$hourtime >= 12 & citibike1$hourtime < 16, "Afternoon", ifelse(citibike1$hourtime >= 16 & citibike1$hourtime < 24, "Evening", "After Hours")))

library(ggplot2)

unique(citibike1$timeOfDay)

ggplot(citibike1, aes(timeOfDay, fill = timeOfDay)) + geom_bar() + labs(x = "Time of Day", y = "Bikes")  + ggtitle("Bikes in Use by Time of Day") + theme(plot.title = element_text(hjust = 0.5)) 

#barplot(table(citibike1$timeOfDay), main="Bikes in Use by Time of Day", xlab="Time of Day", col= "green")

ggplot(citibike, aes(citibike$weekday, fill = citibike$weekend)) + geom_bar() + labs(x = "Day", y= "Bikes Rented", fill = "Day Type") + ggtitle("Bikes in Use by Day") + theme(plot.title = element_text(hjust = 0.5)) 

ggplot(citibike1, aes(citibike1$weekday, citibike1$hourtime, colour = citibike1$weekend)) + geom_violin() + ggtitle("Bike Usage Patterns Throughout the Day") + theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Weekday", y = "Traffic by Hour", colour = "Key")


library(plyr)

busystation <- tapply(citibike1$start.station.name, citibike1$timeOfDay, count)

Morning <- as.data.frame(busystation$Morning)
Morning <- Morning[order(Morning$freq, decreasing = TRUE), ]
head(Morning, 5)

Afternoon <- as.data.frame(busystation$Afternoon)
Afternoon <- Afternoon[order(Morning$freq, decreasing = TRUE), ]
head(Afternoon, 5)

Evening <- as.data.frame(busystation$Evening)
Evening <- Evening[order(Evening$freq, decreasing = TRUE), ]
head(Evening, 5)

afterHours <- as.data.frame(busystation$`After Hours`)
afterHours <- afterHours[order(afterHours$freq, decreasing = TRUE), ]
head(afterHours, 5)


citibike$demographic <- ifelse(citibike$birth.year >= 1900 & citibike$birth.year < 1925, "90 years or older old",ifelse(citibike$birth.year >= 1925 & citibike$birth.year < 1950, "90 to 65 years", ifelse(citibike$birth.year >= 1950 & citibike$birth.year < 1975, "65 to 30 years", "30 years or younger")))

citibike2 <- subset(citibike, !is.na(demographic))

ggplot(citibike2, aes(citibike2$demographic, citibike2$hourtime, colour = citibike2$demographic)) + geom_violin() + labs(x = "Age", y = "Hour of the Day", colour = "Key") 

citibike2$weekday <- factor(weekdays(as.Date(citibike2$starttime, format = "%m/%d/%Y")), levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

ggplot(citibike2, aes(citibike2$demographic, citibike2$hourtime, colour = citibike2$demographic)) + geom_violin() + labs(x = "Age", y = "Hour of the Day", colour = "Key") + facet_wrap(~ citibike2$weekday) + theme(axis.text.x=element_blank())