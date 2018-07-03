#getwd()
#setwd("C:/Users/Richard Buote/Desktop/Trip Data USA/")
#install.packages("dplyr")
#install.packages("data.table")
library(lubridate)
library(stringr)
library(data.table)
library(dplyr)
library(car)
library(kimisc)


trippub <- "/Users/syounkin/NHTS/data/trippub.csv"
# Read .CSV
personaltrips <- read.csv2(trippub, header = TRUE, sep = ",")

# New data, only Portland

portlanddata <- personaltrips[which (personaltrips$HH_CBSA == 38900),]

# New age group, young and old

portlanddata$agegroup <- ifelse(portlanddata$R_AGE <= 18, 0, 1)

# Calculate active minutes (transit minutes divided by 2)

portlanddata$transitminutes <- if_else(portlanddata$TRPTRANS %in% c(11,15,16), portlanddata$TRVLCMIN/2, ifelse(portlanddata$TRPTRANS %in% c(1,2), portlanddata$TRVLCMIN*1, 0))

#Aggregate data
aggregatedportland <- portlanddata %>%
  group_by(HOUSEID, PERSONID) %>%
  summarise(agegroup = first(agegroup),
            transitminutes = sum(transitminutes))

#Aggregating aggregatedportland by agegroup
summaryData <- cbind(
  aggregate(
    aggregatedportland[, "transitminutes"],
    by = list(agegroup = aggregatedportland$agegroup),
    FUN = mean,
    na.rm = FALSE
  ),
  aggregate(
    aggregatedportland[, "transitminutes"],
    by = list(agegroup = aggregatedportland$agegroup),
    FUN = sd,
    na.rm = FALSE
  )[, 2]
)

colnames(summaryData) <- c("agegroup", "mean", "sd")

summaryData$proportion <- NA

agegroups <- unique(aggregatedportland$agegroup)

for (i in 1:length(agegroups)) {
  tempdata <- aggregatedportland[which(aggregatedportland$agegroup == agegroups[i]), ]
  summaryData[i, "proportion"] <- length(which(tempdata$transitminutes > 0))  / nrow(tempdata)
}


