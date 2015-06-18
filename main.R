setwd("F:/Documents/Mine/Kaggle-West_Nile/")

# library
library(dplyr)
library(readr)
library(ggmap)
library(splitstackshape)
library(randomForest)
library(plyr)

# load data
data_dir <- "data"
train <- read_csv(file.path(data_dir, "train.csv"))
test <- read_csv(file.path(data_dir, "test.csv"))
weather <- read.csv(file.path(data_dir, "weather.csv"),stringsAsFactors = F)
spray <- read_csv(file.path(data_dir, "spray.csv"))
mapdata <- readRDS(file.path(data_dir, "mapdata_copyright_openstreetmap_contributors.rds"))

# ========================================================================
# clean data
# ========================================================================

# train
train$Address <- NULL
train$Block <- NULL
train$Street <- NULL
train$AddressNumberAndStreet <- NULL
train$Species <- as.factor(train$Species)
train$Trap <- as.factor(train$Trap)
# test
test$Address <- NULL
test$Block <- NULL
test$Street <- NULL
test$AddressNumberAndStreet <- NULL
test$Species <- as.factor(test$Species)
test$Trap <- as.factor(test$Trap)
# weather
weather$Date <- as.Date(weather$Date)
weather$Tavg <- as.numeric(weather$Tavg)
weather$Tavg[is.na(weather$Tavg)] <- (weather$Tmax[is.na(weather$Tavg)] + weather$Tmin[is.na(weather$Tavg)])/2
weather$Depart <- as.numeric(weather$Depart)
weather$DewPoint <- as.numeric(weather$DewPoint)
weather$WetBulb <- as.numeric(weather$WetBulb)
weather$Heat <- as.numeric(weather$Heat)
weather$Cool <- as.numeric(weather$Cool)
# sunrise
# sunset
# codesum
weather$Depth <- as.factor(weather$Depth)
weather$Water1 <- NULL
weather$SnowFall <- NULL
weather$PrecipTotal <- as.numeric(weather$PrecipTotal)
weather$StnPressure <- as.numeric(weather$StnPressure)
weather$SeaLevel <- as.numeric(weather$SeaLevel)
weather$AvgSpeed <- as.numeric(weather$AvgSpeed)

#####
# map weather to train
#####
train$Station <- NA
train$Tmax <- NA
train$Tmin <- NA
train$Tavg <- NA
train$Depart <- NA
train$DewPoint <- NA
train$WetBulb <- NA
train$Heat <- NA
train$Cool <- NA
train$Sunrise <- NA
train$Sunset <- NA
train$CodeSum <- NA
train$Depth <- NA
train$SnowFall <- NA
train$PrecipTotal <- NA
train$StnPressure <- NA
train$SeaLevel <- NA
train$ResultSpeed <- NA
train$ResultDir <- NA
train$AvgSpeed <- NA
for (i in 1:nrow(train)){
    print(i)
    tempDate <- train$Date[i]
    tempWeather <- weather[weather$Date == tempDate,]
    train$Station[i] <- tempWeather$Station
    train$Tmax[i] <- tempWeather$Tmax
    train$Tmin[i] <- tempWeather$Tmin
    train$Tavg[i] <- tempWeather$Tavg
    train$Depart[i] <- tempWeather$Depart
    train$DewPoint[i] <- tempWeather$DewPoint
    train$WetBulb[i] <- tempWeather$WetBulb
    train$Heat[i] <- tempWeather$Heat
    train$Cool[i] <- tempWeather$Cool
    train$Sunrise[i] <- tempWeather$Sunrise
    train$Sunset[i] <- tempWeather$Sunset
    train$CodeSum[i] <- tempWeather$CodeSum
    train$Depth[i] <- tempWeather$Depth
    train$SnowFall[i] <- tempWeather$SnowFall
    train$PrecipTotal[i] <- tempWeather$PrecipTotal
    train$StnPressure[i] <- tempWeather$StnPressure
    train$SeaLevel[i] <- tempWeather$SeaLevel
    train$ResultSpeed[i] <- tempWeather$ResultSpeed
    train$ResultDir[i] <- tempWeather$ResultDir
    train$AvgSpeed[i] <- tempWeather$AvgSpeed
}

# TOTHINK: no spray data for 2007 2009 and for test data
# map spray to train based on Lat and Lon
# closest location with a spray (distance) on that date?

#####
# map weather to test
#####
test$Station <- NA
test$Tmax <- NA
test$Tmin <- NA
test$Tavg <- NA
test$Depart <- NA
test$DewPoint <- NA
test$WetBulb <- NA
test$Heat <- NA
test$Cool <- NA
test$Sunrise <- NA
test$Sunset <- NA
test$CodeSum <- NA
test$Depth <- NA
test$SnowFall <- NA
test$PrecipTotal <- NA
test$StnPressure <- NA
test$SeaLevel <- NA
test$ResultSpeed <- NA
test$ResultDir <- NA
test$AvgSpeed <- NA
for (i in 1:nrow(test)){
    print(i)
    tempDate <- test$Date[i]
    tempWeather <- weather[weather$Date == tempDate,]
    test$Station[i] <- tempWeather$Station
    test$Tmax[i] <- tempWeather$Tmax
    test$Tmin[i] <- tempWeather$Tmin
    test$Tavg[i] <- tempWeather$Tavg
    test$Depart[i] <- tempWeather$Depart
    test$DewPoint[i] <- tempWeather$DewPoint
    test$WetBulb[i] <- tempWeather$WetBulb
    test$Heat[i] <- tempWeather$Heat
    test$Cool[i] <- tempWeather$Cool
    test$Sunrise[i] <- tempWeather$Sunrise
    test$Sunset[i] <- tempWeather$Sunset
    test$CodeSum[i] <- tempWeather$CodeSum
    test$Depth[i] <- tempWeather$Depth
    test$SnowFall[i] <- tempWeather$SnowFall
    test$PrecipTotal[i] <- tempWeather$PrecipTotal
    test$StnPressure[i] <- tempWeather$StnPressure
    test$SeaLevel[i] <- tempWeather$SeaLevel
    test$ResultSpeed[i] <- tempWeather$ResultSpeed
    test$ResultDir[i] <- tempWeather$ResultDir
    test$AvgSpeed[i] <- tempWeather$AvgSpeed
}

#####
# only 1 value for Station in train and test
train$Station <- NULL
test$Station <- NULL
# only 1 value for Depth in train and test
train$Depth <- NULL
test$Depth <- NULL

# take care of NA's
train$WetBulb[is.na(train$WetBulb)] <- 0
test$WetBulb[is.na(test$WetBulb)] <- 0
train$PrecipTotal[is.na(train$PrecipTotal)] <- 0
test$PrecipTotal[is.na(test$PrecipTotal)] <- 0
train$StnPressure[is.na(train$StnPressure)] <- 0
test$StnPressure[is.na(test$StnPressure)] <- 0

# transform CodeSum
train$CodeSum[train$CodeSum == " "] <- "NA"
test$CodeSum[test$CodeSum == " "] <- "NA"
tempTrain <- cSplit(indt=train,splitCols="CodeSum",sep=" ",direction="long")
tempTest <- cSplit(indt=test,splitCols="CodeSum",sep=" ",direction="long")
rm(tempTrain)
rm(tempTest)
# TOTHINK: FG+
# transform train
train$BR <- 0
train$DZ <- 0
train$FG <- 0
train$HZ <- 0
train$RA <- 0
train$TS <- 0
train$VCTS <- 0
train$BCFG <- 0
for (i in 1:nrow(train)){
    print(i)
    train$BR[i] <- ifelse(grepl("BR",train$CodeSum[i]),1,0)
    train$DZ[i] <- ifelse(grepl("DZ",train$CodeSum[i]),1,0)
    train$HZ[i] <- ifelse(grepl("HZ",train$CodeSum[i]),1,0)
    train$RA[i] <- ifelse(grepl("RA",train$CodeSum[i]),1,0)
    train$TS[i] <- ifelse(grepl("TS",train$CodeSum[i]),1,0)
    train$VCTS[i] <- ifelse(grepl("VCTS",train$CodeSum[i]),1,0)
    train$BCFG[i] <- ifelse(grepl("BCFG",train$CodeSum[i]),1,0)
    train$FG[i] <- ifelse(grepl("FG",train$CodeSum[i]),1,0)
}
train$BR <- as.factor(train$BR)
train$DZ <- as.factor(train$DZ)
train$FG <- as.factor(train$FG)
train$HZ <- as.factor(train$HZ)
train$RA <- as.factor(train$RA)
train$TS <- as.factor(train$TS)
train$VCTS <- as.factor(train$VCTS)
train$BCFG <- as.factor(train$BCFG)
# transform test
test$BR <- 0
test$DZ <- 0
test$FG <- 0
test$HZ <- 0
test$RA <- 0
test$TS <- 0
test$VCTS <- 0
test$BCFG <- 0
for (i in 1:nrow(test)){
    print(i)
    test$BR[i] <- ifelse(grepl("BR",test$CodeSum[i]),1,0)
    test$DZ[i] <- ifelse(grepl("DZ",test$CodeSum[i]),1,0)
    test$HZ[i] <- ifelse(grepl("HZ",test$CodeSum[i]),1,0)
    test$RA[i] <- ifelse(grepl("RA",test$CodeSum[i]),1,0)
    test$TS[i] <- ifelse(grepl("TS",test$CodeSum[i]),1,0)
    test$VCTS[i] <- ifelse(grepl("VCTS",test$CodeSum[i]),1,0)
    test$BCFG[i] <- ifelse(grepl("BCFG",test$CodeSum[i]),1,0)
    test$FG[i] <- ifelse(grepl("FG",test$CodeSum[i]),1,0)
}
test$BR <- as.factor(test$BR)
test$DZ <- as.factor(test$DZ)
test$FG <- as.factor(test$FG)
test$HZ <- as.factor(test$HZ)
test$RA <- as.factor(test$RA)
test$TS <- as.factor(test$TS)
test$VCTS <- as.factor(test$VCTS)
test$BCFG <- as.factor(test$BCFG)

# add a new level to BCFG in train and VCTS in test
# add level upspecified species to train
levels(train$Species) <- levels(test$Species)
levels(train$BCFG) <- levels(test$BCFG)
levels(test$VCTS) <- levels(train$VCTS)

# save cleaned file
write_csv(train,path="data/train_clean.csv")
write_csv(test,path="data/test_clean.csv")

# new Species level in test: UPSPECIFIED CULEX -> 
test$Species[test$Species == "UNSPECIFIED CULEX"] <- "CULEX PIPIENS"
test$Species <- droplevels(test$Species)
levels(train$Species) <- droplevels((train$Species))

# ========================================================================
# models
# ========================================================================

# =====
# 1. random forest
# WnvPresent~.-Date-Sunrise-Sunset-CodeSum-Trap-NumMosquitos
# =====

# train model
train_m1 <- train[,!(colnames(train) %in% c("NumMosquitos","Date","Sunrise","Sunset","Trap","CodeSum"))]
test_m1 <- test[,!(colnames(test) %in% c("Id","Date","Sunrise","Sunset","Trap","CodeSum"))]
m1 <- randomForest(WnvPresent~.,data=train_m1)
save(m1,file="model/m1.RData")
# prediction
m1.pred <- predict(m1,test_m1,type="response")
# prepare submission
m1.sub <- data.frame(Id=test$Id,
                     WnvPresent=m1.pred)
# save
write_csv(m1.sub,path="submissions/submission_m1_rf.csv")

# =====
# 2. random forest
# ADDED: Month
# =====

# add Month to train and test
train$Month <- sapply(train$Date,function(x){format(x,"%b")})
train$Month <- as.factor(train$Month)
test$Month <- sapply(test$Date,function(x){format(x,"%b")})
test$Month <- as.factor(test$Month)
levels(test$Month) <- levels(train$Month)
# train model
train_m2 <- train[,!(colnames(train) %in% c("NumMosquitos","Date","Sunrise","Sunset","Trap","CodeSum"))]
test_m2 <- test[,!(colnames(test) %in% c("Id","Date","Sunrise","Sunset","Trap","CodeSum"))]
m2 <- randomForest(WnvPresent~.,data=train_m2)
save(m2,file="model/m2.RData")
# prediction
m2.pred <- predict(m2,test_m2,type="response")
m2.pred[m2.pred<0] <- 0
# prepare submission
m2.sub <- data.frame(Id=test$Id,
                     WnvPresent=m2.pred)
# save
write_csv(m2.sub,path="submissions/submission_m2_rf.csv")

# =====
# 3. random forest
# ADDED: Trap -> binary variables
# =====

# add Trap to train
allTrap <- union(levels(test$Trap),levels(train$Trap))
train[,allTrap] <- 0
for (i in 1:nrow(train)){
    print(i)
    train[,toString(train$Trap[i])][i] <- 1
}
test[,allTrap] <- 0
for (i in 1:nrow(test)){
    print(i)
    test[,toString(test$Trap[i])][i] <- 1
}
# train model
train_m3 <- train[,!(colnames(train) %in% c("NumMosquitos","Date","Sunrise","Sunset","Trap","CodeSum"))]
test_m3 <- test[,!(colnames(test) %in% c("Id","Date","Sunrise","Sunset","Trap","CodeSum"))]
m3 <- randomForest(WnvPresent~.,data=train_m3)
save(m3,file="model/m3.RData")
# prediction
m3.pred <- predict(m3,test_m3,type="response")
m3.pred[m3.pred<0] <- 0
# prepare submission
m3.sub <- data.frame(Id=test$Id,
                     WnvPresent=m3.pred)
# save
write_csv(m3.sub,path="submissions/submission_m3_rf.csv")

# =====
# 3. random forest
# ADDED: NumMosquitos -> map by Lat and Lon
# =====

# clean data again (new method)
# train
train$AddressNumberAndStreet <- NULL
# group train by row
counts <- count(train, c('Date','Address',"Species"))
newTrain <- aggregate(. ~ Date+Address+Species, data=train[,c(1,2,3,11,12)], FUN=sum)
newTrain <- merge(newTrain,counts,by=c('Date','Address',"Species"))
agg <- merge(newTrain,train,by=c('Date','Address',"Species"))
aggUnique <- agg[!duplicated(agg[,c('Date','Address',"Species")]),]
rm(agg)
rm(counts)
rm(newTrain)
aggUnique$NumMosquitos.y <- NULL
aggUnique$WnvPresent.y <- NULL
# choose closest station
station1 <- c(41.995, -87.933)
station2 <- c(41.786, -87.752)
aggUnique$closerStation <- ifelse(sqrt(((aggUnique$Latitude-station1[1])^2+(aggUnique$Longitude-station1[2])^2))<=
                                      sqrt(((aggUnique$Latitude-station2[1])^2+(aggUnique$Longitude-station2[2])^2)),
                                  1,2)
# weather clean as before
# merge
aggWeather <- merge(aggUnique,weather,by=c("Date"))
rm(aggUnique)
aggWeather$Depth <- NULL
# for depart, copy value from another station
for (i in which(is.na(aggWeather$Depart))){
    if (i %% 2 == 0){
        aggWeather$Depart[i] <- aggWeather$Depart[i-1]
    }
    else {
        aggWeather$Depart[i] <- aggWeather$Depart[i+1]
    }
}
# same for sunrise and sunset
for (i in which(aggWeather$Sunset == "-")){
    if (i %% 2 == 0){
        aggWeather$Sunset[i] <- aggWeather$Sunset[i-1]
        aggWeather$Sunrise[i] <- aggWeather$Sunrise[i-1]
    }
    else {
        aggWeather$Sunset[i] <- aggWeather$Sunset[i+1]
        aggWeather$Sunrise[i] <- aggWeather$Sunrise[i+1]
    }
}
# same for stnPressure
for (i in which(is.na(aggWeather$StnPressure))){
    if (i %% 2 == 0){
        aggWeather$StnPressure[i] <- aggWeather$StnPressure[i-1]
    }
    else {
        aggWeather$StnPressure[i] <- aggWeather$StnPressure[i+1]
    }
}
# same for WetBulb
for (i in which(is.na(aggWeather$WetBulb))){
    if (i %% 2 == 0){
        aggWeather$WetBulb[i] <- aggWeather$WetBulb[i-1]
    }
    else {
        aggWeather$WetBulb[i] <- aggWeather$WetBulb[i+1]
    }
}
# same for PrecipTotal
for (i in which(is.na(aggWeather$PrecipTotal))){
    if (i %% 2 == 0){
        aggWeather$PrecipTotal[i] <- aggWeather$PrecipTotal[i-1]
    }
    else {
        aggWeather$PrecipTotal[i] <- aggWeather$PrecipTotal[i+1]
    }
}
# if still NA, set to 0
aggWeather$PrecipTotal[is.na(aggWeather$PrecipTotal)] <- 0
# choose the closer station
aggWeatherFilt <- aggWeather[aggWeather$closerStation == aggWeather$Station,]
rm(aggWeather)
aggWeatherFilt$Address <- NULL
aggWeatherFilt$Block <- NULL
aggWeatherFilt$WnvPresentPerc <- as.numeric(aggWeatherFilt$WnvPresent.x)/aggWeatherFilt$freq
