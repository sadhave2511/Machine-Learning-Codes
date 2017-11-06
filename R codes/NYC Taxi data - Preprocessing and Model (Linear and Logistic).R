#https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv

#Data dimensions
library(data.table)
data <- fread('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv')
asd <- data
#data <- asd

nrow(data)
ncol(data)

#Histogram of Trip Distance
hist(data$Trip_distance)
summary(data$Trip_distance)
hist(data$Trip_distance)
hist(data$Trip_distance, 
     main="Histogram for Trip Distance", 
     xlab="Distance", 
     border="blue", 
     col="light blue",
     xlim=c(0,20),
     breaks=1000)
boxplot(data$Trip_distance, main = "Trip Distance in miles", ylab = "Distance(miles)")
summary(data$Trip_distance)
?boxplot()

#Number of trips by hour of the day

install.packages("lubridate")
library(lubridate)
data$pickup_time <- ymd_hms(as.character(data$lpep_pickup_datetime))
data$hour <- hour(data$pickup_time)

agg_mean <- aggregate(Trip_distance ~ hour, data, mean)
#tapply_mean <- tapply(data$Trip_distance, data$hour, FUN = mean)
agg_median <- aggregate(Trip_distance ~ hour, data, median)
#tapply_median <- tapply(data$Trip_distance, data$hour, FUN = median)
#class(tapply_mean)
mean_median <- merge(agg_mean, agg_median, by = "hour")
library(plyr)
rename(mean_median, c("Trip_distance.x"="Mean_distance", "Trip_distance.y" = "Median_distance"))

#Number of airport pickups
#plot 
library(leaflet)
index <- sample(seq_len(nrow(data)), size = 0.05*nrow(data))
tre <- data[index,]
str(tre)
leaflet(data = tre) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ Pickup_longitude, ~Pickup_latitude, radius = 1,
                   color = "blue", fillOpacity = 0.3)


leaflet(data = tre) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ Pickup_longitude, ~Dropoff_latitude, radius = 1,
                   color = "blue", fillOpacity = 0.3)

#distance
data$LGA_lat <- 40.7769
data$LGA_long <- -73.8740

er <- subset(data, select=c("Pickup_longitude", "Pickup_latitude"))
head(er)

er_trunc <- subset(er, er$Pickup_latitude > 40.7650 & er$Pickup_latitude < 40.78 )
er_trunc <- subset(er_trunc, er_trunc$Pickup_longitude > -73.89 & er_trunc$Pickup_longitude < -73.860 )
LGA <- subset(data, select=c("LGA_long", "LGA_lat"))
head(LGA)
LGA_trunc <- LGA[1:nrow(er_trunc),]
dim(LGA_trunc)
dim(er_trunc)
data$dist_from_LGA <- (distm(er_trunc, LGA_trunc, fun = distHaversine))/1690
head(data$dist_from_LGA)
summary(data$dist_from_LGA)

data$LGA_lat <- NULL
data$LGA_long <- NULL
#subsetting main data:
data_trunc <- subset(data, data$Pickup_latitude > 40.7650 & data$Pickup_latitude < 40.78 )
data_trunc <- subset(data_trunc, data_trunc$Pickup_longitude > -73.89 & data_trunc$Pickup_longitude < -73.860 )

data_trunc_drop <- subset(data, data$Dropoff_latitude > 40.7650 & data$Dropoff_latitude < 40.78 )
data_trunc_drop <- subset(data_trunc_drop, data_trunc_drop$Dropoff_longitude > -73.89 & data_trunc_drop$Dropoff_longitude < -73.860 )

data_airport <- rbind(data_trunc, data_trunc_drop)
summary(data_airport)
summary(data)



#Derived tip percentage
tip <- mean(data$Tip_amount, na.rm = TRUE)
total <- mean(data$Total_amount, na.rm = TRUE)
per = tip / total

#Modelled tip percentage
str(data)
data$tip_per <- (data$Tip_amount/data$Total_amount)*100
data$tip_per[is.na(data$tip_per)] <- 0

data$Tip_amount <- NULL
data$Total_amount <- NULL

#VendorID
#tesing if both vendors have different data
grp_1 <- subset(data$tip_per, data$VendorID == 1)
grp_2 <- subset(data$tip_per, data$VendorID == 2)
table(data$VendorID)
t.test(grp_1,grp_2)

data$VendorID_1 <- ifelse(data$VendorID==1,1,0)
table(data$VendorID_1)

#Pickup and drop off time
data$pickup_wkday <- weekdays(data$pickup_time)
data$min <- minute(data$pickup_time)
data$sec <- second(data$pickup_time)

data$drop_time <- ymd_hms(as.character(data$Lpep_dropoff_datetime))
data$hour_2 <- hour(data$drop_time)
data$min_2 <- minute(data$drop_time)
data$sec_2 <- second(data$drop_time)

#trip_duration 
data$start <- (data$hour*60*60)+(data$min*60)+data$sec
data$end <- (data$hour_2*60*60)+(data$min_2*60)+data$sec_2
data$duration <- (data$end - data$start)/60
hist(data$duration)
summary(data$duration)
quantile(data$duration, prob = seq(0,1, len = 21))
qua <- quantile(data$duration, prob = seq(0.90,1.0, len = 11))
qua[c("99%")]

data$duration[data$duration <= 0] <- 0.001
data$duration[data$duration > qua[c("99%")]] <- qua[c("99%")]

data$duration_log <- log(data$duration)
summary(data$duration_log)
hist(data$duration_log)
qua <- quantile(data$duration_log, prob = seq(0.00,0.05, len = 11))


#Store_and_fwd_flag
table(data$Store_and_fwd_flag)
data$str_fwd_y <- ifelse(data$Store_and_fwd_flag=="Y",1,0)
table(data$str_fwd_y)

#RateCodeID
table(data$RateCodeID)
data$RateCodeID <- ifelse(data$RateCodeID == 99, 1,data$RateCodeID)
data$RateCodeID <- as.character(data$RateCodeID)
data$Rate_code_1 <- ifelse(data$RateCodeID == "1", 1, 0)
data$Rate_code_2 <- ifelse(data$RateCodeID == "2", 1, 0)
data$Rate_code_3 <- ifelse(data$RateCodeID == "3", 1, 0)
data$Rate_code_4 <- ifelse(data$RateCodeID == "4", 1, 0)
data$Rate_code_5 <- ifelse(data$RateCodeID == "5", 1, 0)

#Passenger count
hist(data$Passenger_count)
table(data$Passenger_count)

#Trip distance
quantile(data$Trip_distance, prob = seq(0,1, len = 11))
perc <- quantile(data$Trip_distance, prob = seq(0.9,1.0, len = 11))
perc[10]
data$Trip_distance[data$Trip_distance > perc[10]] <- perc[10]
hist(data$Trip_distance, main = "Post outlier removal",      
     border="blue", 
     col="light blue")

data$Trip_distance <- ifelse(data$Trip_distance == 0 , 0.001, data$Trip_distance)
data$dist_log <- log(data$Trip_distance)
hist(data$dist_log, main = "Transformed Trip Distance",      
     border="blue", 
     col="light blue")
summary(data$dist_log)

#data$dist_pwr <- data$Trip_distance*data$Trip_distance
#hist(data$dist_pwr, main = "Transformed Trip Distance",      
#     border="blue", 
#     col="light blue")

# data$dist_inv <- 1/data$Trip_distance
# hist(data$dist_inv, main = "Transformed Trip Distance",      
#      border="blue", 
#      col="light blue")

#Fare distance
cor(data$Trip_distance, data$Fare_amount)

#Extra
hist(data$Extra)
table(data$Extra)
data$Extra_rush <- ifelse(data$Extra >0 & data$Extra <1 , 1, 0)
data$Extra_over_night <- ifelse(data$Extra >=1 , 1, 0)

#MTA Tax
hist(data$MTA_tax)
prop.table(table(data$MTA_tax))

#Tolls_amount
hist(data$Tolls_amount)
summary(data$Tolls_amount)
quantile(data$Tolls_amount, prob = seq(0,1, len = 21))
quantile(data$Tolls_amount, prob = seq(0.0,0.1, len = 21))
quantile(data$Tolls_amount, prob = seq(0.9,1.0, len = 21))
data$Tolls_amount <- ifelse(data$Tolls_amount == 0 , 0, 1)
table(data$Tolls_amount)

#Ehail_fee
table(is.na(data$Ehail_fee))

#Improvement Surcharge
summary(data$improvement_surcharge )
table(data$improvement_surcharge)
data$improvement_surcharge <- ifelse(data$improvement_surcharge > 0 , 1, 0)

#Payment method
table(data$Payment_type)
data$Payment_type_credit <- ifelse(data$Payment_type == 1 , 1, 0)
data$Payment_type_cash <- ifelse(data$Payment_type == 2 , 1, 0)
data$Payment_typ_nc <- ifelse(data$Payment_type == 3 , 1, 0)
data$Payment_typ_disp <- ifelse(data$Payment_type == 4 , 1, 0)
data$Payment_typ_unk <- ifelse(data$Payment_type == 5 , 1, 0)

#Tip type
table(data$Trip_type)
data$Trip_type <- ifelse(data$Trip_type == 2, 1, 0)

#Weekday pickup
table(data$pickup_wkday)
data$pkp_wkend <- ifelse(data$pickup_wkday %in% c("Sunday", "Saturday") , 1,0)
table(data$pkp_wkend)

#Pickup longitude
summary(data$Pickup_latitude)
summary(data$Dropoff_longitude)
summary(data$Dropoff_latitude)

summary(data$Pickup_longitude)
qu_low <- quantile(data$Pickup_longitude, prob = seq(0.0,0.05, len = 21))
qu_low[2]
qu_up <- quantile(data$Pickup_longitude, prob = seq(0.9,1, len = 21))
qu_up[20]
data$Pickup_longitude[data$Pickup_longitude > qu_up[20]] <- qu_up[20]
data$Pickup_longitude[data$Pickup_longitude < qu_low[2]] <- qu_low[2]


summary(data$Pickup_latitude)
qu_low <- quantile(data$Pickup_latitude, prob = seq(0.0,0.05, len = 21))
qu_low[2]
qu_up <- quantile(data$Pickup_latitude, prob = seq(0.9,1, len = 21))
qu_up[20]
data$Pickup_latitude[data$Pickup_latitude > qu_up[20]] <- qu_up[20]
data$Pickup_latitude[data$Pickup_latitude < qu_low[2]] <- qu_low[2]

summary(data$Dropoff_latitude)
qu_low <- quantile(data$Dropoff_latitude, prob = seq(0.0,0.05, len = 21))
qu_low[2]
qu_up <- quantile(data$Dropoff_latitude, prob = seq(0.9,1, len = 21))
qu_up[20]
data$Dropoff_latitude[data$Dropoff_latitude > qu_up[20]] <- qu_up[20]
data$Dropoff_latitude[data$Dropoff_latitude < qu_low[2]] <- qu_low[2]

summary(data$Dropoff_longitude)
qu_low <- quantile(data$Dropoff_longitude, prob = seq(0.0,0.05, len = 21))
qu_low[2]
qu_up <- quantile(data$Dropoff_longitude, prob = seq(0.9,1, len = 21))
qu_up[20]
data$Dropoff_longitude[data$Dropoff_longitude > qu_up[20]] <- qu_up[20]
data$Dropoff_longitude[data$Dropoff_longitude < qu_low[2]] <- qu_low[2]

data$airport_pkp_lat <- ifelse(data$Pickup_latitude > 40.7650 & data$Pickup_latitude < 40.78,1,0 )
data$airport_pkp_lon <- ifelse(data$Pickup_longitude > -73.89 & data$Pickup_longitude < -73.860,1,0 )
data$airport_pkp <- ifelse(data$airport_pkp_lat == 1 & data$airport_pkp_lon == 1, 1,0)


data$airport_drop_lat <- ifelse(data$Dropoff_latitude > 40.7650 & data$Dropoff_latitude < 40.78 ,1,0)
data$airport_drop_lon <- ifelse(data$Dropoff_longitude > -73.89 & data$Dropoff_longitude < -73.860,1,0 )
data$airport_drop <- ifelse(data$airport_drop_lat == 1 & data$airport_drop_lon == 1, 1,0)

data$airport <- ifelse(data$airport_drop == 1 | data$airport_pkp == 1, 1, 0)

#Removing variables
data$airport_drop_lat <- NULL
data$airport_drop_lon <- NULL
data$airport_pkp_lat <- NULL
data$airport_pkp_lon <- NULL
data$airport_pkp <- NULL
data$airport_drop <- NULL
data$VendorID <- NULL
data$RateCodeID <- NULL
data$lpep_pickup_datetime <- NULL
data$Lpep_dropoff_datetime <- NULL
data$Store_and_fwd_flag <- NULL
data$Trip_distance <- NULL
data$Fare_amount <- NULL
data$Extra <- NULL
data$MTA_tax <- NULL
data$Ehail_fee <- NULL
data$Payment_type <- NULL
data$Trip_type <- NULL
data$pickup_time <- NULL
data$pickup_wkday <- NULL 
data$min <- NULL 
data$sec <- NULL 
data$drop_time <- NULL
data$min_2 <- NULL 
data$sec_2 <- NULL 
data$hour_2 <- NULL 
data$start <- NULL 
data$end <- NULL 
#data$Pickup_longitude <- NULL
#data$Pickup_latitude <- NULL
#data$Dropoff_longitude <- NULL
#data$Dropoff_latitude <- NULL
data$duration <- NULL

str(data)

apply(data, 2, function(x) any(is.na(x)))
apply(data, 2, function(x) any(is.infinite(x)))
#Test Train Split :

index <- sample(seq_len(nrow(data)), size = 0.75*nrow(data))
train <- data[index,]
test <- data[-index,]

model <- lm(tip_per~ ., data = train)
summary(model)

train$str_fwd_y <- NULL
train$Rate_code_1 <- NULL
train$Rate_code_2 <- NULL
train$Rate_code_3 <- NULL
train$Rate_code_4 <- NULL
train$Rate_code_5 <- NULL
train$Payment_type_cash <- NULL
train$Payment_typ_nc <- NULL
train$Payment_typ_disp <- NULL
train$Payment_typ_unk <- NULL


install.packages("faraway")
library(faraway)
vif(model)


index <- sample(seq_len(nrow(data)), size = 0.75*nrow(data))
train <- data[index,]
test <- data[-index,]

model <- lm(tip_per~ ., data = train)
summary(model)

str(train)

# Predicting on test
str(test)
str(train)
test$str_fwd_y <- NULL
test$Rate_code_1 <- NULL
test$Rate_code_2 <- NULL
test$Rate_code_3 <- NULL
test$Rate_code_4 <- NULL
test$Rate_code_5 <- NULL
test$Payment_type_cash <- NULL
test$Payment_typ_nc <- NULL
test$Payment_typ_disp <- NULL
test$Payment_typ_unk <- NULL

actual <- test$tip_per
test$tip_per <- NULL
Pred <- predict(model, test)  # predict distance
comparison <- cbind(actual, Pred)
head(comparison$actual)
comparison <- as.data.frame(comparison)
comparison$diff <- abs(comparison$Pred - comparison$actual)
comparison$APE <- ifelse(comparison$actual == 0,0, comparison$diff/comparison$actual)
MAPE <- mean(comparison$APE)


#PCA

temp <- train$tip_per 
train$tip_per <- NULL
PCA <- prcomp(train, scale = T)
plot(PCA, type = "lines")

summary(PCA)
names(PCA)

train2 <- data.frame(Tip_percent = temp, PCA$x)
head(train2)
train2 <- train2[,1:15]
model_PCA <- lm(Tip_percent ~ ., data = train2)
summary(model_PCA)

temp2 <- test$tip_per
test$tip_per <- NULL
head(test)
test2 <- predict(PCA, newdata =  test)
head(test2)
test2 <- as.data.frame(test2)
test2 <- test2[,1:14]
ncol(test2)
pred2 <- predict(model_PCA, test2)

#accuracy
comp <- cbind(temp2, pred2)
head(comp)
comp <- as.data.frame(comp)
comp$diff <- abs(comp$pred2 - comp$temp2)
comp$APE <- ifelse(comp$temp2 == 0,0, comp$diff/comp$temp2)
MAPE <- mean(comp$APE)



#Modelling probabilty of tip percentage more than 10%
data$tip_per <- ifelse(data$tip_per > 10,1,0)
#mean 0.36

index <- sample(seq_len(nrow(data)), size = 0.75*nrow(data))
train <- data[index,]
test <- data[-index,]
model <- glm(tip_per ~.,family=binomial(link='logit'),data=train)
summary(model)

train_act <- train$tip_per
train$tip_per <- NULL
Pred2 <- predict(model, train, type = 'response')  # predict distance
head(Pred2)
table(train_act, ifelse(Pred2 > 0.5 ,1 ,0))

#predict
actual <- test$tip_per
test$tip_per <- NULL
Pred <- predict(model, test, type = 'response')  # predict distance
table(actual, ifelse(Pred > 0.5 ,1 ,0))

#re running model by eliminating insignificant variables

train$str_fwd_y <- NULL
train$Rate_code_1 <- NULL
train$Rate_code_2 <- NULL
train$Rate_code_3 <- NULL
train$Rate_code_4 <- NULL
train$Rate_code_5 <- NULL
train$Payment_type_cash <- NULL
train$Payment_typ_nc <- NULL
train$Payment_typ_disp <- NULL
train$Payment_typ_unk <- NULL
model <- glm(tip_per ~.,family=binomial(link='logit'),data=train)
hist(train$tip_per)
summary(model)

test$str_fwd_y <- NULL
test$Rate_code_1 <- NULL
test$Rate_code_2 <- NULL
test$Rate_code_3 <- NULL
test$Rate_code_4 <- NULL
test$Rate_code_5 <- NULL
test$Payment_type_cash <- NULL
test$Payment_typ_nc <- NULL
test$Payment_typ_disp <- NULL
test$Payment_typ_unk <- NULL

#predict
actual <- test$tip_per
test$tip_per <- NULL
Pred <- predict(model, test, type = 'response')  # predict distance
table(actual, ifelse(Pred > 0.5 ,1 ,0))

library(ROCR)
pr <- prediction(Pred, actual)
perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pr, measure = "auc")
auc@y.values[[1]]