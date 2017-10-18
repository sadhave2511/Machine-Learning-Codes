
###### Bicycles hour data from https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset #########
setwd("C:/Education/Courses/Machine Learning/project 1")

data <- read.csv("C:/Education/Courses/Machine Learning/project 1/hour.csv", header = TRUE)
view(data)

data$instant <- NULL
data$holiday <- NULL
data$atemp <- NULL
data$dteday <- NULL
############################## DUMMY VARIABLE CREATION ######################################################
#working day

#weekday
for(i in unique(data$weekday)) {
  data[paste("weekday", i, sep = "_")] <- ifelse(data$weekday == i, 1,0)
}

data$weekday <- NULL
data$weekday_0 <- NULL

#Season
for(i in unique(data$season)) {
  data[paste("season", i, sep = "_")] <- ifelse(data$season == i, 1,0)
}

data$season_1 <- NULL
data$season <- NULL

#yr
for(i in unique(data$yr)) {
  data[paste("yr", i, sep = "_")] <- ifelse(data$yr == i, 1,0)
}

data$yr <- NULL
data$yr_0 <- NULL

#month
for(i in unique(data$mnth)) {
  data[paste("mnth", i, sep = "_")] <- ifelse(data$mnth == i, 1,0)
}

data$mnth <- NULL
data$mnth_1 <- NULL

#hr
for(i in unique(data$hr)) {
  data[paste("hr", i, sep = "_")] <- ifelse(data$hr == i, 1,0)
}

data$hr <- NULL
data$hr_0 <- NULL

#weathersit
for(i in unique(data$weathersit)) {
  data[paste("weathersit", i, sep = "_")] <- ifelse(data$weathersit == i, 1,0)
}

data$weathersit <- NULL
data$weathersit_1 <- NULL

data$casual <- NULL
data$registered <- NULL

data$x1 <- 1
################# Data PArtition ##############

#set.seed(2345)
index <- sample(seq_len(nrow(data)), size = 0.75*nrow(data))
train <- data[index,]
test <- data[-index,]

############################# splitting to x and y ##############


train_y<-as.matrix(train[c("cnt")])
temp <- train 
temp$cnt <- NULL
train_x <- data.matrix(temp)


test_y<-as.matrix(test[c("cnt")])
temp <- test
temp$cnt <- NULL
test_x <- data.matrix(temp)

dim(data)
dim(train_x)
dim(test_x)
dim(train_y)
dim(test_y)

############# simple 1000 iteration way ################
cost<- function(x,y,thetha) {
  sum(((x %*% theta) - y)^2)/(2*length(y))
}

alpha = 0.01
num_iter = 1000
cost_hist <- double()
theta_hist <- list()
theta <- matrix(c(0,0), nrow = 52)

i=1
repeat{
  error <- train_x%*%theta - train_y
  delta <- t(train_x)%*%error / nrow(train_x)
  theta <- theta - alpha*delta
  theta_hist[[i]] <- theta
  cost_hist[i] <- cost(train_x,train_y, theta)
  
  if(i>1){
  cost_diff <- ((cost_hist[i]-cost_hist[i-1])/cost_hist[i-1])*100
  }
  
  if(cost_diff > -0.01){
    break
  }
  i <- i+1
}

## output values
print(theta)
plot(cost_hist)
reg <- lm(train_y~train_x)
summary(reg)
