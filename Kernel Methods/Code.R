##################################################################
# Assignment 1. Kernel Methods
##################################################################
library("geosphere")
library("hms")

# Import station data
stations <- read.csv(
  file   = "C:/Users/namit/Downloads/Machine Learning/Lab3 Block1/stations.csv",
  header = TRUE)

# Import temperature data
temps <- read.csv(
  file   = "C:/Users/namit/Downloads/Machine Learning/Lab3 Block1/temps50k.csv",
  header = TRUE)

# Merge the two datasets
set.seed(1234567890)
st <- merge(stations, temps, by = "station_number")

# Format Date and Time data 
st$date <- as.Date(st$date)
st$time <- strptime(st$time, "%H")

# Paramters of interest
lat    <- 58.4274                                             # Point to predict - latitude
long   <- 14.826                                              # Point to predict - longitude
date   <- "2013-11-04"                                        # Date to predict
times  <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00",   # Times to predict
            "12:00:00", "14:00:00", "16:00:00", "18:00:00",
            "20:00:00", "22:00:00", "24:00:00")

# Format date and time and filter future temperature measurements
date      <- as.Date(date)
times     <- strptime(times, "%H")
st_subset <- st[st$date < date, ]

# Distance similarity from a station to the point of interest
distance.sim <- distHaversine(c(lat, long), st_subset[, c("latitude", "longitude")])

# Distance similarity between the day of temperature measurement and day of interest
date.sim     <- difftime(date, st_subset$date, units = "days")
date.sim     <- sapply(as.numeric(date.sim), function(x) {
  min(x %% 365, 365 - (x %% 365))
})

# Distance similarity between hour of temperature measurement and hour of interest
time.sim <- sapply(times, function(x) {
  timediff <- difftime(x, st_subset$time, units = "hours")
  timediff <- sapply(abs(as.numeric(timediff)), function(y) {
    min(y, 24 - y)
  })
  return(timediff)
})

## Format Date and Time data 
#st$date <- as.Date(st$date)
#st$time <- strptime(paste(st$date, st$time), "%Y-%m-%d %H:%M:%S")
#
#st$time <- strptime(st$time, "%H")
#times <- strptime(times, "%H")
#
## Paramters of interest
#lat    <- 58.4274                                            # Point to predict - latitude
#long   <- 14.826                                             # Point to predict - longitude
#date   <- "2013-11-04"                                       # Date to predict
#times  <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00",  # Times to predict
#            "12:00:00", "14:00:00", "16:00:00", "18:00:00",
#            "20:00:00", "22:00:00", "24:00:00")
#
## Format use date and time and filter future temperature measurements
#date       <- as.Date(date)
#st_subset  <- st[st$date < date, ]
#times      <- strptime(paste(date, times), "%Y-%m-%d %H:%M:%S")
#
#
#times      <- strptime(paste(date, times), "%Y-%m-%d %H:%M:%S")
#st_subset <- st[st$date < date, ]
#st$datetime <- strptime(paste(st$date, st$time), "%Y-%m-%d %H:%M:%S")
#
#
## Distance similarity from a station to the point of interest
#distance.sim <- distHaversine(c(lat, long), st_subset[, c("latitude", "longitude")])
#
## Distance similarity between the day of temperature measurement and day of interest
#date.sim     <- difftime(date, st_subset$date, units = "days")
#date.sim     <- sapply(as.numeric(date.sim), function(x) {
#  min(x %% 365, 365 - (x %% 365))
#})
#
## Distance similarity between hour of temperature measurement and hour of interest
#time.sim <- sapply(times, function(x) {
#  timediff <- difftime(x, st_subset$time, units = "hours")
#  timediff <- sapply(abs(as.numeric(timediff)), function(y) {
#    min(y, 24 - y)
#  })
#  return(timediff)
#})

# Width of gaussian kernels
h_distance <- 200000
h_date     <- 20
h_time     <- 2

# 
k_distance <- exp(-1 * (distance.sim / h_distance) ^ 2) 
k_date     <- exp(-1 * (date.sim / h_date) ^ 2)  
k_time     <- exp(-1 * (time.sim / h_time) ^ 2)

# Sum of three gaussian kernels
k_gaussian1 <- k_distance + k_date + k_time

temp.pred1 <- sapply(seq_along(1:ncol(k_gaussian1)), function(i) {
  sum(k_gaussian1[, i] * st_subset$air_temperature) / sum(k_gaussian1[, i])
})

plot(x    = times, 
     y    = temp.pred1, 
     ylab = "Temperature predictions",
     type = "o", 
     col  = "blue")

# Product of three gaussian kernels
k_gaussian2 <- k_distance * k_date * k_time

temp.pred2 <- sapply(seq_along(1:ncol(k_gaussian2)), function(i) {
  sum(k_gaussian2[, i] * st_subset$air_temperature) / sum(k_gaussian2[, i])
})

plot(x    = times, 
     y    = temp.pred2, 
     ylab = "Temperature predictions",
     type = "o", 
     col  = "blue")
  
#analysis

# Ranges of distance, date and time similarities between the points of interest 
# and training points
a <- st_subset[st_subset$latitude == lat & 
                 st_subset$longitude == long & 
                 st_subset$date == date, ]

time.order <- time.sim[order(time.sim[, 1]), 1]
time.gauss <- exp(-1 * (time.order / h_time) ^ 2)
plot(x    = unique(time.order), 
     y    = unique(time.gauss),
     xlab = "Time Similarity",
     ylab = "Gaussian Kernel",
     type = "o",
     col  = "blue")

check      <- which(time.gauss < 0.1)
time.far   <- st_subset[order(time.sim[, 1])[check[1]:nrow(st_subset)], ] # 29000
time.near  <- st_subset[order(time.sim[, 1])[1:check[1]], ]     # 17000

date.order <- date.sim[order(date.sim)]
date.gauss <- exp(-1 * (date.order / h_date) ^ 2)
plot(x    = unique(date.order), 
     y    = unique(date.gauss),
     xlab = "Date Similarity",
     ylab = "Gaussian Kernel",
     type = "o",
     col  = "blue")

check      <- which(date.gauss < 0.1)
date.far   <- st_subset[order(date.sim)[check[1]:nrow(st_subset)], ]   # 27000
date.near  <- st_subset[order(date.sim)[1:check[1]], ]       # 19000
 
dist.order <- distance.sim[order(distance.sim)]
dist.gauss <- exp(-1 * (dist.order / h_distance) ^ 2)
plot(x    = unique(dist.order), 
     y    = unique(dist.gauss),
     xlab = "Distance Similarity",
     ylab = "Gaussian Kernel",
     type = "o",
     col  = "blue")

check     <- which(dist.gauss < 0.1)
dist.far  <- st_subset[order(distance.sim)[check[1]:nrow(st_subset)], ]  # 18000
dist.near <- st_subset[order(distance.sim)[1:check[1]], ]     # 28000



k_gaussian1.order <- k_gaussian1[order(k_gaussian1[, 1], decreasing = TRUE), 1]
plot(k_gaussian1.order)

check <- which(k_gaussian1.order < 1)
far   <- st_subset[order(k_gaussian1[, 1])[check[1]:nrow(st_subset)], ] # 29000
near  <- st_subset[order(k_gaussian1[, 1])[1:check[1]], ]     # 17000


k_gaussian2.order <- k_gaussian2[order(k_gaussian2[, 1], decreasing = TRUE), 1]
plot(k_gaussian2.order)

check <- which(k_gaussian2.order < 0.1)
far   <- st_subset[order(k_gaussian2[, 1])[check[1]:nrow(st_subset)], ] # 29000
near  <- st_subset[order(k_gaussian2[, 1])[1:check[1]], ]     # 17000

#temp       <- vector(length = length(times))

##################################################################
# Assignment 2. Support Vector Machines
##################################################################
library("kernlab")

# Load spam data
data(spam)

# No of observations in the spam dataset
n <- dim(spam)[1]

# Divide dataset into training, validation and test data (50%, 25%, 25%)
RNGversion('3.5.1')
set.seed(12345)
id1    = sample(1:n, floor(n / 2))
train  = spam[id1, ]
id_rem = setdiff(1:n, id1)

set.seed(12345)
id2   = sample(id_rem, floor(n / 4))
valid = spam[id2, ]
id3   = setdiff(id_rem, id2)
test  = spam[id3, ]

# Three SVM models with C values 0.5, 1 and 5
j <- 1
svm_model <- list()
cfmat     <- list()
mc_rate   <- numeric()

for(i in c(0.5, 1, 5)) {
  # SVM model with different C values
  svm_model[[j]] <- ksvm(x       = type ~ ., 
                         data    = train, 
                         type    = "C-svc",
                         C       = i,
                         kernel  = "rbfdot",
                         kpar    = list(sigma = 0.05))
  
  valid.pred <- predict(object  = svm_model[[j]], 
                        newdata = valid,
                        type    = "response")
  
  cfmat[[j]] <- table(actual    = valid$type, 
                      predicted = valid.pred, 
                      dnn       = c("Truth", "Prediction"))
  
  mc_rate[j] <- 1 - (sum(diag(cfmat[[j]])) / sum(cfmat[[j]]))
  
  j <- j + 1
}

# Model 2 with C = 1 is the best because it gives minimum error on the validation data

# Generalization Error

svm_model <- ksvm(x       = type ~ ., 
                  data    = rrbind.data.frame(train, valid), 
                  type    = "C-svc",
                  C       = i,
                  kernel  = "rbfdot",
                  kpar    = list(sigma = 0.05))

test.pred  <- predict(object  = svm_model, 
                      newdata = test,
                      type    = "response")

cfmat_test <- table(actual    = test$type, 
                    predicted = test.pred, 
                    dnn       = c("Truth", "Prediction"))

mc_test    <- 1 - (sum(diag(cfmat_test)) / sum(cfmat_test))

# SVM for user
print(svm_model)

# Purpose of parameter C
