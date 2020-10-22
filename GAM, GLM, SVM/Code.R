#####################################################################
# Assignment 1. Using GAM and GLM to examine the mortality rates
#####################################################################
library("mgcv")

# Import influenza data
influenza <- xlsx::read.xlsx(
  file      = "C:/Users/namit/Downloads/Machine Learning/Lab2 Block2/Influenza.xlsx",
  sheetName = "Raw data",
  header    = TRUE
)

#--------------------------------------------------------------------
# 1.1 Time series plots
plot(influenza$Time, influenza$Mortality)
plot(influenza$Time, influenza$Influenza)


#--------------------------------------------------------------------
# 1.2 
plot(influenza$Year, influenza$Mortality)
plot(influenza$Week, influenza$Mortality)
gam <- gam(Mortality ~ Year + s(Week, k = length(unique(influenza$Week))), data = influenza)

#--------------------------------------------------------------------
# 1.3
plot(x    = influenza$Time, 
     y    = influenza$Mortality,
     xlab = "Time",
     ylab = "Mortality",
     col  = "blue",
     type = "l")
points(x = influenza$Time, y = gam$fitted.values, col = "red", type = "l")
legend("topleft", c("Actual Values", "Fitted Values"), fill = c("blue", "red"), cex = 0.8)

plot(gam)
summary(gam)

#--------------------------------------------------------------------
# 1.4
# GAM with low penalty
gam_lowp  <- gam(Mortality ~ Year + s(Week, k = length(unique(influenza$Week)), sp = 1e-10),
                 data = influenza)
plot(gam_lowp)
plot(x    = influenza$Time, 
     y    = influenza$Mortality,
     xlab = "Time",
     ylab = "Mortality", 
     col  = "blue",
     type = "l")
points(x = influenza$Time, y = gam_lowp$fitted.values, col = "red", type = "l")
legend("topright", c("Actual Values", "Fitted Values"), fill = c("blue", "red"), cex = 0.8)

# GAM with high penalty
gam_highp <- gam(Mortality ~ Year + s(Week, k = length(unique(influenza$Week)), sp = 100),
                 data = influenza)
plot(gam_lowp)
plot(x    = influenza$Time, 
     y    = influenza$Mortality,
     xlab = "Time",
     ylab = "Mortality", 
     col  = "blue",
     type = "l")
points(x = influenza$Time, y = gam_highp$fitted.values, col = "red", type = "l")
legend("topright", c("Actual Values", "Fitted Values"), fill = c("blue", "red"), cex = 0.8)

# Influence of penalty factor on deviance and estimated degrees 
# of freedom
j        <- 1
deviance <- numeric()
df       <- numeric()
penalty  <- cumprod(c(1e-10, rep(10,12)))
for(i in penalty) {
  gam_i <- gam(Mortality ~ Year + s(Week, k = length(unique(influenza$Week)), sp = i),
               data = influenza)
  # Estimated degrees of freedom
  df[j] <- sum(gam_i$edf)
  # Deviance
  deviance[j] <- gam_i$deviance
  j <- j + 1
}
# Plot deviance 
plot(penalty, deviance, type = "o", col = "red")
# Plot degrees of freedom
plot(penalty, df, type = "o", col = "red")

#--------------------------------------------------------------------
# 1.5
plot(x    = influenza$Time, 
     y    = gam$residuals,
     xlab = "Time",
     ylab = "Residuals",
     col  = "blue",
     type = "l")
points(x = influenza$Time, influenza$Influenza, col = "red", type = "l")
legend("topright", c("Residuals", "Influenza"), fill = c("blue", "red"), cex = 0.8)

#--------------------------------------------------------------------
# 1.6
gam2 <- gam(Mortality ~ s(Year, k = length(unique(influenza$Year))) + 
                        s(Week, k = length(unique(influenza$Week))) + 
                        s(Influenza, k = length(unique(influenza$Influenza))), 
                        data = influenza)

plot(x    = influenza$Time, 
     y    = influenza$Mortality,
     xlab = "Time",
     ylab = "Mortality",
     col  = "blue",
     type = "l")
points(x = influenza$Time, y = gam2$fitted.values, col = "red", type = "l")
legend("topright", c("Actual Values", "Fitted Values"), fill = c("blue", "red"), cex = 0.8)

summary(gam2)

#####################################################################
# Assignment 2. High-dimensional methods
#####################################################################
library("pamr")
library("glmnet")
library("e1071")
library("kernlab")

# Import DB world
DBworld <- read.csv2(
  file   = "C:/Users/namit/Downloads/Machine Learning/Lab2 Block2/data.csv",
  header = TRUE)
DBworld$Conference <- as.factor(DBworld$Conference)

# No of observations in the dataset
n = dim(DBworld)[1]

#--------------------------------------------------------------------
# 2.1
# Divide dataset into training and test data
RNGversion('3.5.1')
set.seed(12345)
id    = sample(1:n, floor(n * 0.7))
train = DBworld[id, ]
test  = DBworld[-id, ]

#train_scaled <- scale(train)
#train_scaled[is.nan(train_scaled)] <- 0
#train_scaled <- as.data.frame(train_scaled)
#train_scaled$Conference <- as.factor(train$Conference)
#
#x <- t(train_scaled[, -4703])
#y <- train_scaled[[4703]]
#data  <- list(x = x, y = as.factor(y), geneid = as.character(1:nrow(x)), genenames = rownames(x))
#model <- pamr.train(data, threshold = seq(0, 5, 0.1))

x            <- scale(train[, -4703])
x[is.nan(x)] <- 0
y            <- train[, 4703]
data  <- list(x = t(x), y = as.factor(y), geneid = as.character(1:ncol(x)), genenames = colnames(x))
model <- pamr.train(data, threshold = seq(0, 5, 0.1))

# Cross validation
model_cv <- pamr.cv(model, data)

# Cross-validation analysis
pamr.plotcv(model_cv)
plot(model_cv$threshold, model_cv$loglik, type = "o", col = "blue")
plot(model_cv$threshold, model_cv$error, type = "o", col = "red")
cv_summary <- cbind.data.frame(
                threshold = model_cv$threshold, 
                nonzero   = model_cv$size, 
                error     = round(model_cv$error, 2), 
                loglik    = round(model_cv$loglik, 2))


# Best model with threshold 0.9
pamr.plotcen(model, data, threshold = 0.9)
features <- pamr.listgenes(model, data, threshold = 0.9)
cat(paste(colnames(train)[as.numeric(features[1:10, 1])], collapse = '\n'))

# Test error
x            <- scale(test[, -4703])
x[is.nan(x)] <- 0
y            <- test[, 4703]

Yfit    <- pamr.predict(fit  = model, newx = t(x), threshold = 0.9, type = "class")
cf_pamr <- table(actual = y, predicted = Yfit, dnn = c("Truth", "Prediction"))
mc_pamr <- 1 - (sum(diag(cf_pamr)) / sum(cf_pamr))

#--------------------------------------------------------------------
# 2.2 (a) Elastic net regression
elasticnet <- cv.glmnet(x            = as.matrix(train[, -4703]),
                        y            = train$Conference,
                        family       = "binomial",
                        alpha        = 0.5,
                        lambda       = seq(0, 0.8, 0.001),                              
                        type.measure = "deviance") 

# Number of features selected for optimum lambda
nzero_1se <- elasticnet$nzero[elasticnet$lambda == elasticnet$lambda.1se]
cvm_1se   <- elasticnet$cvm[elasticnet$lambda == elasticnet$lambda.1se]

nzero_min <- elasticnet$nzero[elasticnet$lambda == elasticnet$lambda.min]
cvm_min   <- elasticnet$cvm[elasticnet$lambda == elasticnet$lambda.min]

# Predictions for optimum lambda
Yfit     <- predict(object = elasticnet, newx = as.matrix(test[, -4703]), type = "class", s = elasticnet$lambda.1se)
cf_elnet <- table(actual = test$Conference, predicted = Yfit, dnn = c("Truth", "Prediction"))
mc_elnet <- 1 - (sum(diag(cf_elnet)) / sum(cf_elnet))

Yfit     <- predict(object = elasticnet, newx = as.matrix(test[, -4703]), type = "class", s = elasticnet$lambda.min)
cf_elnet <- table(actual = test$Conference, predicted = Yfit, dnn = c("Truth", "Prediction"))
mc_elnet <- 1 - (sum(diag(cf_elnet)) / sum(cf_elnet))

# CV score w.r.t lambda
plot(x    = elasticnet$lambda, 
     y    = elasticnet$cvm, 
     main = "MSE vs Lambda",
     col  = "red")

# Number of features w.r.t lambda
plot(x    = elasticnet$lambda, 
     y    = elasticnet$nzero, 
     main = "Number of Features vs Lambda", 
     ylim = c(0, 100),
     col  = "blue")

# CV score w.r.t to log lamba using plot.glmnet()
plot(elasticnet, xvar = "lambda", label = TRUE)

#--------------------------------------------------------------------
# 2.2 (b) Support Vector Machine with Vanilladot kernel
svm_model <- ksvm(x       = Conference ~ ., 
                  data    = train, 
                  type    = "C-svc",
                  kernel  = "vanilladot")
# Training Error
mc_train <- attr(svm_model, "error")

#svm_model <- svm(x       = train[, -4703],
#                 y       = train$Conference,
#                 type    = "C-classification",
#                 kernel  = "linear")
## Train Error
#cf_svm_tr <- table(train$Conference, svm_model$fitted, dnn = c("Truth", "Prediction"))
#mc_svm_tr <- 1 - (sum(diag(cf_svm_tr)) / sum(cf_svm_tr))

# Test Error
Yfit   <- predict(object = svm_model, newdata = test[, -4703])
cf_svm <- table(test$Conference, Yfit, dnn = c("Truth", "Prediction"))
mc_svm <- 1 - (sum(diag(cf_svm)) / sum(cf_svm))

#--------------------------------------------------------------------
# 2.3 Benjamini-Hochberg method

ttests <- lapply(DBworld[, -4703], function(x) {
  t.test(x ~ DBworld[[4703]], data = DBworld, alternative = "two.sided", var.equal = FALSE)
})
#pvalues <- sapply(X = ttests, FUN = getElement, name = "p.value")
BH <- p.adjust(p = pvalues, method = "BH")

comparison <- as.data.frame(cbind(pvalues, BH))
comparison <- comparison[which(BH < 0.05), ]
comparison <- comparison[order(comparison$BH), ]

#--------------------------------------------------------------------
M   <- ncol(DBworld[, -4703]) # Total tests
FDR <- 0.05                   # FDR 

# p-values
pvalues <- sapply(DBworld[, -4703], function(x) {
  t.test(formula     = x ~ DBworld[[4703]], 
         data        = DBworld, 
         alternative = "two.sided", 
         var.equal   = FALSE)$p.value
})
pvals_signif <- pvalues[pvalues < 0.05]      # Significant p-values
pvals_sorted <- sort(pvals_signif)           # Sorted p-values
pvals_BH     <- order(pvals_sorted)/M * FDR  # BH adjusted p-values

# Selected features using BH
BH <- pvals_sorted[pvals_sorted < pvals_BH]

