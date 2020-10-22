## Assignment 1 - Spam classification with nearest neighbors

# 1. Import spam data from excel file
spambase <- xlsx::read.xlsx(
              file      = "C:/Users/namit/Downloads/Machine Learning/Lab1 Block1/spambase.xlsx", 
              sheetName = "spambase_data",
              header    = TRUE)

# No of observations in the dataset
n <- dim(spambase)[1]

# Divide dataset into training and test data
set.seed(12345)
index <- sample(1:n, floor(n / 2))
train <- spambase[index, ]
test  <- spambase[-index, ]

# Create a logistic regression model using the training dataset
logistic.reg <- glm(formula = Spam ~ ., 
                    data    = train, 
                    family  = binomial(link = "logit"))

# Use the logistic model to predict spam in the training dataset
result_train <- predict(object  = logistic.reg, 
                        newdata = train, 
                        type    = "response")

# Use the logistic model to predict spam in the test dataset
result_test  <- predict(object  = logistic.reg, 
                        newdata = test, 
                        type    = "response")

# 2. using classification threshold 0.5, compare the confusion matrices and 
# misclassification rates for training and test datasets
train_pred1          <- ifelse(result_train > 0.5, 1, 0)
train_conf_mat1      <- table(actual = train$Spam, predicted = train_pred1)
train_misclass_rate1 <- 1 - (sum(diag(train_conf_mat1)) / sum(train_conf_mat1))

test_pred1           <- ifelse(result_test > 0.5, 1, 0) 
test_conf_mat1       <- table(actual = test$Spam, predicted  = test_pred1)
test_misclass_rate1  <- c("Misclassification Rate" = 
                            1 - (sum(diag(test_conf_mat1)) / sum(test_conf_mat1)))

  
# 3. using classification threshold 0.8, compare the confusion matrices and 
# misclassification rates for training and test datasets
train_pred2          <- ifelse(result_train > 0.8, 1, 0)
train_conf_mat2      <- table(actual = train$Spam, predicted = train_pred2)
train_misclass_rate2 <- 1 - (sum(diag(train_conf_mat2)) / sum(train_conf_mat2))

test_pred2           <- ifelse(result_test > 0.8, 1, 0) 
test_conf_mat2       <- table(actual = test$Spam, predicted  = test_pred2)
test_misclass_rate2  <- 1 - (sum(diag(test_conf_mat2)) / sum(test_conf_mat2))

train_conf_mat1 
train_misclass_rate1
test_conf_mat1
test_misclass_rate1

train_conf_mat2
train_misclass_rate2
test_conf_mat2
test_misclass_rate2

# The rate of misclassification increased with the increase in threshold
# value used for classification. Hence, the classification 0.5 is much more 
# effective than the classification 0.8

# 4. Use kknn to classify spam emails in training and test datasets 
# using k = 30 nearest neighbours

# Classify Spam in training dataset
kknn_train1 <- kknn::kknn(formula = Spam ~ ., 
                          train   = train,
                          test    = train,
                          k       = 30)

# Classify Spam in test dataset
kknn_test1  <- kknn::kknn(formula = Spam ~ ., 
                          train   = train,
                          test    = test,
                          k       = 30) 

# Because there are only two classes, Spam = 1 and No Spam = 0,
# we classify the fitted values as 0 or 1 based on threshold 
# probability 0.5
train_pred_1         <- ifelse(kknn_train1$fitted.values > 0.5, 1, 0)
train_conf_mat1      <- table(actual = train$Spam, predicted = train_pred_1)
train_misclass_rate1 <- 1 - (sum(diag(train_conf_mat1)) / sum(train_conf_mat1))

test_pred_1          <- ifelse(kknn_test1$fitted.values > 0.5, 1, 0)
test_conf_mat1       <- table(actual = test$Spam, predicted = test_pred_1)
test_misclass_rate1  <- 1 - (sum(diag(test_conf_mat1)) / sum(test_conf_mat1))

# 5. Use kknn to classify spam emails in training and test datasets 
# using k = 1 nearest neighbours

# Classify Spam in training dataset
kknn_train2 <- kknn::kknn(formula = Spam ~ ., 
                          train   = train,
                          test    = train,
                          k       = 1)

# Classify Spam in test dataset
kknn_test2  <- kknn::kknn(formula = Spam ~ ., 
                          train   = train,
                          test    = test,
                          k       = 1) 

# Because there are only two classes, Spam = 1 and No Spam = 0,
# we classify the fitted values as 0 or 1 based on threshold 
# probability 0.5
train_pred_2         <- ifelse(kknn_train2$fitted.values > 0.5, 1, 0)
train_conf_mat2      <- table(actual = train$Spam, predicted = train_pred_2)
train_misclass_rate2 <- 1 - (sum(diag(train_conf_mat2)) / sum(train_conf_mat2))

test_pred_2          <- ifelse(kknn_test2$fitted.values > 0.5, 1, 0)
test_conf_mat2       <- table(actual = test$Spam, predicted = test_pred_2)
test_misclass_rate2  <- 1 - (sum(diag(test_conf_mat2)) / sum(test_conf_mat2))

# The resulting misclassification rate using the kknn classification
# (with k = 30) is greater than the misclassification rate using the logistic 
# model (with threshold = 0.5) for the training dataset whereas it is the 
# same for the test data. The two models are comparable in terms of 
# their performance. 

# On the other hand, the kknn classification proves to be a better model 
# than the logistic model (with threshold = 0.8) as it has lower 
# misclassifcation rates for both test and training data. 

# By reducing k, The misclassification rate using the kknn classification
# (with k = 1) is zero for training data but very high for the test data.
# The model is overfitted to the training data and is poorer compared 
# to the logistic model (with threshold = 0.5 or 0.8) because it has higher
# misclassification rates on the test dataset. 


## Assignment 3 - Feature selection by cross-validation in a linear model

# 3.1 Implement an R function that performs feature selection (best 
# subset selection) in linear regression by using k-fold

# Linear regression
mylin = function(X, Y, Xpred) {
  X1     <- cbind(1, X)
  Xpred1 <- cbind(1, Xpred)
  beta   <- solve(t(X1) %*% X1) %*% (t(X1) %*% Y)
  Res    <- Xpred1 %*% beta
  return(Res)
}

# Cross validation 
myCV = function(X ,Y, Nfolds) {
  n <-length(Y)
  p <- ncol(X)
  
  set.seed(12345)
  ind      <- sample(n, n)
  X1       <- X[ind, ]
  Y1       <- Y[ind]
  sF       <- floor(n / Nfolds)
  MSE      <- numeric(2 ^ p - 1)
  Nfeat    <- numeric(2 ^ p - 1)
  Features <- list()
  curr     <- 0
  
  # We assume 5 features.
  for (f1 in 0:1)
    for (f2 in 0:1)
      for(f3 in 0:1)
        for(f4 in 0:1)
          for(f5 in 0:1) {
            model <- c(f1, f2, f3, f4, f5)
            if (sum(model) == 0) next()
            SSE <- 0
            
            for (k in 1:Nfolds) {
              # Compute which indices should belong to current fold
              index   <- (((k - 1) * sF) + 1):(k * sF)
              
              # Implement cross-validation for model with features in "model" 
              # and iteration k.              
              test_x  <- X1[index, which(model == 1)]
              train_x <- X1[-index, which(model == 1)] 
              Yp      <- Y1[index]
              train_y <- Y1[-index]
              
              # Get the predicted values for fold 'k', Ypred, and the original 
              # values for fold 'k', Yp.  
              Ypred <- mylin(train_x, train_y, test_x)
              SSE   <- SSE + sum((Ypred - Yp) ^ 2)
            }
            curr             <- curr + 1
            MSE[curr]        <- SSE / n
            Nfeat[curr]      <- sum(model)
            Features[[curr]] <- model
          }
  
  # Plot MSE against number of features
  plot(Nfeat, MSE)
  
  # Best fit model
  i <- which.min(MSE)
  return(list(CV = MSE[i], Features = Features[[i]]))
}

# 3.2 Test implementation using swiss dataset
myCV(as.matrix(swiss[, 2:6]), swiss[[1]], 5)

## Assignment 4 - Linear Regression and Regularization

# 1. Import tecator data from excel file
tecator <- xlsx::read.xlsx(
  file      = "C:/Users/namit/Downloads/Machine Learning/Lab1 Block1/tecator.xlsx", 
  sheetName = "data",
  header    = TRUE)

# Plot Moisture vs Protein
plot(tecator$Moisture, tecator$Protein, col = 2)

# 2. Models Mi where expected moisture is a polynomial function (upto power i)
# of protein and the moisture is normally distributed

# 3. Fit Models Mi, i = 1,2,3,4,5,6 for training and test datasets

# No of observations in the dataset
n <- dim(tecator)[1]

# Divide dataset into training and test data
suppressWarnings(RNGversion(("3.5.9")))
set.seed(12345)
index <- sample(1:n, floor(n / 2))
train <- tecator[index, ] 
test  <- tecator[-index, ]

MSE_training   <- numeric(6)
MSE_validation <- numeric(6)

for(i in 1:6) {
  # For Model Mi: Mosture ~ Protein ^ i, calculate the training and 
  # validation MSE on training and test datasets
  
  # Training SSE and MSE
  linreg            <- lm(formula = Moisture ~ poly(Protein, i, raw = TRUE), 
                          data    = train)
  Ypred             <- predict(object  = linreg, 
                               newdata = train,
                               type    = "response")
  Yp                <- train$Moisture  
  MSE_training[i]   <- mean((Ypred - Yp) ^ 2)
  
  # Validation SSE and MSE
  Ypred             <- predict(object  = linreg, 
                               newdata = test,
                               type    = "response")
  Yp                <- test$Moisture  
  MSE_validation[i] <- mean((Ypred - Yp) ^ 2)
}

# Plot training and validation MSE vs highest degree of protein 
# polynomial function
ggplot2::ggplot(mapping = ggplot2::aes(x = 1:6, y = MSE_training)) +
  ggplot2::geom_point(shape = 1) +
  ggplot2::theme_classic()       + 
  ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                 panel.border = ggplot2::element_rect(fill = NA))  +
  ggplot2::geom_smooth(method = stats::loess, se = FALSE, col = 2) +
  ggplot2::labs(x = "Highest degree of Protein function",
                y = "Training MSE")

ggplot2::ggplot(mapping = ggplot2::aes(x = 1:6, y = MSE_validation)) +
  ggplot2::geom_point(shape = 1) +
  ggplot2::theme_classic()       +  
  ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                 panel.border = ggplot2::element_rect(fill = NA))  + 
  ggplot2::geom_smooth(method = stats::loess, se = FALSE, col = 4) +
  ggplot2::labs(x = "Highest degree of Protein function",
                y = "Validation MSE")

plot(1:6, MSE_training, col="red", type="l", pch=16, ylim=c(31,35))
points(1:6, MSE_validation, col="blue", type="l", pch=16)


# 4. Variable Selection using stepAIC
linreg  <- lm(formula = Fat ~ . - Sample - Protein - Moisture, 
              data    = tecator)
stepAIC <- MASS::stepAIC(object    = linreg, 
                         direction = "both", 
                         trace     = FALSE)

linreg2 <- lm(Fat ~ Channel1  + Channel2  + Channel4  + Channel5  + 
                    Channel7  + Channel8  + Channel11 + Channel12 + Channel13 + 
                    Channel14 + Channel15 + Channel17 + Channel19 + Channel20 + 
                    Channel22 + Channel24 + Channel25 + Channel26 + Channel28 + 
                    Channel29 + Channel30 + Channel32 + Channel34 + Channel36 + 
                    Channel37 + Channel39 + Channel40 + Channel41 + Channel42 + 
                    Channel45 + Channel46 + Channel47 + Channel48 + Channel50 + 
                    Channel51 + Channel52 + Channel54 + Channel55 + Channel56 + 
                    Channel59 + Channel60 + Channel61 + Channel63 + Channel64 + 
                    Channel65 + Channel67 + Channel68 + Channel69 + Channel71 + 
                    Channel73 + Channel74 + Channel78 + Channel79 + Channel80 + 
                    Channel81 + Channel84 + Channel85 + Channel87 + Channel88 + 
                    Channel92 + Channel94 + Channel98 + Channel99, tecator)
Ypred  <- predict(linreg2, tecator)
MSE    <- mean((Ypred - tecator$Fat) ^ 2)

# 5. Ridge Regression
ridgereg <- glmnet::glmnet(x      = as.matrix(tecator[ ,2:101]), 
                           y      = tecator$Fat,
                           family = "gaussian",
                           alpha  = 0)

plot(ridgereg, xvar="lambda", label = TRUE)

# 6. Lasso Regresssion
lassoreg <- glmnet::glmnet(x      = as.matrix(tecator[ ,2:101]), 
                           y      = tecator$Fat,
                           family = "gaussian",
                           alpha  = 1)

plot(lassoreg, xvar="lambda", label = TRUE)

# 7. Cross validation to find optimal LASSO model
lasso.cv <- glmnet::cv.glmnet(x            = as.matrix(tecator[ ,2:101]),
                              y            = tecator$Fat,
                              family       = "gaussian",
                              alpha        = 1,
                              lambda       = seq(0, 7, 0.001),                              
                              type.measure = "mse") 

# Number of features selected for optimum lambda
feat_optimum <- lasso.cv$nzero[lasso.cv$lambda == lasso.cv$lambda.1se]
cv_optimum   <- lasso.cv$cvm[lasso.cv$lambda == lasso.cv$lambda.1se]

# CV score w.r.t lambda
plot(x = lasso.cv$lambda, lasso.cv$cvm, main = "MSE vs Lambda")

# Number of features w.r.t lambda
plot(x = lasso.cv$lambda, lasso.cv$nzero, main = "Number of Features vs Lambda")

# CV score w.r.t to log lamba using plot.glmnet()
plot(lasso.cv, xvar = "lambda", label = TRUE)

# For minimum lambda = 0.01073, the MSE is minimum 11.37 with 22 selected
# features. For lambda = 0.02986, we get a MSE of 12.34 with 14 selected 
# features


# 7. Cross validation to find optimal LASSO model
lasso.cv <- glmnet::cv.glmnet(x            = as.matrix(tecator[ ,2:101]),
                              y            = tecator$Fat,
                              family       = "gaussian",
                              alpha        = 1,
                              lambda       = seq(0, 7, 0.001),                              
                              type.measure = "mse") 

# Number of features selected for optimum lambda
feat_optimum <- lasso.cv$nzero[lasso.cv$lambda == lasso.cv$lambda.min]
cv_optimum   <- lasso.cv$cvm[lasso.cv$lambda == lasso.cv$lambda.min]

# CV score w.r.t lambda
plot(x = lasso.cv$lambda, lasso.cv$cvm, main = "MSE vs Lambda")

# Number of features w.r.t lambda
plot(x = lasso.cv$lambda, lasso.cv$nzero, main = "Number of Features vs Lambda")

# CV score w.r.t to log lamba using plot.glmnet()
plot(lasso.cv, xvar = "lambda", label = TRUE)

# For minimum lambda = 0.01073, the MSE is minimum 11.37 with 22 selected
# features. For lambda = 0.02986, we get a MSE of 12.34 with 14 selected 
# features

