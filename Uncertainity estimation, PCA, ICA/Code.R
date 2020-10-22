# Assignment 2 - Analysis of Credit Scoring
library("tree")
library("e1071")

# 1. Import data and divide into train, validation and test datasets

# Import creditscore data
creditscore <- xlsx::read.xlsx(
  file      = "C:/Users/namit/Downloads/Machine Learning/Lab2 Block1/creditscoring.xls",
  sheetName = "credit",
  header    = TRUE
)

# No of observations in the dataset
n = dim(creditscore)[1]

# Divide dataset into training, validation and test data
RNGversion('3.5.1')
set.seed(12345)
id1    = sample(1:n, floor(n / 2))
train  = creditscore[id1, ]
id_rem = setdiff(1:n, id1)

set.seed(12345)
id2   = sample(id_rem, floor(n / 4))
valid = creditscore[id2, ]
id3   = setdiff(id_rem, id2)
test  = creditscore[id3, ]

# 2. Fit a decision tree to training data using different measures of impurity

# a) Deviance
deviance_tree <- tree(formula = good_bad ~ .,
                      data    = train,
                      split   = c("deviance"))

# Predicted values and misclassification rates for train and test datasets
pred_train1    <- predict(object = deviance_tree, newdata = train, type = "class")
cfmat_train1   <- table(actual = train$good_bad, predicted = pred_train1)
mc_rate_train1 <- 1 - (sum(diag(cfmat_train1)) / sum(cfmat_train1))

pred_test1    <- predict(object = deviance_tree, newdata = test, type = "class")
cfmat_test1   <- table(actual = test$good_bad, predicted = pred_test1)
mc_rate_test1 <- 1 - (sum(diag(cfmat_test1)) / sum(cfmat_test1))


# b) Gini index
gini_tree <- tree(formula = good_bad ~ .,
                  data    = train,
                  split   = c("gini"))

# Predicted values and misclassification rates for train and test datasets
pred_train2    <- predict(object = gini_tree, newdata = train, type = "class")
cfmat_train2   <- table(actual = train$good_bad, predicted = pred_train2)
mc_rate_train2 <- 1 - (sum(diag(cfmat_train2)) / sum(cfmat_train2))

pred_test2     <- predict(object = gini_tree, newdata = test, type = "class")
cfmat_test2    <- table(actual = test$good_bad, predicted = pred_test2)
mc_rate_test2  <- 1 - (sum(diag(cfmat_test2)) / sum(cfmat_test2))

plot(deviance_tree)
text(deviance_tree, pretty = 0)

plot(gini_tree)
text(gini_tree, pretty = 0)

# Fitting a decision tree using deviance as th impurity measure gives a 
# better / lower misclassification rate.  


# 3. Selecting optimal tree by train and validation 
fit <- tree(formula = good_bad ~ ., data = train, split = c("deviance"))

trainScore <- rep(0,15)
testScore  <- rep(0,15)
for(i in 2:15) {
  prunedTree    <- prune.tree(fit, best = i)
  pred          <- predict(prunedTree, newdata = valid, type = "tree")
  trainScore[i] <- deviance(prunedTree)
  testScore[i]  <- deviance(pred)
}

# Plot deviance against tree sizes for train and validation data
plot(x    = 2:15, 
     y    = trainScore[2:15], 
     type = "b", 
     col  = "red", 
     ylim = c(250,600),
     xlab = "Number of Leaves",
     ylab = "Deviance")
points(2:15, testScore[2:15], type = "b", col = "blue")

# Choose optimal tree depth by pruning 
finalTree   <- prune.tree(fit, best = 4)
Yfit        <- predict(object = finalTree, newdata = test, type = "class")
cfmat_test  <- table(actual = test$good_bad, predicted = Yfit)
mc_test     <- 1- (sum(diag(cfmat_test)) / sum(cfmat_test))

tree_summary <- summary(finalTree)  # Summary of the optimal tree
tree_summary$used                   # Variables used in tree construction
length(tree_summary$used)           # Optimal tree depth

# Plot optimal tree 
plot(finalTree)
text(finalTree, pretty = 0)

# Optimal tree has 4 terminal nodes and uses three of the variables for 
# splitting the nodes. The variables used are savings, duration and history. 
# The misclassifcation rate of the optimal tree on the test data is 0.256

# 4. Classification using Naive Bayes
naivebayes <- naiveBayes(good_bad ~ ., data = train)

# Predicted values and misclassification rates for train and test datasets
pred_train    <- predict(object = naivebayes, newdata = train, type = "class")
cfmat_train   <- table(actual = train$good_bad, predicted = pred_train)
mc_rate_train <- 1 - (sum(diag(cfmat_train)) / sum(cfmat_train))

pred_test    <- predict(object = naivebayes, newdata = test, type = "class")
cfmat_test   <- table(actual = test$good_bad, predicted = pred_test)
mc_rate_test <- 1 - (sum(diag(cfmat_test)) / sum(cfmat_test))

# The misclassification rate on the test data is higher in case of naives bayes
# classification when compared to the optimal tree classification. 

# 5. Compare Optimal Tree and Naivebayes models and plot ROC
Yfit_optimaltree <- predict(object = finalTree, newdata = test)
Yfit_naivebayes  <- predict(object = naivebayes, newdata = test, type = "raw")

Yfit_optimaltree <- as.data.frame(Yfit_optimaltree)
Yfit_naivebayes  <- as.data.frame(Yfit_naivebayes)

pi_seq <- seq(0.05, 0.95, 0.05)
TPR <- data.frame(ot = numeric(length(pi_seq)),         # TPR for optimal tree
                  nb = numeric(length(pi_seq)))         # TPR for naive bayes
FPR <- data.frame(ot = numeric(length(pi_seq)),         # FPR for optimal tree
                  nb = numeric(length(pi_seq)))         # FPR for naive bayes
i <- 1
for (pi in pi_seq) {
  # Apply Classification principle on predictions from both models
  Yfit_ot  <- ifelse(Yfit_optimaltree$good > pi, 1, 0)
  Yfit_nb  <- ifelse(Yfit_naivebayes$good > pi, 1, 0)
  
  # Confusion matrices for the two models
  cfmat_ot <- table(test$good_bad, factor(Yfit_ot))
  cfmat_nb <- table(test$good_bad, factor(Yfit_nb))
  
  print(cfmat_ot)
  print(cfmat_nb)
  tryCatch({
      # True positive rates for the two models
      TPR$ot[i] <- cfmat_ot["good", "1"] / sum(cfmat_ot["good", ])
      TPR$nb[i] <- cfmat_nb["good", "1"] / sum(cfmat_nb["good", ])
      
      # False positive rates for the two models  
      FPR$ot[i] <- cfmat_ot["bad", "1"] / sum(cfmat_ot["bad", ])
      FPR$nb[i] <- cfmat_nb["bad", "1"] / sum(cfmat_nb["bad", ])
      
      i <- i + 1 
    }, 
    error = function(e) {}
  )
}
# Plot the ROC curves for both optimal tree and naive bayes models
plot(x    = FPR$ot, 
     y    = TPR$ot, 
     type = "o", 
     col  = "red", 
     xlab = "FPR",
     ylab = "TPR")
points(FPR$nb, TPR$nb, type = "o", col = "blue")
legend("topleft", 
       legend = c("Regression Tree", "Naive Bayes"), 
       fill   = c("red", "blue"),
       cex    = 0.8)

# The red ROC curve represents the TPR and FPR for the optimal regression tree 
# classifier and the blue ROC curve gives the TPR and FPR for the naive bayes 
# classifier. It can be seen that the area under the curve (AUC) is greater for 
# the blue curve than the red and hence, for the same FPR,cex = 0.8 the former gives a 
# higher TPR. Depending on how many False Positives are acceptable, it may be 
# said that the naive bayes is a better classifier than the optimal regression 
# tree.

# 6. Naive Bayes classification with a loss matrix
L <- data.frame(pred.bad  = c(0, 1),
                pred.good = c(10, 0),
                row.names = c("obs.bad", "obs.good"))

naivebayes <- naiveBayes(good_bad ~ ., data = train)
Yfit       <- predict(object = naivebayes, newdata = test, type = "raw")
Yfit_loss  <- data.frame(bad  = as.data.frame(Yfit)$bad * 10,
                         good = as.data.frame(Yfit)$good * 1)
Yfit_val   <- ifelse(Yfit_loss$good > Yfit_loss$bad, "good", "bad")
cfmat      <- table(actual = test$good_bad, predicted = Yfit_val)
mc_rate    <- 1 - (sum(diag(cfmat)) / sum(cfmat))

# False positive rate comparison
FPR        <- c(cfmat["bad", "good"] / sum(cfmat["bad", ]), 
                cfmat_test["bad", "good"] / sum(cfmat_test["bad", ]))

# The misclassification rate is higher when the naive bayes classification 
# uses the loss matrix. However, the false positive rate is much smaller when 
# compared to naive bayes classification with even loss. Because the penalty 
# for classifying a bad customer as good is very high, the model uses a higher 
# threshold for classifying a customer as "good". As a result, we see an 
# decrease in the true and false positive rates. 

# Assignment 3 - Uncertainty estimation
library("boot")

# Import state data
state <- read.csv2(
  file      = "C:/Users/namit/Downloads/Machine Learning/Lab2 Block1/State.csv",
  header    = TRUE
)

# No of observations in the dataset
n <- dim(state)[1]

# 1. Plot EX versus MET
state_ord <- state[order(state$MET), ]
plot(x    = state_ord$MET,
     y    = state_ord$EX,
     col  = "blue", 
     xlab = "% population in standard metropolitan areas",
     ylab = "Per capita state and local public expenditures ($)")

# 2. Fit a regression tree model
regtree <- tree(formula = EX ~ MET,
                data    = state_ord, 
                control = tree.control(nobs = n, minsize = 8))

cvtree <- cv.tree(regtree)
plot(x = cvtree$size, 
     y = cvtree$dev, 
     type = "b", 
     col  = "red", 
     main = "Regression Tree Deviances",
     xlab = "Size",
     ylab = "Deviance")

# Optimized regression tree
finalTree <- prune.tree(regtree, best = 3)
plot(finalTree)
text(finalTree, pretty = 0)

# Predictions and residuals
Yfit      <- predict(object = finalTree, newdata = state_ord)
residuals <- residuals(finalTree)

## Plot fitted values and actual values and residuals 
plot(Yfit, residuals, col = "red")
points(state_ord$EX, residuals, col = "blue")

# Plot fitted values and actual values
plot(Yfit, col = "red", ylim = c(0,500))
points(state_ord$EX, type = "b", col = "blue")

# Plot Residuals
hist(residuals) 
plot(density(residuals))

# 3. Plot 95% confidence bands for the regression tree using non-parametric bootstrap

# computing bootstrap samples
f_np <- function(data, ind){
  # Extract bootstrap sample
  bs_sample <- data[ind, ]  

  # Fit regression tree
  regtree <- prune.tree(tree(EX ~ MET, bs_sample, minsize = 8), best = 3) 
  # Predict EX for all MET from the original data
  exPredict = predict(regtree, newdata = state_ord)
  return(exPredict)
}

# Make bootstrap
np_bootstrap <- boot(state_ord, f_np, R = 1000) 

# Compute confidence bands
cf_band   <- envelope(np_bootstrap)
#regtree   <- prune.tree(tree(EX ~ MET, state_ord, minsize = 8), best = 3) 
#exPredict <- predict(regtree)
exPredict <- np_bootstrap$t0

# Plot fitted line
plot(state_ord$MET, state_ord$EX, pch = 21, bg = "orange")
points(state_ord$MET, exPredict, type = "l") 

# Plot cofidence bands
points(state_ord$MET, cf_band$point[2,], type = "l", col = "blue")
points(state_ord$MET, cf_band$point[1,], type = "l", col = "blue")


# 4. Plot 95% onfidence bands for the regression tree using parametric bootstrap
MLE <- prune.tree(tree(EX ~ MET, state_ord, minsize = 8), best = 3) 

RNG <- function(data, mle) {
  datanew <- data.frame(EX = data$EX, MET = data$MET)
  n       <- length(data$EX)
  # Generate new Expenditure
  datanew$EX <- rnorm(n, predict(MLE, newdata = datanew), sd(residuals(MLE)))
  return(datanew)
}

fp <- function(data){
  # Fit regression tree
  regtree <- prune.tree(tree(EX ~ MET, data, minsize = 8), best = 3)  
  # Predict values for all MET values from the original data
  exPredict <- predict(regtree, newdata = state_ord)
  return(exPredict)
}

# Make bootstrap
p_bootstrap = boot(state_ord, statistic = fp, R = 1000, mle = MLE, ran.gen = RNG, sim = "parametric")

# Compute confidence bands
cf_band   <- envelope(p_bootstrap)
#regtree   <- prune.tree(tree(EX ~ MET, state_ord, minsize = 8), best = 3) 
#exPredict <- exPredict <- predict(regtree)
exPredict <- p_bootstrap$t0

# Plot fitted line
plot(state_ord$MET, state_ord$EX, pch = 21, bg = "orange")
points(state_ord$MET, exPredict, type = "l") 

# Plot cofidence bands
points(state_ord$MET, cf_band$point[2,], type = "l", col = "blue")
points(state_ord$MET, cf_band$point[1,], type = "l", col = "blue")

# 5. 

# Assignment 4 - Principal Components
library("fastICA")

# Import near-infrared spectra data
NIRSpectra <- read.csv2(
  file      = "C:/Users/namit/Downloads/Machine Learning/Lab2 Block1/NIRSpectra.csv",
  header    = TRUE
)

# No of observations in the dataset
n <- dim(NIRSpectra)[1]

# 1. Standard PCA
PC        <- prcomp(NIRSpectra[, -127])                   # PCA
lambda    <- PC$sdev ^ 2                                  # Eigenvalues  
variation <- sprintf("%2.3f", lambda / sum(lambda) * 100) # Proportion of variation
screeplot(PC)

# Data in (PC1, PC2) - Scores (z)
plot(x    = PC$x[, 1], 
     y    = PC$x[, 2],
     xlab = "Feature 1",
     ylab = "Feature 2")
     
# 2. Trace plots of the principal component loadings
U <- PC$rotation
plot(U[, 1], main = "Traceplot, PC1")                    
plot(U[, 2], main = "Traceplot, PC2")                    


# 3. Independent Component Analysis
library(fastICA)
set.seed(12345)
ica = fastICA(NIRSpectra[, -127], 2)
Wt = ica$K %*% ica$W
#Wt
plot(Wt[,1], main = "Traceplot, PC1")
plot(Wt[,2], main = "Traceplot, PC2")

set.seed (12345)
IC          <- fastICA(NIRSpectra[, -127], 2)             # ICA   
W_transpose <- IC$K %*% IC$W                              # Estimated un-mixing matrix   

# 3.a Trace plots of columns of W'
plot(W_transpose[, 1], main = "Traceplot, PC1")
plot(W_transpose[, 2], main = "Traceplot, PC2")

# 3.b Scores of the first two latent features

# Plot latent features
# This is the Matrix which gives the loadings i.e. when X is projected
# on to these, we get independent component. Just like how in PCA, we 
# get a matrix with the eigenvectors (loadings) and we project data 
# onto those direcions, we get the principal components
v = solve(IC$W)
z = IC$X %*% W_transpose
z = z %*% v
plot(z)

# (or) 
# This is correct 
# (After projecting X onto the independent component directions we get S)
# S = XKW
# K = pre whitening matrix i.e. the principal component directions (eigenvector
# vector/loadings matrix)
# W = Un-mixing matrix i.e. the one that extracts the independent components
# inv(A) where X = SA. Independent components in S mixed linearly as based 
# by A
plot(IC$S)

