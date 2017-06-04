# function for lasso feature selection
# Author: Jason Zhao


#Usage:
# 1. Load the function by: source("path/to/this/script")
# 2. call the function by: featureSelectionLasso(yourDataframeWithoutLabel, LabelVector)

#Note: 
# dataFrame is a m*n dataframe
# IF m represents features and n represents samples, keep default
# If n represents features and m represents samples, set rowIsFeature to FALSE when calling the function


#package
require(glmnet)


featureSelectionLasso <- function(dataFrame, targetVec, nfolds = 3, alpha = 1, rowIsFeature = TRUE){


#transpose the data
if (rowIsFeature == TRUE){
  x <- t(as.matrix(dataFrame))
}
else{
  x <- as.matrix(dataFrame)
}

#make target variable to factor 
targetVec <- as.factor(targetVec)

#run lasso
fit <- cv.glmnet(x, targetVec, alpha = alpha, family = "binomial", type.measure = "auc", nfolds = nfolds)

#select the features index with weights not zero
tmp_vec <- as.vector((coef(fit, s="lambda.min") != 0))

#get the selected features
if (rowIsFeature == TRUE){
  featureSelected <- rownames(dataFrame)[tmp_vec]
}
else{
  featureSelected <- colnames(dataFrame)[tmp_vec]
}

 
#return
return(featureSelected)

}