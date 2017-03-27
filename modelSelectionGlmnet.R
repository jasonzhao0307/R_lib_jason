#index from 1 to 10: alpha 0.1 to 1
#nSample split, nCV fold cross-validation.
require(glmnet)
require(caret)


modelSelectionGlmnet <- function(dataFrame, targetVec, nSample = 5, nCV = 5, logisticRegression = FALSE){

if (logisticRegression == FALSE){
meanRsquare <- rep(0,10)
for (i in 1:nSample){
  indexFold <- createFolds(dataFrame, k = nCV, list = TRUE, returnTrain = FALSE)
  
  for (k in 1:nCV){
    sample <- c()
    for (kk in 1:nCV){
      if (kk != k){
        sample <- c(sample, indexFold[[kk]])
      }
    }

  train = t(as.matrix(dataFrame[,sample]))
  test = t(as.matrix(dataFrame[,indexFold[[k]]]))
  index = 1
  for (j in seq(0.1,1,0.1)){
    fit <- cv.glmnet(train, targetVec[sample], nfolds = 10, alpha = j)  
    rsquare <- round(cor(predict(fit, newx = test, s = "lambda.min"),targetVec[indexFold[[k]]]),3)
    meanRsquare[index] <- meanRsquare[index] + rsquare
    index <- index + 1
  }
    
  }

}
meanRsquare <- meanRsquare/(nCV*nSample)
return(meanRsquare)  
} else{
targetVec <- as.factor(targetVec)
meanAUC <- rep(0,10)
for (i in 1:nSample){
  indexFold <- createFolds(dataFrame, k = nCV, list = TRUE, returnTrain = FALSE)
  
  for (k in 1:nCV){
    sample <- c()
    for (kk in 1:nCV){
      if (kk != k){
        sample <- c(sample, indexFold[[kk]])
      }
    }

  train = t(as.matrix(dataFrame[,sample]))
  test = t(as.matrix(dataFrame[,indexFold[[k]]]))
  index = 1
  for (j in seq(0.1,1,0.1)){
    fit <- cv.glmnet(train, targetVec[sample], nfolds = 5, alpha = j, family = "binomial", type.measure = "auc")
    testProb <- predict(fit,type="response", newx = test, s = 'lambda.min')
    pred <- prediction(testProb, targetVec[indexFold[[k]]])
    auc <- performance(pred,"auc")
    auc <- unlist(slot(auc, "y.values"))
    meanAUC[index] <- meanAUC[index] + auc
    index <- index + 1
  }
    
  }

}
meanAUC <- meanAUC/(nCV*nSample)
return(meanAUC)

}




}