#alpha = 0, ridge regression
#nTimes runs
#LOO cross-validation.
require(glmnet)
require(caret)
require(ROCR)
require(bioDist)

LOOCV_Glmnet_ridge <- function(dataFrame, targetVec, nTimes = 5, nfolds = 5, logisticRegression = FALSE, useAUC = TRUE, useHamming = FALSE, useProbPearson = FALSE, useKendallTau = FALSE){

if (logisticRegression == FALSE){

index <- 1
for (i in 1:nTimes){
  rSquareVec = c()
  for (k in 1:ncol(dataFrame)){
  train = t(as.matrix(dataFrame[,-k]))
 	test = t(as.matrix(dataFrame[,k]))
	fit <- cv.glmnet(train, targetVec[-k], nfolds = nfolds, alpha = 0)  
  #rsquare <- round(cor(predict(fit, newx = test, s = "lambda.min"),targetVec[k]),3)
  predictionVec[k] <- predict(fit, newx = test, s = "lambda.min")
  }
  vecRsquare[index] <- round(cor(predictionVec,targetVec),3)
  index <- index + 1 
}

return(vecRsquare)  

} else{

#change target vec into zero-one binary vector  
targetVecFactor <- as.factor(targetVec)
levels(targetVecFactor) <- c(0,1)
#MUST into character before into numeric for "factor to numeric"
targetVecBinary <- as.numeric(as.character(targetVecFactor))


vecAUC <- c()
vecHamming <- c()
vecPearson <- c()
vecTau <- c()

listReturn <- list()


for (i in 1:nTimes){
  testPredictionClassVec <- c()
  testPredictionProbVec <- c() 

  for (k in 1:ncol(dataFrame)){
    #results for each run of LOOCV

    #train and test data
  	train = t(as.matrix(dataFrame[,-k]))
  	test = t(as.matrix(dataFrame[,k]))
    #fit the model and predict. Give both probability or class prediction results. 
    fit <- cv.glmnet(train, targetVecBinary[-k], nfolds = nfolds, alpha = 0, family = "binomial", type.measure = "class")
    testPredictionProbVec[k] <- predict(fit,type="response", newx = test, s = 'lambda.min')
    testPredictionClassVec[k] <- predict(fit,type="class", newx = test, s = 'lambda.min')
  }
  testPredictionProbVec <- as.numeric(testPredictionProbVec)
  testPredictionClassVec <- as.numeric(testPredictionClassVec)

  if (useAUC == TRUE){

    pred <- prediction(testPredictionProbVec, targetVecBinary)
    auc <- performance(pred,"auc")
    auc <- unlist(slot(auc, "y.values"))
    vecAUC[i] <- auc	  
  }
    
  if (useHamming == TRUE){

  	vecHamming[i] <- sum(testPredictionClassVec == targetVecBinary) / length(targetVecBinary)
 	}		

 	if (useProbPearson == TRUE){
    tmp1 <- testPredictionProbVec
    tmp2 <- targetVecBinary
 		tmp1 <- tmp1[1] + 0.001
 		tmp2 <- tmp2[1] + 0.001
 		vecPearson[i] <- cor(tmp1, tmp2)
 	}

 	if (useKendallTau == TRUE){
    
 		Matrix_tau <- t(as.matrix(data.frame(testPredictionProbVec, targetVecBinary)))
 		vecTau[i] <- tau.dist(Matrix_tau)[1] 			
 	}

}
listReturn[[1]] <- vecAUC
listReturn[[2]] <- vecHamming
listReturn[[3]] <- vecPearson
listReturn[[4]] <- vecTau
names(listReturn) <- c("AUC", "Hamming", "Pearson", "Tau")
return(listReturn)


}


}