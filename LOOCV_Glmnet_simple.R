#s=0, simple LR, without regularization
#nTimes runs
#LOO cross-validation.
require(glmnet)
require(caret)
require(ROCR)
require(bioDist)

LOOCV_Glmnet_simple<- function(dataFrame, targetVec, logisticRegression = FALSE, useAUC = TRUE, useHamming = FALSE, useProbPearson = FALSE, useKendallTau = FALSE){

if (logisticRegression == FALSE){

  predictionVec = c()
  for (k in 1:ncol(dataFrame)){
    train = t(as.matrix(dataFrame[,-k]))
 	  test = t(as.matrix(dataFrame[,k]))
    fit <- glmnet(train, targetVec[-k])  
    predictionVec[k] <- predict(fit, newx = test, s=0)
  }
  finalRsquare <- round(cor(predictionVec,targetVec),3)
  return(finalRsquare)  


} else{

#change target vec into zero-one binary vector  
targetVecFactor <- as.factor(targetVec)
levels(targetVecFactor) <- c(0,1)
#MUST into character before into numeric for "factor to numeric"
targetVecBinary <- as.numeric(as.character(targetVecFactor))


outputAuc <- NA
outputPearson <- NA
outputHamming <- NA
outputTau <- NA

vecReturn <- c()



  testPredictionClassVec <- c()
  testPredictionProbVec <- c() 

  for (k in 1:ncol(dataFrame)){
    #results for each run of LOOCV

    #train and test data
  	train = t(as.matrix(dataFrame[,-k]))
  	test = t(as.matrix(dataFrame[,k]))
    #fit the model and predict. Give both probability or class prediction results. 
    fit <- glmnet(train, targetVecBinary[-k],family = "binomial")
    testPredictionProbVec[k] <- predict(fit,type="response", newx = test, s = 0)
    testPredictionClassVec[k] <- predict(fit,type="class", newx = test, s = 0)
  }
  testPredictionProbVec <- as.numeric(testPredictionProbVec)
  testPredictionClassVec <- as.numeric(testPredictionClassVec)

  if (useAUC == TRUE){

    pred <- prediction(testPredictionProbVec, targetVecBinary)
    auc <- performance(pred,"auc")
    outputAuc <- unlist(slot(auc, "y.values"))
  }
    
  if (useHamming == TRUE){

  	outputHamming <- sum(testPredictionClassVec == targetVecBinary) / length(targetVecBinary)
 	}		

 	if (useProbPearson == TRUE){
    tmp1 <- testPredictionProbVec
    tmp2 <- targetVecBinary
 		tmp1 <- tmp1[1] + 0.001
 		tmp2 <- tmp2[1] + 0.001
 		outputPearson <- cor(tmp1, tmp2)
 	}

 	if (useKendallTau == TRUE){
    
 		Matrix_tau <- t(as.matrix(data.frame(testPredictionProbVec, targetVecBinary)))
 		outputTau <- tau.dist(Matrix_tau)[1] 			
 	}



vecReturn <- c(outputAuc, outputPearson, outputHamming, outputTau)
names(vecReturn) <- c("AUC", "Pearson","Hamming","Tau")
return(vecReturn)


}


}




