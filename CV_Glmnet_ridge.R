#alpha = 0, ridge regression
#nSample split, nCV fold cross-validation.
require(glmnet)
require(caret)
require(ROCR)
require(bioDist)

CV_Glmnet_ridge <- function(dataFrame, targetVec, nSample = 5, nCV = 5, nfolds = 5, logisticRegression = FALSE, useAUC = TRUE, useJaccard = FALSE, useHamming = FALSE, useProbPearson = FALSE, useKendallTau = FALSE){

if (logisticRegression == FALSE){
vecRsquare <- rep(0,nSample*nCV)
index <- 1
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
	fit <- cv.glmnet(train, targetVec[sample], nfolds = nfolds, alpha = 0)  
    rsquare <- round(cor(predict(fit, newx = test, s = "lambda.min"),targetVec[indexFold[[k]]]),3)
    vecRsquare[index] <- rsquare
    index <- index + 1 
  }
}

return(vecRsquare)  

} else{
targetVec <- as.factor(targetVec)
vecAUC <- rep(0,nSample * nCV)
index <- 1
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
    fit <- cv.glmnet(train, targetVec[sample], nfolds = nfolds, alpha = 0, family = "binomial", type.measure = "class")
    if (useAUC == TRUE){
    	testProb <- predict(fit,type="response", newx = test, s = 'lambda.min')
    	pred <- prediction(testProb, targetVec[indexFold[[k]]])
    	auc <- performance(pred,"auc")
    	auc <- unlist(slot(auc, "y.values"))
    	vecAUC[index] <- auc
    	  
    }
    else{
    	testProb <- predict(fit,type="class", newx = test, s = 'lambda.min')
 		predictionBinary <- as.numeric(testProb)
 		targetBinary <- as.numeric(targetVec[indexFold[[k]]])
 		if (useJaccard == FALSE & useHamming == FALSE & useProbPearson == FALSE & useKendallTau == FALSE){
 			print("You must specify at least one of Jaccard or Hamming or ProbPearson to be TRUE")
 			return("You must specify at least one of Jaccard or Hamming or ProbPearson to be TRUE")
 		}
 		if (useJaccard == TRUE){
 		#denominator could be zero
 			if ((sum((predictionBinary == 0) & (targetBinary == 1)) + sum((predictionBinary == 1) & (targetBinary == 0)) + sum((predictionBinary == 1) & (targetBinary == 1))) == 0){
 				vecAUC[index] <- 0
 			}
 			else{
  				jaccardSimilarity <- sum((predictionBinary == 1) & (targetBinary == 1)) / (sum((predictionBinary == 0) & (targetBinary == 1)) + sum((predictionBinary == 1) & (targetBinary == 0)) + sum((predictionBinary == 1) & (targetBinary == 1)))
 				vecAUC[index] <- jaccardSimilarity			
 			}
 		}
  		if (useHamming == TRUE){
  			HammingSimilarity <- sum(predictionBinary == targetBinary) / length(targetBinary)
 			vecAUC[index] <- HammingSimilarity			
 		}		
 		if (useProbPearson == TRUE){
    		testProb <- predict(fit,type="response", newx = test, s = 'lambda.min')
 			predictionProb <- as.numeric(testProb)
 			#add a random number. Or for all zero or all one vector in targetBinary, the cor would be NA
 			predictionProb[1] <- predictionProb[1] + 0.001
 			targetBinary[1] <- targetBinary[1] + 0.001
 			vecAUC[index] <- cor(predictionProb, targetBinary)

 		}
 		if (useKendallTau == TRUE){
    		testProb <- predict(fit,type="response", newx = test, s = 'lambda.min')
 			predictionProb <- as.numeric(testProb)
 			#Kendall tau distance: better for binary and continuous correlation meature
 			predictionProb[1] <- predictionProb[1]
 			targetBinary[1] <- targetBinary[1]
 			Matrix_tau <- t(as.matrix(data.frame(predictionProb, targetBinary)))
 			vecAUC[index] <- tau.dist(Matrix_tau)[1] 			
 		}

    }
    index <- index + 1
  
  }
}

return(vecAUC)

}




}