# test

require(glmnet)
require(ROCR)
require(e1071)

colorVec <- c("red", "blue", "green", "black", "purple","yellow", "orange")

LOOAUC_simple_multiple <- function(dfList, targetVec,signatureNameVec, title = "ROC curve", xLegendLocation=0.7, yLegendLocation = 0.5){
par(mar=c(3,3,3,3))
aucList <- list()

for (i in 1:length(dfList)){
	df <- dfList[[i]]
	nSample <- ncol(df)
	vecProbTmp <- c()
	for (j in 1:nSample){
		train = t(as.matrix(df[,-j]))
		test = t(as.matrix(df[,j]))
  	 	 fit <- glmnet(train, targetVec[-j], family = "binomial")
  	 	 testProb <- predict(fit,type="response", newx = test, s = 0)
  	 	 vecProbTmp <- c(vecProbTmp, testProb)
	}
	loo.pred = prediction(vecProbTmp, targetVec)
	loo.perf = performance(loo.pred,"tpr","fpr")
#plot the curve
	if (i==1){
		plot(loo.perf,main=title,col=colorVec[i],lwd=3, cex=4)
	} else{
		par(new = TRUE)
		plot(loo.perf,col=colorVec[i],lwd=3, cex=4)
	}


	auc <- performance(loo.pred,"auc")
	auc <- unlist(slot(auc, "y.values"))
	aucRound <- round(auc,3)
	aucList[[i]] <- aucRound
	names(aucList)[i] <- colorVec[i]
}

abline(a=0,b=1,lwd=2,lty=2,col="gray")

signatureAndAUC <- c()
for (i in 1:length(dfList)){
	signatureAndAUC[i] <- paste(signatureNameVec[i], aucList[[i]], sep=", AUC=")
}
legend(xLegendLocation,yLegendLocation, signatureAndAUC, lty=c(rep(1,length(dfList))), col=colorVec)

return(aucList)
}




LOOAUC_simple_multiple_noplot <- function(dfList, targetVec){
auc.vec <- c()
for (i in 1:length(dfList)){
	df <- dfList[[i]]
	nSample <- ncol(df)
	vecProbTmp <- c()
	for (j in 1:nSample){
		train = t(as.matrix(df[,-j]))
		test = t(as.matrix(df[,j]))
  	 	 fit <- glmnet(train, targetVec[-j], family = "binomial")
  	 	 testProb <- predict(fit,type="response", newx = test, s = 0)
  	 	 vecProbTmp <- c(vecProbTmp, testProb)
	}
	loo.pred = prediction(vecProbTmp, targetVec)
	loo.perf = performance(loo.pred,"tpr","fpr")
	auc <- performance(loo.pred,"auc")
	auc <- unlist(slot(auc, "y.values"))
	aucRound <- round(auc,3)
	auc.vec <- c(auc.vec, aucRound)

}
  return(auc.vec)
}



LOOAUC_simple_multiple_noplot_one_df <- function(df, targetVec){
auc.vec <- c()
	nSample <- ncol(df)
	vecProbTmp <- c()
	testPredictionClassVec <- c()
	for (j in 1:nSample){
		train = t(as.matrix(df[,-j]))
		test = t(as.matrix(df[,j]))
  	 	 fit <- glmnet(train, targetVec[-j], family = "binomial")
  	 	 testProb <- predict(fit,type="response", newx = test, s = 0)
  	 	 vecProbTmp <- c(vecProbTmp, testProb)
			 testPredictionClassVec[j] <- predict(fit,type="class", newx = test, s = 0)
	}
	loo.pred = prediction(vecProbTmp, targetVec)
	loo.perf = performance(loo.pred,"tpr","fpr")
	auc <- performance(loo.pred,"auc")
	auc <- unlist(slot(auc, "y.values"))
	aucRound <- round(auc,3)
	auc.vec <- c(auc.vec, aucRound)
	# for other metric
	testPredictionClassVec <- as.numeric(testPredictionClassVec)
	cm = confusionMatrix(testPredictionClassVec, targetVec)
 	output.list <- list()
	output.list[[1]] <- auc.vec
	output.list[[2]] <- cm$byClass
	names(output.list) <- c("auc", "other")
  return(output.list)
}

Bootstrap_LOOCV_LR_AUC <- function(df, target.vec, nboot){
  output.auc.vec <- c()
	output.other.df <- NULL
  for (i in 1:nboot){
    index.boot <- sample(1:ncol(df), ncol(df), replace = T)
    df.tmp <- df[,index.boot]
    loo.output.list <- LOOAUC_simple_multiple_noplot_one_df(df.tmp, target.vec[index.boot])
    output.auc.vec[i] <- loo.output.list[[1]]
		output.other.df <- rbind(output.other.df, loo.output.list[[2]])
  }

	output.list <- list()
	output.list[[1]] <- output.auc.vec
	output.list[[2]] <- as.data.frame(output.other.df)
	names(output.list) <- c("auc", "other")
	return(output.list)

}
