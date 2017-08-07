# test

require(glmnet)
require(ROCR)

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
