require(glmnet)
require(ROCR)

colorVec <- c("red", "blue", "green", "black", "purple","yellow")

LOOAUC_multiple <- function(dfList, targetVec,signatureNameVec, nTimes = 50, nfolds = 3, title = "ROC curve", xLegendLocation=0.7, yLegendLocation = 0.5){
par(mar=c(3,3,3,3))
aucList <- list()

for (i in 1:length(dfList)){
	df <- dfList[[i]]
	nSample <- ncol(df)
	vecProb <- rep(0.0, length(targetVec))
	for (k in 1:nTimes){
		vecProbTmp <- c()
		for (j in 1:nSample){
			train = t(as.matrix(df[,-j]))
			test = t(as.matrix(df[,j]))
  	 	 	fit <- cv.glmnet(train, targetVec[-j], nfolds = nfolds, alpha = 0, family = "binomial", type.measure = "class")
  	 	 	testProb <- predict(fit,type="response", newx = test, s = 'lambda.min')
  	 	 	vecProbTmp <- c(vecProbTmp, testProb)
		}
		vecProb <- vecProb + vecProbTmp


	}

	vecProb <- vecProb / nTimes
	loo.pred = prediction(vecProb, targetVec)


#auc
#text(0.6, 0.02, paste("AUC = ", round(auc,3)), cex=1, pos=4, col="red")
	#compute area under curve
	loo.perf = performance(loo.pred,"tpr","fpr")
#plot the curve
if (i==1){
	plot(loo.perf,main=title,col=colorVec[i],lwd=3, cex=4)
	#legend(xLegendLocation,0.7,signatureNameVec[i], col=colorVec[i])
}
else{
	par(new = TRUE)
	plot(loo.perf,col=colorVec[i],lwd=3, cex=4)
	#legend(xLegendLocation,0.8-0.1*i,signatureNameVec[i],col=colorVec[i])
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










LOOAUC_single_df_noplot <- function(df, targetVec, nTimes = 50, nfolds = 3){
	auc.vec <- c()
	nSample <- ncol(df)
	for (k in 1:nTimes){
		vecProbTmp <- c()
		for (j in 1:nSample){
			train = t(as.matrix(df[,-j]))
			test = t(as.matrix(df[,j]))
  	 	fit <- cv.glmnet(train, targetVec[-j], nfolds = nfolds, alpha = 0, family = "binomial", type.measure = "class")
  	 	testProb <- predict(fit,type="response", newx = test, s = 'lambda.min')
  	 	vecProbTmp <- c(vecProbTmp, testProb)
		}
		  loo.pred = prediction(vecProbTmp, targetVec)
			loo.perf = performance(loo.pred,"tpr","fpr")
			auc <- performance(loo.pred,"auc")
			auc <- unlist(slot(auc, "y.values"))
			aucRound <- round(auc,3)
			auc.vec[k] <- aucRound
	}
return(auc.vec)
}
