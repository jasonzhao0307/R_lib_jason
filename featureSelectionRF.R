
require(randomForest)
# Call sourceDirectory("/Users/yuezhao/Desktop/projects/R_lib_jason/", modifiedOnly=TRUE)

featureSelectionRF <- function(dataFrame, targetVec, numRuns = 5, nTree = 1000, useOverlap = FALSE, weirdSymbol = NA){

rownamesBackup <- rownames(dataFrame)

if(!is.factor(targetVec)){
	targetVec <- as.factor(targetVec)
}

if (length(weirdSymbol) == 1){
	if (!is.na(weirdSymbol)){
		for (wSb in weirdSymbol){

			gsubReplace <- paste("\\", wSb, sep="")
			rownames(dataFrame) <- gsub(gsubReplace, "", rownames(dataFrame))
		}
	}

} else{
		for (wSb in weirdSymbol){
			gsubReplace <- paste("\\", wSb, sep="")
			rownames(dataFrame) <- gsub(gsubReplace, "", rownames(dataFrame))
		}


}






tmp <- dataFrame
rownames(tmp) <- hyphenToUnderscore(rownames(tmp))

df <- t(tmp)
Y <- targetVec
df <- cbind(df, Y)

# doing CV and plot error with number of features
result.cv <- rfcv(df, Y, cv.fold = 5)
with(result.cv, plot(n.var, error.cv, log = "x", type = "o", lwd = 2))



if (useOverlap == TRUE){
	featureSelectionOverlap <- c()
	numOverlap <- c()
	for (i in seq(1,numRuns,1)) {
		fit_tmp=randomForest(factor(Y)~., ntree = nTree, data=df, importance=T)
		VI_F_tmp=data.frame(importance(fit_tmp))
		VI_F_ordered_tmp <- VI_F_tmp[order(VI_F_tmp$MeanDecreaseGini, decreasing = T),]
		result_vec_gene_tmp <- rownames(VI_F_ordered_tmp)[VI_F_ordered_tmp$MeanDecreaseGini > 0]
		if (i==1){
    		featureSelectionOverlap <- result_vec_gene_tmp
		}
  		featureSelectionOverlap <- intersect(result_vec_gene_tmp, featureSelectionOverlap)
	}
	return(featureSelectionOverlap)
}
else{




	featureMeanDecreaseGiniVec <- c()
	for (i in seq(1,numRuns,1)){
		fit_tmp=randomForest(factor(Y)~., ntree = nTree, data=df, importance=T)
		VI_F_tmp=data.frame(importance(fit_tmp))
		if (i==1){
    		featureMeanDecreaseGiniVec <- VI_F_tmp$MeanDecreaseGini
		}
		else{
			featureMeanDecreaseGiniVec <- featureMeanDecreaseGiniVec + VI_F_tmp$MeanDecreaseGini
		}

	}
	featureMeanDecreaseGiniVec <- featureMeanDecreaseGiniVec / numRuns
	indexOrdered <- order(featureMeanDecreaseGiniVec, decreasing = T)
	featureMeanDecreaseGiniVec <- featureMeanDecreaseGiniVec[indexOrdered]
	featureName <- rownamesBackup[indexOrdered]
	#	featureName <- hyphenToUnderscore(featureName, UnderscoreToHyphen = TRUE)
	scoreDF <- data.frame(featureName, featureMeanDecreaseGiniVec, stringsAsFactors = FALSE)
	return(scoreDF)


}


}
