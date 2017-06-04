
require(randomForest)
# Call sourceDirectory("/Users/yuezhao/Desktop/projects/R_lib_jason/", modifiedOnly=TRUE)

Get_RF_prediction <- function(dataFrameTrain, targetVec, dataFrameTest, featureSelected, nTree = 1000, useOverlap = FALSE, weirdSymbol = NA){

rownamesBackup <- rownames(dataFrameTrain)

if(!is.factor(targetVec)){
	targetVec <- as.factor(targetVec)
}

if (length(weirdSymbol) == 1){
	if (!is.na(weirdSymbol)){
		for (wSb in weirdSymbol){
			
			gsubReplace <- paste("\\", wSb, sep="")
			rownames(dataFrameTrain) <- gsub(gsubReplace, "", rownames(dataFrameTrain))
			rownames(dataFrameTest) <- gsub(gsubReplace, "", rownames(dataFrameTest))
		}
	}	

} else{
		for (wSb in weirdSymbol){
			gsubReplace <- paste("\\", wSb, sep="")
			rownames(dataFrameTrain) <- gsub(gsubReplace, "", rownames(dataFrameTrain))
			rownames(dataFrameTest) <- gsub(gsubReplace, "", rownames(dataFrameTest))
		}
	

}



data_rf <- as.data.frame(t(cbind(dataFrameTrain, dataFrameTest)))

inTrain <- seq(1,ncol(dataFrameTrain),1)
data_rf_train <- data_rf[inTrain,]
data_rf_train$progress <- as.factor(c(targetVec))
data_rf_test <- data_rf[-inTrain,]
featureSelected <- hyphenToUnderscore(featureSelected)

#print(featureSelected)

#body
varNames <- paste(featureSelected, collapse = "+")
rf.form <- as.formula(paste("as.factor(progress)", varNames, sep = " ~ "))
my_forest <- randomForest(rf.form, data=data_rf_train, importance=TRUE, ntree=nTree, keep.forest=TRUE)
rank <- round(importance(my_forest), 2)

# Make your prediction using the test set
my_prediction <- predict(my_forest, data_rf_test)

return(my_prediction)
}
