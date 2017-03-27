SVM_output <- function(dataFrameTrain, dataFrameTest, targetVecTrain, targetVecTest, featureSelected, title = "ROC Curve for SVM"){
#package
require(e1071)
require(ROCR)
require(R.utils)
sourceDirectory("/Users/yuezhao/Desktop/projects/R_lib_jason/", modifiedOnly=TRUE)

#data prepare
data_rf <- as.data.frame(t(cbind(dataFrameTrain, dataFrameTest)))
data_rf$progress <- as.factor(c(targetVecTrain, targetVecTest))
colnames(data_rf) <- hyphenToUnderscore(colnames(data_rf))
inTrain <- seq(1,ncol(dataFrameTrain),1)
data_rf_train <- data_rf[inTrain,]
data_rf_test <- data_rf[-inTrain,]
featureSelected <- hyphenToUnderscore(featureSelected)

#body
varNames <- paste(featureSelected, collapse = "+")
rf.form <- as.formula(paste("as.factor(progress)", varNames, sep = " ~ "))
# rf.form <- as.formula(paste("progress", varNames, sep = " ~ "))
my_svm <- svm(rf.form, data=data_rf_train, type="C-classification",probability = TRUE)
# Make your prediction using the test set
my_prediction <- predict(my_svm, data_rf_test, decision.values = TRUE, probability = TRUE)
my_svm.pr = predict(my_svm,type="prob",newdata=data_rf_test,decision.values = TRUE, probability = TRUE)

#prediction is ROCR function
my_svm.pred = prediction(attributes(my_svm.pr)$decision.values, data_rf_test$progress)

#performance in terms of true and false positive rates
my_svm.perf = performance(my_svm.pred,"tpr","fpr")

#Not sure if this part is necessary.
par(mar=c(3,3,3,3))

#plot the curve
plot_handle <- plot(my_svm.perf,main=title,col=2,lwd=3, cex=4)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#compute area under curve
auc <- performance(my_svm.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc
text(0.6, 0.02, paste("AUC = ", round(auc,3)), cex=1, pos=4, col="red")
}



