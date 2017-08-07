RF_output <- function(dataFrameTrain, dataFrameTest, targetVecTrain, targetVecTest, featureSelected, nTree = 100, title = "Random Forest ROC Curve"){
#package
require(randomForest)
require(ROCR)
require(R.utils)
sourceDirectory("/Users/yuezhao/Desktop/projects/R_lib_jason/", modifiedOnly=TRUE)


data_rf_train <- as.data.frame(t(cbind(dataFrameTrain)))
data_rf_test <- as.data.frame(t(cbind(dataFrameTest)))

data_rf_train$progress <- as.factor(c(targetVecTrain))
data_rf_test$progress <- as.factor(c(targetVecTest))

colnames(data_rf_train) <- hyphenToUnderscore(colnames(data_rf_train))
colnames(data_rf_test) <- hyphenToUnderscore(colnames(data_rf_test))

featureSelected <- hyphenToUnderscore(featureSelected)

#print(featureSelected)

#body
varNames <- paste(featureSelected, collapse = "+")
rf.form <- as.formula(paste("as.factor(progress)", varNames, sep = " ~ "))
my_forest <- randomForest(rf.form, data=data_rf_train, importance=TRUE, ntree=nTree, keep.forest=TRUE)
rank <- round(importance(my_forest), 2)

# Make your prediction using the test set
my_prediction <- predict(my_forest, data_rf_test)

# generate probabilities instead of class labels type="prob" ensures that
#randomForest generates probabilities for both the class labels,
#we are selecting one of the value [2] at the end does that
my_forest.pr = predict(my_forest,type="prob",newdata=data_rf_test)[,2]

#prediction is ROCR function
my_forest.pred = prediction(my_forest.pr, data_rf_test$progress)

#performance in terms of true and false positive rates
my_forest.perf = performance(my_forest.pred,"tpr","fpr")

#Not sure if this part is necessary.
par(mar=c(3,3,3,3))

#plot the curve
plot_handle <- plot(my_forest.perf,main=title,col=2,lwd=3, cex=4)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#compute area under curve
auc <- performance(my_forest.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc
text(0.6, 0.02, paste("AUC = ", round(auc,3)), cex=1, pos=4, col="red")
}
