Ridge_output <- function(dataFrameTrain, dataFrameTest, targetVecTrain, targetVecTest, featureSelected, title = "Ridge Logistic Regression ROC Curve"){
#package
require(glmnet)
require(ROCR)
require(R.utils)
#sourceDirectory("/Users/yuezhao/Desktop/projects/R_lib_jason/", modifiedOnly=TRUE)


data_lr_train <- t(as.matrix(dataFrameTrain[which(rownames(dataFrameTrain) %in% featureSelected),]))
data_lr_test <- t(as.matrix(dataFrameTest[which(rownames(dataFrameTest) %in% featureSelected),]))
fit <- glmnet(data_lr_train, targetVecTrain, family = "binomial")
testProb <- predict(fit, type="response", newx = data_lr_test, s = 0)
lr.pred = prediction(testProb, targetVecTest)
lr.perf = performance(lr.pred,"tpr","fpr")

#Not sure if this part is necessary.
par(mar=c(3,3,3,3))

#plot the curve
plot_handle <- plot(lr.perf,main=title,col=2,lwd=3, cex=4)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#compute area under curve
auc <- performance(lr.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc
text(0.6, 0.02, paste("AUC = ", round(auc,3)), cex=1, pos=4, col="red")
}
