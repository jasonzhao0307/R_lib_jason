glmnetOutput <- function(dataFrameTrain,dataFrameTest, geneFeatureVec, targetTrain, targetTest, logisticRegression = FALSE, corMethod = "spearman", nFolds = 10, alpha = 1, title = "Linear Regression on Testing Set", xLab = "Progression Time (days)", yLab = "Prediction (days)"){
require(glmnet)
require(ggplot2)


#Get the data prepared for glmnet
dataTrain <- t(as.matrix(dataFrameTrain[geneFeatureVec,]))
dataTest <- t(as.matrix(dataFrameTest[geneFeatureVec,]))

if(logisticRegression == FALSE){
#CV.glmnet
fit1 <- cv.glmnet(dataTrain, targetTrain, nfolds = nFolds, alpha = alpha)  
rsquare <- round(cor(predict(fit1, newx = dataTest, s = "lambda.min"),targetTest, method = corMethod),3)
data1 <- as.data.frame(cbind(targetTest, predict(fit1, newx = dataTest, s = "lambda.min"))) 
colnames(data1) <- c("v1", "v2")
plot_handle <- ggplot(data1, aes(v1, v2)) + geom_point() + geom_abline(intercept = 0, slope = 1) + annotate("text", x = 1600, y = -250, label = paste('Cor = ',rsquare), size = 7) + labs(title = title, x = xLab, y = yLab) + theme(text = element_text(size=22))

tmp_vec <- as.vector((coef(fit1, s="lambda.min") != 0))
featureFromGlmnet <- geneFeatureVec[tmp_vec]

results <- list()
results[[1]] <- featureFromGlmnet
results[[2]] <- plot_handle
return(results)
} else{
#factor is required for classification
targetTrain <- as.factor(targetTrain)
targetTest <- as.factor(targetTest)

#CV.glmnet
fit1 <- cv.glmnet(dataTrain, targetTrain, nfolds = nFolds, alpha = alpha, family = "binomial", type.measure = "auc")  
rsquare <- round(cor(predict(fit1, newx = dataTest, s = "lambda.min"),targetTest, method = corMethod),3)
data1 <- as.data.frame(cbind(targetTest, predict(fit1, newx = dataTest, s = "lambda.min"))) 
colnames(data1) <- c("v1", "v2")
plot_handle <- ggplot(data1, aes(v1, v2)) + geom_point() + geom_abline(intercept = 0, slope = 1) + annotate("text", x = 1600, y = -250, label = paste('Cor = ',rsquare), size = 7) + labs(title = title, x = xLab, y = yLab) + theme(text = element_text(size=22))

tmp_vec <- as.vector((coef(fit1, s="lambda.min") != 0))
featureFromGlmnet <- geneFeatureVec[tmp_vec]

results <- list()
results[[1]] <- featureFromGlmnet
results[[2]] <- plot_handle
return(results)
}



}