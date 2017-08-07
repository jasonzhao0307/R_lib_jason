require(randomForest)
require(ROCR)


# Input: training data, training label


# This function will do the following things:
# 1. Generate a grid of hyper parameters (numTree, numRuns) for generating randomForest signature.
# 2. For each point in grid, do LOOCV, get the signature from m-1 samples, train again only on the top 10% genes and test on the left sample.
# 3. Train the whole dataset with the highest AUC numTree and numRuns, pick 10% gene as signature.




LoocvRF <- function(df, label.vec, nfolds = 3, numRepeat = 50){

aucVec <- c()

for (j in 1:numRepeat){
nSample <- ncol(df)
vecProb <- c()
for (i in 1:nSample){
	train = t(as.matrix(df[,-i]))
	test = t(as.matrix(df[,i]))
    fit <- cv.glmnet(train, label.vec[-i], nfolds = nfolds, alpha = 0, family = "binomial", type.measure = "class")
    testProb <- predict(fit,type="response", newx = test, s = 'lambda.min')
    vecProb <- c(vecProb, testProb)
}



loo.pred = prediction(vecProb, label.vec)
#compute area under curve
auc <- performance(loo.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
aucRound <- round(auc,3)
aucVec <- c(aucVec, aucRound)

}
return(aucVec)



}
