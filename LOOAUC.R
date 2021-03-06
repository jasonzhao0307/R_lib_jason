require(glmnet)
require(ROCR)

# Here, we do cv.glmnet, aka elastic net. when alpha = 0, it's ridge regression.
# Thus, we need to repeat for a couple of times (numRepeat)
# However, if we do simple regression, we then just need run glmnet once. 

LOOAUC <- function(df, targetVec, nfolds = 3, numRepeat = 50){

aucVec <- c()

for (j in 1:numRepeat){
nSample <- ncol(df)
vecProb <- c()
for (i in 1:nSample){	
	train = t(as.matrix(df[,-i]))
	test = t(as.matrix(df[,i]))
    fit <- cv.glmnet(train, targetVec[-i], nfolds = nfolds, alpha = 0, family = "binomial", type.measure = "class")
    testProb <- predict(fit,type="response", newx = test, s = 'lambda.min')
    vecProb <- c(vecProb, testProb)
}



loo.pred = prediction(vecProb, targetVec)
#compute area under curve
auc <- performance(loo.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
aucRound <- round(auc,3)
aucVec <- c(aucVec, aucRound)

}
return(aucVec)



}





