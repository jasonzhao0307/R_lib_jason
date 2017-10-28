require(caret)
require(ROCR)

Model_AUC <- function(model.name, df.train, y.train, df.test, y.test, signature, seed = 10, num.fold = 10, num.repeat = 10){
  # format data
  df.train <- as.data.frame(t(as.matrix(df.train[which(rownames(df.train) %in% signature),])))
  df.train <- cbind(df.train, data.frame(Class = y.train))
  df.test <- as.data.frame(t(as.matrix(df.test[which(rownames(df.test) %in% signature),])))
  df.test <- cbind(df.test, data.frame(Class = y.test))

  # caret model tuning
  output.list <- list()
  fitControl <- trainControl(method = "repeatedcv",
                             number = num.fold,
                             repeats = num.repeat,
                             ## Estimate class probabilities
                             classProbs = TRUE,
                             ## Evaluate performance using
                             ## the following function
                             summaryFunction = twoClassSummary)

  set.seed(seed)
  model.fit <- train(Class ~ ., data = df.train,
                   method = model.name,
                   trControl = fitControl,
                   ## Specify which metric to optimize
                   metric = "ROC")

  # predict and get probabilities
  model.prob = predict(model.fit, type="prob", newdata=df.test)[,2]
  # prediction is ROCR function
  model.pred = prediction(model.prob, df.test$Class)
  model.auc <- performance(model.pred, "auc")
  model.auc <- unlist(slot(model.auc, "y.values"))
  output.list[[1]] <- model.fit
  output.list[[2]] <- model.auc
  names(output.list) <- c("model", "testing set AUC")
  return(output.list)
}



Model_AUC_Multiple <- function(model.name.vec, df.train, y.train, df.test, y.test, signature, seed = 10, num.fold = 10, num.repeat = 10){
  multiple.output.list <- list()
  for (i in 1:length(model.name.vec)){
    multiple.output.list[[i]] <- Model_AUC(model.name.vec[i], df.train, y.train, df.test, y.test, signature, seed, num.fold, num.repeat)
  }
  names(multiple.output.list) <- model.name.vec
  return(multiple.output.list)
}
