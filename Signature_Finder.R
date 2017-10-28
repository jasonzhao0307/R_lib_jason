##### Jason
###



# User provides a feature number range or a specific number.
# Range: For each number in the range, it will run logistic regression for <num.runs> times (each run pairs with a feature random sampling),
# and output the best number as well as corresponding feature

# Specific number: Simple version of range


# load packages
#sourceDirectory("/Users/yuezhao/Desktop/projects/R_lib_jason/R_lib_jason/")

Signature_Finder <- function(df, target.int.vec, signature.pool.vec, use.range = FALSE, num.feature, num.feature.range, num.runs = 200){
  if (use.range == FALSE){
    signature.list.single <- list()
    df.list.single <- list()
    for (i in 1:num.runs){
      signature.list.single[[i]] <- sample(signature.pool.vec, num.feature)
      df.list.single[[i]] <- df[which(rownames(df) %in% signature.list.single[[i]]), ]
    }
    loocv.auc.vec <- LOOAUC_simple_multiple_noplot(df.list.single, target.int.vec)
    index.vec <- which(loocv.auc.vec %in% max(loocv.auc.vec))
    signature.single <- signature.list.single[[index.vec[1]]]
    return(signature.single)
  } else{
      output.list <- list()
      signature.list.range <- list()
      auc.vec.range <- c()
      for (j in 1:length(num.feature.range)){
        df.list.single <- list()
        signature.list.single <- list()
        for (i in 1:num.runs){
          signature.list.single[[i]] <- sample(signature.pool.vec, num.feature.range[j])
          df.list.single[[i]] <- df[which(rownames(df) %in% signature.list.single[[i]]), ]
        }
        loocv.auc.vec <- LOOAUC_simple_multiple_noplot(df.list.single, target.int.vec)
        index.vec <- which(loocv.auc.vec %in% max(loocv.auc.vec))
        signature.single <- signature.list.single[[index.vec[1]]]
        signature.list.range[[j]] <- c(num.feature.range[j], max(loocv.auc.vec), signature.single)
        auc.vec.range[j] <- max(loocv.auc.vec)
      }
      # number of feature, LOOCV auc value, signature
      output.list[[1]] <- signature.list.range
      # index with largest auc
      output.list[[2]] <- which(auc.vec.range %in% max(auc.vec.range))
      return(output.list)


    }




}


Signature_Finder_Grow <- function(df, target.int.vec, signature.pool.vec){
  signature.best <- c()
  auc.best <- 0.0
  for (i in 1:length(signature.pool.vec)){
    signature.vec <- c(signature.pool.vec[i])
    signature.pool.left <- signature.pool.vec[which(!signature.pool.vec %in% signature.vec)]
    signature.auc <- 0.0
    exit.flag = 0
    while (length(signature.pool.left) > 0 && exit.flag == 0){
      df.list <- list()
      signature.list <- list()
      for (j in 1:length(signature.pool.left)){
        signature.list[[j]] <- c(signature.vec, signature.pool.left[j])
        df.list[[j]] <- df[which(rownames(df) %in% signature.list[[j]]), ]
      }
      loocv.auc.vec <- LOOAUC_simple_multiple_noplot(df.list, target.int.vec)
      if (max(loocv.auc.vec) > signature.auc){
        index.vec <- which(loocv.auc.vec %in% max(loocv.auc.vec))
        signature.vec <- signature.list[[index.vec[1]]]
        signature.pool.left <- signature.pool.left[which(!signature.pool.left %in% signature.vec)]
        signature.auc <- max(loocv.auc.vec)
        if (signature.auc > auc.best){
          signature.best <- signature.vec
          auc.best <- signature.auc
        }
      } else{
        exit.flag = 1
        break
      }

    }

  }
  output.list <- list(signature.best, auc.best)
  return(output.list)
}
