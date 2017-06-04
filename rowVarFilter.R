rowVarFilter <- function(sampleDataFrame, topFilter = 0.5){


RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

nFeature <- round(topFilter * nrow(sampleDataFrame))

indexInclude <- order(RowVar(sampleDataFrame))[seq(1,nFeature)]
return(sampleDataFrame[indexInclude,])

}




