deseq2_norm_rle <- function(dataFrame){
# RLE normalization: relative log expression


library(DESeq2)
scaling.dataFrame <- estimateSizeFactorsForMatrix(dataFrame)
dataFrame.scaled <- dataFrame
for(i in 1:ncol(dataFrame)){
  dataFrame.scaled[,i] <- dataFrame[,i]/scaling.dataFrame[i]
}

return(dataFrame.scaled)
}