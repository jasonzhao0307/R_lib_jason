plot_PCA <- function(sampleDataFrame, labelVector, useLabel = TRUE, labelSize = 2, title = "PCA plot", printUsage = F){

# print usage
# use cat to create newline in R
if (printUsage == T){
	cat("Usage:
method 1:
pca_results <- plot_PCA(dataFrame, labelVector, useLabel = T, labelSize = 4)
pca_results[[1]]: plot plot_handle, which could be used to output the figure to any file
pca_results[[2]]: pca summary

method 2:
plot_PCA(dataFrame, labelVector, useLabel = T, labelSize = 4)
which will show both figure and summary directly.

To load all the functions from <R_lib_jason> in your own script:
1. install.package(\"R.utils\")
2. require(R.utils)
3. sourceDirectory(\"path_to_<R_lib_jason>\", modifiedOnly=TRUE)")
}


#package loading
require(ggplot2)
require(ggfortify)

#PCA
sampleDataFrame_pca <- as.data.frame(t(sampleDataFrame))
sampleDataFrame_pca_df <- as.matrix(sampleDataFrame_pca)
sampleDataFrame_pca <- cbind(sampleDataFrame_pca, labelVector)
pca_result <- prcomp(sampleDataFrame_pca_df, scale = TRUE)
pca_summary <- summary(pca_result)
if (useLabel == FALSE){
  plot_handle <- autoplot(pca_result, data = sampleDataFrame_pca, colour = 'labelVector', main = title)
}
else{
  plot_handle <- autoplot(pca_result, data = sampleDataFrame_pca, colour = 'labelVector',shape = FALSE, label.size = labelSize, main = title)
}

results <- list(plot_handle, pca_summary)
return(results)

}








