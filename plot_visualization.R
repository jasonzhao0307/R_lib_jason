plot_visualization <- function(dataFrame, dataLabel, useLabel = FALSE, dataName = "Data", colorGroup = c(progressor="red", control="green"), log2Trans = FALSE, corMethod = "pearson"){
	#load R_lib_jason
	library(R.utils)
	sourceDirectory("./", modifiedOnly=TRUE)
	plot_pheatmap(dataFrame, dataLabel, colorGroup, title = paste(dataName, "Log-transformed Heatmap", sep = " "), log2Trans = log2Trans)
	plot_pheatmap(cor(dataFrame,dataFrame, method = corMethod), dataLabel, colorGroup, title = paste(dataName, "Correlation Heatmap", sep = " "))
	plot_PCA_new(dataFrame, dataLabel, title = paste(dataName, "PCA", sep = " "), useLabel = useLabel)
}