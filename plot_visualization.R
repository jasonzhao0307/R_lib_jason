require(ggplot2)
plot_visualization <- function(dataFrame, dataLabel, useLabel = FALSE, dataName = "Data", colorGroup = c(progressor="red", control="green"), log2Trans = FALSE, corMethod = "pearson", pdfOutput = FALSE, width = 9, height = 8){
	
	if (pdfOutput == TRUE){
	pdf(paste(dataName, "Log-transformed Heatmap.pdf", sep = " "), height= height, width = width, onefile = FALSE)
	plot_pheatmap(dataFrame, dataLabel, colorGroup = colorGroup, title = paste(dataName, "Log-transformed Heatmap", sep = " "), log2Trans = log2Trans)
	dev.off()

	pdf(paste(dataName, "Correlation Heatmap.pdf", sep = " "), height= height, width = width, onefile = FALSE)
	plot_pheatmap(cor(dataFrame,dataFrame, method = corMethod), dataLabel, colorGroup = colorGroup, rowScaling=FALSE, title = paste(dataName, "Correlation Heatmap", sep = " "))
	dev.off()


	gg3 <- plot_PCA_new(dataFrame, dataLabel, title = paste(dataName, "PCA", sep = " "), useLabel = useLabel)
	ggsave(paste(dataName, "PCA.pdf", sep = " "), gg3, height = height, width=width)

	gg4 <- plot_tsne(dataFrame, dataLabel, title = paste(dataName, "t-SNE", sep = " "), plotLabel = useLabel)
	ggsave(paste(dataName, "t-SNE.pdf", sep = " "), gg4, height = height, width=width)
	}


	plot_pheatmap(dataFrame, dataLabel, colorGroup = colorGroup, title = paste(dataName, "Log-transformed Heatmap", sep = " "), log2Trans = log2Trans)
	plot_pheatmap(cor(dataFrame,dataFrame, method = corMethod), dataLabel, colorGroup = colorGroup, rowScaling=FALSE, title = paste(dataName, "Correlation Heatmap", sep = " "))
	gg3 <- plot_PCA_new(dataFrame, dataLabel, title = paste(dataName, "PCA", sep = " "), useLabel = useLabel)
	gg4 <- plot_tsne(dataFrame, dataLabel, title = paste(dataName, "t-SNE", sep = " "), plotLabel = useLabel)
	ggList <- list(gg3, gg4)

	return(ggList)

}