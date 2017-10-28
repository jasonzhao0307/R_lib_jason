require(ggplot2)

# simple functions
RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}
# 4 figures
plot_visualization <- function(dataFrame, dataLabel, point.label.name = rep(1, length(dataLabel)), useLabel = FALSE, dataName = "Data",
															colorGroup = c(progressor="red", control="green"), log2Trans = TRUE, corMethod = "spearman",
															pdfOutput = FALSE, width = 9, height = 8,
															tsne.perplexity = 10,
															showRowName = TRUE, showColName = TRUE,
															top.var = 2000){

	indexInclude <- order(RowVar(dataFrame))[seq(1,top.var)]
	dataFrame <- dataFrame[indexInclude,]

  # change the label to the combination of original sample name as well as provided im "point.label.name"
	if (useLabel == TRUE){
		colnames(dataFrame) <- paste(colnames(dataFrame), point.label.name, sep = "-")
	}


	if (pdfOutput == TRUE){
		pdf(paste(dataName, "Log-transformed Heatmap.pdf", sep = " "), height= height, width = width, onefile = FALSE)
		plot_pheatmap(dataFrame, dataLabel, colorGroup = colorGroup, title = paste(dataName, "Log-transformed Heatmap", sep = " "),
		log2Trans = log2Trans, showRowName = showRowName, showColName = showColName)
		dev.off()

		pdf(paste(dataName, "Correlation Heatmap.pdf", sep = " "), height= height, width = width, onefile = FALSE)
		plot_pheatmap(cor(dataFrame,dataFrame, method = corMethod), dataLabel, colorGroup = colorGroup, rowScaling=FALSE, title = paste(dataName, "Correlation Heatmap", sep = " "))
		dev.off()


		gg3 <- plot_PCA_new(dataFrame, dataLabel, title = paste(dataName, "PCA", sep = " "), useLabel = useLabel)
		ggsave(paste(dataName, "PCA.pdf", sep = " "), gg3, height = height, width=width)

		gg4 <- plot_tsne(dataFrame, dataLabel, title = paste(dataName, "t-SNE", sep = " "), plotLabel = useLabel, perplexity = tsne.perplexity)
		ggsave(paste(dataName, "t-SNE.pdf", sep = " "), gg4, height = height, width=width)
	}

	# heatmap
	gg1 <- plot_pheatmap(dataFrame, dataLabel, colorGroup = colorGroup, title = paste(dataName, "Log2-transformed Heatmap",
	sep = " "), log2Trans = log2Trans, showRowName = showRowName, showColName = showColName)

	# Cor heatmap
	gg2 <- plot_pheatmap(cor(dataFrame,dataFrame, method = corMethod), dataLabel, colorGroup = colorGroup,
	rowScaling=FALSE, title = paste(dataName, "Correlation Heatmap", sep = " "))

	# PCA
	gg3 <- plot_PCA_new(dataFrame, dataLabel, title = paste(dataName, "PCA", sep = " "), useLabel = useLabel)

	# t-SNE
	gg4 <- plot_tsne(dataFrame, dataLabel, title = paste(dataName, "t-SNE", sep = " "), plotLabel = useLabel,
	perplexity = tsne.perplexity)

	# Save all handles to a list
	ggList <- list(gg1, gg2, gg3, gg4)
	return(ggList)

}
