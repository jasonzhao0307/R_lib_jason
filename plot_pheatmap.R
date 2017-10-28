plot_pheatmap <- function(dataFrame, dataLabel, rowScaling = TRUE, colorGroup = c(progressor="red", control="green"), log2Trans = FALSE, title = "Heatmap", outputPDF = FALSE, outputName = "Heatmap_undefined.pdf", showRowName = TRUE, showColName = TRUE, width = 7, height = 7){
require(pheatmap)



if (log2Trans == TRUE){
	dataFrame <- log2(dataFrame + 1)
}


if (rowScaling == TRUE){
	dataFrame <- as.data.frame(t(scale(t(as.matrix(dataFrame)))))
}





df <- data.frame(group= factor(dataLabel))
rownames(df) <- colnames(dataFrame)
lt <- list(group=colorGroup)
if (outputPDF == TRUE){
	pdf(outputName, width = width, height = height, onefile = FALSE)
	pheatmap(dataFrame, annotation_col= df,
	annotation_colors= lt[1], main = title, show_rownames = showRowName, show_colnames = showColName)
	dev.off()
}

	handle <- pheatmap(dataFrame, annotation_col= df,
	annotation_colors= lt[1], main = title, show_rownames = showRowName, show_colnames = showColName)


return(handle)
}
