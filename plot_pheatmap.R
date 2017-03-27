plot_pheatmap <- function(dataFrame, dataLabel, colorGroup = c(progressor="red", control="green"), log2Trans = FALSE, title = "Heatmap", outputPDF = FALSE, outputName = "Heatmap_undefined.pdf", width = 9, height = 8){
require(pheatmap)

if (log2Trans == TRUE){
	dataFrame <- log2(dataFrame + 1)
}

df <- data.frame(group= factor(dataLabel))
rownames(df) <- colnames(dataFrame)
lt <- list(group=colorGroup)
if (outputPDF == TRUE){
	pdf(outputName, width = width, height = height, onefile = FALSE)
	pheatmap(dataFrame, annotation_col= df,
	annotation_colors= lt[1], main = title)
	dev.off()
} else{
	pheatmap(dataFrame, annotation_col= df,
	annotation_colors= lt[1], main = title)
}

}



