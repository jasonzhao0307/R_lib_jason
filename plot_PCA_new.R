plot_PCA_new <- function(sampleDataFrame, condition, useLabel = FALSE, title = "PCA plot", useVST = FALSE, useAllFeature = TRUE, nFeature = 1000, printUsage = F, pointSize = 4, useFrame = FALSE, outputPDF = FALSE, outputName = "PCA_undefined.pdf", width = 16, height = 14){

# print usage
# use cat to create newline in R
if (printUsage == T){
	cat("Note:
Filter your data before doing PCA!
e.g. :
IndianData <- fullIndian[rowSums(fullIndian) > 20,]

If useVST == TRUE,
the data has to be raw read counts!!!!

usage:
plot_PCA_new(sampleDataFrame, condition, useLabel = TRUE, labelSize = 2, title = \"PCA plot\", useVST = TRUE, useAllFeature = TRUE, nFeature = 1000, printUsage = F)

To load all the functions from <R_lib_jason> in your own script:
1. install.package(\"R.utils\")
2. require(R.utils)
3. sourceDirectory(\"path_to_<R_lib_jason>\", modifiedOnly=TRUE)")
}


#package loading
require(DESeq2)
require(ggplot2)
require(ggfortify)


#test and fix the constant/zero row
if (sum(rowSums(as.matrix(sampleDataFrame)) == 0) > 0){
	sampleDataFrame <- sampleDataFrame[-which(rowSums(as.matrix(sampleDataFrame)) == 0),]
}


# simple functions
RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

if (useAllFeature == TRUE){
	nFeature <- nrow(sampleDataFrame)
}

if (useVST == TRUE){
	# dds structure
colData <- data.frame(condition = condition)
rownames(colData) <- colnames(sampleDataFrame)
#Add gender info later when available
dds <- DESeqDataSetFromMatrix(countData = sampleDataFrame,
                                colData = colData,
                                design = ~ condition)
}

if (useVST == TRUE){
	if (useLabel == TRUE){
	  	vst <- varianceStabilizingTransformation(dds, blind=TRUE)
		data <- plotPCA(vst, intgroup=c("condition"), returnData=TRUE, ntop = nFeature)
		percentVar <- round(100 * attr(data, "percentVar"))
		# plot_handle <- ggplot()
		ggplot(data, aes(PC1, PC2, color=condition, label = colnames(sampleDataFrame))) +
 		xlab(paste0("PC1: ",percentVar[1],"% variance")) +
 		ylab(paste0("PC2: ",percentVar[2],"% variance")) +labs(title=title) + geom_text(size = pointSize) + theme(plot.title = element_text(hjust = 0.5))
	}
	else{
		vst <- varianceStabilizingTransformation(dds, blind=TRUE)
		data <- plotPCA(vst, intgroup=c("condition"), returnData=TRUE, ntop = nFeature)
		percentVar <- round(100 * attr(data, "percentVar"))
		# plot_handle <- ggplot()
		ggplot(data, aes(PC1, PC2, color=condition, label = colnames(sampleDataFrame))) +
 		xlab(paste0("PC1: ",percentVar[1],"% variance")) +
 		ylab(paste0("PC2: ",percentVar[2],"% variance")) +labs(title=title) + geom_point(size = pointSize) + theme(plot.title = element_text(hjust = 0.5))
	}
	#result <- plot_handle
	#return(result)

}
else {

	indexInclude <- order(RowVar(sampleDataFrame))[seq(1,nFeature)]
	if (useAllFeature == FALSE){
		sampleDataFrame <- sampleDataFrame[indexInclude,]
	}
	sampleDataFrame_pca <- as.data.frame(t(sampleDataFrame))
	sampleDataFrame_pca_df <- as.matrix(sampleDataFrame_pca)
	sampleDataFrame_pca <- cbind(sampleDataFrame_pca, condition)
	pca_result <- prcomp(sampleDataFrame_pca_df, scale = TRUE)
	pca_summary <- summary(pca_result)
	pca_mat <- pca_summary$importance
	percentVar <- c(round(100 * pca_mat[2,1],2), round(100 * pca_mat[2,2],2))



	if (useLabel == FALSE){
		# plot_handle <- autoplot()
		autoplot(pca_result, data = sampleDataFrame_pca, colour = 'condition', size = pointSize, frame = useFrame,
		main = title, xlab = paste0("PC1: ",percentVar[1],"% variance"), ylab = paste0("PC2: ",percentVar[2],"% variance")) + theme(plot.title = element_text(hjust = 0.5))
	}
	else{
		# plot_handle <- autoplot()
		autoplot(pca_result, data = sampleDataFrame_pca, colour = 'condition', label.size = pointSize, frame = useFrame,
		shape = FALSE, main = title,
		xlab = paste0("PC1: ",percentVar[1],"% variance"), ylab = paste0("PC2: ",percentVar[2],"% variance")) + theme(plot.title = element_text(hjust = 0.5))
	}

	#result <- plot_handle
	#return(result)

}

if (outputPDF == TRUE){
	ggsave(outputName, width = width, height = height, units = "cm")
}

}
