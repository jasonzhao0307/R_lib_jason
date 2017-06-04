require(ggplot2)
require(Rtsne)

plot_tsne <- function(dataFrame, dataLabel, title = "t-SNE 2D Embedding of Products Data", perplexity = 10, plotLabel = FALSE, pointSize = 4, outputPDF = FALSE, outputName = "Heatmap_undefined.pdf", width = 16, height = 14){


set.seed(42) # Sets seed for reproducibility

tsne_out <- Rtsne(t(as.matrix(dataFrame)), check_duplicates = FALSE, pca = TRUE, perplexity=perplexity, theta=0.5, dims=2) 
#ggplot tsne
embedding <- as.data.frame(tsne_out$Y)
embedding$Class <- dataLabel     

if (plotLabel == FALSE){                   
ggplot(embedding, aes(x=V1, y=V2, color=Class, label = colnames(dataFrame))) +
geom_point(size=pointSize) +
guides(colour = guide_legend(override.aes = list(size=6))) +
xlab("T-SNE 1") + ylab("T-SNE 2") +
ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
}
else{
ggplot(embedding, aes(x=V1, y=V2, color=Class, label = colnames(dataFrame))) +
geom_text(size=pointSize) +
guides(colour = guide_legend(override.aes = list(size=6))) +
xlab("T-SNE 1") + ylab("T-SNE 2") +
ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
}

if (outputPDF == TRUE){
	ggsave(outputName, width = width, height = height, units = "cm")
}


}



