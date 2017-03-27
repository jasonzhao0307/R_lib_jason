hyphenToUnderscore <- function(dataVec, hyphen = "-", underscore = "_", UnderscoreToHyphen = FALSE){
	for (i in seq(1,length(dataVec),1)) {
		tmp <- dataVec[i]
		if (UnderscoreToHyphen == FALSE){
			if (grepl(hyphen,tmp)){
				dataVec[i] <- gsub(hyphen,underscore, tmp)
			}
		} else{
			if (grepl(underscore,tmp)){
				dataVec[i] <- gsub(underscore, hyphen,tmp)
			}
			
		}
	}
	return(dataVec)
}
