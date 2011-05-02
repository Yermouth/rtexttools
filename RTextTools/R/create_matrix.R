create_matrix <- function(trainingColumn, language="en", minWordLength=3, minDocFreq=1, removeNumbers=FALSE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, stripWhitespace=TRUE, toLower=TRUE) {
	corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
	matrix <- DocumentTermMatrix(corpus,control=list(language=language,tolower=toLower,stopwords=removeStopwords,removePunctuation=removePunctuation,removeNumbers=removeNumbers, stripWhitespace=TRUE, minWordLength=3 , minDocFreq=1));
    if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms) # Advisable value for removeSparseTerms: 0.9998
	
	gc()
	return(matrix)
}