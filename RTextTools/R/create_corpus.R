create_corpus <- function(matrix,labels,trainSize,testSize,virgin) {
	totalSize <- sort(append(trainSize,testSize))
	column_names <- colnames(matrix)
	data_matrix <- dtm_to_sparsem(matrix[totalSize])
	
	#data_matrix <- data_matrix[,!(colSums(abs(data_matrix)) == 0)]
	#data_matrix <- data_matrix[(rowSums(data_matrix == 0) != ncol(data_matrix)),]
	
	matrix_train_predict <- data_matrix[trainSize,]
	matrix_test_predict <- data_matrix[testSize,]

    train_code <- as.factor(labels[trainSize])
	if (length(unique(is.na(train_code))) > 1) stop("All data in the training set must have corresponding codes.")
	
    test_code <- as.factor(labels[testSize])
	#if (virgin == TRUE) test_code <- as.factor(sapply(test_code,function(x) x <- -1))
	if (virgin == FALSE && length(unique(is.na(test_code))) > 1) stop("The data to be classified does not have corresponding codes. To treat this data set as virgin data, set virgin=TRUE.")
	
    setClass("matrix_container",representation(training_matrix="matrix.csr", classification_matrix="matrix.csr", training_codes="factor", testing_codes="factor", column_names="vector", virgin="logical"))
    container <- new("matrix_container", training_matrix=matrix_train_predict, classification_matrix=matrix_test_predict, training_codes=train_code, testing_codes=test_code, column_names=column_names, virgin=virgin)
    
    gc()
    return(container)
}