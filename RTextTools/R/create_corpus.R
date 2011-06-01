create_corpus <- function(matrix,labels,trainSize,testSize,extraFeatures=NULL) {
	totalSize <- append(trainSize,testSize)
	column_names <- colnames(matrix)
	data_matrix <- dtm_to_sparsem(matrix[totalSize])
	
	#data_matrix <- data_matrix[,!(colSums(abs(data_matrix)) == 0)]
	#data_matrix <- data_matrix[(rowSums(data_matrix == 0) != ncol(data_matrix)),]
	
	matrix_train_predict <- data_matrix[trainSize,]
	matrix_test_predict <- data_matrix[testSize,]

    train_code <- as.factor(labels[trainSize])
	if (length(unique(is.na(train_code))) > 1) stop("All data in the training set must have corresponding codes.")
	
    test_code <- as.factor(labels[testSize])
	if (length(unique(is.na(test_code))) > 1) stop("All data in the testing set must either have a corresponding code or be left blank, not a combination of the two.")
	if (unique(is.na(test_code))) test_code <- as.factor(sapply(test_code,function(x) x <- -1))
	
    setClass("matrix_container",representation(training_matrix="matrix.csr", classification_matrix="matrix.csr", training_codes="factor", testing_codes="factor", column_names="vector"))
    container <- new("matrix_container", training_matrix=matrix_train_predict, classification_matrix=matrix_test_predict, training_codes=train_code, testing_codes=test_code, column_names=column_names)
    
    gc()
    return(container)
}