create_container <- function(matrix,labels,trainSize,testSize,extraFeatures=NULL,matrixType="normal") {
	totalSize <- append(trainSize,testSize)
	data_matrix <- as.matrix(matrix[totalSize],"matrix.csr")
	data_matrix <- data_matrix[,!(colSums(abs(data_matrix)) == 0)]
	#data_matrix <- data_matrix[(rowSums(data_matrix == 0) != ncol(data_matrix)),]
	
	matrix_train_predict <- data_matrix[trainSize,]
	matrix_test_predict <- data_matrix[testSize,]
	
    train_code <- as.factor(labels[trainSize])
    test_code <- as.factor(labels[testSize])

    setClass("matrix_container",representation(training_matrix="matrix", classification_matrix="matrix", training_codes="factor", testing_codes="factor"))
    container <- new("matrix_container", training_matrix=matrix_train_predict, classification_matrix=matrix_test_predict, training_codes=train_code, testing_codes=test_code)
    
    gc()
    return(container)   
}