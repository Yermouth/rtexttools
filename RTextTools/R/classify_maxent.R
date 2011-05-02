classify_maxent <-
function(feature_matrix) {
	if (maxent_trained == FALSE) {
		print("ERROR: Model has not been trained; use train_maxent(feature_matrix,code_vector) to train a model.");
		return(NULL);
	}
	
	features <- colnames(feature_matrix);
	results <- maxent$classify_samples(features,feature_matrix);
	
	return(as.vector(results$codes,mode="integer"));
}

