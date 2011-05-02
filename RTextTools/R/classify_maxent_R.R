classify_maxent_R <-
function(feature_matrix) {
	if (maxent_trained == FALSE) {
		print("ERROR: Model has not been trained; use train_maxent(feature_matrix,code_vector) to train a model.");
		return(NULL);
	}
	
	frame <- as.data.frame(feature_matrix);
	features <- colnames(frame);
	results <- c();
	for (x in c(1:nrow(frame))) {
		weights <- as.vector(frame[x,],mode="integer");
		sample <- maxent$classify_sample(features,weights);
		results <- append(results,as.integer(sample));
	}
	
	return(results);
}

