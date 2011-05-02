train_maxent <-
function(feature_matrix,code_vector,feature_cutoff=0,gaussian_prior=0,inequality_constraints=0) {
	if (gaussian_prior > 0 && inequality_constraints > 0) {
		print("ERROR: Gaussian priors and inequality modeling cannot be used together.");
		return(NULL);
	}
	
	if (maxent_trained == TRUE) {
		print("ERROR: Model has been already trained; call new_maxent() to create a new model.");
		return(NULL);
	}
	
	features <- colnames(feature_matrix);
	code_vector <- sapply(code_vector,toString);
	maxent$add_samples(code_vector,features,feature_matrix);
	maxent$train_model(feature_cutoff,gaussian_prior,inequality_constraints);
	assign("maxent_trained",TRUE,envir=globalenv());

	return(TRUE);
}

