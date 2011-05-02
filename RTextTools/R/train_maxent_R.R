train_maxent_R <-
function(feature_matrix,code_vector,feature_cutoff=0,gaussian_prior=0,inequality_constraints=0) {
	if (gaussian_prior > 0 && inequality_constraints > 0) {
		print("ERROR: Gaussian priors and inequality modeling cannot be used together.");
		return(NULL);
	}
	
	if (maxent_trained == TRUE) {
		print("ERROR: Model has been already trained; call new_maxent() to create a new model.");
		return(NULL);
	}
	
	frame <- as.data.frame(feature_matrix);
	features <- colnames(frame);
	code_vector <- sapply(code_vector,toString)
	for (x in c(1:nrow(frame))) {
		weights <- as.vector(frame[x,],mode="integer");
		maxent$add_sample(code_vector[x],features,weights);
	}
	
	maxent$train_model(feature_cutoff,gaussian_prior,inequality_constraints);
	assign("maxent_trained",TRUE,envir=globalenv());
}

