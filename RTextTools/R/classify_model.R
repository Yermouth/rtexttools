classify_model <-
function(matrix_container, SVM=NULL,NAIVE=NULL,BOOSTING=NULL,BAGGING=NULL,RF=NULL,GLMNET=NULL,
                           TREE=NULL,NNET=NULL,MAXENT=NULL,s=0.01,...) {

	gc()
    extract_maximum_prob <- function(x) return(x[which.max(x)])
    extract_label_from_prob <- function(x) return(which.max(x))
    
    if (!is.null(SVM)){
        svm_results <- predict(SVM,matrix_container@classification_matrix, prob=TRUE) #Extract Label
        svm_pred <- svm_results[1:length(svm_results)]
        svm_prob <- apply(attr(svm_results,"prob"),1,extract_maximum_prob)

        results_table <- data.frame(as.numeric(as.character(svm_pred)),svm_prob) #need to chang svm_pred to numeric for agreescore creation
        colnames(results_table)[1] <- "SVM_LABEL"
        colnames(results_table)[2] <- "SVM_PROB"
    } else
    
    if (!is.null(NAIVE)){
        naive_results <- predict(NAIVE,matrix_container@classification_matrix)
        results_table <- data.frame(as.numeric(as.character(naive_results)))
        
        colnames(results_table)[1] <- "NBAYES_LABEL"
    } else

    if (!is.null(BOOSTING)) {
        aboost_results <- predict(BOOSTING,data.frame(matrix_container@classification_matrix),type="probability") #Probability
        aboost_pred <- apply(aboost_results,1,extract_label_from_prob) #Extract Label Based on Probability
        aboost_prob <- apply(aboost_results,1,extract_maximum_prob) #Extract Highest Probability
        
        results_table <- data.frame(as.numeric(as.character(aboost_pred)),aboost_prob)
        colnames(results_table)[1] <- "ADABOOST_LABEL"
        colnames(results_table)[2] <- "ADABOOST_PROB"
    } else
    
    if (!is.null(BAGGING)) {
        bagging_results <- predict(BAGGING,data.frame(matrix_container@classification_matrix), type="probability")
        bagging_pred <- apply(bagging_results,1,extract_label_from_prob) #Extract Label Based on Probability
        bagging_prob <- apply(bagging_results,1,extract_maximum_prob) 
        
        results_table <- data.frame(as.numeric(as.character(bagging_pred)),bagging_prob)
        colnames(results_table)[1] <- "BAGGING_LABEL"
        colnames(results_table)[2] <- "BAGGING_PROB"
    } else
    
    if (!is.null(RF)){
        rf_results <- predict(RF,newdata=matrix_container@classification_matrix,type="prob")
		rf_pred <- apply(rf_results,1,extract_label_from_prob)
        rf_prob <- apply(rf_results,1,extract_maximum_prob)

        results_table <- data.frame(as.numeric(as.character(rf_pred)),rf_prob)
        colnames(results_table)[1] <- "FORESTS_LABEL"
        colnames(results_table)[2] <- "FORESTS_PROB"
    } else
    
    if (!is.null(GLMNET)){
        glmnet_results <- predict(GLMNET,newx=matrix_container@classification_matrix,s=s,type="response")
        glmnet_pred <- apply(glmnet_results[,,1],1,extract_label_from_prob) 
        glmnet_prob <- apply(glmnet_results,1,extract_maximum_prob) 
        
        results_table <- data.frame(as.numeric(as.character(glmnet_pred)),glmnet_prob)
        colnames(results_table)[1] <- "GLMNET_LABEL"
        colnames(results_table)[2] <- "GLMNET_PROB"
    } else
    
    if (!is.null(TREE)){
        tree_results <- predict(TREE,newdata=data.frame(matrix_container@classification_matrix), type="vector")
        tree_pred <- apply(tree_results,1,extract_label_from_prob)
        tree_prob <- apply(tree_results,1,extract_maximum_prob) 
        
        results_table <- data.frame(as.numeric(as.character(tree_pred)),tree_prob)
        colnames(results_table)[1] <- "TREE_LABEL"
        colnames(results_table)[2] <- "TREE_PROB"
    } else

    if (!is.null(NNET)){
        nnet_results <- predict(NNET,newdata=data.frame(matrix_container@classification_matrix)) #probabilities
        nnet_pred <- apply(nnet_results,1,extract_label_from_prob) #Extract Highest Probability Score
        nnet_prob <- apply(nnet_results,1,extract_maximum_prob) #Extract Probability
        
        results_table <- data.frame(as.numeric(as.character(nnet_pred)),nnet_prob)
        colnames(results_table)[1] <- "NNETWORK_LABEL"
        colnames(results_table)[2] <- "NNETWORK_PROB"
    } else
							   
	if (!is.null(MAXENT)) {
		maxent_results <- classify_maxent(matrix_container@classification_matrix)
		results_table <- data.frame(as.numeric(as.character(maxent_results)))
		
		colnames(results_table)[1] <- "MAXENTROPY_LABEL"
	}
	
	gc()
	return(results_table)
}

