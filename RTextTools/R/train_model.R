train_model <- function(matrix_container,SVM=FALSE,NAIVE=FALSE,BOOSTING=FALSE,BAGGING=FALSE,
						RF=FALSE,GLMNET=FALSE,TREE=FALSE,NNET=FALSE,MAXENT=FALSE,
						method="C-classification",maxitglm=500,size=1,maxitnnet=1000,ntree=200, ...) {
        
        # CLEAN UP FROM PREVIOUS MODEL TRAINED
        gc()
        
        # CONDITIONAL TRAINING OF MODEL
        if (SVM==TRUE) {
			model <- svm(x=matrix_container@training_matrix, y=matrix_container@training_codes, method=method, cross=0, probability=TRUE)
		} else if (NAIVE == TRUE) {
           model <- naiveBayes(x=matrix_container@training_matrix, y=matrix_container@training_codes)
        } else if (BOOSTING == TRUE) {
            model <- AdaBoostM1(matrix_container.training_codes ~ ., data=data.frame(matrix_container@training_matrix,matrix_container@training_codes), control=Weka_control(W="J48"))
        } else if (BAGGING == TRUE) {
            model <- Bagging(matrix_container.training_codes ~ .,data=data.frame(matrix_container@training_matrix,matrix_container@training_codes),control=Weka_control(W="J48"))
        } else if (RF == TRUE) {
            model <- randomForest(x=matrix_container@training_matrix, y=matrix_container@training_codes, ntree=ntree)
        } else if (GLMNET == TRUE) {
            model <- glmnet(x=matrix_container@training_matrix, y=matrix_container@training_codes, family="multinomial", maxit=maxitglm)
        } else if (TREE == TRUE) {
            model <- tree(matrix_container.training_codes ~ .,data=data.frame(matrix_container@training_matrix,matrix_container@training_codes))
        } else if (NNET == TRUE) {
            model <- nnet(matrix_container.training_codes ~ .,data=data.frame(matrix_container@training_matrix,matrix_container@training_codes), size=size, maxit=maxitnnet, MaxNWts=10000, rang=0.1, decay=5e-4)
        } else if (MAXENT == TRUE) {
			new_maxent()
			model <- train_maxent(matrix_container@training_matrix,as.vector(matrix_container@training_codes))
		}
		
		# RETURN TRAINED MODEL
		gc() # CLEAN UP AFTER MODEL
		return(model)
}