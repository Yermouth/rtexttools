cross_validate <-
function(fnct,matrix_container,nfold,type=c("SVM","NAIVE","BOOSTING","BAGGING","RF","GLMNET","TREE","NNET","MAXENT"), seed=NA,
                            size=1,maxitglm=500,maxitnnet=1000) {

    options(warn=-1) #Repress warnings
    if (!is.na(seed)) #Set seed for replicability
        set.seed(seed)
    #Bring in info from the matrix_container function    
	alldata <- rbind(matrix_container@training_matrix,matrix_container@classification_matrix) #put all data together
	#alldata <- matrix_container@full_matrix
	allcodes <- as.factor(c(matrix_container@training_codes,matrix_container@testing_codes))
    #Sample
    rand <- sample(nfold,dim(alldata)[1], replace=T) #replace
    
    cv_accuracy <- NULL
    for (i in sort(unique(rand))) {
        if (type=="SVM") {
            model <- fnct(x=alldata[rand!=i,], y=allcodes[rand!=i]) #put function here
            pred <- predict(model,alldata[rand==i,])
        } else
		if (type=="NAIVE") {
			alldata <- as.matrix(alldata)
			colnames(alldata) <- matrix_container@column_names
			model <- fnct(x=alldata[rand!=i,], y=allcodes[rand!=i]) #put function here
            pred <- predict(model,alldata[rand==i,])
		} else
        
        if (type=="RF") {
			alldata <- rbind(as.matrix(matrix_container@training_matrix),as.matrix(matrix_container@classification_matrix))
			colnames(alldata) <- matrix_container@column_names
			data_and_codes <-cbind(alldata,allcodes)
            model <- fnct(as.factor(allcodes)~.,data=data_and_codes[rand!=i,])
            pred <- predict(model,newdata=alldata[rand==i,])
        } else
        if (type=="GLMNET") {
			alldata <- rbind(as.matrix(matrix_container@training_matrix),as.matrix(matrix_container@classification_matrix))
			colnames(alldata) <- matrix_container@column_names
            #have to use the try exception, if there is a "0" in one of the categories then model will fail
            #however, the code continues to run but it appears the some of the accuracies are incorrect.
            try(model <- fnct(x=alldata[rand!=i,], y=allcodes[rand!=i],family="multinomial", maxit=maxitglm),silent=FALSE)
            prob <- predict(model,alldata[rand==i,],s=0.01,type="response")            
            pred <- apply(prob[,,1],1,which.max)
 
        } else
        if (type=="BOOSTING"| type=="BAGGING") {
			alldata <- rbind(as.matrix(matrix_container@training_matrix),as.matrix(matrix_container@classification_matrix))
			colnames(alldata) <- matrix_container@column_names
			data_and_codes <- cbind(alldata,allcodes)
            model <- fnct(as.factor(allcodes)~ ., data = data.frame(data_and_codes[rand!=i,]), control = Weka_control(W = "J48"))
            pred <- predict(model,data.frame(alldata[rand==i,]))
        } else
        if (type=="TREE") {
			alldata <- rbind(as.matrix(matrix_container@training_matrix),as.matrix(matrix_container@classification_matrix))
			colnames(alldata) <- matrix_container@column_names
            data_and_codes <- cbind(alldata,allcodes)
            model <- fnct(as.factor(allcodes)~ ., data = data.frame(data_and_codes[rand!=i,]))
            prob <- predict(model,newdata=data.frame(alldata[rand==i,]), type="vector")
            pred <- apply(prob,1,which.max)

        } else
        if(type=="NNET") {
			alldata <- rbind(as.matrix(matrix_container@training_matrix),as.matrix(matrix_container@classification_matrix))
			colnames(alldata) <- matrix_container@column_names
			data_and_codes <- cbind(alldata,allcodes)
            model <- fnct(as.factor(allcodes)~ ., data = data.frame(data_and_codes[rand!=i,]),size=size,maxit=maxitnnet)
            prob <- predict(model,newdata=data.frame(alldata[rand==i,]))
            pred <- apply(prob,1,which.max)
        } else
		if (type=="MAXENT") {
			new_maxent()
			model <- train_maxent_sparse(matrix_container@training_matrix,as.vector(matrix_container@training_codes))
			pred <- classify_maxent_sparse(alldata[rand==i,])
		}

		#try(confusion <- confusion_create(allcodes[rand==i],pred),silent=FALSE) #Internal function "confusion_create" used here
		#cv_accuracy[i] <- round(sum(diag(confusion))/length(allcodes[rand==i]),3)
        cv_accuracy[i] <- predictive_accuracy(allcodes[rand==i],pred)
		
        cat("Fold ",i," Out of Sample Accuracy"," = ",cv_accuracy[i],"\n",sep="")
    }
    #GLMNET sometimes has problems with 
    if (type=="GLMNET") {
        return(list(cv_accuracy))
    } else {
        return(list(cv_accuracy,meanAccuracy=mean(cv_accuracy)))
    }
}

