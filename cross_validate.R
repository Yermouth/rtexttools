##############################################################
#                       cross_validate                       #
##############################################################
#Loren Collingwood 3/06/2011 Alpha version Rtexttools

#n-fold cross validation for the eight "built-in" R functions.

#Arguments

#fnct -- the function name of the algorithm (i.e., svm, randomForest)
#train_test -- train_test class object
#type -- The type of algorithm to be cross-validated
#seed -- A number for set.seed() function. Allows replicable results
#size -- specific to nnet. Number of units in the hidden layer. Can be zero if there are skip-layer units.
#maxitglm -specific to GLMNET. Number. Maximum number of outer-loop iterations for "multinomial families". Default=500.
#maxitnnet -- specific to nnet. Number. Maximum number of iterations. Default=1000.

#Value
#For all algorithms except GLMNET, produces a list object of length two, with list item one a vector of CV accuracy scores
#and list item two the mean value from these scores. GLMNET currently has some problems so only the vector is produced.
 
cross_validate <- function(fnct,train_test,nfold,type=c("SVM","NAIVE","BOOSTING","BAGGING","RF","GLMNET","TREE","NNET"), seed=NA,
                            size=1,maxitglm=500,maxitnnet=1000) {

    options(warn=-1) #Repress warnings
    if (!is.na(seed)) #Set seed for replicability
        set.seed(seed)
    #Bring in info from the train_test function    
    alldata <- rbind(train_test@trainpredict,train_test@testpredict) #put all data together
    alldata <- dtm_matrix_clean(alldata) #will probably want to just put this in test_train function
    allcodes <- as.factor(c(train_test@traincode,train_test@test_code))
    data_and_codes <-cbind(alldata,allcodes)
    #Sample
    rand <- sample(nfold,dim(alldata)[1], replace=T) #replace
    
    cv_accuracy <- NULL
    for (i in sort(unique(rand))) {
        
        if (type=="SVM" | type=="NAIVE") {
            model <- fnct(x=alldata[rand!=i,], y=allcodes[rand!=i]) #put function here
            pred <- predict(model,alldata[rand==i,])
        } else
        
        if (type=="RF") {
            model <- fnct(as.factor(allcodes)~.,data=data_and_codes[rand!=i,])
            pred <- predict(model,newdata=alldata[rand==i,])
        } else
        if (type=="GLMNET") {

            #have to use the try exception, if there's a "0" in one of the categories then model will fail
            #however, the code continues to run but it appears the some of the accuracies are incorrect.
            try(model <- fnct(x=alldata[rand!=i,], y=allcodes[rand!=i],family="multinomial", maxit=maxitglm),silent=FALSE)
            prob <- predict(model,alldata[rand==i,],s=0.01,type="response")            
            pred <- apply(prob[,,1],1,which.max)
 
        } else
        if (type=="BOOSTING"| type=="BAGGING") {
            model <- fnct(as.factor(allcodes)~ ., data = data.frame(data_and_codes[rand!=i,]),
                                 control = Weka_control(W = "J48"))
            pred <- predict(model,data.frame(alldata[rand==i,]))
        } else
        if (type=="TREE") {
            
            model <- fnct(as.factor(allcodes)~ ., data = data.frame(data_and_codes[rand!=i,]))
            prob <- predict(model,newdata=data.frame(alldata[rand==i,]), type="vector")
            pred <- apply(prob,1,which.max)

        } else
        if(type=="NNET") {
            model <- fnct(as.factor(allcodes)~ ., data = data.frame(data_and_codes[rand!=i,]),size=size,maxit=maxitnnet)
            prob <- predict(model,data.frame(alldata[rand==i,]))
            pred <- apply(prob,1,which.max)
            
        }    

        try(confusion <- confusion_create(allcodes[rand==i],pred),silent=FALSE) #Internal function "confusion_create" used here
        cv_accuracy[i] <- round(sum(diag(confusion))/length(allcodes[rand==i]),3)
        
        cat("Fold ",i," Out of Sample Accuracy"," = ",cv_accuracy[i],"\n",sep="")
    }
    #GLMNET sometimes has problems with 
    if (type=="GLMNET") {
        return(list(cv_accuracy))
    } else {
        return(list(cv_accuracy,meanAccuracy=mean(cv_accuracy)))
    }
}

#cvsvm <- cross_validate(fnct=svm,train_test=train_test,nfold=3,type="SVM")
#set seed for replicability
#cvsvm <- cross_validate(fnct=svm,train_test=train_test,nfold=3,type="SVM",seed=3444)
#cvsvm
#cvnaive <- cross_validate(fnct=naiveBayes,train_test=train_test,nfold=5,type="NAIVE")
#cvnaive
#cvrf <- cross_validate(fnct=randomForest,train_test=train_test,nfold=3,type="RF")
#cvrf
#Folds predictions after the error message "Error in if(outlist$msg!="unknown error") return(outlist): argument is of length zero"
#   are wrong. Ignore those, still trying to figure out how to fix. However, this probably won't happen with large datasets.
#cvglmnet <- cross_validate(fnct=glmnet,train_test=train_test,nfold=5,type="GLMNET")
#cvglmnet
#cvboosting <- cross_validate(fnct=AdaBoostM1,train_test=train_test,nfold=5,type="BOOSTING")
#cvboosting
#cvbagging <- cross_validate(fnct=Bagging,train_test=train_test,nfold=5,type="BAGGING")
#cvbagging
#cvtree <- cross_validate(fnct=tree,train_test=train_test,nfold=6,type="TREE")
#cvtree
#cvnnet <- cross_validate(fnct=nnet,train_test=train_test,nfold=3,type="NNET")
#cvnnet
