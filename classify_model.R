##################################################
#               classify_model                   #
##################################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools
#Accepts trained models and classifies them one by one producing a column of 
#length test vector/virgin data and most probabilities for each label.

#Argument
#train_test -- the output from the train_test function.  See ?train_test.
#type -- character string indicating which algorithm. Only one at at time: "SVM","NAIVE","BOOSTING","BAGGING","RF","GLMNET","TREE","NNET".
#SVM -- Object name of the trained model. Only matters for SVM classification, otherwise blank.
#NAIVE -- Object name of the trained model. Only matters for NAIVE classification, otherwise blank.
#BOOSTING -- Object name of the trained model. Only matters for BOOSTING classification, otherwise blank.
#BAGGING -- Object name of the trained model. Only matters for BAGGING classification, otherwise blank.
#RF -- Object name of the trained model. Only matters for RF classification, otherwise blank.
#GLMNET -- Object name of the trained model. Only matters for GLMNET classification, otherwise blank.
#TREE -- Object name of the trained model. Only matters for TREE classification, otherwise blank.
#NNET -- Object name of the trained model. Only matters for NNET classification, otherwise blank.
#s -- specific to glmnet. Value(s) of the penalty parameter lambda at which predictions are required. Default is 0.01. See ?predict.glmnet

#Values
#dataframe output of 1 or 2 columns, the first is the predicted label, and the second is the probability. 
#Probability not avaiable for Naive Bayes. There were some problems extracting this one.
classify_model <- function(train_test,type=c("SVM","NAIVE","BOOSTING","BAGGING","RF","GLMNET","TREE","NNET"),
                           SVM=SVM,NAIVE=NAIVE,BOOSTING=BOOSTING,BAGGING=BAGGING,RF=RF,GLMNET=GLMNET,
                           TREE=TREE,NNET=NNET,s=0.01,...) {

    #Used for extraction probabilities
    probextract <- function(x){
        probextract <- x[which.max(x)]
        return(probextract)
    }
    
    if (type=="SVM"){
        svm_pred <- predict(SVM,train_test@testpredict) #Extract Label
        svm_prob1 <- predict(SVM,train_test@testpredict, prob=TRUE)#probability extraction
        svm_prob2 <- attr(svm_prob1,"prob")
        svm_prob_out <- apply(svm_prob2,1,probextract) #Extract Probability
        outdata<- data.frame(as.numeric(as.character(svm_pred)),svm_prob_out) #need to chang svm_pred to numeric for agreescore creation
        colnames(outdata)[1] <- "svm_pred"
        return(outdata)
    } else
    
    if (type=="NAIVE"){
        naive_pred <- predict(NAIVE,train_test@testpredict)
        outdata <- data.frame(as.numeric(as.character(naive_pred)))
        colnames(outdata)[1] <- "naive_pred"
        return(outdata)
    } else

    if (type=="BOOSTING") {
        aboost_pred <- predict(BOOSTING,data.frame(train_test@testpredict)) #Extract Label
        aboost_prob <- predict(BOOSTING,data.frame(train_test@testpredict),type="probability") #Probability
        aboost_probout <- apply(aboost_prob,1,probextract) #Extract Highest Probability
        outdata <- data.frame(as.numeric(as.character(aboost_pred)),aboost_probout)
        colnames(outdata)[1] <- "aboost_pred"
        return(outdata)
    } else
    if (type=="BAGGING") {
        bagging_pred <- predict(BAGGING,data.frame(train_test@testpredict))
        bagging_prob <- predict(BAGGING,data.frame(train_test@testpredict), type="probability")
        bagging_probout <- apply(bagging_prob,1,probextract) 
        outdata <- data.frame(as.numeric(as.character(bagging_pred)),bagging_probout)
        colnames(outdata)[1] <- "bagging_pred"
        return(outdata)
    } else
    if (type=="RF"){
        rf_pred <- predict(RF,newdata=train_test@testpredict) 
        rf_prob <- predict(RF,newdata=train_test@testpredict,type="prob")
        rf_probout <- apply(rf_prob,1,probextract)
        outdata <- data.frame(as.numeric(as.character(rf_pred)),rf_probout)
        colnames(outdata)[1] <- "rf_pred"
        return(outdata)
    } else
    
    if (type=="GLMNET"){
        glmnet_prob <- predict(GLMNET,newx=train_test@testpredict,s=s,type="response")
        glmnet_prob_out <- apply(glmnet_prob,1,probextract) 
        glmnet_pred <- apply(glmnet_prob[,,1],1,which.max) 
        outdata <- data.frame(as.numeric(as.character(glmnet_pred)),glmnet_prob_out)
        colnames(outdata)[1] <- "glmnet_pred"
        return(outdata)
    }
    if (type=="TREE"){
        tree_prob <- predict(TREE,newdata=data.frame(train_test@testpredict), type="vector")
        tree_prob_out <- apply(tree_prob,1,probextract) 
        tree_pred <- apply(tree_prob,1,which.max)
        outdata <- data.frame(as.numeric(as.character(tree_pred)),tree_prob_out)
        colnames(outdata)[1] <- "tree_pred"
        return(outdata)
    }

    if (type=="NNET"){
        nnet_prob <- predict(NNET,data.frame(train_test@testpredict)) #probabilities
        nnet_prob_out <- apply(nnet_prob,1,probextract) #Extract Probability
        nnet_pred <- apply(nnet_prob,1,which.max) #Extract Highest Probability Score
        outdata <- data.frame(as.numeric(as.character(nnet_pred)),nnet_prob_out)
        colnames(outdata)[1] <- "nnet_pred"
        return(outdata)
    }
}


#Classify
#SVM_CLASSIFY <- classify_model(train_test=train_test,type="SVM",SVM=SVM)
#NAIVE_CLASSIFY <- classify_model(train_test,type="NAIVE",NAIVE=NAIVE)
#BOOSTING_CLASSIFY <- classify_model(train_test,type="BOOSTING",BOOSTING=BOOSTING)
#BAGGING_CLASSIFY <- classify_model(train_test,type="BAGGING",BAGGING=BAGGING)
#RF_CLASSIFY <- classify_model(train_test,type="RF",RF=RF)
#GLMNET_CLASSIFY <- classify_model(train_test,type="GLMNET",GLMNET=GLMNET)
#TREE_CLASSIFY <- classify_model(train_test,type="TREE",TREE=TREE)
#NNET_CLASSIFY <- classify_model(train_test,type="NNET",NNET=NNET)
