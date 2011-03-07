##############################################
#                 train_model                #
##############################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools
#Function trains each model 1 by 1. These models will then be passed to the classifier function.

#Arguments
#train_test --the output from the train_test function. See ?traintest.
#SVM -- TRUE/FALSE logical, default is FALSE. Support Vector Machine Algorithm from libsvm.
#NAIVE -- TRUE/FALSE logical, default is FALSE. Naive Bayes Algorithm from libsvm.
#BOOSTING -- TRUE/FALSE logical, default is FALSE. AdaBoost Multinomial Algorithm from RWeka.
#BAGGING -- TRUE/FALSE logical, default is FALSE. Bagging Algorithm from RWeka.
#RF -- TRUE/FALSE logical, default is FALSE. Random Forests algorithm from randomForest.
#GLMNET -- TRUE/FALSE logical, default is FALSE. Generalized Linear Model with lasso multinomial function from glmnet.
#TREE -- TRUE/FALSE logical, default is FALSE. Tree algorithm from tree.
#NNET -- TRUE/FALSE logical, default is FALSE. Neural Networks from nnet.
#method -- specific to SVM. Character. See ?svm.
#size -- specific to nnet. Number of units in the hidden layer. Can be zero if there are skip-layer units.
#maxitglm -specific to GLMNET. Number. Maximum number of outer-loop iterations for "multinomial families". Default=500.
#maxitnnet -- specific to nnet. Number. Maximum number of iterations. Default=1000.

#Values
#Various outputs from each model. Not terribly useful at this point until after classification.

library(RWeka)
library(randomForest)
library(glmnet)
library(tree)
library(nnet)


train_model <- function(train_test,SVM=FALSE,NAIVE=FALSE,BOOSTING=FALSE,
                        BAGGING=FALSE,RF=FALSE,GLMNET=FALSE,TREE=FALSE,NNET=FALSE,
                        method="C-classification",maxitglm=500,size=1,maxitnnet=1000,...) {
        
        #MODELS
        if (SVM==TRUE){
            model1 <- svm(x=train_test@trainpredict, y=train_test@traincode, method=method, cross=0, probability=TRUE)
            return(model1)
        } else
        if(NAIVE==TRUE){
            model2 <- naiveBayes(x=train_test@trainpredict, y=train_test@traincode)
            return(model2)
        } else
        if (BOOSTING==TRUE) {
            model3 <- AdaBoostM1(as.factor(train_code)~ ., data=train_test@train_data_codes, control=Weka_control(W="J48"))
            return(model3)
        } else
        if (BAGGING == TRUE) {
            model4 <- Bagging(as.factor(train_code)~.,data=data.frame(train_test@train_data_codes),control=Weka_control(W="J48"))
            return(model4)
        } else
        if (RF==TRUE){
            model5 <- randomForest(x=train_test@trainpredict, y=train_test@traincode)
            return(model5)
        } else
        if (GLMNET==TRUE){
            model6 <- glmnet(x=train_test@trainpredict, y=train_test@traincode, family="multinomial", maxit=maxitglm)
            return(model6)
        } else
        if (TREE==TRUE){
            model7 <- tree(as.factor(train_code)~.,data=data.frame(train_test@train_data_codes))
            return(model7)
        } else
        if (NNET==TRUE){
            model8 <- nnet(as.factor(train_code)~.,data=data.frame(train_test@train_data_codes), size=size, maxit=maxitnnet)
            return(model8)
        } 
       
}

#Train the data for each model
#SVM <- train_model(train_test=train_test,SVM=TRUE)
#NAIVE <- train_model(train_test=train_test,NAIVE=TRUE)
#BOOSTING <- train_model(train_test=train_test,BOOSTING=TRUE)
#BAGGING <- train_model(train_test=train_test,BAGGING=TRUE)
#RF <- train_model(train_test=train_test,RF=TRUE)
#GLMNET <- train_model(train_test=train_test,GLMNET=TRUE)
#TREE <- train_model(train_test=train_test,TREE=TRUE)
#NNET <- train_model(train_test=train_test,NNET=TRUE)
