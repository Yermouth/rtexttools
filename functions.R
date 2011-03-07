#################################################
#Loren Collingwood                              #
#Started: 2/26/2011; 2/27/2011                  #
#Reading in various types of files              #
#Rtexttools code                                #
#Project with: Wouter Van Atteveldt, Tim Jurka, #
#              Amber Boydstun, Emiliano Grossman#
#################################################

#Set working directory
setwd("C:\\Users\\Loren\\Documents\\My Dropbox\\RTextTools\\Datasets")

library(RODBC)
library(RWeka)
library(randomForest)
library(glmnet)
library(tree)
library(nnet)
library(RMySQL)
library(openNLP)
library(SparseM)
library(tm)
library(Snowball)
#Packages need to be installed but not loaded. The example below is done on the French data, 
#install.packages("openNLPmodels.en")
#install.packages("openNLPmodels.es")


#install.packages("RODBC")
#library(Rexttools)

options(stringsAsFactors=FALSE) # Be sure to set each time or at least in some of the functions



###############################################################
#                          datainput                          #
###############################################################
#This function brings in CSV file, Text File, or Access
#filename = name of the file you are reading in, character string
#tablename = Access table name, only for Access call
#type -- one of four data types the user is reading in.

#LC Note: Add code to read in multiple text files
#use "assign" on left side of arrow and use a loop to read in the files then rbind them
datainput <- function(filename,tablename=NULL, type=c("csv","tab","accdb","mdb"),...) {

    Call <- match.call()
    indx <- match(c("filename", "tablename", "type"), names(Call), 
        nomatch = 0)
    if (indx[1] == 0) 
        stop("An Access, CSV, or tab delimited file must be used")
    if (indx[3] == 0) 
        stop("User must indicate type of file to input (e.g., 'csv', 'tab')")

    #Comma Delimted CSV files
    if (type=="csv") {
        datainput <- read.csv(filename,header=T)
    }    else
        #Tab delimted .txt files
        if (type=="tab") { 
            datainput <- read.delim(filename,header=TRUE, sep="\t")
        }
            #Bring in accdb or mdb file
            #Needs RODBC package + database must be ODBC'd
            else {    
                datainput <- datagrab(filename, tablename, path=".", adb_vers=2007)
            } 
    invisible(datainput)
}

#CSV
da <- datainput("French_news1_csv.csv", type="csv")
#Tab Delimited
db <- datainput("French_news1_tab.txt",type="tab")
#Access mdb or accdb
dc <- datainput("nyt_front_page_1996_2006_Scott_Recodes_Fall2010_decile01.mdb", tablename="Issues", type="mdb")

#MySQL
library(RMySQL) #sometimes has problems installing on Windows. Something to do with Namespaces and registry,
# had to use Mac

mysql_in <- function(dbname, user, password=NULL, host, tablename){
    
    drv <- dbDriver("MySQL")
    con <- dbConnect(drv, dbname=dbname, user=user, password=password, host=host)
    result <- dbGetQuery(con, paste('select * from',tablename,sep=" "))
    return(result)
}

#This grabs data from a phpMyadmin account
senateshort <- mysql_in(dbname="cbp", user="lorenc2", password='MADEUPPASSWORD', 
                 host='cbp.serverk.org', tablename='actorsSenate')

#####################################################
#                   text_to_df                      #
#####################################################

#Read from multiple text files with a "Control" file
#Put all records in separate text files in a directory. All other files
#in that directory should be moved to another directory. Also need to have 
#a control file, a comma delimted file listing as rows the file name, the code
#of that file, and any other information.

#LC Note: May have to do something with the Language component here.
setwd("F:\\CAPPP\\texttools\\ReadMe")

#directory -- where the text files are stored
#controlfile -- A .txt file usually consisting of three comma separated columns: unique identifier, 
#   Code, Training/Test set. This is a similar set up to Gary King's Readme package. Should include path to file.
#   First 5 rows shown of control file.
#ROWID,TRUTH,TRAININGSET
#1.txt,20,1
#2.txt,13,1
#3.txt, 1,1
#4.txt,20,1
#5.txt,20,1
#identifier -- is character column name in the control file. Should be the same name as the files including .txt
text_to_df <- function(directory,controlfile, identifier) {
    Call <- match.call()
    indx <- match(c("directory", "controlfile", "identifier"), names(Call), 
        nomatch = 0)
    if (indx[1] == 0) 
        stop("A directory path where the .txt files reside must be included")
    if (indx[2] == 0) 
        stop("A control file must be included")
    if (indx[3] == 0) 
        stop("A unique identifier from the control file must be included")
 
    olddir <- getwd()
    setwd(directory)
    textcolumn <- NULL
    identcolumn <- NULL
    for (i in 1:length(list.files())) {
        #so we don't take directories, only files
        a <- strsplit(list.files()[i],"")
        b <- a[[1]][length(a[[1]])-3]
        if (b==".") {
            con <- file(list.files()[i],"r")
            #Puts file names into a vector
            identcolumn[i] <- list.files()[i]
            textcolumn[i] <- readLines(con)
            close(con)
        }
    }
    setwd(olddir)
    #put two vectors into a dataframe
    dataout <-data.frame(cbind(identcolumn,textcolumn))
    #Read in control file
    controlfile <- read.table(controlfile, header=T, sep=",")
    #controlfile <- controlfile[-1,] #won't neeed this for normal

    control_order <- controlfile[order(controlfile[,identifier]),]#sort the control file before
    rownames(control_order) <- seq(1,nrow(control_order),1)

    final <- merge(dataout,control_order, by.x="identcolumn", by.y=identifier)

}

#Text to Data frame, produces dataframe
system.time(
    textdf <- text_to_df(directory="F:\\CAPPP\\texttools\\ReadMe",
                    controlfile="F:\\CAPPP\\texttools\\ReadMe\\controllist\\control.txt",
                    identifier="ROWID")
)
#   user  system elapsed 
#  16.13    2.04   18.25 

################################################
#               word_trunc                     #
################################################
#Function performs word truncation on each text row in a dataset. This can be developed further 
#to replace the dataframe text column.
library(openNLP)
#Packages need to be installed but not loaded. The example below is done on the French data, 
#so language here doesn't appear to be a problem.
#install.packages("openNLPmodels.en")
#install.packages("openNLPmodels.es")

word_trunc <- function(textcolumn,wordtrunc) {

    Call <- match.call()
    indx <- match(c("textcolumn", "wordtrunc"), names(Call), 
        nomatch = 0)
    if (indx[1] == 0) 
        stop("A character vector must be included")
    if (indx[2] == 0) 
        stop("A number must be included")

    word_token_trunc <- function(textstring, truncation){
    
        tokenwords <- tokenize(textstring)
        token_trunc <- tokenwords[1:truncation]
        backtogether <- paste(token_trunc, collapse=" ")
        return(backtogether)
    }

    newvec <- NULL
    newvec_clean <- NULL
    for (i in 1:length(textcolumn)) { #loop over each text item
        newvec[i] <- word_token_trunc(textcolumn[i],truncation=wordtrunc)
        #Need to delete the NA's that form from text that are blank or less than the truncation specification
        newvec_clean[i] <- gsub("NA","",newvec[i],fixed=TRUE)
    }
    rm(newvec) #clean up
return(newvec_clean)
}

setwd("C:/Users/Loren/Documents/My Dropbox/RTextTools/Datasets")
da <- datainput("French_news1_csv.csv", type="csv")

system.time(
truncated_words <- word_trunc(da$headline,wordtrunc=4)
)
#French dataset. 10,183 observations
#   user  system elapsed 
# 541.19   20.11  569.70 

###############################################
#               multi_text                    #
###############################################
#Loren Collingwood, 3/6/2011 Alpha Version Rtexttools
#Ability to use two - four columns of text. Execute prior to DTM creation

#Arguments
#column1 = character vector of text
#column2 = character vector of text    
#column3 = character vector of text
#column4 = character vector of text
#length = number, how many columns the analyst is concatenating

#Values
#Returned vector of concatenated text.

multi_text <- function(column1, column2, column3=NULL, column4=NULL, length=2) {

    if (length==2){
        columnout <- paste(column1,column2,sep="")
    } else
    if (length==3){
        columnout <- paste(column1,column2,column3,sep="")
    } else
    if (length==4)
        columnout <- paste(column1,column2,column3,column4,sep="")
    return(columnout)
}

setwd("C:/Users/Loren/Documents/My Dropbox/RTextTools/Datasets")
da <- datainput("French_news1_csv.csv", type="csv")

#Paste same columns next to each other
fr2 <- da[1:1000,] # minimize the cases so most computers can do it.
headline <- fr2$headline #headline column from French dataset
textcolumn <- fr2$text   #text column from French dataset
#Two Columns    
cat2 <- multi_text(headline,textcolumn)
#Three Columns
cat3 <- multi_text(headline,textcolumn,textcolumn, length=3)
#Four Columns
cat4 <- multi_text(headline,textcolumn,textcolumn,headline,length=4)

##########################################################
#                   textual_param                        #
##########################################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools
#This function trims the text, including whitespace removal, lowercase, and removing stopwords

#Must include packages: tm, Snowball
library(tm); library(Snowball)

#Arguments
#textual_vect = character vector from dataframe or wherever
#language =  language you are working with: "english","french", "spanish", etc.
#whitespace = TRUE/FALSE Whitespace removal
#lowercase = TRUE/FALSE Convert all text to lowercase
#removestopwords =TRUE/FALSE Remove stopwords in text

#Values
#Cleaned up corpus of text. Everything except stemming, which has to be mapped separately.

textual_param <- function(textual_vect, language, whitespace=TRUE, lowercase=TRUE,
                          removestopwords=TRUE, ...) {
    #Read in initial character vector
    corp <- Corpus(VectorSource(textual_vect), readerControl=list(language=language))

    #Strip Whitespace
    if (whitespace)
        corp <- tm_map(corp,stripWhitespace)
    #Lowercase
    if (lowercase)
        corp <- tm_map(corp,tolower)
    #Remove Stopwords        
    if (removestopwords)
        corp <- tm_map(corp, removeWords,stopwords(language))
    return(corp)
}

#Function Execution
setwd("C:/Users/Loren/Documents/My Dropbox/RTextTools/Datasets")
da <- datainput("French_news1_csv.csv", type="csv")

#Paste same columns next to each other
fr2 <- da[1:1000,] # minimize the cases so most computers can do it.

textparam_out <- textual_param(textual_vect=fr2$text, language="french")
#For some reason Word Stemming (mapping) won't work inside the function when it 
#goes to return the object; probably because it's mapping something an not an object. Tim?
tm_map(textparam_out, stemDocument)

################################################
#                   dtm                        #
################################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools
#Document Term Matrix production with option for sparsity control of the matrix
#corpus -- a corpus of text, produced from textual_param and tm_map(corpus, stemDocument)
#may want to make a few different classes, but can consider later
dtm <- function(corpus, sparsity=.9998) {
    dtm <- DocumentTermMatrix(corpus)
    out <- removeSparseTerms(dtm,sparsity)
}

french_dtm <- dtm(textparam_out,sparsity=.98) # this sparsity number will need to be toyed with, sometimes R will crap out 
                                              #in the next function becuase it's too big.

#######################################################
#                      traintest                      #
#######################################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools

#This function takes a document term matrix and produces the appropriate 
#training matrixes and labeling vectors. Requires SparseM package for sparse matrix
#production. Function assumes all data are in a standard n*k dataframe (note: this may change).
#Output is a list.
#dtm -- document term matrix as produced by dtm or some other way
#label -- a vector of the label/code category of the text for training and testing if not on virgin text, or training if on virgin text.
#trainsize -- a sequence (i.e., 1:800) of text itmes used for training.
#testsize -- a sequence (i.e., 801:1000) of text items used for test evaluation. Usually comes from the same dataframe as trainsize.
#extrafeature -- dataframe, matrix, or vector of extra features.
#ef -- TRUE/FALSE logical. Default is false, indicating that no extra features are added.

library(SparseM)

#Example
trainsize=1:400
testsize=401:600
dtm <- french_dtm
label <- da$Codegros
extrafeature <- data.frame(fr2$Code,fr2$Code)

traintest <- function(dtm,label,trainsize,testsize,extrafeature,ef=FALSE) {
    
    train <- dtm[trainsize]
    test <- dtm[testsize]

    train_code <- as.factor(label[trainsize])
    test_code <- as.factor(label[testsize])
   
    if (ef==TRUE) { #Extra Features Branch
    
        if (length(table(complete.cases(extrafeature)))>1) { #Missing Data Issue
            stop ("Extra features cannot have missing data")
            }
        extrafeature_train <- extrafeature[trainsize,]
        extrafeature_test <- extrafeature[testsize,] 
        #Need non-sparse matrix for prediction
        matrix_train_predict <- as.matrix(cbind(as.matrix(train, "matrix.csr"),extrafeature_train))
        matrix_test_predict <- as.matrix(cbind(as.matrix(test, "matrix.csr"),extrafeature_test))
            
    } #Close Extra Features branch
    else { #Regular Matrix Creation Branch
    
    matrix_train_predict <- as.matrix(train, "matrix.csr")
    matrix_test_predict <- as.matrix(test, "matrix.csr")
    } #Close regular matrix branch
    
    #Need sparse matrix for algorithm    
    matrix_train_sparse <- as.matrix.csr(matrix_train_predict)
    matrix_test_sparse <- as.matrix.csr(matrix_test_predict)
        
    #Create Class for output
    setClass("traintest",representation(trainmat_sparse="matrix.csr",testmat_sparse="matrix.csr",
                                        trainpredict="matrix",testpredict="matrix",
                                        traincode="factor",test_code="factor"))

    outdata <-  new("traintest", trainmat_sparse=matrix_train_sparse,testmat_sparse=matrix_test_sparse,
                trainpredict=matrix_train_predict,testpredict=matrix_test_predict,
                traincode=train_code,test_code=test_code)
    
    return(outdata)   
}

train_test_features <- traintest(dtm=french_dtm,label=da$Codegros,trainsize=1:400, testsize=401:600,extrafeature=data.frame(fr2$Code,fr2$Code),ef=TRUE)
#This won't work because of missing data, need to fill that in. For now, we can just replace NA with zero
extrafeature[is.na(extrafeature)] <-0
train_test_features <- traintest(dtm=french_dtm,label=da$Codegros,trainsize=1:400, testsize=401:600,extrafeature=extrafeature,ef=TRUE)
names(train_test_features)
#Regular, No features
train_test <- traintest(dtm=french_dtm,label=da$Codegros,trainsize=1:400, testsize=401:600)
names(train_test)

head(train_test_features$trainpredict) #Features added to end
head(train_test$trainpredict) #No features on end


##############################################
#                 train_model                #
##############################################
#Function trains each model 1 by 1. These models will then be passed the classifier function.
#train_test --the output from the train_test function. Includes a host of list objects. See ?train_test.
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

train_model <- function(train_test,SVM=FALSE,NAIVE=FALSE,BOOSTING=FALSE,
                        BAGGING=FALSE,RF=FALSE,GLMNET=FALSE,TREE=FALSE,NNET=FALSE,
                        method="C-classification",maxitglm=500,size=1,maxitnnet=1000,...) {
        
        #PUT TRAIN DATA INTO DATA.FRAME FOR SOME ALGORITHM CALLS
        train_data <- dtm_matrix_clean(train_test@trainpredict) #Rtexttools internal function
        train_data <- data.frame(train_data2,train_test@traincode)

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
            model3 <- AdaBoostM1(as.factor(train_test.traincode)~ ., data=train_data, control=Weka_control(W="J48"))
            return(model3)
        } else
        if (BAGGING == TRUE) {
            model4 <- Bagging(as.factor(train_test.traincode)~.,data=data.frame(train_data),control=Weka_control(W="J48"))
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
            model7 <- tree(as.factor(train_test.traincode)~.,data=data.frame(train_data))
            return(model7)
        } else
        if (NNET==TRUE){
            model8 <- nnet(as.factor(train_test.traincode)~.,data=data.frame(train_data), size=size, maxit=maxitnnet)
            return(model8)
        } 
       
}

#Train the data for each model
SVM <- train_model(train_test,SVM=TRUE)
NAIVE <- train_model(train_test,NAIVE=TRUE)
BOOSTING <- train_model(train_test,BOOSTING=TRUE)
BAGGING <- train_model(train_test,BAGGING=TRUE)
RF <- train_model(train_test,RF=TRUE)
GLMNET <- train_model(train_test,GLMNET=TRUE)
TREE <- train_model(train_test,TREE=TRUE)
NNET <- train_model(train_test,NNET=TRUE)

#Train with Features
SVM_f <- train_model(train_test_features,SVM=TRUE)
NAIVE_f <- train_model(train_test_features,NAIVE=TRUE)
BOOSTING_f <- train_model(train_test_features,BOOSTING=TRUE)
BAGGING_f <- train_model(train_test_features,BAGGING=TRUE)
RF_f <- train_model(train_test_features,RF=TRUE)
GLMNET_f <- train_model(train_test_features,GLMNET=TRUE)
TREE_f <- train_model(train_test_features,TREE=TRUE)
NNET_f <- train_model(train_test_features,NNET=TRUE)

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
        bagging_probout <- apply(bagging_prob,1,probextract) #Extract Highest Probability
        outdata <- data.frame(as.numeric(as.character(bagging_pred)),bagging_probout)
        colnames(outdata)[1] <- "bagging_pred"
        return(outdata)
    } else
    if (type=="RF"){
        rf_pred <- predict(RF,newdata=train_test@testpredict) #extract prediction
        rf_prob <- predict(RF,newdata=train_test@testpredict,type="prob")#probability extraction
        rf_probout <- apply(rf_prob,1,probextract)
        outdata <- data.frame(as.numeric(as.character(rf_pred)),rf_probout)
        colnames(outdata)[1] <- "rf_pred"
        return(outdata)
    } else
    
    if (type=="GLMNET"){
        glmnet_prob <- predict(GLMNET,newx=train_test@testpredict,s=s,type="response")
        glmnet_prob_out <- apply(glmnet_prob,1,probextract) #Extract Highest Probability
        glmnet_pred <- apply(glmnet_prob[,,1],1,which.max) #Extract prediction
        outdata <- data.frame(as.numeric(as.character(glmnet_pred)),glmnet_prob_out)
        colnames(outdata)[1] <- "glmnet_pred"
        return(outdata)
    }
    if (type=="TREE"){
        tree_prob <- predict(TREE,newdata=data.frame(train_test@testpredict), type="vector")
        tree_prob_out <- apply(tree_prob,1,probextract) #Extract Probability
        tree_pred <- apply(tree_prob,1,which.max) #Extract prediction
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
SVM_CLASSIFY <- classify_model(train_test=train_test,type="SVM",SVM=SVM)
NAIVE_CLASSIFY <- classify_model(train_test,type="NAIVE",NAIVE=NAIVE)
BOOSTING_CLASSIFY <- classify_model(train_test,type="BOOSTING",BOOSTING=BOOSTING)
BAGGING_CLASSIFY <- classify_model(train_test,type="BAGGING",BAGGING=BAGGING)
RF_CLASSIFY <- classify_model(train_test,type="RF",RF=RF)
GLMNET_CLASSIFY <- classify_model(train_test,type="GLMNET",GLMNET=GLMNET)
TREE_CLASSIFY <- classify_model(train_test,type="TREE",TREE=TREE)
NNET_CLASSIFY <- classify_model(train_test,type="NNET",NNET=NNET)

#Classify with Features
SVM_CLASSIFY_f <- classify_model(train_test=train_test_features,type="SVM",SVM=SVM_f)
NAIVE_CLASSIFY_f <- classify_model(train_test_features,type="NAIVE",NAIVE=NAIVE_f)
BOOSTING_CLASSIFY_f <- classify_model(train_test_features,type="BOOSTING",BOOSTING=BOOSTING_f)
BAGGING_CLASSIFY_f <- classify_model(train_test_features,type="BAGGING",BAGGING=BAGGING_f)
RF_CLASSIFY_f <- classify_model(train_test_features,type="RF",RF=RF_f)
GLMNET_CLASSIFY_f <- classify_model(train_test_features,type="GLMNET",GLMNET=GLMNET_f)
TREE_CLASSIFY_f <- classify_model(train_test_features,type="TREE",TREE=TREE_f)
NNET_CLASSIFY_f <- classify_model(train_test_features,type="NNET",NNET=NNET_f)

#Examine Accuracy Predictions
algorithm_predictions <- cbind(SVM_CLASSIFY,NAIVE_CLASSIFY,BOOSTING_CLASSIFY,BAGGING_CLASSIFY,
                                RF_CLASSIFY,GLMNET_CLASSIFY,TREE_CLASSIFY,NNET_CLASSIFY)
algorithm_predictions_f <- cbind(SVM_CLASSIFY_f,NAIVE_CLASSIFY_f,BOOSTING_CLASSIFY_f,BAGGING_CLASSIFY_f,
                                 RF_CLASSIFY_f,GLMNET_CLASSIFY_f,TREE_CLASSIFY_f,NNET_CLASSIFY_f)


#Combine onto new dataframe with all the old columns of just the tested data.
test_results <- cbind(fr2[401:600,],algorithm_predictions) #fr2 is original data frame
test_resultsf <- cbind(fr2[401:600,],algorithm_predictions_f) 
test_resultsn <- cbind(fr2[401:600,],algorithm_predictions_n) 
#Ensemble Consensus
accuracyout <- accuracy("Codegros", c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
                        "glmnet_pred","tree_pred","nnet_pred"), data=test_results)
accuracyout #list the results

accuracyout_f <- accuracy("Codegros", c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
                        "glmnet_pred","tree_pred","nnet_pred"), data=test_resultsf)
accuracyout_f #list the results

accuracyout_n <- accuracy("Codegros", c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
                        "glmnet_pred","tree_pred","nnet_pred"), data=test_resultsn)
accuracyout_n #list the results

###############################################################
#                        agreescore                           #
###############################################################
#Loren Collingwood, 3/6/2011, Alpha Version Rtexttools

#Function calculates agreement/consensus score across algorithms.
#Currently, this works for just the eight build-in algorithms. Will have to change code slightly
#for when maximum entropy is incorporated.
#Arguments
#x is a numeric vector of algorithms labels. This will be the row in the data.
#Values
# A numeric score from 1-8 indicating the algorithmic agreement/consensus.

agreescore <- function (x) {
    
    a <- table(x)
    #LENGTH = 1
    if (length(a) == 1) 
        score <- 8
    #LENGTH = 2
    if (length(a) == 2) {
        if (any(a==7))
            score <- 7
        else if (any(a==6))
            score <- 6
        else if (any(a==5))
            score <- 5
        else if (any(a==4))
            score <- 4
    }
    #LENGTH = 3       
    if (length(a) == 3) {
        if (any(a==6))
            score <- 6
        else if (any(a==5))
            score <- 5
        else if (any(a==4))
            score <- 4
    }
    #LENGTH = 4
    if (length(a) == 4) {
        if (any(a==5))
            score <- 5
        else if (any(a==4))
            score <- 4
        else if (any(a==3))
            score <- 3
        else if (all(a==2))
            score <- 2
    }
    #LENGTH = 5
    if (length(a) == 5) {
        if (any(a==4))
            score <- 4
        else if (any(a==3))
            score <- 3
        else score <- 2
    }
    #LENGTH = 6
    if (length(a) == 6) {
        if(any(a==3))
            score <- 3
        else score <- 2
    }
    
    #LENGTH = 7
    if (length(a) == 7) 
        score <- 2

    #LENGTH = 8
    if (length(a) == 8) 
        score <- 1
    
    return(score)
}     

####################################################
#                  score_classify                  #
####################################################
#Loren Collingwood, 3/6/2011 Alpha Version Rtexttools

#This function scores each row in the datset for the level of consensus across algorithms
#and adds an additional column onto the dataframe. It uses the agreescore internal Rtexttools
#algorithm

#Arguments
# dataframe -- a dataframe object
# columnnames -- a character vector of columnames from the above dataframe

#Values
# A new dataframe object with a scores column appended. Data can then be subsetted 
# based on this column.
score_classify <- function (dataframe, columnnames) {

    a <- dataframe
    a2 <- as.matrix(a[match(c(columnnames), colnames(a))])
    scores <- apply(a2, 1, agreescore) #agreescore function inserted here
    a3 <- cbind(a, scores)
    invisible(a3)

}

scores <- score_classify(test_resultsn,c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
                        "glmnet_pred","tree_pred","nnet_pred"))

#Subset the data for just those rows with very high scores
newdat <- scores[scores$scores>6,]
table(newdat$Codegros,newdat$bagging_pred)
table(newdat$Codegros,newdat$aboost_pred)

######################################################
#  OUTPUT THE THE DATA CODED DATA WITH PROBABILITIES #
######################################################

write.csv(scores,"scores_french_small_train.csv")
