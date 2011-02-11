#Loren Collingwood
#R TextTools Experiments

setwd("H:\\Loren")
setwd("F:\\Rtexttools_group")
#Libraries
library(tm) # document term matrix
library("e1071") #svm and other algorithms
library(slam) #not sure if we need this one
library(SparseM) #Converting to Sparse Matrices
memory.size(TRUE)
options(stringsAsFactors=FALSE) # Be sure to set each time or at least in some of the functions

##############################################################
#French Data
##############################################################
#Started: 1/20/2011

french <- read.delim("fn.txt",sep="\t", header=TRUE)[,1:7]

#########################################
#           Grab Text Only              #
#########################################

fr <- VectorSource(french$headline)
system.time(
    french_head <- Corpus(fr,readerControl=list(language="french"))
)

french_corp <- french_head[1:10000]
french_corp <- tm_map(french_corp,stripWhitespace)
french_corp <- tm_map(french_corp,tolower)

system.time(
    french_corp <- tm_map(french_corp, removeWords,stopwords("french"))
)
#Had problems here within CSDE-Terminal Server calling rJava and stuff
system.time(
    tm_map(french_corp,stemDocument)
)
#Create Document Term Matrix : Can probably send to C or Python
system.time(
    french_dtm <- DocumentTermMatrix(french_corp)
)
french_dtm2 <- removeSparseTerms(french_dtm,.9998) # this is really critical here to get this right

#######################################################
#           Create Train and Test Sets                #
#######################################################

train <- french_dtm2[1:8000]
test <- french_dtm2[8001:10000]

#Convert to matrix; sparse matrix
train3 <- as.matrix(train, "matrix.csr")
test3 <- as.matrix(test, "matrix.csr")

#####################################################
#               MAJOR TOPIC CODES                   #
#####################################################

code <- as.factor(french$Codegros)[1:10000]
train_code <- code[1:8000]
test_code <- code[8001:10000]

#####################################################
#                 RUN MODEL -- SVM                  #
#####################################################

system.time(
model1 <- svm(x=train3, y=train_code, method="C-classification", cross=5)
)
#722.16 Elapsed Seconds!!
#1145.92 Elapsed Seconds! with 80-20 split
#May need to do a regular expression on the matrix names and change them to get rid of X. for the few of them

summary(model1)
pred <- fitted(model1)
table(pred,train_code)

#from Venables and Ripley (2002)
table(true=train_code, predicted = predict(model1))

#Analyze output and TEST
system.time(
predtest <- predict(model1,test3)
)

classAgreement(table(test_code,predtest),match.names=F) # Check accuracy, etc.
#OK -- it worked, now just need to figure out how to improve the accuracy via tuning and such

###########################################################
#                       More Learners                     #
###########################################################
#Need to investigate further

#Need to create dataframe for RWeka libraries
train_data <- data.frame(cbind(train_code, train3))

library(RWeka) # Requires Java for R 32; most computers should have this

###################################################
#                   Boosting                      #
###################################################

#Boosting M1, J48 Formula
#IMPORTANT!! NEED TO ELIMINATE ANY NON-ALPHANUMERIC COLUMNS, E.G. NO COLUMNS NAMES "..."
#WILL PROBABLY WRITE A FUNCTION TO DEAL WITH THIS IF WE END UP GOING THE ROUTE OF 
#MATRIX CREATION IN R
m1 <- AdaBoostM1(as.factor(train_code)~ ., data = train_data,
        control = Weka_control(W = "J48"))
    
aboost_pred <- predict(m1,data.frame(test3)) #Predict Code
confusion <- table(test_code,aboost_pred)
sum(diag(confusion))/length(aboost_pred) #Diagonal accuracy


###################################################
#                   Bagging                       #
###################################################

#Bagging = Bootstrap Averaging

bagging_mod <- Bagging(as.factor(train_code)~.,data=data.frame(train_data),
                    control=Weka_control(W="J48"))
bagging_pred <- predict(bagging_mod,data.frame(test3))

confusion <- table(test_code,bagging_pred)
sum(diag(confusion))/length(bagging_pred) #diagonal accuracy


###################################################
#                  Random Forests                 #
###################################################

library(randomForest)

#Trying with Congress Text
rf_mod <- randomForest(x=train3, y=as.factor(train_code))
rf_pred <- predict(rf_mod,newdata=test3)

confusion <- table(test_code,rf_pred)
sum(diag(confusion))/length(rf_pred) 
