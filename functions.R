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
#install.packages("RODBC")
#library(Rexttools)

options(stringsAsFactors=FALSE) # Be sure to set each time or at least in some of the functions

#########################################
#                datagrab               #
#########################################
#Function to be called from datain; this function brings in an Access file
#requires ROBDC package, 
#internal function -- hidden from user

datagrab <- function(accessdata, tablename, path = ".", adb_vers = 2007, ...) 
{
    Call <- match.call()
    indx <- match(c("accessdata", "tablename", "path"), names(Call), 
        nomatch = 0)
    if (indx[1] == 0) 
        stop("An Access database must be used")
    if (indx[2] == 0) 
        stop("Must Supply a table name")
    oldwd <- getwd()
    setwd(path)
    if (adb_vers == "2007") 
        channel <- odbcConnectAccess2007(accessdata, rows_at_time = 1)
    else channel <- odbcConnectAccess(accessdata, rows_at_time = 1)
    dataframe <- sqlFetch(channel, tablename)
    close(channel)
    setwd(oldwd)
    dataframe
}

###############################################################
#                          datainput                          #
###############################################################
#This function brings in CSV file, Text File, or Access
#filename = name of the file you are reading in, character string
#tablename = Access table name, only for Access call
#type -- one of four data types the user is reading in.

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

system.time(
truncated_words <- word_trunc(da$headline,wordtrunc=4)
)
#French dataset. 10,183 observations
#   user  system elapsed 
# 541.19   20.11  569.70 

###############################################
#               multi_text                    #
###############################################
#Ability to use two - four columns of text
#column1 = character vector of text
#column2 = character vector of text    
#column3 = character vector of text
#column4 = character vector of text
#length = number, how many columns the analyst is concatenating
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

#Paste same columns next to each other
fr2 <- french[1:1000,]
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

#Must include packages: tm, Snowball
#This function trims the text, including whitespace removal, lowercase, and removing stopwords

#textual_vect = character vector from dataframe or wherever
#language =  language you are working with: "english","french", "spanish", etc.
#whitespace = TRUE/FALSE Whitespace removal
#lowercase = TRUE/FALSE Convert all text to lowercase
#removestopwords =TRUE/FALSE Remove stopwords in text
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

textparam_out <- textual_param(textual_vect=fr2$text, language="french")
#For some reason Word Stemming (mapping) won't work inside the function when it 
#goes to return the object; probably because it's mapping something an not an object. Tim?
tm_map(textparam_out, stemDocument)

################################################
#                   dtm                        #
################################################
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
#This function takes a document term matrix and produces the appropriate 
#training matrixes and labeling vectors. Requires SparseM package for sparse matrix
#production. Function assumes all data are in a standard n*k dataframe (note: this may change).
#Output is a list.
#dtm -- document term matrix as produced by dtm or some other way
#label -- the label/code category of the text for training and testing if not on virgin text, or training if on virgin text
#trainsize -- a sequence (i.e., 1:800) of text itmes used for training
#testsize -- a sequence (i.e., 801-1000) of text items used for test evaluation
library(SparseM)

traintest <- function(dtm,label,trainsize,testsize) {
    train <- dtm[trainsize]
    test <- dtm[testsize]

    train_code <- as.factor(label[trainsize])
    test_code <- as.factor(label[testsize])
    
    #Need non-sparse matrix for prediction
    matrix_train_predict <- as.matrix(train, "matrix.csr")
    matrix_test_predict <- as.matrix(test, "matrix.csr")
    
    #Need sparse matrix for algorithm    
    matrix_train_sparse <- as.matrix.csr(matrix_train_predict)
    matrix_test_sparse <- as.matrix.csr(matrix_test_predict)
        
    #return a list of these four items
    return(list(trainmat=matrix_train_sparse, testmat=matrix_test_sparse,
                trainpredict=matrix_train_predict, testpredict=matrix_test_predict,
                train_code=train_code, test_code=test_code))
}

train_test <- traintest(dtm=french_dtm,label=da$Codegros,trainsize=1:800, testsize=801:1000)
#Look at the names of the list items.
names(train_test)
########################################################
#Run a model from SVM
########################################################

#now need to add in something regarding the code category

library("e1071") #svm and other algorithms

#SVM
#Train
model1 <- svm(x=train_test$trainpredict, y=train_test$train_code, method="C-classification", cross=5)
summary(model1) # not good because case size is very small so I could run on my computer

#Classify/ Predict
predtest <- predict(model1,train_test$testpredict)

#Check accuracy
classAgreement(table(train_test$test_code,predtest),match.names=F) 


###################################################################
#                           accuracy                              #
###################################################################
#Purpose: To check how well the test dataset does against the pre-existing codes
#Assumes new codes have been put into a dataframe and appended onto the earlier dataframe.
#For example, look at the congres dataset below. This is a subset of 
#U.S. congressional bills that have been classified

#prelabel -- the handcode/pre-existing label for the dataset, must be in "character" format
#columnames -- character vector of columnames of the algorithms in the dataset.
#data -- the dataframe from which the labels are stored. So the original data + appended columns

setwd("C:\\Users\\Loren\\Documents\\My Dropbox\\RTextTools\\Datasets")

congress <- datainput("congress.csv", type="csv", header=TRUE) # use datainput function from above

accuracy <- function(prelabel,columnames,data) {

    Call <- match.call()
    indx <- match(c("prelabel", "columnames", "data"), names(Call), 
        nomatch = 0)
    if (indx[1] == 0) 
        stop("A numeric vector must be included")
    if (indx[2] == 0) 
        stop("A character vector must be included")    
    if (indx[3] == 0) 
        stop("A data frame must be included")    

    dat <- data
    #Extract just the columns you want from the data
    datmat <- as.matrix(dat[match(c(prelabel,columnames), colnames(dat))])
    out <- NULL
    out2 <- NULL
    #use classAgreement function to extract diag scores, kappa, rand index, and rand index corrected
    #Note, first column of datmat is "Pre-existing code", that's why we skip it here
    for (i in 2:ncol(datmat)) {
        a <- table(datmat[,1],datmat[,i])
        out[[i]]<- classAgreement(a) #use the classAgreemnt function from e1071 package
        out2[[i]] <- c(out[[i]]$diag,out[[i]]$kappa, out[[i]]$rand, out[[i]]$crand)
    }

    #Rbind the rows together with a loop
    finalout <- out2[[2]] # note, first list item is blank (because we skipped it)
    for (i in 3:length(out2)){
        finalrotate <- rbind(finalout, out2[[i]])
        finalout <- finalrotate
    }
    colnames(finalout) <- c("Diag_Predict", "Kappa_Diag", "Rand_Index", "Rand_Index_Cor")
    rownames(finalout) <- columnames
    
    return(round(finalout,3))
}

accuracyout <- accuracy(prelabel="major", columnames=c("new_code_naive","new_code_maxent", "new_code_ling", "new_code_svm"), data=congress)
accuracyout
library(xtable) # for LaTeX code
xtable(accuracyout)
#If you want to extract the columns you need to turn into a dataframe.



#Probably will want to have another column with some sort of confidence prediction. 
#So either Xvalidation scores, probability scores. This is not possible for every algorithm however. 
#Only the algorithms where this is available.


#Next Steps
#May want to create a confusion matrix function for Diagnostic Purposes

#append the column of scores to the dataset and then write that dataset out
