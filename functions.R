#################################################
#Loren Collingwood                              #
#Started: 2/26/2011; 2/27/2011                  #
#Reading in various types of files              #
#Rtexttools code                                #
#Project with: Wouter Van Atteveldt, Tim Jurka, #
#              Amber Boydstun, Emiliano Grossman#
#################################################
s
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
install.packages("openNLPmodels.en")
install.packages("openNLPmodels.es")


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
