#####################################################
#                   text_to_df                      #
#####################################################
#Loren Collingwood, 3/6/2011 Alpha Version Rtexttools
#Read from multiple text files with a "Control" file
#Put all records in separate text files in a directory. All other files
#in that directory should be moved to another directory. Also need to have 
#a control file, a comma delimted file listing as rows the file name, the code
#of that file, and any other information. Unless absolutely necessary, I don't recommend this format.

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

#setwd("C:/Users/Loren/Documents/My Dropbox/RTextTools/Datasets/ReadMe")

#textdf <- text_to_df(directory="C:/Users/Loren/Documents/My Dropbox/RTextTools/Datasets/ReadMe",
#                    controlfile="C:/Users/Loren/Documents/My Dropbox/RTextTools/Datasets/ReadMe/controllist/control.txt",
#                    identifier="ROWID")
