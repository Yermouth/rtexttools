text_to_df <-
function(directory,controlfile, identifier) {
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

