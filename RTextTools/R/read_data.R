read_data <-
function(filename, tablename=NULL, type=c("csv","tab","accdb","mdb"), ...) {
    Call <- match.call()
    index <- match(c("filename", "tablename", "type"), names(Call), nomatch = 0)
    if (index[1] == 0) stop("Please specify the path to a CSV, tab-delimited file, or Access database.")
    if (index[3] == 0) stop("Please specify the type of file used for input. Choices are: 'csv', 'tab', 'accdb', or 'mdb'.")

    # CONDITIONAL INPUT OF DATA
    if (type=="csv") { rawData <- read.csv(filename,header=T) # CSV FILES
    } else if (type=="tab") { rawData <- read.delim(filename,header=TRUE, sep="\t") # TAB-DELIMITED FILES
	} else { rawData <- read_access(filename, tablename, path=".", adb_vers=2007) } # ACCESS DATABASES
	
    return(rawData)
}