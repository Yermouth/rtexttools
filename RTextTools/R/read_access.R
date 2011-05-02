read_access <- function(accessdata, tablename, path = ".", adb_vers = 2007, ...) 
{
    Call <- match.call()
    index <- match(c("accessdata", "tablename", "path"), names(Call), nomatch = 0)
    if (index[1] == 0) stop("Please specify a Microsoft Access database to be used.")
    if (index[2] == 0) stop("Please specify a table you'd like to retrieve from the database.")
    
	oldwd <- getwd()
    setwd(path)
	
    if (adb_vers == "2007") channel <- odbcConnectAccess2007(accessdata, rows_at_time = 1)
    else channel <- odbcConnectAccess(accessdata, rows_at_time = 1)
    
	dataframe <- sqlFetch(channel, tablename)
    close(channel)
    
	setwd(oldwd)
    
	return(dataframe)
}

