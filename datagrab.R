#########################################
#                datagrab               #
#########################################
#Loren Collingwood 3/06/11 Alpha Version Rtexttools

#Function to be called from datain; this function brings in an Access file
#requires ROBDC package, 
#internal function -- hidden from user

#Arguments
#accessdata -- character name of Access database. 
#tablename -- character name of table in Access database.
#type -- character vector of Access type (i.e., "accdb", "mdb")

#Values
#Data frame returned
library(RODBC)
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
