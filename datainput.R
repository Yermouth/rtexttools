###############################################################
#                          datainput                          #
###############################################################
#Loren Collingwood , 3/6/2011 Alpha Version Rtexttools

#This function brings in CSV file, Text File, or Access
#Arguments
#filename = name of the file you are reading in, character string
#tablename = Access table name, only for Access call
#type -- one of four data types the user is reading in.

#Values
#Returned dataframe

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
#da <- datainput("French_news1_csv.csv", type="csv")
#Tab Delimited
#db <- datainput("French_news1_tab.txt",type="tab")
#Access mdb or accdb
#dc <- datainput("nyt_front_page_1996_2006_Scott_Recodes_Fall2010_decile01.mdb", tablename="Issues", type="mdb")
