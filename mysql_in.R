##################################################
#                   mysql_in                     #
##################################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools
#This function  brings in a dataframe from a MySQL database
#This is probably the least supported and most problematic way to bring in data.
#However, in the long run, I do recommend moving all data to MySQL.
#Arguments
#dbname -- name of the database. Character.
#user -- The MySQL user name. Character.
#password -- The MySQSL password. Character.
#host -- The MysQL host name. Character.
#tablename -- The table name of the dataframe accessed MySQL. Character.

#Values
#Dataframe object.
library(RMySQL) #sometimes has problems installing on Windows. Something to do with Namespaces and registry,
# had to use Mac

mysql_in <- function(dbname, user, password=NULL, host, tablename){
    
    drv <- dbDriver("MySQL")
    con <- dbConnect(drv, dbname=dbname, user=user, password=password, host=host)
    result <- dbGetQuery(con, paste('select * from',tablename,sep=" "))
    return(result)
}

#This grabs data from a phpMyadmin account
#senateshort <- mysql_in(dbname="cbp", user="lorenc2", password='MADEUPPASSWORD', 
#                 host='cbp.serverk.org', tablename='actorsSenate')
