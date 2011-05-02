multi_text <-
function(column1, column2, column3=NULL, column4=NULL, length=2) {

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

