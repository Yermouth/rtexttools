###############################################
#               multi_text                    #
###############################################
#Loren Collingwood, 3/6/2011 Alpha Version Rtexttools
#Ability to use two - four columns of text. Execute prior to DTM creation

#Arguments
#column1 = character vector of text
#column2 = character vector of text    
#column3 = character vector of text
#column4 = character vector of text
#length = number, how many columns the analyst is concatenating

#Values
#Returned vector of concatenated text.

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
#fr2 <- da[1:1000,] # minimize the cases so most computers can do it.
#headline <- fr2$headline #headline column from French dataset
#textcolumn <- fr2$text   #text column from French dataset
#Two Columns    
#cat2 <- multi_text(headline,textcolumn)
#Three Columns
#cat3 <- multi_text(headline,textcolumn,textcolumn, length=3)
#Four Columns
#cat4 <- multi_text(headline,textcolumn,textcolumn,headline,length=4)
