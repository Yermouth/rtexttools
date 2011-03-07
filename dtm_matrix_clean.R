#################################################
#               dtm_matrix_clean                #
#################################################
#Loren Collingwood, 3/5/2011, Alpha version
#Gets rid of "...", --azgt, my@424, bd..ddxc type columns. These columns screw up some of the algorithms
#dtm_matrix -- the document term matrix that has been changed to a matrix

dtm_matrix_clean <- function(dtm_matrix) {
    out <- dtm_matrix[,grep('^[A-Za-z]+$',colnames(dtm_matrix))]
    return(out)
}
