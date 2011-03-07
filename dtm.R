################################################
#                   dtm                        #
################################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools
#Document Term Matrix production with option for sparsity control of the matrix

#Arguments
#corpus -- a corpus of text, produced from textual_param and tm_map(corpus, stemDocument)
#sparsity -- a decimal number. default is .9998, but this can be adjusted (to say .98) if computer goes slow or freezes.

#Values
#A document term matrix.
dtm <- function(corpus, sparsity=.9998) {
    dtm <- DocumentTermMatrix(corpus)
    out <- removeSparseTerms(dtm,sparsity)
}

#french_dtm <- dtm(textparam_out,sparsity=.98) # this sparsity number will need to be toyed with, sometimes R will crap out 
                                              #in the next function becuase it's too big.
