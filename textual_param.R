##########################################################
#                   textual_param                        #
##########################################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools
#This function trims the text, including whitespace removal, lowercase, and removing stopwords

#Must include packages: tm, Snowball
library(tm); library(Snowball)

#Arguments
#textual_vect = character vector from dataframe or wherever
#language =  language you are working with: "english","french", "spanish", etc.
#whitespace = TRUE/FALSE Whitespace removal
#lowercase = TRUE/FALSE Convert all text to lowercase
#removestopwords =TRUE/FALSE Remove stopwords in text

#Values
#Cleaned up corpus of text. Everything except stemming, which has to be mapped separately.

textual_param <- function(textual_vect, language, whitespace=TRUE, lowercase=TRUE,
                          removestopwords=TRUE, ...) {
    #Read in initial character vector
    corp <- Corpus(VectorSource(textual_vect), readerControl=list(language=language))

    #Strip Whitespace
    if (whitespace)
        corp <- tm_map(corp,stripWhitespace)
    #Lowercase
    if (lowercase)
        corp <- tm_map(corp,tolower)
    #Remove Stopwords        
    if (removestopwords)
        corp <- tm_map(corp, removeWords,stopwords(language))
    return(corp)
}

#Function Execution

#textparam_out <- textual_param(textual_vect=fr2$text, language="french")
#For some reason Word Stemming (mapping) won't work inside the function when it 
#goes to return the object; probably because it's mapping something an not an object. Tim?
#tm_map(textparam_out, stemDocument)
