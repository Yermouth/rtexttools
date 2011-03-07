###########################################################
#                     confusion_create                    #
###########################################################
#Loren Collingwood, 3/5/2011, Alpha version
#Function creates a confusion matrix. Used internally in 
#accuracy function
#tr -- vector of "true" codes
#pr -- vector of "predicted" codes
confusion_create <- function(tr,pr) {
        table_vars <- table(as.numeric(tr),as.numeric(pr))
        
        if (ncol(table_vars)!=nrow(table_vars)) { #If not all possible categories are predicted, add columns of 0s so diagonals line up.
            dat <- data.frame(tr, pr)
        
            class <- c(1:length(tr))
            m <- max(c(length(unique(tr)), length(unique(pr))))
            for(j in 1:length(class)) 
            {
                class[j] <- sub(' ','',paste(dat[j,1],dat[j,2])) 
            }
        
            dat <- data.frame(dat, class)
            mat <- matrix(0, nrow=m, ncol=m)
            for (k in 1:m)
            {
                for (l in 1:m)
                {
                    mat[k,l] <- sub(' ','',paste(k,l))
                }
            }
        
            cat <- matrix(0, nrow=(m), ncol=(m))
        
           for (p in 1:m)
            {
                for(q in 1:m)
                {
                    cat[p,q]<- nrow(dat[dat$class==mat[p,q],])
                }
            }
        
            colnames(cat) <- rownames(table_vars)
            rownames(cat) <- rownames(table_vars)
        } #end if branch
        else { #if all categories are predicted
            cat <- table_vars
        } 

    return(cat)
}
#confusion_create(allcodes,allcodes)
