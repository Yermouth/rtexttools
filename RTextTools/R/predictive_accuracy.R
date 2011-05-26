predictive_accuracy <- function(test_codes, predict_codes) {
    
    a <- cbind(test_codes,predict_codes)
    identical_row <- function(vector) {
    	vec1 <- vector[1]
    	vec2 <- vector[2]
    	if (vec1 %in% vec2==FALSE) {
    		answer <- "FALSE"
    	}
		else {
			answer <- "TRUE"
		}
		return(answer)
	}
	out <- apply(a,1,identical_row)
	out2 <- table(out)[2]/sum(table(out))
	names(out2) <-"Pred. Accuracy"
	return(out2)
}
