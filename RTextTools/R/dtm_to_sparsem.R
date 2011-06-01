dtm_to_sparsem <- function(dtm) {
	i <- dtm$i;
	new_i <- c(1);
	old_i <- 1;
	total <- 1;
	count <- 0;
	seq <- 1;
	
	for (curr_i in i) {
		if (curr_i == old_i && curr_i == seq) {
			count <- count + 1;
		} else {
			if (count == 1) {
				new_i <- append(new_i,total);
				count <- 2;
			} else {
				total <- total + count;
				new_i <- append(new_i,total);
				count <- 1;
			}
			seq <- seq + 1;
			old_i <- curr_i;
		}
	}
	
	new_i <- append(new_i,length(i)+1);
	sparsem <- new("matrix.csr",ra=as.numeric(dtm$v),ja=dtm$j,ia=as.integer(new_i),dimension=dim(dtm));
	
	return(sparsem);
}