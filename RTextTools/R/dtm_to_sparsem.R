dtm_to_sparsem <- function(dtm) {
	ia <- c(1);
	for (n in 1:dim(dtm)[1]) {
		el <- sum(dtm$i == n)+ia[length(ia)];
		ia <- append(ia,el);
	}
	
	sparsem <- new("matrix.csr",ra=as.numeric(dtm$v),ja=dtm$j,ia=as.integer(ia),dimension=dim(dtm));
	
	return(sparsem);
}