new_maxent <-
function(heldout=0) {
	maxent$new_model();
	maxent$set_heldout(heldout);
	assign("maxent_trained",FALSE,envir=globalenv());
}

