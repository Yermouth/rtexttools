# FILE: RTextTools_InstallDependencies_v1.0b
# AUTHOR(s): Timothy Jurka
# DATE: May 2011
# DESCRIPTION: Script to install dependencies for RTextTools.

required_packages <- c('openNLP','SparseM','RWeka','randomForest','glmnet','tree','nnet','tm','Snowball','e1071','Rcpp','RcppClassic');
for (package in required_packages) {
	package_available <- require(package, quietly=TRUE, character.only=TRUE);
	if (!package_available) {
		install.packages(package);
		require(package, quietly=TRUE, character.only=TRUE);
	}
}

suggested_packages <- c('maxent','RODBC');
for (package in suggested_packages) {
	package_available <- require(package, quietly=TRUE, character.only=TRUE);
	if (!package_available) {
		print(paste("Suggested package",package,"not installed. Manually install package to gain additional functionality."));
	}
}