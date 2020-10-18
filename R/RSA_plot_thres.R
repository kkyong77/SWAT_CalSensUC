#' Plotting function for Regional Sensitivity Analysis
#'
#' This function plots Regional Sensitivity Analysis
#'
#' @param X matrix \code{(N, M)} set of input samples
#' @param idxb vector \code{(N)} indices of samples statisfying the condition
#' @param n_col scalar number of panels per row in the plot (default: \code{min(5,M)})
#' @param labels vector \code{(M)} labels for the horizontal axis (default: \code{c("#1", "#2",...)})
#' @param str_legend vector (2) text for legend (default: no legend)

#' @seealso \code{\link{RSA_indices_thres}} \code{\link{RSA_convergence_thres}}

#' @export

#' @examples

#' # See the demo
#' # demo("workflow_rsa_hymod")

RSA_plot_thres <- function(X, idxb, n_col = 5,labels = NULL, str_legend = NULL) {

	 stopifnot(is.matrix(X), is.numeric(X),
	 is.logical(idxb), length(idxb) == nrow(X),
	 is.scalar(n_col), n_col >=0, n_col == floor(n_col))
	 
	 N <- nrow(X)
	 M <- ncol(X)
	 
	 if(!is.null(labels)){
	 	stopifnot(length(labels) == M)
	 } else {
	 	labels <- paste("#", seq(1, M), sep ="")
	 }
	 
	# Define below and above subsamples:
	Xb  <- X[idxb,] 
	Xnb <- X[!idxb,]
	
	n_col <- min(floor(n_col), M)
	n_row <- ceiling(M / n_col)
	par(mfrow = c(n_row, n_col))

	for(i in 1:M){

		aCDF <- approximatecdfpair(Xb[,i], Xnb[,i])
		CDFb <- aCDF$CDF1
		CDFnb <- aCDF$CDF2
		xx <- aCDF$x
		
		plot(xx, CDFb, lwd = 2, type ="l", xlab = labels[i], ylab = "cdf")
		lines(xx, CDFnb, col = "gray", lwd = 2)	
		
		if(!is.null(str_legend)) legend("bottomleft", str_legend, col = c("black", "gray"), lwd = 2)
		
		}

	par(mfrow = c(1, 1))
}
