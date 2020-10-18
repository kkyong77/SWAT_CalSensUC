#' Plotting function for Regional Sensitivity Analysis with grouping
#'
#' Plotting function for Regional Sensitivity Analysis with grouping. Plot \code{Ng} CDFs of the samples in \code{X} with different colours.

#'
#' @param X matrix \code{(N, M)} set of input samples
#' @param idx vector \code{(N)} index of group to which input samples belong
#' @param Yk vector \code{(Ng + 1)} range of \code{Y} in each group 
#' @param n_col scalar number of panels per row in the plot (default: \code{min(5, M)})
#' @param col color of the lines in the plot (default \code{rainbow(Ng)}) 
#' @param labels vector \code{(M)} labels for the horizontal axis (default: \code{c("X1", "X2",...)})
#' @param ... other

#' @seealso \code{\link{RSA_indices_groups}} \code{\link{RSA_plot_thres}}

#' @export

#' @examples

#' # See the demo
#' # demo("workflow_rsa_hymod")

RSA_plot_groups <- function(X, idx, Yk, n_col = 5, labels = NULL, col = rainbow(1:Ng), ...) {

	 stopifnot(is.matrix(X), is.numeric(X),
	 is.numeric(Yk),
	 length(idx) == nrow(X),
	 is.scalar(n_col), n_col >=0, n_col == floor(n_col))
	 
	 Ng <- length(Yk) - 1
	 
	 N <- nrow(X)
	 M <- ncol(X)
	 
	 if(!is.null(labels)){
	 	stopifnot(length(labels) == M)
	 } else {
	 	labels <- paste("X", seq(1, M), sep ="")
	 }
	 
	n_col <- min(floor(n_col), M)
	n_row <- ceiling(M / n_col)
	par(mfrow = c(n_row, n_col))
	
	for(i in 1:M){
		
		  xxi <- unique(sort(X[, i]))
		  CDFj <- sapply(1:Ng, function(h)  ecdf(X[idx == h, i])(xxi))     
		    
		matplot(xxi, CDFj, type ="l", xlab = labels[i], ylab = "cdf", col = rev(rainbow(Ng + 1, end = 5 / 6)))
    
     legend('bottomright', legend = round(Yk, 2), lwd = 2, col = rev(rainbow(Ng + 1, end = 5 / 6)))
		    
    }


	par(mfrow = c(1, 1))
}
