#' Regional Sensitivity Analysis (with threshold)
#'
#' Computation function for Regional Sensitivity Analysis (with threshold). It splits the samples in a dataset \code{X} into two datasets (\code{Xb} and \code{Xnb}) depending on whether the associated sample in Y satisfies the condition:\eqn{Y(i, j) < threshold(j)}     for \eqn{j = 1,...,P}. Then assess the distance between the CDFs of \code{Xb} and \code{Xnb} by a suitable measure (maximum vertical distance, area between curves, etc.). Use the function \code{\link{RSA_plot_thres}} to visualize results.
#'
#' @param X matrix \code{(N, M)} set of inputs samples
#' @param Y matrix \code{(N, P)} set of output samples
#' @param threshold vector \code{(P)} threshold for output values. Default value: \code{threshold = median(Y)}  
#' @param flag scalar specify the statistic to assess the distance between CDFs                                   \code{flag = 1}: \code{stat} maximum vertical distance (see \code{mvd} below), \code{flag = 2}: \code{stat} area between curves (see \code{spread} below) \code{flag = 3}: \code{stat} input range reduction (see \code{irr} below) Default value: \code{flag = 1}
#' @param Nboot scalar, number of resamples used for boostrapping. Default \code{Nboot = 0}, i.e. no bootstrapping.
#' @param alfa scalar significance level for the confidence intervals estimated by bootstrapping. Default: \code{0.05}

#' @return List containing: 
#' \itemize{
#'   \item \code{stat} vector \code{(M)} distance measure between CDFs for each input
#'   \item \code{idxb} vector \code{N} indices of samples statisfying the condition.  
#'}
#' You can easily derive the two datasets \code{Xb} and \code{Xnb} as: \code{Xb  = X[idxb, ]}, \code{Xnb = X[!idxb, ]}. 
#' If \code{Nboot > 1} it also contains
#' \itemize{ 
#'   \item \code{stat_lb} vector \code{(M)} lower bound of vector \code{(stat)} from bootstrapping
#'   \item \code{stat_ub} vector \code{(M)} upper bound of \code{stat} from bootstrapping vector
#'}

#' @seealso \code{\link{RSA_plot_thres}} \code{\link{RSA_convergence_thres}}

#' @export

#' @examples

#' # See the demo
#' # demo("workflow_rsa_hymod")

RSA_indices_thres <- function(X, Y, threshold = NULL, flag = 1, Nboot = 0, alfa = 0.05) {

##############
# Check inputs
##############

 stopifnot(is.matrix(X), is.numeric(X),
 flag %in% 1L:3,
is.scalar(Nboot), Nboot >=0, Nboot == floor(Nboot),
is.numeric(alfa), alfa <= 1, alfa >= 0)

N <- nrow(X)
M <- ncol(X)
 
 if(!is.matrix(Y)) Y <- matrix(Y, ncol = 1)
 
 stopifnot(is.numeric(Y), nrow(Y) == N)

P <- ncol(Y)


###########################
# Recover and check optional inputs
###########################

# Set optional arguments to their default values:

if(is.null(threshold)) threshold <- apply(Y, 2, median)
stopifnot(is.numeric(threshold),  length(threshold) == P)

###########################
# RSA
###########################

if(Nboot > 0){
	
	     stat_j <- matrix(nrow = Nboot, ncol =M) 
         idxb <- matrix(nrow = N, ncol = Nboot)
	
	for (n in 1:Nboot){
		B <- sample.int(N, replace = TRUE)
		Xi <- X[B,]
        Yi <- Y[B,]
        
        ci <- compute_indices(Xi, Yi, threshold, flag) 
        
        stat_j[n,] <- ci$stat
        idxb[B, n] <- ci$idxb
		
		}

        # # # # Notice that RSA may return a vector of NaNs if the bootstrap
        # # # # resample Yi is such that the threshold condition is never
        # # # # satisfied (or always satisfied).
        # # # # Therefore, in the following calculations, we will make sure that
        # # # # any row of NaNs in 'stat_j' be excluded (and display a warning
        # # # # message).
        stat      <- colMeans(stat_j, na.rm = TRUE)
        stat_std  <- apply(stat_j, 1, sd, na.rm = TRUE)
        idx <- rowSums(is.na(stat_j)) == 0
        
        if( sum(idx) < Nboot){
            warning( sprintf("Statistics were computed using %d bootstrap resamples instead of %d", sum(idx), Nboot))
            }

        stat_sorted <- apply(stat_j[idx,], 2, sort)
        stat_lb <- stat_sorted[max(1, round(sum(idx) * alfa / 2)),]
        stat_ub <- stat_sorted[max(1, round(sum(idx) * (1 - alfa / 2))),]
        
        robj <- list(stat = stat, idxb = idxb, stat_lb = stat_lb, stat_ub = stat_ub)
} else {
	robj <- compute_indices(X, Y, threshold,flag)
	robj <- list(stat = robj$stat, idxb = robj$idxb)
}

return(robj)
}