#' Regional Sensitivity Analysis (with threshold)
#'
#' This function computes and plots the sensitivity indices obtained by \code{\link{RSA_indices_thres}} using an increasing number of output samples.
#'
#' @param X matrix \code{(N, M)} set of inputs samples
#' @param Y matrix \code{(N, P)} set of output samples
#' @param NN vector \code{(R)} of subsample sizes at which indices will be estimated (\code{max(NN)} must not exceed \code{N})
#' @param threshold vector \code{(P)} threshold for output values. Default value: \code{threshold = median(Y)}   
#' @param flag scalar specify the statistic to assess the distance between CDFs                                   \code{flag = 1}: \code{stat} maximum vertical distance (see \code{mvd} below), \code{flag = 2}: \code{stat} area between curves (see \code{spread} below) \code{flag = 3}: \code{stat} input range reduction (see \code{irr} below) Default value: \code{flag = 1}
#' @param Nboot scalar, number of resamples used for boostrapping. Default \code{Nboot = 0}, i.e. no bootstrapping.
#' @param alfa scalar significance level for the confidence intervals estimated by bootstrapping. Default: \code{0.05}

#' @return List containing: 
#' \itemize{
#'   \item \code{stat} distance measure between CDFs for each input vector \code{(M)}. 
#' }
#' If \code{Nboot > 1} it also contains
#' \itemize{ 
#'   \item \code{stat_lb} lower bound of vector \code{(stat)} from bootstrapping vector \code{(M)}
#'   \item \code{stat_ub} upper bound of \code{stat} from bootstrapping vector \code{(M)}
#'}

#' @seealso \code{\link{RSA_plot_thres}} \code{\link{RSA_indices_thres}}

#' @export

#' @examples

#' # See the demo
#' # demo("workflow_rsa_hymod")


RSA_convergence_thres <- function(X, Y, NN, threshold = NULL, flag = 1, Nboot = 0, alfa = 0.05) {

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

stopifnot(NN >= 0, NN - floor(NN) < .1^6, diff(NN) > 0, max(NN) <= N)

###########################
# Recover and check optional inputs
###########################

# Set optional arguments to their default values:

if(is.null(threshold)) threshold <- apply(Y, 2, median)
stopifnot(is.numeric(threshold),  length(threshold) == P)

###########################
# Compute indices
###########################

R1 <- length(NN)

R <- ifelse(max(NN) == N, R1 - 1, R1)

stat <- matrix(nrow = R1, ncol = M)
stat_lb <- matrix(nrow = R1, ncol = M)
stat_ub <- matrix(nrow = R1, ncol = M)

for (j in 1:R){
    
    idx_new <- shrink.lhcube(X, NN[j], type = "indices")
    Xj <- X[idx_new, , drop = FALSE]
    Yj <- Y[idx_new, ]
    
    if(Nboot > 1) {
    	
       rsatr <- RSA_indices_thres(Xj,Yj,threshold,flag,Nboot,alfa)
        
        stat_lb[j,] <- rsatr$stat_lb
        stat_ub[j,] <- rsatr$stat_ub
        stat[j,] <- rsatr$stat
      
        
    } else {
        stat[j,] <- RSA_indices_thres(Xj,Yj,threshold,flag)$stat

    }

}

if(max(NN) == N){
	
	    if(Nboot > 1) {
    	
       rsatr <- RSA_indices_thres(X, Y, threshold,flag,Nboot,alfa)
        
        stat_lb[R1,] <- rsatr$stat_lb
        stat_ub[R1,] <- rsatr$stat_ub
        stat[R1,] <- rsatr$stat
      
        
    } else {
        stat[R1,] <- RSA_indices_thres(X,Y,threshold,flag)$stat

    }
	
}



if(Nboot > 1){
        robj <- list(stat = stat, stat_lb = stat_lb, stat_ub = stat_ub)
} else {
	robj <- list(stat = stat)
}

return(robj)
}