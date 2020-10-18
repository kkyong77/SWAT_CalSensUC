#' Regional Sensitivity Analysis (with grouping)
#'
#' Computation function for Regional Sensitivity Analysis (with grouping). It splits the samples in a dataset X into \code{ngroups} sub-sets corresponding to \code{ngroup} equally spaced values of \code{Y} (as first proposed by Wagener et al., 2001).
#' Then assess the distance between the CDFs of \code{X} in the different datasets by a statistic (maximum or median) of the maximum vertical distance between the CDFs, i.e.
#'
#'    \eqn{stat = max( max_x( | Fi(x) - Fj(x) | ) )}
#'
#' or
#'
#'\eqn{stat = median( max_x( | Fi(x) - Fj(x) | ) )}
#'
#' where \eqn{Fi()} is the CDF of \code{X} in i-th dataset and \eqn{Fj()} is the CDF in the
#' j-th dataset.

#'
#' @param X matrix \code{(N, M)} set of inputs samples
#' @param Y matrix \code{(N, P)} set of output samples
#' @param ngroup number of groups considered (default: 10)   
#' @param flag scalar, 1 or 2. Statistic for the definition of the RSA index. \code{flag = 1}: median (default), \code{flag = 2}: maximum (see 'spread' below)
#' @param Nboot scalar, number of resamples used for boostrapping. Default \code{Nboot = 0}, i.e. no bootstrapping.
#' @param alfa scalar significance level for the confidence intervals estimated by bootstrapping. Default: \code{0.05}

#' @return List containing: 
#' \itemize{
#'   \item \code{stat} vector \code{(M)} distance measure between CDFs for each input
#'   \item \code{idxb} vector \code{N} respective group of the samples
#'   \item \code{Yk} vector \code{ngroup + 1} range of \code{Y} in each group
#'}
#' You can easily derive the n_groups datasets \code{Xi} as: \code{Xi = X[idx == i]}
#'
#' If \code{Nboot > 1} it also contains
#' \itemize{ 
#'   \item \code{stat_lb} vector \code{(M)} lower bound of vector \code{(stat)} from bootstrapping
#'   \item \code{stat_ub}  vector \code{(M)} upper bound of \code{stat} from bootstrapping
#'}

#' @references Wagener, T., Boyle, D. P., Lees, M. J., Wheater, H. S., Gupta, H. V., and Sorooshian, S. (2001): A framework for development and application of hydrological models, Hydrol. Earth Syst. Sci., 5, 13-26.

#' @seealso \code{\link{RSA_plot_groups}} \code{\link{RSA_indices_thres}}

#' @export

#' @examples

#' # See the demo
#' # demo("workflow_rsa_hymod")

RSA_indices_groups <- function(X, Y, ngroup = 10, flag = 1, Nboot = 0, alfa = 0.05) {

##############
# Check inputs
##############

 stopifnot(is.matrix(X), is.numeric(X),
 is.scalar(ngroup), ngroup > 1, ngroup == floor(ngroup),
 flag %in% 1L:2,
is.scalar(Nboot), Nboot >=0, Nboot == floor(Nboot),
is.numeric(alfa), alfa <= 1, alfa >= 0)

N <- nrow(X)
M <- ncol(X)
 
 if(!is.matrix(Y)) Y <- matrix(Y, ncol = 1)
 
 stopifnot(is.numeric(Y), nrow(Y) == N)

########################
# RSA_indices_groups
########################

# Compute idx and Yk for the entire sample

# Arrange inputs and output according to ascending output values
	Y_sort <- sort(Y)
	ord <- order(Y)

# Define indices for splitting inputs into ngroup:
	split <- seq(0, N, by = floor(N / ngroup))

	idx <- numeric(N)
	
	for ( i in 1:ngroup){
		idx[ord[(split[i] + 1):split[i + 1]]] <- i
		}

	Yk <- c(Y_sort[split[-length(split)] + 1], Y_sort[N])

if (Nboot > 1){

    B <- matrix(sample.int(N, N * Nboot, replace = TRUE), N, Nboot)
    
    stat_n <- apply(B, 2, function(h)  RSA_groups_compute_stat(X[h,], Y[h], ngroup, flag))
    
    stat <- rowMeans(stat_n,  na.rm = TRUE)
    stat_sd  <- apply(stat_n, 1, sd, na.rm = TRUE)
    stat_sorted <- apply(stat_n, 1, sort) 
    stat_lb <-  stat_sorted[max(1, round(Nboot * alfa / 2)),]
    stat_ub <- stat_sorted[max(1, round(Nboot * (1 - alfa / 2))),]
    
    robj <- list(stat = stat, idx = idx, Yk = Yk, stat_lb = stat_lb, stat_ub = stat_ub)
    
    
  } else {
  	
        stat <- RSA_groups_compute_stat(X, Y, ngroup, flag)
        
       robj <- list(stat = stat, idx = idx, Yk = Yk)
        
       }
       
return(robj)
}