RSA_groups_compute_stat <- function(X, Y, ngroup, flag){
	
	N <- nrow(X)
	M <- ncol(X)
	
	Y_sort <- sort(Y)
	ord <- order(Y)
	
	# Define indices for splitting inputs into ngroup:
	
	split <- seq(0, N, by = floor(N / ngroup))
	
	idx <- numeric(N)


	for (i in 1:ngroup){
		idx[ord[(split[i] + 1):split[i + 1]]] <- i
		}
	
	mvd <- matrix(nrow = ngroup * (ngroup - 1) / 2 , ncol = M) # Vector of distance between the CDFs

for(i in 1:M){
    
    # Approximate CDF of the i-th parameter for each group:
     
    xxi <- unique(sort(X[, i]))
    CDF_ <- sapply(1:ngroup, function(h)  ecdf(X[idx == h, i])(xxi))         
     
    mvd[,i] <- c(dist(t(CDF_), "maximum"))

}

if (flag == 1){
    		stat <- apply(mvd, 2, median)
    } else { #if flag==2
    		stat <- apply(mvd, 2, max)
    }

return(stat)

}