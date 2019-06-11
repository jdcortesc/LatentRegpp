expand <- function(tab,ncatg) {
  Y <- matrix(0, nrow(tab), ncatg*ncol(tab))
  for(i in 1:nrow(tab)){
    moda <- tab[i,]
    n <- length(moda)
    x <- matrix(0, n, ncatg)
    x[(1:n) + n * (moda - 1)] <- 1
    x <- t(x)
    x <- as.vector(x)
    Y[i,] <- x 
  }
  return(Y)
} 
