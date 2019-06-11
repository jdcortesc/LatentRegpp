#funcion que organiza los datos por patrones:
patrones=function(datos){
  pats <- apply(datos, 1, paste, collapse = "/")
  freqs <- table(pats) 
  nfreqs <- length(freqs)
  X <- unlist(strsplit(cbind(names(freqs)), "/"))
  X <- matrix(as.numeric(X), nfreqs, ncol(datos), TRUE)
  return(list("data"=X,"freqs"=as.vector(freqs)))
}

