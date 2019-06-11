prob_gpcm_M=function(betas.item,pt.cuad,item,ind1,ind2,ncatg){
  
  pr=matrix(NA,nrow=length(pt.cuad),ncol=length(c(ind1[item]:ind2[item])))
  
  for(i in 1:nrow(pr)){ 
  node=pt.cuad[i] 
  
    etas=
    betas.item[length(betas.item)]*(node-betas.item[1:(length(betas.item)-1)])
  
  cumsumetas=cumsum(etas)
  den=1+sum(exp(cumsumetas))
  pr[i,1]=1/den
  
    for(k in 2:(ncatg[item])){ 
      pr[i,k]=exp(etas[k-1])*pr[i,k-1]
    }
  }
  pr[pr<=sqrt(2.2e-16)]=sqrt(2.2e-16)
  pr[pr>=1 - 1e-6]=1 - 1e-6
  return(pr)
  
  }
  # rowSums(pr)
# colSums(pr)
  