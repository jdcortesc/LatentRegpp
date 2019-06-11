prob=function(betas,pt.cuad,nitems,ncatg){

  probs=list()
  for(g in 1:length(pt.cuad)){###para cada nodo:
  
      node=pt.cuad[g]
      p=prob_gpcm(betas = betas,node = node,nitems = nitems,ncatg)
    
  p<-lapply(p, function(x) ifelse(x <= sqrt(2.2e-16) , sqrt(2.2e-16), x )  ) 
  p<- lapply(p, function(x) ifelse(x >=  1 - 1e-6,  1 - 1e-6 , x) )
  probs[[g]]=p
  
  
  }
  return(probs)
}

#pr=prob(betas,pt.cuad,nitems,ncatg)
#lapply(pr[[8]],function(x)sum(x))
