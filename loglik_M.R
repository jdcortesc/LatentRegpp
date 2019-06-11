loglik_M=function(betas.item,r,pt.cuad,item,ind1,ind2,ncatg){

  p=prob_gpcm_M(betas.item,pt.cuad,item,ind1,ind2,ncatg)
  r.item=r[,c(ind1[item]:ind2[item])]
  return(-sum(r.item*log(p)))
  
}

###recuperacion de betas, modificado.
#loglik_M(param.item = param.item,r = r,pt.cuad = pt.cuad,item=1,ind1=ind1,ind2=ind2)


