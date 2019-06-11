# node=pt.cuad[4]
prob_gpcm=function(node,betas,nitems,ncatg){

  etas=
lapply(betas,function(x,node){x[length(x)]*(node-x[1:(length(x)-1)])},node=node)

cumsumetas=lapply(etas,function(x)cumsum(x))
den=lapply(cumsumetas,function(x){1+sum(exp(x))})
p=list()
for(i in 1:nitems){p[[i]]=vector(length = ncatg[i]);
p[[i]][1]=1/den[[i]]}

for(i in 1:nitems){ 
  for(k in 2:(ncatg[i])){ 
    p[[i]][k]=exp(etas[[i]][k-1])*p[[i]][k-1]
    }
}
p<-lapply(p, function(x) ifelse(x <= sqrt(2.2e-16) , sqrt(2.2e-16), x )  ) 
p<- lapply(p, function(x) ifelse(x >=  1 - 1e-6,  1 - 1e-6 , x) )

# p=lapply(p,function(x){x[-1]})
return(p)
}

# prob_gpcm(betas = betas,node =  pt.cuad[],nitems = nitems,ncatg = ncatg)
