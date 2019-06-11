rm(list=ls())

library(statmod)
library(optimx)
library(ltm)

setwd("~/Desktop/SICS/noviembre 2016/gpcm optim/")
source("loglik_M.R")
source("prob.R")
source("posterior.R")
source("rr.R")  
source("patrones.R")
source("expand.R")
source("expand.list.R")
source("prob_gpcm_M.R")
source("prob_gpcm.R")

estim_grm=function(datos){      
  
  ###numero de categorias por ítem y el ítem a estimar.
  ncatg <- apply(datos, 2, function (x) if (any(is.na(x))) length(unique(x)) - 1 else length(unique(x)))
  ind1 <- c(1, cumsum(ncatg[-ncol(datos)]) + 1)
  ind2 <- cumsum(ncatg)
  nitems=ncol(datos)
  
  ###valores iniciales:
  betas=list()
  # param=list()
  for(i in 1:nitems){
     betas[[i]]=c(seq(-1.5,1.5,length=ncatg[i]-1),1)
    #  betas[[i]]=as.vector(valini[i,])
    # # param[[i]]=c(betas[[i]][1],log(diff(betas[[i]][-ncatg[i]])),1)
  }
  
  
  ###nodos y pesos:
  Cuad = gauss.quad(n=40,"hermite") #nodos y pesos
  pt.cuad = Cuad[[1]]*sqrt(2) 
  w.cuad = Cuad[[2]]/sqrt(pi) #transforman los pesos
  
  #patrones y frecuencias
  pats=patrones(datos)$data
  npats=nrow(pats)
  freqs=patrones(datos)$freqs
  
  #datos expandidos por item
  pats.cod=expand(pats,ncatg)
  
  #datos dicotomizados para cada item, y para todas las categorias
  dat.dic=expand.list(pats=pats,ncatg = ncatg,npats)
  
  
  ########### ALGORITMO EM ###########
  
  #em:
  seguir = TRUE
  mm = 0
  betas.ant=betas
  #param=param.ant=param.true  #deltas verdaderos
  #betas=betas.ant=param.sim  #betas verdaderos
  # warn=NULL
  
  while(seguir) {
    
    ##contando los ciclos:
    mm = mm+1
    
    #PASO E
    r=rr(pt.cuad = pt.cuad,w.cuad = w.cuad,betas = betas,pats = pats,ncatg = ncatg,freqs = freqs,npats=npats,dat.dic=dat.dic,pats.cod=pats.cod)
    print(sum(r))
    
    #PASO M
    for(item in 1:nitems){  
      betas.item=betas[[item]]
      opt<-optimx(par = betas.item,fn = loglik_M,method = "BFGS",r=r,pt.cuad=pt.cuad,item=item,ind1=ind1,ind2=ind2,ncatg=ncatg)
      betas.item=unlist(opt[1:length(betas.item)])
      betas[[item]]=betas.item
    }
    
   # print(mm)
    
    ###evaluando convergencia:
    if(max(mapply(function(x,y){max(x-y)},betas.ant,betas)) < 10^(-3)){
      seguir = FALSE
    }
    betas.ant=betas
    # if(mm==500){warn="no converge";seguir=FALSE}
  }
  return(betas)
}

source("simulate.poly.uni.R")
simm=simulate.poly.uni(ncatgs = rep(4,10))
datos=simm$data
# write.table(datos,"~/Desktop/data.csv",sep=";")

# ini=Sys.time()
# estim=estim_grm(datos = datos)
# Sys.time()-ini

# estim=save(estim,file = "estim.RData")
load("estim.RData")
estltm=gpcm(datos)
estim
