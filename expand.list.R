expand.list <- function(pats,ncatg,npats) {
  dat.dicotom=list()
  for(s in 1:npats){
  
  P<- length(ncatg)
  Y <- list()
  
  for(item in 1:P)
  {	
  y <- vector(length=(ncatg[item]))
  y[pats[s,item]] =T	
  y<- ifelse(y,1,0)
  Y[[item]]<- y
  }
  dat.dicotom[[s]]=Y
                 }
return(dat.dicotom)
}
#exp.list=expand.list(pats = pats,ncatg = ncatg)


##mini pruebas de expan y expand.list

#pats.cod[1,]
#pats[1,]
#exp.list[[1]]

#pats.cod[59,]
#pats[59,]
#exp.list[[59]]

#pats.cod[50,]
#pats[50,]
#exp.list[[50]]