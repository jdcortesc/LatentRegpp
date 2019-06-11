posteriori=function(pt.cuad,w.cuad,betas,pats,ncatg,npats,dat.dic){
nitems=length(betas)
Pi=matrix(NA,nrow=length(pt.cuad),ncol=npats)
p=prob(betas = betas,pt.cuad = pt.cuad,nitems=nitems,ncatg)

for(l in 1:nrow(pats)){

    dat.dic.pat=dat.dic[[l]]
  
    for(g in 1:length(pt.cuad)){
 
      p.node=p[[g]]
      
      expon=mapply(function(x,y) x^y,p.node,dat.dic.pat,SIMPLIFY = F)
      prod.catgs=lapply(expon,prod)
      Pi[g,l]=do.call(prod,prod.catgs)*(w.cuad[g])
                               }
                         }
      post=Pi/matrix(colSums(Pi),nrow=nrow(Pi),ncol=ncol(Pi),byrow=T)
     return(post)
}

#post=posteriori(pt.cuad = pt.cuad,w.cuad = w.cuad,betas = betas,pats = pats,ncatg = ncatg,npats=nrow(pats),dat.dic=dat.dic)

#colSums(post)
#mini-prueba:

#l=91;g=1
#p=prob(betas = betas,pt.cuad = pt.cuad)
#dat.dic.pat=dat.dic[[l]]
#p.node=p[[g]]
#expon=mapply(function(x,y) x^y,p.node,dat.dic.pat,SIMPLIFY = F)
#prod.catgs=lapply(expon,prod)

##observacion
#post[1,]
#post[nrow(post)]

#####################cambios:

#1)do.call(prod,prod.catgs)*(w.cuad[g]^sum(ncatg))
