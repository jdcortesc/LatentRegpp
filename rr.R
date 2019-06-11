rr=function(pt.cuad,w.cuad,betas,pats,ncatg,freqs,npats,dat.dic,pats.cod){

pii=posteriori(pt.cuad = pt.cuad,w.cuad = w.cuad,betas = betas,pats = pats,ncatg = ncatg,npats,dat.dic)
freq.matrix=matrix(freqs,nrow=length(pt.cuad),ncol=length(freqs),byrow=T)
(pii*freq.matrix)%*%pats.cod

}

#sum(rr(pt.cuad = pt.cuad,w.cuad = w.cuad,betas = betas,pats = pats,ncatg = ncatg,freqs = freqs,npats=npats,dat.dic=dat.dic,pats.cod=pats.cod))
