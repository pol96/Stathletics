delta_punti= function(n=105){
  casa=NULL
  trasf=NULL
  sub=NULL
  for(i in 1:n){
    x=PPSG[PPSG$squadre==schedule[i,2],] 
    y=PPSG[PPSG$squadre==schedule[i,3],]
    casa=rbind(casa,x) #tutti i punteggi delle squadre di casa
    trasf=rbind(trasf,y) #tutti i punteggi delle squadre ospiti
  }
  casa2=casa[,-1] 
  trasf2=trasf[,-1]
  casa3=cbind(casa[,1]) 
  trasf3=cbind(trasf[,1]) 
  sub=casa2-trasf2
  return(sub)
}
tab=delta_punti()

d= function(){
  index= lapply(seq(1,105,5),function(x) ((x-1)*1+1):((x-2)*1+6))
  col= lapply(index, function(x) tab[x,])  
  tab2=do.call(cbind,col)
  prova=tab2[,c(rep(TRUE,1),rep(FALSE,21))] #comprende solo le 'vere' vittorie
  return(prova)
}
to_print=d()
