schedule=schedule[-106,]

casa=NULL
trasf=NULL
sub=NULL
  for(i in 1:n){
    x=PPSG[PPSG$squadra==schedule[i,2],] 
    y=PPSG[PPSG$squadra==schedule[i,3],]
    casa=rbind(casa,x) #tutti i punteggi delle squadre di casa
    trasf=rbind(trasf,y) #tutti i punteggi delle squadre ospiti
  }
  
  #colonna dei nomi delle squadre
  casa2=as.vector(cbind(casa[,1])) 
  trasf2=as.vector(cbind(trasf[,1])) 
  casa3=casa[,-1]
  trasf3=trasf[,-1]

home_points= function(){
  index= lapply(seq(1,105,5),function(x) ((x-1)*1+1):((x-2)*1+6))
  col= lapply(index, function(x) casa3[x,])  
  tab2=do.call(cbind,col)
  prova=tab2[,c(rep(TRUE,1),rep(FALSE,21))] #comprende solo le 'vere' vittorie
  return(prova)
}
Home=home_points()
out_points= function(){
  index= lapply(seq(1,105,5),function(x) ((x-1)*1+1):((x-2)*1+6))
  col= lapply(index, function(x) trasf3[x,])  
  tab2=do.call(cbind,col)
  prova=tab2[,c(rep(TRUE,1),rep(FALSE,21))] #comprende solo le 'vere' vittorie
  return(prova)
}
Out=out_points()
tab=rbind(Home,Out)
write.table(tab, file = "eff_dif.xls",sep = "\t",dec = ',')

















#ricavare i punti subiti dalla squadra 
efficienza_dif=function(w){
x=t(punti$`pt Out`[punti$Casa==w])
x2=t(punti$Giornata[punti$Casa==w])
y=t(punti$`pt Home`[punti$Trasferta==w])
y2=t(punti$Giornata[punti$Trasferta==w])
game=t(cbind(x2,y2))
punti_sub=t(cbind(x,y))
tab=cbind(game,punti_sub)
return(tab)
}
sB=efficienza_dif(w='B')
sL=efficienza_dif(w='L')
  
