schedule=schedule[-106,]

Pro_Classifica= function(n=105){
casa=NULL
trasf=NULL
sub=NULL
for(i in 1:n){
x=PPSG[PPSG$squadre==schedule[i,2],] 
y=PPSG[PPSG$squadre==schedule[i,3],]
casa=rbind(casa,x) #tutti i punteggi delle squadre di casa
trasf=rbind(trasf,y) #tutti i punteggi delle squadre ospiti
}

#punteggi totali delle squadre
casa2=casa[,-1] 
trasf2=trasf[,-1]

sub=casa2-trasf2 

#colonna dei nomi delle squadre
casa3=cbind(casa[,1]) 
trasf3=cbind(trasf[,1]) 

#ciclo for: se sub<0 allora vince sq. casa, altrimenti vince sq. ospite 
vince=NULL
for(z in 1:nrow(sub)){
  w=NULL
  for(j in 1:ncol(sub)){
    if(sub[z,j]>0)
      win=casa3[z]
    else{
        win=trasf3[z]
    }
    w=cbind(w,win)
  }
  vince=rbind(vince,w)
}
return(vince)
  }

tab=Pro_Classifica() #comprende tutte le squadre vincenti in tutti i possibili scenari

Vittorie= function(){
index= lapply(seq(1,105,5),function(x) ((x-1)*1+1):((x-2)*1+6))
col= lapply(index, function(x) tab[x,])  
tab2=do.call(cbind,col)
prova=tab2[,c(rep(TRUE,1),rep(FALSE,21))] #comprende solo le 'vere' vittorie
return(prova)
}
Win_to_extact=Vittorie()
Win_to_extact2=t(Win_to_extact)
write.table(Win_to_extact2, file = "campionato.xls",sep = "\t",dec = ',')
