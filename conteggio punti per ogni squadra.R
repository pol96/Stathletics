punti=punti[-1,-22]
punti$PT=as.numeric(punti$PT)

#sommo i punti totali di ogni giocatore per ogni squadra

PPSG=function(giornata=21){ # combino i vettori incolonnandoli (matrice)
  Punti_tot=NULL #matrice di ritorno con i punteggi totali per squadra per giornata
  for(p in 1:giornata){
    z=NULL #vettore per giornata del totale di squadra in quella giornata
  for(j in 1:nrow(teams)){ #combino in un vettore i risultati di ogni giornata per squadra (vettore)
    s=0
    for(i in 2:(ncol(teams))){ #sommo i punteggi di ogni singolo giocatore (numero)
      x=t(punti$PT[punti$GIOCATORE==teams[j,i]])
      x=as.vector(x)
      s=s+x[p]          
    }
    z=cbind(z,s)  
  }
    Punti_tot=rbind(Punti_tot,z)
  }
  return(Punti_tot)
}
Tab=t(round(as.matrix(PPSG()),2))

#esporto la tabella creata
write.table(as.table(Tab), file = "Canestri segnati- PPPG.xls",sep = "\t",quote = TRUE,dec = ',')

