#####importo dati e scelta della directory #####
  #per impostare la directory: session --> set_working_directory --> choose_directory --> downloads
  #importo dati
    library(readxl)
      data <- read_excel("G:/uni usi/tesi/new (10 teams)/recap- PLAYERS.xlsx", 
                            sheet = "Analisi giocatori")
      View(data)
      data=data[,1:4]
      
#####creo una squadra #####
squadre= function(x,i){
  
  #creo vettori per ogni categoria (A,C,G,HC)
  a=sample(1:10,4)
  b=sample(11:16,2)
  c=sample(17:26,4)
  d=sample(27:29,1)
  x=c(a,b,c,d)
  
  CR=0
  players=NULL
  somma=sum(data[x,3])
  
  #definisco il punteggio totale e i giocatori considerati se la somma 
  #dei crediti è <=95 --> lavoro su data
  if(somma<=95 && somma>=65){
    players=t(data[x,1])
    CR= sum(data[x,3])
    y=c(CR,players)
  }
  
  else{
    players=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    CR=0
    y=c(CR,players)
  }
  return(y) 
}

#####ripeto il processo n volte #####
creasquadre=function(n){
z=matrix(NA,n,12)
z[,1]=0
while (z[1,1]==0) {
  z[1,]=squadre() #crea la prima squadra casuale (continua a runnare fino a quando non si ottiene un risultato)
  
}
for(i in 2:n){
  #nei passi successivi si introducono le condizioni: 
    #1. squadre tali che 65<Cr<95 
    #2. ripetizione <5
  while(z[i,1]=='0'){ 
    z[i,]=squadre()
    for(j in 1:(i-1)){
      if(sum(as.numeric(z[i,]%in%z[j,]))>5){
        z[i,1]=0
      }
    }
  }
}
return(z)
}

teams=creasquadre(15)      

#####esporto le tabelle con le squadre ottenute #####
write.table(as.table(teams), file = "prova.xls",sep = "\t")




#####note####
  #ogni volta che il programma verrà lanciato genererà combinazioni diverse e di conseguenza
  #squadre diverse ogni giro. perchè si ottengano sempre le stesse squadre sarà necessario settare
f=123  
set.seed(f)
  #dove f è un numero a piacere da impostare costante ogni volta.
  #nelle squadre specificate nella tesi non è stato settato all'inizio.