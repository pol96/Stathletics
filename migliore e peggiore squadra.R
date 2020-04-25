minmax_team=function(n=1000){
  team=list()
  pt_per_team=NULL
  costo_per_team=NULL
  for(i in 1:n){
    #estraggo 10 giocatori secondo i criteri e creo la squadra
    guard=recap_TEAMS[sample(17:26,4),]
    forw=recap_TEAMS[sample(1:10,4),]
    centr=recap_TEAMS[sample(11:16,2),]
    coach=recap_TEAMS[sample(27:29,1),]
    t=rbind(guard,forw,centr,coach)
    #creo la lista delle squadre
    team[i]=list(t)
    #creo una matrice di punti e costi totali per ogni squadra creata (NB. considero solo le squadre con costo 70<x<95)
    costo=sum(team[[i]]$`PREZZO (SET. 2018)`)
    costo_per_team=c(costo_per_team,costo)
    if(costo<=95 && costo>=70){
      punti_tot=sum(team[[i]]$`PUNTI TOT`)
    }else{punti_tot=3500}
    pt_per_team=c(pt_per_team,punti_tot)
    costo_punti=rbind(costo_per_team,pt_per_team)
  }
  #punti massimo e punti minimo+ posizione squadra
  punti_max=max(costo_punti[2,])
  punti_min=min(costo_punti[2,])
  v=which(costo_punti[2,]==punti_min)
  w=which(costo_punti[2,]==punti_max)
  x=c(team[w],team[v])
  names(x)=c("max","min")
return(x)
}
MC_minmax=function(m=50, RandomTeam=1000){
y=NULL
for(i in 1:m){
  c=0
  x=minmax_team(n = RandomTeam)
  y[i]=list(x)
}
s1=NULL
s2=NULL
for(i in 1:m){
  f1=sum(y[[i]]$max[,3])
  f2=sum(y[[i]]$min[,3])
  s1=c(s1,f1)
  s2=c(s2,f2)
}
M=max(s1)
mi=min(s2)
v=which(M==s1)
w=which(mi==s2)
BestWorst=c(y[[v]][1],y[[w]][2])
return(BestWorst)
}
  


x=MC_minmax(m = 200, RandomTeam = 1000)
y=MC_minmax(m = 200, RandomTeam = 1000)
z=MC_minmax(m = 200, RandomTeam = 1000)
a=MC_minmax(m = 200, RandomTeam = 1000)
b=MC_minmax(m = 200, RandomTeam = 1000)
tot=c(x,y,z,a,b)
M=NULL
m=NULL
k=10
  
  for (i in 1:k){
    M=c(M,sum(tot[i]$max$`PUNTI TOT`))
    m=c(m,sum(tot[i]$min$`PUNTI TOT`))
  }

massimo=max(M)
minimo=min(m[-which(m==0)])
v=which(massimo==M) 
w=which(minimo==m)  



BestWorst=list(tot[[v]],tot[[w]])
names(BestWorst)=c("max","min")


