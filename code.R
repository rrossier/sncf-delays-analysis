file="~/Documents/OPEN DATA/SNCF/Ponctualité des TGV à l'arrivée "
mois<-c("DEC","NOV","OCT","SEPT","AOU","JUIL","JUIN","MAI","AVR","MAR")
vars<-NULL
lignes<-NULL
header=c("depart","arrivee","nb.trains","nb.retards")
for(i in seq_along(mois)){
  retards<-read.csv(paste(file,mois[i],".csv",sep=""),sep=";",header=TRUE,check.names=FALSE)
  colnames(retards)<-header
  # on supprime les lignes sans l'information sur les retards
  # cad tous les terminus à l'étranger
  retards<-retards[!is.na(retards["nb.retards"]),]
  vars[[mois[i]]]<-as.data.frame(retards)
  lignes[[mois[i]]]<-with(vars[[mois[i]]],data.frame(depart=depart,arrivee=arrivee))
  lignes[[mois[i]]]$trajet<-with(lignes[[mois[i]]],paste(depart,"/",arrivee,sep=""))
}
tlignes<-lignes$DEC
for(i in seq_along(mois)){
  tlignes<-merge(tlignes["trajet"],lignes[[mois[i]]]["trajet"])
}
lignes2<-lignes$DEC[match(tlignes[,"trajet"],lignes$DEC[,"trajet"]),]

tri.retards.mois<-function(m,ascend=FALSE){
  p<-ifelse(ascend,1,-1)
  props<-round(m$nb.retards/m$nb.trains,3)
  m$props<-props
  m<-m[order(p*m[,5]),]
  return(m)
}

tri.retards.lignes<-function(l,ascend=FALSE){
  p<-ifelse(ascend,1,-1)
  retards<-NULL
  depart<-as.character(l$depart)
  arrivee<-as.character(l$arrivee)
  for(i in seq_along(mois)){
    temp<-tri.retards.mois(vars[[mois[i]]])
    temp<-temp[temp$arrivee==arrivee,]
    temp<-temp[temp$depart==depart,]
    retards<-rbind(retards,data.frame(mois=mois[i],nb.trains=temp$nb.trains,nb.retards=temp$nb.retards,props=temp$props))
  }
  return(retards)
}

tous.retards<-NULL
for(i in 1:nrow(lignes2)){
  temp<-tri.retards.lignes(lignes2[i,])
  mu<-mean(temp$props)
 tous.retards<-rbind(tous.retards,data.frame(depart=lignes2[i,"depart"],arrivee=lignes2[i,"arrivee"],props=mu)) 
}
tous.retards<-tous.retards[order(-tous.retards[,3]),]
retards.departs<-function(v){
  stopifnot(exists("tous.retards"))
  temp<-tous.retards[tous.retards$depart==v,]
  return(temp)
}
retards.arrivees<-function(v){
  stopifnot(exists("tous.retards"))
  temp<-tous.retards[tous.retards$arrivee==v,]
  return(temp)
}
#10 plus gros retards
retards.10<-tous.retards[1:10,]
par(mar=c(4,4,2,2))
plot.new()
cl<-rainbow(10)
for(i in 1:10){
  l<-data.frame(depart=retards.10[i,"depart"],arrivee=retards.10[i,"arrivee"])
  temp<-tri.retards.lignes(l)
  par(new=TRUE)
  if(i==10){
    plot(temp$props,col=cl[i],type='l',xlab="mois",ylab="%",xaxt="n",lwd=2)
    axis(1,at=seq(1,10),labels=temp$mois)
  }else{
    plot(temp$props,col=cl[i],type='l',xlab="",ylab="",xaxt="n",yaxt="n",axes=FALSE,lwd=2)
  }
}
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
legend("topleft",legend=paste(retards.10$depart,retards.10$arrivee,sep="/"),pch=15,col=cl,cex=0.7,inset=c(0.1,0.1))

retard.moyen.ville<-function(v){
  retards.ville<-retards.departs(v)
  mu<-mean(retards.ville$props)
  return(mu)
}
retard.moyen.ville2<-function(v){
  retards.ville<-retards.arrivees(v)
  mu<-mean(retards.ville$props)
  return(mu)
}

retards.villes.departs<-NULL
for(i in levels(tous.retards$depart)){
  retards.villes.departs<-rbind(retards.villes.departs,data.frame(ville=i,retard=retard.moyen.ville(i)))
}
retards.villes.departs<-retards.villes.departs[order(-retards.villes.departs[,2]),]
retards.villes.arrivees<-NULL
for(i in levels(tous.retards$arrivee)){
  retards.villes.arrivees<-rbind(retards.villes.arrivees,data.frame(ville=i,retard=retard.moyen.ville2(i)))
}
retards.villes.arrivees<-retards.villes.arrivees[order(-retards.villes.arrivees[,2]),]

trains.francfort<-NULL
for(i in seq_along(mois)){
  trains<-vars[[mois[i]]]
  departs.francfort<-trains[trains$depart=="FRANCFORT",]
  trains.francfort<-rbind(trains.francfort,departs.francfort)
}

trains.creusot<-NULL
for(i in seq_along(mois)){
  trains<-vars[[mois[i]]]
  arrivees.creusot<-trains[trains$arrivee=="LE CREUSOT-MONTCEAU-MONTCHANIN",]
  trains.creusot<-rbind(trains.creusot,arrivees.creusot)
}