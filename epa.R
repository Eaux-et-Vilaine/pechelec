# 2013 données EPA traitement/ gaëlle
# 
# Author: cedric.briand
###############################################################################
require(stringr)
require(ggplot2)
setwd("C:/Users/cedric.briand/Documents/workspace/p/")

load(file="pechelec/dens.Rdata")
#str(dens)
dens$stcod<-as.character(dens$stcod)
ia<-read.csv("H:/IAV/Publications/PubliEPA/2013/ia.csv",sep=";")
habitats<-read.csv("H:/IAV/Publications/PubliEPA/2013/habitats.csv",sep=";")
habitats<-habitats[!is.na(habitats$station),c(1:3)]
colnames(habitats)<- c("stcod","longueur","largeur") 
habitats$stcod<-str_c("0",habitats$stcod)
#str(ia)
colnames(ia)<-c("stcod","taille")
# nombres par station
ian<-tapply(ia$taille,ia$stcod,function(X)length(X))
ian<-data.frame("stcod"=names(ian),"N"=ian)
ian$stcod<-as.character(ian$stcod)
ian$stcod<-str_c("0",ian$stcod)
dens2013<-subset(dens,dens$annee==2013)
dens2013<-merge(dens2013,ian,by="stcod",all.x=TRUE,all.y=FALSE)
dens2013<-merge(dens2013,habitats,by="stcod",all.x=TRUE,all.y=FALSE)
dens2013$ia<-dens2013$N/30
dens2013$densCS100<-100*dens2013$densCS
dens2013<-dens2013[dens2013$stcod!="04560148",]
# Modèle linéaire
lm1<-lm(densCS100~ia+0,data=dens2013)
summary(lm1)
? predict.lm
x11()
plot(dens2013$densCS,dens2013$ia)


x11()
plot(dens2013$ia,dens2013$densCS100,xlim=c(0,5),ylim=c(0,300))

newdata<-data.frame("ia"=seq(from=0,to=5,by=0.1))
points(newdata$ia,predict.lm(lm1,newdata,se.fit=TRUE)$fit,col="red",type="l")
points(newdata$ia,predict.lm(lm1,newdata,se.fit=TRUE)$se.fit+
				predict.lm(lm1,newdata,se.fit=TRUE)$fit,col="green",type="l")
points(newdata$ia,predict.lm(lm1,newdata,se.fit=TRUE)$fit-predict.lm(lm1,newdata,se.fit=TRUE)$se.fit
				,col="green",type="l")
		
plot(dens2013$ia,dens2013$densCS100,xlim=c(0,1),ylim=c(0,100))	
dens2013$dept<-substr(dens2013$stcod,3,4)
# influence du département => Non
x11()
ggplot(dens2013,aes(x=ia,y=densCS100,col=dept))+
		geom_point()+
		scale_y_continuous(name="Densite estimee (ang.100 m-2)",limits=c(0,100))+
		scale_x_continuous(name="Nombre d'anguille par point",limits=c(0,2.5))+
		geom_smooth(aes(colour=dept,fill=dept), method="lm",fullrange=TRUE,formula=y ~ x-1)+
		scale_colour_manual("Methode",values = c("chevauchement" = "black","sans chevauchement" = "grey50"))+
		scale_fill_manual("Methode",values = c("chevauchement" = "grey50","sans chevauchement" = "grey80"))+
		theme_bw() +		
		annotate("text",x=3.2, y=180, label="Dens=85.4*EPA", color="grey20",size=3,angle=35) +
		annotate("text",x=3.2, y=80, label="Dens=36.3*EPA", color="grey50",size=3,angle=18)+
		annotate("text", x = 0, y = 280, label = "A",size=5)

sta_ope_pois<-read.csv("pechelec/sta_ope_pois2013.csv",header = TRUE,sep=";")
sta_ope_pois$opdtd<-sta_ope_pois$opdtd.x
sta_ope_pois$opdtd<-as.character(sta_ope_pois$opdtd)
sta_ope_pois$date<-strptime(sta_ope_pois$opdtd,format="%d/%m/%Y")
sta_ope_pois$annee<-strftime(sta_ope_pois$date,format="%Y")
sta_ope_pois<-sta_ope_pois[sta_ope_pois$annee=="2013",]
sta_ope_pois$stcod<-as.character(sta_ope_pois$stcod)
sta_ope_pois$stcod<-str_c("0",sta_ope_pois$stcod)
ia$stcod<-str_c("0",ia$stcod)
head(sta_ope_pois)
sop<-sta_ope_pois[,c("stcod","polmi")]
colnames(sop)[2]<-"taille"
sop$cat<-"pec2p"
ia$cat<-"ia"
sop<-rbind(ia,sop)
sop<-sop[sop$stcod!="0NA" ,]
ggplot(sop)+geom_bar(aes(x=taille,fill=cat),alpha=0.8,position="stack")+facet_wrap(~stcod)

fun=function(X){
	h=hist(x=X,breaks=c(seq(from=50,to=1000,by=50)),plot=FALSE,right=FALSE)
	v=h$counts # comptage des éléments par classe
	return(v)
}
res<-tapply(sop$taille,sop$cat,fun)

mids<-hist(x=sop$taille,breaks=c(seq(from=50,to=1000,by=50)),plot=FALSE,right=FALSE)$mids
plot(mids,res$ia/(res$ia+res$pec2p),type="b")
pourc<-res$ia/(res$ia+res$pec2p)
pourc<-pourc[1:9]
pouc<-data.frame("taille"=mids[1:9],"pourc"=pourc)
lm2<-(lm(pourc~taille,data=pouc))
summary(lm2)
