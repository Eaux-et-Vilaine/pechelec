# DEPRECATED voir main pechelec
# 
# Author: cedric
###############################################################################

# ce fichier correspond à un export de series_pro_vilaine.xls
# les bidouilles de calcul sont expliquées à l'interieur
setwd("E:/workspace/p")
ges<-read.table("export_gestion.csv",sep=";",header=TRUE)
# graphique qui ne presente aucun doute
colnames(ges)
#[1] "season" "year"      "rec"       "cpec"      "pass"      "sed"       "ech"      
#[7] "tard"      "sr"        "eff"       "tx"        "trans"     "Netestu"  
#[13] "passem"    "pecxp"     "eclm"      "recfluciv" "aj"        "ajcoh"  
# text int kg	kg	kg	kg	kg	kg	%	%	%	kg	kg millions	millions	millions	milions	milliers	milliers		date	date	text
# Annee Recrutement 	Captures pêcherie obs.	Passe obs. 	sédentarisation	Echappement	
#Arrivées tardives 	%S/R	Efficacité passe	Taux d'exploitation	Transport civelles + captures pour marquage recaptures+écluse	
# Montées passes 	Pêches expérimentales	Ecluses expérimentations 	recrutement fluvial net (en milliers)	Migration anguilles jaunes	Migration anguilles jaunes par cohorte
plot(ges$pass,type="b")	
points(ges$aj*10,type="b",col="blue")


mortxp=0.6
mortpas=0.6
# In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
ges$recr_flu_aj_m1=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000

mortxp=0.8
mortpas=0.6
# In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
ges$recr_flu_aj_m2=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000

mortxp=1
mortpas=0.6
# In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
ges$recr_flu_aj_m3=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000

mortxp=1
mortpas=1
ges$recr_flu_aj_m4=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000



load("spl.Rdata") # spl issued from Traitement densite
# for downstream sector
spld<-spl[spl$classdist=="<50rkm",] # downstream
spld<-spld[order(spld$annee),]
gesflu=ges[,c("year","passem","pecxp","eclm","recfluciv","aj","recr_flu_aj_m1","recr_flu_aj_m2","recr_flu_aj_m3")] # fluvial data
# replacing missing values for electrofishing data
flu<-merge(gesflu,spld,by.x="year",by.y="annee",all.x=TRUE)
# having missing data I will only assume that the densities were the mean of previous and next year
for (i in c(9,11,13)){
flu[i,"denscs"]<-mean(c(flu[i-1,"denscs"],flu[i+1,"denscs"]))
}

scale=function(vec){
	vec/max(vec,na.rm=TRUE)
}
# CORRIGER POUR LES ANNEES OU IL N'Y A PAS DE DONNEES DE SUIVI
ccf(flu$denscs[3:14],flu$recr_flu_aj_m1[1:12]) # 2 year lag 
cor(flu$denscs[3:14],flu$recr_flu_aj_m1[1:12]) # 0.71
cor(flu$denscs[3:14],flu$recr_flu_aj_m2[1:12]) # 0.76
cor(flu$denscs[3:14],flu$recr_flu_aj_m3[1:12]) # 0.79
##########################
# graphique du rapport
##########################
barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
rect(xleft=c(3.8,5,7.4,8.6,9.8,11), ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.3,5.5,7.9,9.1,10.3,11.5), ytop=(ges$recr_flu_aj_m2/1e6)[c(4,5,7,8,9,10)],col="slateblue1") 
rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5), ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12), ytop=(ges$recr_flu_aj_m1/1e6)[c(4,5,7,8,9,10)],col="skyblue") 

#barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
#barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
plot(flu$denscs[3:14],type="b")
barplot(ges$recr_flu_aj_m1/1e6,names.arg=1996:2009,col="skyblue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))

#######################################
# premier graphique de l'animation
#########################################
# les fichiers .png permettent de gérer la transparence
png(filename = "C:/Documents and Settings/cedric/Mes documents/Migrateur/programmes/workspace3.5/various/transport1.png", width = 800, height = 600, bg = "transparent")
par(fg="white",col.axis="white",col.lab="white",col.main="white",cex=2)
barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5)-0.3, ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12)-0.2, ytop=(ges$recr_flu_aj_m1/1e6)[c(4,5,7,8,9,10)],col="skyblue") 
#barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
#barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
par(fg="black",col.axis="black",col.lab="black",col.main="black",cex=1)
dev.off()
# returns to default setting
#######################################
# deuxième graphique de l'animation
#########################################
# les fichiers .png permettent de gérer la transparence
png(filename = "C:/Documents and Settings/cedric/Mes documents/Migrateur/programmes/workspace3.5/various/transport2.png", width = 800, height = 600, bg = "transparent")
par(fg="white",col.axis="white",col.lab="white",col.main="white",cex=2)
barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5)-0.3, ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12)-0.2, ytop=(ges$recr_flu_aj_m2/1e6)[c(4,5,7,8,9,10)],col="skyblue") 
#barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
#barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
par(fg="black",col.axis="black",col.lab="black",col.main="black",cex=1)
dev.off()
# returns to default setting
#######################################
# troisième graphique de l'animation
#########################################
# les fichiers .png permettent de gérer la transparence
png(filename = "C:/Documents and Settings/cedric/Mes documents/Migrateur/programmes/workspace3.5/various/transport3.png", width = 800, height = 600, bg = "transparent")
par(fg="white",col.axis="white",col.lab="white",col.main="white",cex=2)
barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
#rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5)-0.3, ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12)-0.2, ytop=(ges$recr_flu_aj_m2/1e6)[c(4,5,7,8,9,10)],col="skyblue") 
#barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
#barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
par(fg="black",col.axis="black",col.lab="black",col.main="black",cex=1)
dev.off()
# returns to default setting
# analyse des 0 et 1
# La correlation croisée est maximale avec 1 an de délai pour les zero un an (bonne nouvelle !)
ccf(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m1[2:13][c(1:8,10,12,14)]) # 1 year lag 
cor(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m1[2:13][-c(7,9,11)]) # 0.6
cor(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m2[2:13][-c(7,9,11)]) # 0.53
cor(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m3[2:13][-c(7,9,11)]) # 0.44

plot(flu$year[1:14],(flu$denscs1[1:14]),pch=19,col="green",type="b")
plot(flu$year[2:13][-c(9,11)],(flu$denscs1[3:14]/100)[-c(9,11)],pch=19,col="black",type="b")

points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
