# Traitement_structure_taille.R
# version 8 novembre 2011
# lancé à partir de main_pechelec.R
# Author: cedric
# attention ne fonctionne qu'en 32 bits, lancer manuellement
###############################################################################


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# REQUETAGE DE LA BASE ET CORRECTION MANUELLE D'ERREURS OPE<> station 
# et manque des données poissons d'une opération
# le fichier est à copier depuis C:/WamaVB/File
# attention le fichier fonctionne sur un R 32 bits

datawd <- "C:/workspace/pechelec/data/"
setwd("C:/workspace/pechelec")

CY <- 2022
library(stringr)
datawdy=str_c(datawd,CY,"/")

library(RODBC)
library(stacomirtools)
#channel=odbcConnectAccess2007("Wama.mdb",uid = "", pwd = "nsldgmi")
file.exists("T:/12-PoleMAB/Socle/Migrateurs/pechelec/Wama.mdb")
channel <- odbcConnectAccess("T:/12-PoleMAB/Socle/Migrateurs/pechelec/Wama.mdb",uid = "", pwd = "nsldgmi")

#channel=odbcConnectAccess2007("pechelec//Wama.mdb",uid = "", pwd = "nsldgmi")
 query <- "select * from Opera"
t_ope <- sqlQuery(channel, query, errors = TRUE)
#colnames(t_ope) # Op_Id_PtPrelMetier
query <- "select * from poisson"
t_pois <- sqlQuery(channel, query, errors = TRUE)
#colnames(t_pois)
query <- "select * from Reseaux , Sta_res, StaSandre where Sr_Id_Reseau =Id_Reseau and Sr_Id_Station=Id_Station"
t_sta <- sqlQuery(channel, query, errors = TRUE)
#colnames(t_sta)
InfoAnguille <- sqlQuery(channel, "SELECT * FROM InfoANguille", errors = TRUE)
save(InfoAnguille,file=str_c(datawdy,"InfoAnguille.Rdata"))

#sqlTables(channel)[sqlTables(channel)$TABLE_TYPE=="TABLE",]
query="select * from PointPrelMetier"
t_ptPrel=sqlQuery(channel, query, errors = TRUE)
t_op=killfactor(merge(t_ptPrel,t_ope,by.x="Id_PtPrelMetier",by.y="Op_Id_PtPrelMetier"))
t_ops=killfactor(merge(t_op,t_sta,by.x="Pm_Id_PointPrel",by.y="Sr_Id_PointPrel")) # operation ptprel station
#str(t_ops)

write.table(t_ops,sep=";",file="c:/temp/tops.csv",row.names=FALSE)
save(t_ops,t_sta,t_pois,t_ope,file=str_c(datawdy,"sauvbase.Rdata"))
# load(file=str_c(datawdy,"sauvbase.Rdata"))
odbcCloseAll()


t_ops2 <- t_ops[t_ops$ Sr_Id_Resea%in%c(1084,1412,771),c("CdStationMesureEauxSurface", "CodeSandreRdd","CdStationTemp","LbStationMesureEauxSurface","Pm_Id_PointPrel","Id_PtPrelMetier","Pm_Lieu", "Id_Reseau","CodeSandreRdd","Id_Opera","Op_code",
				"Op_Dtd","Op_Eau","Op_Hdb","Op_Lim","Op_Lng","Op_Lam","Op_Tmm","Op_Prf")]
t_ops2$Op_code=as.character(t_ops2$Op_code)
#t_pois[,"Po_Id_Opera"]
# ex(t_ops2[year(t_ops2$Op_Dtd)==2007,])
# ex(t_ops2[year(t_ops2$Op_Dtd)==2015,])
sta_ope_pois  <-  merge(t_ops2,t_pois,by.x="Id_Opera",by.y="Po_Id_Opera")
sta_ope_pois  <-  sta_ope_pois[sta_ope_pois$Po_CdTAxon==2038,]#12576 lignes




# €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€
# LES CORRECTIONS CI DESSOUS ON ETE APPORTEES
# NOTE IL RESTE UN PB POUR L'OPERATION 20020000051 les poids d'anguille ne sont pas tous connus
# €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€
#0435XX16
#00324900016
#05-Oct-99
#CHEZa-99-135
#CHEZa-135

# t_ops2[t_ops2$Op_code=="20020000051",]
#tail <- c(247,350,240,246,230,260,285,233,210,265,185,205,205,215,235,245,255,255,265,265,275,305,335,395)
#pass <- c(rep(2,11),rep(1,13))
#newope <- data.frame("NumPois"=1:length(tail),
#		"Op_code"="20020000051",
#		"zoneama"="$SANS",
#		"espoi"="ANG",
#		"Po_Lmi"=tail,
#		"polmx"="-9",
#		"Po_Lot"="N",
#		"Po_Eff "=1,
#		"Po_Pds"=NA,
#		"lib30"="30N",
#		"Lib31"="31N",
#		Popth=NA, Pomqg=NA, Poloc=NA, ponum=NA,
#		"Po_Pas"=pass,
#		lib35=NA, Potpl=NA, Poidl=NA, Age=NA, Act_Marq=NA,		
#		"Op_Dtd"=strptime("1999-10-05",format="%Y-%m-%d"))
#ope_pois <- rbind(ope_pois,newope)
#head(ope_pois)
#tail(ope_pois)
# Correction de problèmes par Brice
# en 1999, 2001, 2002 et 2005
#sta_ope_pois$stcod[sta_ope$stcod=="04350123"&sta_ope_pois$Op_code=="20020000037"] <- "04350126"
#sta_ope$stcod[sta_ope$stcod=="04350123"&sta_ope$Op_code=="20020000190"] <- "04350126"
#sta_ope$stcod[sta_ope$stcod=="04350113"&sta_ope$Op_code=="20020000110"] <- "04350114"
#sta_ope$stcod[sta_ope$stcod=="04350113"&sta_ope$Op_code=="20020000129"] <- "04350114"


##OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# Script R pour identifier les enregistrements
##OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
#t_ops2[t_ops2$Op_code==20020000037|t_ops2$Op_code==20020000110|t_ops2$Op_code==20020000129|t_ops2$Op_code==20020000190,]
#tt <- t_ops2[(t_ops2$CdStationTemp=="04350126"|t_ops2$CdStationTemp=="04350114"),]
#tt <- tt[!is.na(tt$CodeSandreRdd),]
#(tt <- tt[c(1,11),c("Pm_Id_PointPrel","Id_PtPrelMetier","Op_code","CdStationTemp")])
#
#"update Opera set Id_PtPrelMetier=13207 where Op_code=20020000037" # initialement 13206
#"update Opera set Id_PtPrelMetier=13207 where Op_code=20020000190" # initialement 13206
#"update Opera set Id_PtPrelMetier=13197 where Op_code=20020000110" # initialement 13196
# La 20020000129 est déjà OK


##OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# Script sql utilisé pour les corrections (lancé autant de fois que de lignes pour les poissons anguilles manquantes)
##OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
#Insert into Poisson (Poisson.Po_CdTAxon, Poisson.Po_Lmi, Poisson.Po_Lmx, Poisson.Po_Lot, Poisson.Po_Eff, Poisson.Po_Pds, Poisson.Lib30, Poisson.Lib31, Poisson.Po_Pth, Poisson.Po_Mqg, Poisson.Po_Loc, Poisson.Po_Num, Poisson.Po_Pas, Poisson.Lib35, Poisson.Po_Tpl, Poisson.Po_Idl, Poisson.Po_Age, Poisson.Po_Act_Marq, Poisson.Po_Num_Prelev, Poisson.Po_InfoPois, Poisson.Po_Id_Opera, Poisson.Po_Dt_Cre, Poisson.Po_Dt_Maj)
#
#SELECT  2038, 0, Poisson.Po_Lmx, N, 1, 0, Poisson.Lib30, Poisson.Lib31, Poisson.Po_Pth, Poisson.Po_Mqg, Poisson.Po_Loc, Poisson.Po_Num, 1, Poisson.Lib35, Poisson.Po_Tpl, Poisson.Po_Idl, Poisson.Po_Age, Poisson.Po_Act_Marq, Poisson.Po_Num_Prelev, Poisson.Po_InfoPois, Poisson.Po_Id_Opera, Poisson.Po_Dt_Cre, Poisson.Po_Dt_Maj
#FROM Poisson Where poisson.Id_Poisson=4434;
#
#
#select  * FROM Poisson Where Po_Id_Opera = 52 and Po_CdTAxon=2038;
#
#// changement des références d'une station de l'étang de chevré et de l'Aron
#		update Opera set Op_Id_PtPrelMetier='13207' where Op_code='20020000037'; --initialement 13206
#		update Opera set Op_Id_PtPrelMetier='13207' where Op_code='20020000190'; --initialement 13206
#		update Opera set Op_Id_PtPrelMetier='13197' where Op_code='20020000110'; --initialement 13196
#		
#		select * from Opera where Op_code='20020000037';
##OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
#head(sta_ope_pois)
#colnames(sta_ope_pois)
# pour vérifier les dates
#ope_pois$Op_Dtd[order(ope_pois$Op_Dtd)]
#sta_ope_pois[sta_ope_pois$Po_Lmi<60,]
#sta_ope_pois$Po_Lmi[sta_ope_pois$Po_Lmi>850]

# X=sta_ope_pois$Po_Lmi # pour débug dans la fonction
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# STRUCTURE DE TAILLE
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# ci dessous fonction pour faire l'histogramme des valeurs
# 50-80 = inférieur à 80
# 800-900 = supérieur à 800
# sinon classes de 10 mm
sta_ope_pois$Po_Lmi[sta_ope_pois$Po_Lmi==36] <- 360
sta_ope_pois <- sta_ope_pois[sta_ope_pois$Po_Lmi!=-9,]
fun=function(X){
	h=hist(x=X,breaks=c(50,seq(from=80,to=400,by=10),500,600,700,800,900),plot=FALSE,right=FALSE)
	v=h$counts # comptage des éléments par classe
	return(v)
}
# tapply applique la fonction à toutes les valeurs unique l'index (ici à chaque opération)
tail <- tapply(X=sta_ope_pois$Po_Lmi,INDEX=sta_ope_pois$Op_code,fun)
#str(tail)
dftail <- tail[[1]]
for (i in 2: length(tail)){
	dftail=cbind(dftail,tail[[i]])
} # end for
rownames(dftail) <- c("<80",paste(c(seq(from=80,to=390,by=10),400,500,600,700),"-",c(seq(from=90,to=400,by=10),500,600,700,800),sep=""),">800")
colnames(dftail) <- names(tail)	
# setwd("C:/Users/cedric.briand/Documents/workspace/p/")
write.table(dftail, str_c(datawdy,"struct_taille.csv"), sep=";", row.names=TRUE, col.names=TRUE)
write.table(sta_ope_pois, str_c(datawdy,"sta_ope_pois.csv"), sep=";", row.names=FALSE, col.names=TRUE)
save(sta_ope_pois, file=str_c(datawdy,"sta_ope_pois.csv"))
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# STRUCTURE D'AGE
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
require(stringr)
cle <- read.table(file=str_c(datawd,"cle_taille_age.csv"),sep=";",header=FALSE)
colnames(cle) <- rownames(dftail)
rownames(cle) <- paste("age",c(0:4),sep="")
strage <- t(dftail)%*%t(cle)
strage <- as.data.frame(strage)
strage$Op_code <- rownames(strage)
# vérification qu'on ne perd personne en route
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# REINTEGRATION TAILLE ET AGE
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
stopifnot(all(
as.vector(round(rowSums(strage[,1:5])))==
as.vector(tapply(sta_ope_pois$Po_Lot,sta_ope_pois$Op_code, function(X)length(X)))))
sta_ope1 <- merge(t_ops2,strage,by="Op_code",all.x=TRUE,all.y=FALSE)
dftail1 <- as.data.frame(t(dftail))
dftail1$Op_code <- rownames(dftail1)
sta_ope2 <- merge(sta_ope1,dftail1,by="Op_code",all.x=TRUE,all.y=FALSE)
#sta_ope2[sta_ope2$Op_code=="20020000129",] # pour vérification
#sta_ope2[sta_ope2$Op_code=="20020000049",] # pour vérification
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# histogramme par classes de 50 mm
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
fun=function(X){
	h=hist(x=X,breaks=c(seq(from=50,to=1000,by=50)),plot=FALSE,right=FALSE)
	v=h$counts # comptage des éléments par classe
	return(v)
}
# tapply applique la fonction à toutes les valeurs unique l'index (ici à chaque opération)
tail50=tapply(X=sta_ope_pois$Po_Lmi,INDEX=sta_ope_pois$Op_code,fun)
# str(tail50)
dftail50=tail50[[1]]
for (i in 2: length(tail50)){
	dftail50=cbind(dftail50,tail50[[i]])
} # end for
rownames(dftail50) <- c(paste(c(seq(from=50,to=950,by=50)),"-",c(seq(from=100,to=1000,by=50)),sep=""))
colnames(dftail50) <- names(tail50)
dftail2 <- as.data.frame(t(dftail50))
dftail2$Op_code <- rownames(dftail2)
pec <- merge(sta_ope2, dftail2, by="Op_code",all.x=TRUE,all.y=FALSE)
#pec[pec$Op_code=="20020000049",] # pour vérification
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Calcul des effectifs par passage...
# debug :: # 
# datpois <- sta_ope_pois; pas <- 3;datres <- pec
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# note on suppose qu'on a eu deux passages systématiquement et trois passages exceptionnellement
# car on a pas trouvé le nombre de passages dans la table opération
nbperpas <- function(pas,datpois,datres){
	datpois <- subset(datpois,datpois$Po_Pas==pas)
	vec <- tapply(datpois$Po_Eff,INDEX=list(datpois$Op_code),function(X)length(X))
	nom <- paste("Eff",pas,sep="")
	res <- data.frame("Op_code"=names(vec),nom=vec)
	colnames(res) <- c("Op_code", nom)
	datres <- merge(datres,res,by="Op_code",all.x=TRUE,all.y=FALSE)
	if (pas!=3)	datres[,nom][is.na(datres[,nom])] <- 0
	return(datres)
}
pec <- nbperpas(1,sta_ope_pois,pec)
pec <- nbperpas(2,sta_ope_pois,pec)
pec <- nbperpas(3,sta_ope_pois,pec)
#datpois <- sta_ope_pois;datres <- pec
poidstotal <- function(datpois,datres){
	vec <- tapply(datpois$Po_Pds,datpois$Op_code,sum, na.rm=TRUE)
	res <- data.frame("Op_code"=names(vec),"poidstotal"=vec)
	colnames(res) <- c("Op_code", "poidstotal")
	datres <- merge(datres,res,by="Op_code",all.x=TRUE,all.y=FALSE)
	return(datres)
}
# calcule le poids total non corrigé de Carle et Strubbe et non corrigé de la surface
pec <- poidstotal(sta_ope_pois,pec)
# une station dont on a perdu les poids sauf excel
#pec$poidstotal["20020000051"] <- 1982*24/37
save(pec,file=str_c(datawdy,"pec.Rdata"))
