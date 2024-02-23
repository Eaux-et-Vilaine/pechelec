# main_pechelec.R 
# Lancement principal du traitement des données de pêche électrique pour le rapport
# Appelle traitement_structure_taille.R pour construire un tableau des structures de taille et d'âge Ã  partir des données de wama (poissons et opérations)
# Rassemble sous R tous les tableaux et graphiques qui avant 2011 étaient faits Ã  partir d'une feuille excel.
# et d'une clé taille age dont le fichier se trouve dans le répertoire pechelec
# Utilise la librairies FSA pour réaliser le calcul des densités de Carle et Strubb
# Author: cedric.briand, Brice Sauvaget; 
###############################################################################
#  install.packages('FSA',,'http://www.rforge.net/') 

setwd("C:/Users/cedric.briand/Documents/workspace/p/")
#setwd("C:/Users/brice.sauvaget/workspace/p/")
source("pechelec/traitement_structure_taille.R") # creation d'un tableau pec
#save(pec,file="pechelec/pec.Rdata")
#load(file="pechelec/pec.Rdata")


str(pec)
nrow(pec)#253 #257
# quelles sont les stations "gardée"
# calcul des densites de Carle et Strubb
stations<-as.character(pec$stcod)[strftime(pec$opdtd,"%Y")==2007]
pec<-pec[as.character(pec$stcod)%in%stations,]

nrow(pec)#
pec$annee<-strftime(pec$opdtd,"%Y")
pec$mois<-strftime(pec$opdtd,"%m")
#pec[pec$annee==2002,"stcod"][order(pec[pec$annee==2002,"stcod"])]
#pec[pec$annee==2003,"stcod"][order(pec[pec$annee==2003,"stcod"])]
library(FSA)

######################################
# CALCUL DES DENSITES DE CARLE ET STRUBB
######################################
#Confidence intervals are computed using standard large-sample normal distribution theory. 
#Note that the confidence intervals for the 2- and 3-pass special cases are only approximately correct if the estimated population size is greater than 200.
#If the estimated population size is between 50 and 200 then a 95% CI behaves more like a 90% CI.

pec$NCS<-NA
pec$LCI<-NA
pec$UCI<-NA
pec$p<-NA
for (i in 1:nrow(pec)){
			if (!is.na(pec$Eff2[i])){
				if (is.na(pec$Eff3[i])){
					if (pec$Eff2[i]<pec$Eff1[i]){
						p<-removal(c(pec$Eff1[i],pec$Eff2[i]),type="Zippin")
						pec$Nzip[i]<-p$est["No"]
						pec$pzip[i]<-p$est["p"]
						pec$LCIzip[i]<-confint(p)[1,1]
						pec$UCIzip[i]<-confint(p)[1,2]	
					} else {
						pec$Nzip[i]<-NA
					}
				} else{
					p<-removal(c(pec$Eff1[i],pec$Eff2[i],pec$Eff3[i]),type="Zippin")
					pec$Nzip[i]<-p$est["No"]
					pec$pzip[i]<-p$est["p"]
					pec$LCIzip[i]<-confint(p)[1,1]
					pec$UCIzip[i]<-confint(p)[1,2]
				}		
		}
}
for (i in 1:nrow(pec)){
	if (!is.na(pec$Eff2[i])){
		p<-removal(c(pec$Eff1[i],pec$Eff2[i]),type="CarleStrub")
		pec$NCS[i]<-p$est["No"]
		pec$p[i]<-p$est["p"]
		pec$LCI[i]<-confint(p)[1,1]
		pec$UCI[i]<-confint(p)[1,2]
		
	} else {pec$NCS[i]<-NA
	}
}
pec$densCS<-pec$NCS/(pec$oplng*pec$oplam)
pec$densZip<-pec$Nzip/(pec$oplng*pec$oplam)
pec$eff=pec$Eff1/pec$NCS
pec$Eff3calcul<-pec$Eff3
pec$Eff3calcul[is.na(pec$Eff3calcul)]<-0
pec$biomCS<-pec$poidstotal*(pec$NCS/(pec$Eff1+pec$Eff2+pec$Eff3calcul))/(pec$oplng*pec$oplam)

pec[,c("age0","age1","age2","age3","age4")]<-pec[,c("age0","age1","age2","age3","age4")]*(pec$NCS/((pec$Eff1+pec$Eff2+pec$Eff3calcul)*pec$oplng*pec$oplam))
library(ggplot2)
library(lattice)

##################################################################
# MERGE AVEC LE FICHIER DES STATIONS QUI CONTIENT LES DISTANCES
##################################################################
stations=read.table("pechelec/code_stations.csv",sep=";",header=TRUE,colClasses=c(rep("character",4),"numeric"))
dens<-merge(pec,stations,by="stcod")
head(dens)
dens$classdist<-cut(dens$distance_mer,breaks=c(0,50,100,300))
levels(dens$classdist)<-c("<50rkm","50-100rkm",">100rkm")

save(dens,file="pechelec/dens.Rdata")
#write.table(dens,file="dens.csv",sep=";")
#load(file="pechelec/dens.Rdata")
#####################################
# Pour récupérer le tableau 1, t.test permet de récupérer les intervalles de confiance
#####################################
#str(t.test(pec$densCS))
t.test(pec$densCS)$conf.int[2]-t.test(pec$densCS)$estimate
(tableau1<-data.frame(
					"Nb"=tapply(pec$densCS,pec$annee,function(X)length(X)),
					"Annee"=unique(pec$annee),
					"Densite moyenne"=round(tapply(pec$densCS,pec$annee,mean),2),
					#"Ecart type densite"=round(tapply(pec$densCS,pec$annee,function(X)sd(X)),2),
					"Intervalle confiance 95 densite"=paste("+-",round(tapply(pec$densCS,pec$annee,function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2)),
					"Efficacite p1 div densCS"=round(tapply(pec$eff,pec$annee,mean),2),
					#"Ecart type efficacite"=round(tapply(pec$eff,pec$annee,function(X)sd(X)),2),
					"Intervalle confiance 95 efficacite"=paste("+-",round(tapply(pec$eff,pec$annee,function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2)),
		        	"Biomasse moyenne"=round(tapply(pec$biomCS,pec$annee,mean),2), 
		            "Intervalle confiance 95 biomasse"=paste("+-",round(tapply(pec$biomCS,pec$annee,function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2))
			))

#!!!! NOTE A CE STADE VERIFIER LE NOMBRE DE STATIONS.... ET LES DONNEES. 

#############################
# Un modèle très simple pour voir les tendances
#############################

densityplot(pec$densCS)
shapiro.test(pec$densCS) # W = 0.7237, p-value < 2.2e-16      #   Performs the Shapiro-Wilk test of normality.  p<0.1 données non normales
shapiro.test(log(pec$densCS)) # W = 0.9914, p-value = 0.3442 
# il faut faire une log transformation pour avoir des données normalisées
pec$annee=as.factor(pec$annee)
glmpec<-glm(log(densCS)~stcod+annee,data=pec)
summary(glmpec)
library(car)
crPlot(model=glmpec,terms=~annee,main="Component residual plot for glm (log(densite) ~ station + annee",variable="annee")
drop1(glmpec) # le principal effet est l'effet station
anova(glmpec,test="F") # les deux facteurs sont significatifs
library(multcomp)
pec.glht<-glht(glmpec,linfct=mcp(annee ="Tukey"))
summary(pec.glht)
pec.cld<-cld(pec.glht) # compact letter display seuil 0.05
old.par <- par( mai=c(1,1,1.25,1))
plot(pec.cld,col=c("green", "green", "darkgreen","green","green","turquoise","blue","blue","blue","darkblue"),ylab=log)
par(old.par)

resid(glmpec)

df=glmpec$df.residual
#DévianceExpliquée=1-DévianceRésiduelle/DévianceNulle
#Le degré de concordance entre les observations et les valeurs ajustées par le modèle (Pearce & Ferrier 2000) est mesuré
#Plus un modèle explique convenablement les variations de la réponse et plus la proportion de déviance expliquée est proche de 1.
(pdev=1-(deviance(glmpec)/glmpec$null.deviance)) # 67 %

################################################################
# Pour faire le même graphique dans un ggplot et en format non log
################################################################
pec$resid<-resid(glmpec)
coef<-summary(glmpec)$coefficient
coef<-as.matrix(coef)[c(1,grep("annee",dimnames(coef)[[1]])),1]
coef<-coef[1]+coef[2:length(coef)]
coef=c(coef,"annee1998"=0)
df_coef<-data.frame("annee"=gsub("annee","",names(coef)),"coefficient"=coef)
pecmod<-merge(pec,df_coef,by="annee") ;rm("df_coef","coef")
# component + residual
pecmod$cr=exp(pecmod$coefficient+pecmod$resid)
df_letter<-pec.cld$mcletters[["Letters"]]
df_letter<-data.frame("annee"=names(df_letter),"group"=df_letter)
pecmod<-merge(pecmod,df_letter,by="annee");rm(df_letter)


########################################
# Figure 1. Tendance des densités en fonction des années et test statistique de la tendance globale
########################################

x11()
g<-ggplot(pecmod,aes(x=annee,y=densCS))
g+geom_point(col="grey40",size=1.8)+
		geom_boxplot(aes(fill=group),alpha=0.5)+
		scale_fill_brewer(name="group",palette="Set1")+
		scale_y_continuous(name=expression(paste(densite~ang.m^-2)))+
		ggtitle(iconv("Tendance des densités en fonction des années","UTF8"))


######################################"
########################################

#  Figure 2. Tendance des densités en fonction des années et de la distance mer
########################################
x11()
g<-ggplot(dens,aes(x=annee,y=densCS))
g+geom_boxplot(aes(fill=annee))+
		facet_grid(~ classdist)+
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(densite~ang.m^-2)))+
		geom_hline(yintercept=0.3,col="red")+
		#annotate("text",x=1,y=0.25,label="cible de gestion",col="red")+
		ggtitle(iconv("Tendance des densités en fonction des années et de la distance mer","UTF8"))

# densitéCS et IC Ã  0.05
round(tapply(dens$densCS,list(dens$classdist,dens$annee),mean),2)
round(tapply(dens$densCS,list(dens$classdist,dens$annee),function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2)

########################################
#  Figure 3. Tendance des biomasses en fonction des années et de la distance mer
########################################
x11()
g<-ggplot(dens,aes(x=annee,y=biomCS))
g+geom_boxplot(aes(fill=annee))+
		facet_grid(~ classdist)+
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(biomasse~g.m-2)))+
		#annotate("text",x=1,y=0.25,label="cible de gestion",col="red")+
		ggtitle(iconv("Tendance des biomasses en fonction des années et de la distance mer","UTF8"))

# biomasseCS et IC Ã  0.05
round(tapply(dens$biomCS,list(dens$classdist,dens$annee),mean),2)
round(tapply(dens$biomCS,list(dens$classdist,dens$annee),function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2)

########################################
#  Figure 4. Tendance des densités en fonction de l'âge
########################################
data_age<-melt(dens[,c("Opcod","annee","classdist","age0","age1","age2","age3","age4")],id.vars=c("Opcod","annee","classdist"),variable_name = "age")
colnames(data_age)<-c("Opcod","annee","classdist","age","densiteCS")
data_age$age<-as.factor(data_age$age)
levels(data_age$age)<-c("0","1","2","3","4+")
x11()
g<-ggplot(data_age,aes(x=age,y=densiteCS))
g+geom_boxplot(aes(fill=annee))+		
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(densite~ang.m^-2)))+
		#annotate("text",x=1,y=0.25,label="cible de gestion",col="red")+
		ggtitle(iconv("Tendance des densités en fonction de l'âge","UTF8"))

# densitéCS et IC Ã  0.05
round(tapply(data_age$densiteCS,list(data_age$age,data_age$annee),mean),2)
round(tapply(data_age$densiteCS,list(data_age$age,data_age$annee),function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2)
				
########################################
# Figure 5 (annexe ?) Tendance des densités en fonction de l'âge et de la distance mer
########################################
x11()
g<-ggplot(data_age,aes(x=age,y=densiteCS))
g+geom_boxplot(aes(fill=annee))+	
		facet_grid(~ classdist)
scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(densite~ang.m^-2))+
		opts(title="Tendance des densités en fonction de l'âge et de la distance à la mer"))

########################################
# Figure 6 (annexe ?) Tendance des densités en fonction des rivières
########################################
x11()
g<-ggplot(dens,aes(x=annee,y=densCS))
g+geom_boxplot(aes(fill=annee))+	
		facet_wrap(~ riviere)+
scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(densite~ang.m-2)))+
		ggtitle(iconv("Tendance des densités sur les différents affluents","UTF8"))+
		theme(axis.text.x=element_text(angle=90, hjust=0.5 )) 

x11()
g<-ggplot(dens,aes(x=annee,y=densCS))
g+geom_boxplot(aes(fill=annee))+	
		facet_wrap(~ riviere,scales="free_y")+
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(densite~ang.m-2)))+
		ggtitle(iconv("Tendance des densités sur les différents affluents","UTF8"))+
		theme(axis.text.x=element_text(angle=90, hjust=0.5 )) 

# pour une rivière donnée
denstrev<- dens[dens$riviere=="TREVELO",]
x11()
g<-ggplot(denstrev,aes(x=annee,y=densCS))
g+geom_point(col="grey40",size=1.8)+
geom_boxplot(aes(fill=annee),alpha=0.5)+
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste("densité ang."," m-^²")))+
		opts(title="Tendance des densités sur le Trévelo entre 1998 et 2011")+
        opts(axis.text.x=theme_text(angle=90, hjust=0.5 ))

# densitéCS
round(tapply(dens$densCS,list(dens$riviere,dens$annee),mean),2)

# Sauvegarde du data.frame dens
save(dens, file="X:/Migrateur/Pechelec/2011/dens2013.Rdata")

write.table(pec,"pechelec/pec.csv",sep=";")
write.table(dens,"pechelec/dens.csv",sep=";")


# *******************************************
# Figure 7 SURVIE DES CIVELLES A PARTIR DES SUIVIS ARZAL
# anal_gestion.R Ã  lancer pour le graphique de la relation recrutement-stock
# préparation d'un tableau de synthèse ci dessous
#******************************************
# 
# il est possible de relancer le script seulement Ã  partir de lÃ  en décommentant les lignes ci-dessous.

#pec<-read.table("pechelec/pec.csv",sep=";")
#dens<-read.table("pechelec/dens.csv",sep=";")
load(file="X:/Migrateur/Pechelec/2011/dens2011.Rdata")
ges<-read.table("export_gestion.csv",sep=";",header=TRUE)
# graphique qui ne presente aucun doute
colnames(ges)
#[1] "season" "year"      "rec"       "cpec"      "pass"      "sed"       "ech"      
#[7] "tard"      "sr"        "eff"       "tx"        "trans"     "Netestu"  
#[13] "passem"    "pecxp"     "eclm"      "recfluciv" "aj"        "ajcoh"  
plot(ges$aj*10,type="b",col="blue")

# Calcul de survies probables pour différents scénarios de mortalité au stade civelle
mortpas=0
mortxp=mortpas

# In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
ges$recr_flu_aj_m1=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000

mortxp=(1+mortpas)/2

# In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
ges$recr_flu_aj_m2=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000

mortxp=1

# In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
ges$recr_flu_aj_m3=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000

mortxp=1
mortpas=1
ges$recr_flu_aj_m4=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000
library(ggplot2)
sumplussd=function(df,var) mean(df$"densCS",na.rm=TRUE)+sd(df$"densCS",na.rm=TRUE) 
summinussd=function(df) mean(df$"densCS",na.rm=TRUE)-sd(df$"densCS",na.rm=TRUE)
meanspl=function(df) mean(df$"densCS",na.rm=TRUE)
meanspl1=function(df) mean(df[,c("1")],na.rm=TRUE)
sumplussd1=function(df,var) mean(df$"1",na.rm=TRUE)+sd(df$"1",na.rm=TRUE) 
summinussd1=function(df) mean(df$"1",na.rm=TRUE)-sd(df$"1",na.rm=TRUE)
spl=ddply(dens,.variables=c("annee","classdist"),.fun=sumplussd)
smi=ddply(dens,.variables=c("annee","classdist"),.fun=summinussd)
colnames(spl)[3]<-"sumplussd"
spl$summinussd=smi[,3]
spl$denscs=ddply(dens,.variables=c("annee","classdist"),.fun=meanspl)[,3]
spl$denscs1=ddply(dens,.variables=c("annee","classdist"),.fun=meanspl1)[,3]
spl$summinussd1=ddply(dens,.variables=c("annee","classdist"),.fun=summinussd1)[,3]
spl$sumplussd1=ddply(dens,.variables=c("annee","classdist"),.fun=sumplussd1)[,3]

save(spl,file="pechelec/spl.Rdata")

load("spl.Rdata") # spl issued from Traitement densite
# for downstream sector
spld<-spl[spl$classdist=="<50rkm",] # downstream
spld<-spld[order(spld$annee),]
gesflu=ges[,c("year","passem","pecxp","eclm","recfluciv","aj","recr_flu_aj_m1","recr_flu_aj_m2","recr_flu_aj_m3")] # fluvial data
# replacing missing values for electrofishing data
flu<-merge(gesflu,spld,by.x="year",by.y="annee",all.x=TRUE)
# having missing data I will only assume that the densities were the mean of previous and next year
for (i in c(9,11,13,15)){
	flu[i,"denscs"]<-mean(c(flu[i-1,"denscs"],flu[i+1,"denscs"]))
}

scale=function(vec){
	vec/max(vec,na.rm=TRUE)
}
# CORRIGER POUR LES ANNEES OU IL N'Y A PAS DE DONNEES DE SUIVI
ccf(flu$denscs[3:16],flu$recr_flu_aj_m1[1:14]) # 2 year lag 
cor(flu$denscs[3:16],flu$recr_flu_aj_m1[1:14]) # 0.73
cor(flu$denscs[3:16],flu$recr_flu_aj_m2[1:14]) # 0.77
cor(flu$denscs[3:16],flu$recr_flu_aj_m3[1:14]) # 0.78
##########################
# graphique du rapport
##########################
require(grDevices)
X11()
par("mar"=c(5.1,5.1,4.1,5.1))
plot(1996:2011,ges$recr_flu_aj_m3,xlab="annee",xaxt="n",ylab=expression("Civelles x " * 1000),type="b",yaxt="n",frame = FALSE,col="grey60",lwd=2)
at<-seq(0,2.5,by=0.5)
at3<-at*10^6
at2<-at*10^3
at1<-at3/25
axis(1, at = 1996:2011)
axis(2, at = at3, labels = formatC(at2, big.mark = " ", format = "d"), 
		las = 3)
axis(side=4,at=at3,labels=at1)
mtext("Anguilles jaunes",4,line=3)
abline(h=at3,col="grey70")
points(1996:2011,ges$recr_flu_aj_m3,lty=2,type="b",col="black",lwd=2)
symbols(x=2007,y=(ges$eclm*10^6)[12],squares=0.3,add=TRUE,inches=F,bg="grey")
points(1996:2011,ges$recr_flu_aj_m4*25,lty=2,pch=17,type="b",col="black")
symbols(x=(1996:2011)[ges$pecxp>0],y=(ges$pecxp*10^6)[ges$pecxp>0],
		stars=matrix(rep(c(0.5,0.1,0.5,0.1,0.5,0.1,0.5,0.1,0.5,0.1)/2,7),7,10),add=TRUE,inches=F,bg="grey")
dev.off()

x11()
y <- sort(10*runif(10))
z <- runif(10)
z3 <- cbind(z, 2*runif(10), runif(10))
symbols(x, y, thermometers = cbind(.5, 1, z), inches = .5, fg = 1:10)
symbols(x, y, thermometers = z3, inches = FALSE)
text(x,y, apply(format(round(z3, digits=2)), 1, paste, collapse = ","),
		adj = c(-.2,0), cex = .75, col = "purple", xpd = NA)



barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2011,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2")
rect(xleft=c(3.8,5,7.4,8.6,9.8,11.01,18.2), ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10,16)], xright=c(4.3,5.5,7.9,9.1,10.3,11.5,18.7), ytop=(ges$recr_flu_aj_m2/1e6)[c(4,5,7,8,9,10,16)],col="slateblue1",lty=2) 
rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5,18.7), ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10,16)], xright=c(4.8,6,8.4,9.6,10.8,12,19.2), ytop=(ges$recr_flu_aj_m1/1e6)[c(4,5,7,8,9,10,16)],col="skyblue",lty=2) 

#barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
#barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2011,col="red",add=TRUE)
points((c(0.5:13.5)+0.2*(c(1:14)))[c(1:8,10,12,14)],(flu$denscs[3:16])[-c(9,11,13)],pch=19,col="green",type="b")
points((c(0.5:13.5)+0.2*(c(1:14)))[c(1:8,10,12,14)],(flu$denscs[3:16])[-c(9,11,13)],pch=19,col="violetred4",type="p",cex=1.8)
points((c(0.5:13.5)+0.2*(c(1:14)))[c(1:8,10,12,14)],(flu$denscs[3:16])[-c(9,11,13)],pch=19,col="limegreen",type="p",cex=1.2)

legend("topright",legend=iconv(c("recrutement fluvial passe",
						"recrutement fluvial transport, survie 50% / passe",
						"recrutement fluvial transport, survie 100%",
						"recrutement fluvial anguilles jaunes"),"utf8"),
		fill=c("blue","slateblue1","skyblue","red"),				
				bty="n")
legend(6,2,legend=iconv("densités en pêche électrique Y+2","utf8"),col="limegreen",pch=19,bty="n")
########################################
## premier graphique de l'animation
##########################################
## les fichiers .png permettent de gérer la transparence
#png(filename = "C:/Documents and Settings/cedric/Mes documents/Migrateur/programmes/workspace3.5/various/transport1.png", width = 800, height = 600, bg = "transparent")
#par(fg="white",col.axis="white",col.lab="white",col.main="white",cex=2)
#barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
#rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5)-0.3, ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12)-0.2, ytop=(ges$recr_flu_aj_m1/1e6)[c(4,5,7,8,9,10)],col="skyblue") 
##barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
##barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
#barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
#points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
#points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
#par(fg="black",col.axis="black",col.lab="black",col.main="black",cex=1)
#dev.off()
## returns to default setting
########################################
## deuxième graphique de l'animation
##########################################
## les fichiers .png permettent de gérer la transparence
#png(filename = "C:/Documents and Settings/cedric/Mes documents/Migrateur/programmes/workspace3.5/various/transport2.png", width = 800, height = 600, bg = "transparent")
#par(fg="white",col.axis="white",col.lab="white",col.main="white",cex=2)
#barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
#rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5)-0.3, ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12)-0.2, ytop=(ges$recr_flu_aj_m2/1e6)[c(4,5,7,8,9,10)],col="skyblue") 
##barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
##barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
#barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
#points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
#points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
#par(fg="black",col.axis="black",col.lab="black",col.main="black",cex=1)
#dev.off()
## returns to default setting
########################################
## troisième graphique de l'animation
##########################################
## les fichiers .png permettent de gérer la transparence
#png(filename = "C:/Documents and Settings/cedric/Mes documents/Migrateur/programmes/workspace3.5/various/transport3.png", width = 800, height = 600, bg = "transparent")
#par(fg="white",col.axis="white",col.lab="white",col.main="white",cex=2)
#barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
##rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5)-0.3, ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12)-0.2, ytop=(ges$recr_flu_aj_m2/1e6)[c(4,5,7,8,9,10)],col="skyblue") 
##barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
##barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
#barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
#points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
#points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
#par(fg="black",col.axis="black",col.lab="black",col.main="black",cex=1)
#dev.off()
## returns to default setting
## analyse des 0 et 1
## La correlation croisée est maximale avec 1 an de délai pour les zero un an (bonne nouvelle !)
#ccf(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m1[2:13][c(1:8,10,12,14)]) # 1 year lag 
#cor(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m1[2:13][-c(7,9,11)]) # 0.6
#cor(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m2[2:13][-c(7,9,11)]) # 0.53
#cor(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m3[2:13][-c(7,9,11)]) # 0.44
#
#plot(flu$year[1:14],(flu$denscs1[1:14]),pch=19,col="green",type="b")
#plot(flu$year[2:13][-c(9,11)],(flu$denscs1[3:14]/100)[-c(9,11)],pch=19,col="black",type="b")
#
#points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
#
