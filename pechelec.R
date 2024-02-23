### R code from vignette source 'pechelec.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: init
###################################################
rm(list=ls(all=TRUE)) # nettoyage complet
graphics.off()
x11()
load_library=function(necessary) {
	if(!all(necessary %in% installed.packages()[, 'Package']))
		install.packages(necessary[!necessary %in% installed.packages()[, 'Package']], dep = T)
	for(i in 1:length(necessary))
		library(necessary[i], character.only = TRUE)
}
setwd("C:/Users/cedric.briand/Documents/workspace/p/pechelec")
load_library('Hmisc')
load_library('xtable')
load_library('stacomirtools')
load_library('FSA') #install.packages('FSA',,'http://www.rforge.net/') citation("FSA")
load_library("stringr")
load_library("multcomp") # pour post hoc tests
load_library("car") # Pour CrPlots
load_library("stargazer")
load_library('lubridate')
load_library('reshape2')
load_library('dplyr')
#'==========================================================
#' fonction de nettoyage du code latex
#'==========================================================

sanitizeLatexS <- function(str) {
	gsub('([#$%&~_\\^\\\\{}])', '\\\\\\\\\\1', str, perl = TRUE);
}
#'==========================================================
#' fonction d'impression des nombres
#'==========================================================

sn <- function(x,digits=0,scientific=FALSE)
{
	if (class(x)=="character") {                
		warning("sn appliqué a un character")
		return(x)
	}
	if (is.infinite(x)) {                
		warning("sn appliqué à Inf")
		return(x)
	}
	if (length(x)==0) {                
		warning("sn length 0")
		return("???")
	}
	if (x==0) return("0")
	ord <- floor(log(abs(x),10))
	if (scientific==FALSE&ord<9){
		if (digits==0) {
			digits=max(1,ord) # digits must be >0
			nsmall=0
		}else {
			nsmall=digits
		}
		x<-format(x,big.mark="~",small.mark="~",digits=digits,nsmall=nsmall)
		return(str_c("$",as.character(x),"$"))                
	} else {
		x <- x / 10^ord
		if (!missing(digits)) x <- format(x,digits=digits)
		if (ord==0) return(as.character(x))
		return(str_c("$",x,"\\\\times 10^{",ord,"}$"))
	}
}
#'==========================================================
#' fonction d'application de la transparence à des couleurs.
#'==========================================================
makeTransparent = function(..., alpha=0.5) {
	
	if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
	
	alpha = floor(255*alpha)  
	newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
	
	.makeTransparent = function(col, alpha) {
		rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
	}
	
	newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
	
	return(newColor)
	
}


###################################################
### code chunk number 2: tableau1
###################################################
rec<-data.frame("Annee"=
				c(1981,1998:2013),
		"Capt(t)"=
				c(57,17.5,
						15.3,
						14.2,
						8.16,
						15.8,
						8.9,
						7.0,
						6.8,
						6.1,
						6.9,
						4.2,
						2.6,
						3.0,
						3.9,
						2.99,
						2.1
				))
# pour l'année prochaine adapter en virant le ,17
res<-cbind(rec[c(1:6),],rec[7:12,],rec[c(13:17,17),])
res[nrow(res),c(5:6)]<-c("","")
colnames(res)<-rep(c("Annee","Capt (t)"),3)
xtarec<-xtable(res,
		label="tabrec",
		caption=c("Série de captures de la pêcherie professionnelle en Vilaine. 
						A partir de 2010, l'introduction des quotas a modifié la série de données."),
		digits=c(0,0,1,0,1,0,1)
)
print(xtarec,file="data/tarec.tex",
		table.placement="htbp",
		caption.placement = "top",
		NA.string = "",
		include.rownames=FALSE)	


###################################################
### code chunk number 3: figure2recrutement
###################################################
orangetr<-makeTransparent("orange",alpha=0.2)
ges<-read.table("C:/Users/cedric.briand/Documents/workspace/p/export_gestion.csv",sep=";",header=TRUE)
load("C:/Users/cedric.briand/Documents/workspace/p/pechelec/data/spl.Rdata")
load("C:/Users/cedric.briand/Documents/workspace/p/pechelec/data/dens.Rdata")
# Fonction de calcul du recrutement fluvial en fonction d'hypothèsese de survie
#' parametre mortpas => mortalité des civelles sur la passe
#' parametre mortxp => mortalité des civelles transportées 
fnrec<-function(mortpas,mortxp,mortecl){
# In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
	recflu<-(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortecl))*1e6+ges$aj*1000
	return(recflu)
}
# A FAIRE METTRE UN VECTEUR DE MORTALITE EN PRENANT LES MORTALITES REPEUPLEMENT.
ges$recr_flu_aj_m1<-fnrec(mortpas=0,mortxp=0,mortecl=0)
ges$recr_flu_aj_m2=fnrec(mortpas=0,mortxp=0.5,mortecl=0)
ges$recr_flu_aj_m3=fnrec(mortpas=0,mortxp=1,mortecl=1)
ges$yellow=fnrec(mortpas=1,mortxp=1,mortecl=1)

pdf(file="image/figure2recrutement.pdf",width=7,height=6)
par("mar"=c(5.1,5.1,4.1,5.1))
x<-1996:2013
y<-ges$recr_flu_aj_m1
l<-length(x)
mino<-0
maxo<-max(y)
# Graphique du recrutement fluvial net
plot(x,y,xlab="annee", type="n",xaxt="n",ylab=expression("Civelles x " * 1000),yaxt="n",frame = FALSE,col="grey60",lwd=2)
xx <- c(x, rev(x))
yy <- c(rep(0,l), rev(y))   
polygon(xx, yy, col="light green", border=FALSE)
lines(x,y, lwd=2.5,col="dark green")
at<-seq(0,8,by=0.5)
at3<-at*10^6
at2<-at*10^3
at1<-round(at3/25000)
axis(1, at = x)
axis(2, at = at3, labels = formatC(at2, big.mark = " ", format = "d"), 
		las = 3)
axis(side=4,at=at3,labels=at1,col="blue",col.axis="blue",col.lab="blue")
abline(h=at3,col="grey70")
debut <- 2008.5
fin <- 2011.5
polygon(c(debut, debut, fin, fin), c(maxo, mino, mino, maxo), col="#FA050535", border=FALSE)
text(fin-(fin-debut)/2, 8*maxo/10, iconv("arrêt \n 31/03","UTF8"), col="red")

debut <- 2005
fin <- 2008.5
polygon(c(debut, debut, fin, fin), c(maxo, mino, mino, maxo), col="#0015FF25", border=FALSE)
text(fin-(fin-debut)/2, 7*maxo/10, iconv("Diminution \n saison","UTF8"), col="dark blue")

debut <- 2011.5
fin <- 2013
polygon(c(debut, debut, fin, fin), c(maxo, mino, mino, maxo), col=orangetr, border=FALSE)
text(fin-(fin-debut)/2, 9*maxo/10, "Quota \n et \n transports", col="#FB861B")

mtext(expression("Anguille jaunes x  " * 1000),4,line=3,col="blue")

points(x,ges$recr_flu_aj_m3,lty=2,type="b",col="black",lwd=2)
symbols(x=c(2007,2012,2013),y=(ges$eclm*10^6)[c(12,17,18)],squares=c(0.3,0.3,0.3),add=TRUE,inches=F,bg=c("white","white","white"))
points(x,ges$yellow*25,lty=2,pch=17,type="b",col="blue")
symbols(x=x[ges$pecxp>0],y=(ges$pecxp*10^6)[ges$pecxp>0],
		stars=matrix(rep(c(0.7,0.1,0.7,0.1,0.7,0.1,0.7,0.1,0.7,0.1)/2,9),9,10),add=TRUE,inches=F,bg="yellow")
dev.off()
recciv<-ges$recr_flu_aj_m1/10^6
quelcmin<-which.min(recciv)
quelcmax<-which.max(recciv)
quelymin<-which.min(ges$yellow)
quelymax<-which.max(ges$yellow)



###################################################
### code chunk number 4: chargement
###################################################
anneeencours<-2013
# main_pechelec.R 
# Lancement principal du traitement des données de pêche électrique pour le rapport
# Appelle traitement_structure_taille.R pour construire un tableau des structures de taille et d'âge a  partir des données de wama (poissons et opérations)
# Rassemble sous R tous les tableaux et graphiques qui avant 2011 étaient faits a  partir d'une feuille excel.
# et d'une clé taille age dont le fichier se trouve dans le répertoire pechelec
# Utilise la librairies FSA pour réaliser le calcul des densités de Carle et Strubb
# Author: cedric.briand, Brice Sauvaget; 
###############################################################################
#  install.packages('FSA',,'http://www.rforge.net/') 

#setwd("C:/Users/cedric.briand/Documents/workspace/p/"
# A RELANCER SI ON FAIT RETOURNER LE SCRIPT ET POUR DES NOUVELLES DONNEES
#source("pechelec/traitement_structure_taille.R") # creation d'un tableau pec
#save(pec,file="pechelec/pec.Rdata")
load(file="data/pec.Rdata")
#nrow(pec)#253 #257
# quelles sont les stations "gardée"
# calcul des densites de Carle et Strubb
stationsrepetees<-as.character(pec$stcod)[strftime(pec$opdtd,"%Y")==2007]
pec<-pec[as.character(pec$stcod)%in%stationsrepetees,]
#nrow(pec)#
pec$annee<-strftime(pec$opdtd,"%Y")
pec$mois<-strftime(pec$opdtd,"%m")
#pec[pec$annee==2002,"stcod"][order(pec[pec$annee==2002,"stcod"])]
#pec[pec$annee==2003,"stcod"][order(pec[pec$annee==2003,"stcod"])]
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
pec$eff=pec$Eff1/pec$NCS # effectif du premier passage divisé par le nombre total trouvé
pec$Eff3calcul<-pec$Eff3
pec$Eff3calcul[is.na(pec$Eff3calcul)]<-0# effectif du troisième passage
pec$biomCS<-pec$poidstotal*(pec$NCS/(pec$Eff1+pec$Eff2+pec$Eff3calcul))/(pec$oplng*pec$oplam)

pec[,c("age0","age1","age2","age3","age4")]<-pec[,c("age0","age1","age2","age3","age4")]*(pec$NCS/((pec$Eff1+pec$Eff2+pec$Eff3calcul)*pec$oplng*pec$oplam))
library(ggplot2)
library(lattice)

##################################################################
# MERGE AVEC LE FICHIER DES STATIONS QUI CONTIENT LES DISTANCES
##################################################################
stations=read.table("data/code_stations.csv",sep=";",header=TRUE,colClasses=c(rep("character",4),"numeric"))
dens<-merge(pec,stations,by="stcod")
#head(dens)
dens$classdist<-cut(dens$distance_mer,breaks=c(0,50,100,300))
levels(dens$classdist)<-c("<50rkm","50-100rkm",">100rkm")

#save(dens,file="data/dens.Rdata")
#write.table(dens,file="data/dens.csv",sep=";")
#load(file="data/dens.Rdata")
dens<-chnames(dens,"code_iav","station")


###################################################
### code chunk number 5: tableau2_dens_eff_annee
###################################################
#####################################
# Pour récupérer le tableau 1, t.test permet de récupérer les intervalles de confiance
#####################################
#str(t.test(pec$densCS))
#t.test(pec$densCS)$conf.int[2]-t.test(pec$densCS)$estimate
(tableau2<-data.frame(
					"Nb"=tapply(pec$densCS,pec$annee,function(X)length(X)),
					"Annee"=unique(pec$annee),
					"$D$"=round(tapply(pec$densCS,pec$annee,mean),2),
					#"Ecart type densite"=round(tapply(pec$densCS,pec$annee,function(X)sd(X)),2),
					"$IC~D$"=round(tapply(pec$densCS,pec$annee,function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2),
					"$\\Phi$"=round(tapply(pec$eff,pec$annee,mean),2),
					#"Ecart type efficacite"=round(tapply(pec$eff,pec$annee,function(X)sd(X)),2),
					"IC~$\\Phi$"=round(tapply(pec$eff,pec$annee,function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2),
					"$B_e$"=round(tapply(pec$biomCS,pec$annee,mean),2), 
					"$IC~B_e$"=round(tapply(pec$biomCS,pec$annee,function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2)
			))
colnames(tableau2)<-c("Nb","annee","$D$","$IC~D$","$\\Phi$","$IC~\\Phi$","$B_e$","$IC~B_e$")
xtableau2<-xtable(tableau2,
		label="tableau2_dens_eff_annee",
		caption=c(str_c("Densités ($D$) et biomasses moyennes $B_e$ en anguilles (méthode Carle et Strub) et efficacités de pêche $\\Phi$ calculées pour
								les 19 stations prospectées entre 1998 et ",anneeencours,". Les intervalles de confiance (IC) sont
								à 0.05."),"Densités et biomasses.") )
print(xtableau2,file="data/tableau2_dens_eff_annee.tex",
		table.placement="htbp",
		caption.placement = "top",
		NA.string = "",
		include.rownames=FALSE,
		sanitize.colnames.function = identity)

#!!!! NOTE A CE STADE VERIFIER LE NOMBRE DE STATIONS.... ET LES DONNEES. 


###################################################
### code chunk number 6: analysestat (eval = FALSE)
###################################################
## #############################
## # Un modèle très simple pour voir les tendances
## #############################
## dens$station<-as.factor(dens$station)
## dens$mois2<-"oct_nov"
## dens$mois2[as.numeric(dens$mois)<10]<-"aout_sept"
## #densityplot(pec$densCS)
## shapiro.test(dens$densCS) # W = 0.7237, p-value < 2.2e-16      #   Performs the Shapiro-Wilk test of normality.  p<0.1 données non normales
## shapiro.test(log(dens$densCS)) # W = 0.9914, p-value = 0.3442 
## # il faut faire une log transformation pour avoir des données normalisées
## dens$annee=as.factor(dens$annee)
## summary(glmdens<-glm(log(densCS)~station+annee+mois2,data=dens))
## 
## crPlot(model=glmdens,terms=~annee,main="Component residual plot for glm (log(densite) ~ station + annee",variable="annee")
## #crPlot(model=glmdens,terms=~mois,main="Component residual plot for glm (log(densite) ~ station + annee + mois",variable="mois")
## 
## drop1(glmdens) # le principal effet est l'effet station
## anova(glmdens,test="F") # les deux facteurs sont significatifs
## 
## #DévianceExpliquée=1-DévianceRésiduelle/DévianceNulle
## #Le degré de concordance entre les observations et les valeurs ajustées par le modèle (Pearce & Ferrier 2000) est mesuré
## #Plus un modèle explique convenablement les variations de la réponse et plus la proportion de déviance expliquée est proche de 1.
## 
## 
## 
## ################################################################
## # Analyse des résidus
## ################################################################
## dens$resid<-resid(glmdens)
## pdf(file="image/residus.pdf",height=5,width=7)
## plot(resid~station,data=dens,ylab="residus")
## dev.off()
## plot(resid~annee,data=dens,ylab="residus")
## # problème de résidus pas distribués normalement
## ################################################################
## # gls
## ################################################################
## library(nlme)
## 
## m.lm<-gls(log(densCS)~station+annee+mois2,data=dens)
## summary(m.lm)
## varianceState<-varIdent(form=~1|station)
## m.gls1<-gls(log(densCS)~station+annee+mois2,weights=varianceState,data=dens)
## # l'AIC du modèle avec une variance par site est meilleure
## anova(m.gls1,m.lm)
## # le test du rapport de vraissemblance indique que le modèle avec une variance différente
## # par sites et meilleur ce qui conduit à rejetter l'hypothèse que les variances sont toutes égales.
## summary(m.gls1)
## E<-resid(m.gls1)
## coplot(E~annee|station,data=dens) # il n'y a plus de problème
## m.gls2<-gls(log(densCS)~station+mois2,weights=varianceState,data=dens)
## anova(m.gls2,m.gls1) # pas mieux bien sûr
## AIC(m.gls2,m.gls1)
## m.gls3<-gls(log(densCS)~annee+riviere,weights=varianceState,data=dens)
## anova(m.gls3,m.gls1) # pas mieux bien sûr
## m.gls4<-gls(log(densCS)~station+annee,weights=varianceState,data=dens)
## AIC(m.gls4,m.gls1)
## plot(m.gls4)
## star<-stargazer(m.lm,m.gls1,m.gls2,m.gls4, title="Résultats de la régression,
## 				comparaison des modèles, (1) modèle linéaire $\\log (D){\\approx}a+s+m+\\epsilon$ avec $\\epsilon_s=N(0,{\\sigma}^2) $
## 				, (2) meilleur modèle (mixte) $\\log (D){\\approx}a+s+m+\\epsilon_s$ avec $\\epsilon_s=N(0,{\\sigma_{s}}^2) ~s=1,\\dots,19$,
## 				(3) $\\log (D){\\approx}s+m+\\epsilon_s$,
## 				(4) $\\log (D){\\approx}a+s+\\epsilon_s$",
## 		single.row=TRUE,
## 		label="annex_regression_result")
## cat(star,file="data/star.tex",sep="\n")
## ############################################################
## # Comparaison multiple avec gls
## ############################################################
## # j'ai trouvé sur internet
## model.matrix.gls <- function(object, ...) {
## 	model.matrix(terms(object), data = getData(object), ...)} 
## model.frame.gls <- function(object, ...) {model.frame(formula(object), data = getData(object), ...) }
## terms.gls <- function(object, ...) {
## 	terms(model.frame(object),...) }
## dens.glht<-glht(m.gls1,linfct=mcp(annee ="Tukey"))
## # Je prends le modèle le plus simple parce que glht est mal équipé pour gérer les effets des autres facteurs
## summary(dens.glht)
## dens.cld<-cld(dens.glht) # compact letter display seuil 0.05
## save(dens.cld,file="data/dens.cld.Rdata")
## save(m.gls1, file="data/m.gls1.Rdata")
## 


###################################################
### code chunk number 7: figure7_anx_glht
###################################################
pdf(file="image/clddensiteannuelle.pdf",height=5,width=7)
load(file="data/dens.cld.Rdata") # sauvé depuis l'étape précédente
old.par <- par( mai=c(1,1,2,1))
plot(dens.cld,
		col=c("green", "#98ED5E", "green","#98ED5E","green","turquoise","slateblue","darkblue","blue","darkblue","blue"),
		ylab="log (densite)")
par(old.par)
dev.off()


###################################################
### code chunk number 8: figure_densite_annee
###################################################
########################################
# Figure Tendance des densités en fonction des années test de comparaison des groupes
# graphique identique à figure 7 mais en ggplot et dans un format non log
########################################
load("data/m.gls1.Rdata")
coef<-summary(m.gls1)$coefficient
coef<-as.matrix(coef)[c(1,grep("annee",names(coef))),1]
coef<-coef[1]+coef[2:length(coef)]
coef=c(coef,"annee1998"=0)
df_coef<-data.frame("annee"=gsub("annee","",names(coef)),"coefficient"=coef)
dens$resid<-resid(m.gls1)
densmod<-merge(dens,df_coef,by="annee") ;rm("df_coef","coef")
# component + residual
densmod$cr=exp(densmod$coefficient+densmod$resid)
df_letter<-dens.cld$mcletters[["Letters"]]
df_letter<-data.frame("annee"=names(df_letter),"group"=df_letter)
densmod<-merge(densmod,df_letter,by="annee");rm(df_letter)
pdf(file="image/dens_annee.pdf",height=5,width=7)
g<-ggplot(densmod,aes(x=annee,y=densCS))
g+geom_point(col="grey40",size=1.8)+
		geom_boxplot(aes(fill=group),alpha=0.5)+
		scale_fill_brewer(name="group",palette="Set1")+
		scale_y_continuous(name=expression(paste(densite~ang.m^-2)))
		#ggtitle("Tendance des densités en fonction des années")
dev.off()


###################################################
### code chunk number 9: figure_densite_annee_dist
###################################################
########################################
#  Figure Tendance des densités en fonction des années et de la distance mer
########################################
pdf(file="image/dens_annee_dist.pdf",height=5,width=7)
g<-ggplot(dens,aes(x=annee,y=densCS))
g+geom_boxplot(aes(fill=annee))+
		facet_grid(~ classdist)+
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(densite~ang.m^-2)))+
		geom_hline(yintercept=0.3,col="red")+
		#annotate("text",x=1,y=0.25,label="cible de gestion",col="red")+
		#ggtitle("Densités en fonction des années et de la distance mer")+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


###################################################
### code chunk number 10: table_densite_annee_dist
###################################################
# TODO
# densitéCS et IC Ã  0.05
me<-round(tapply(dens$densCS,list(dens$classdist,dens$annee),mean),2)
iconf<-round(tapply(dens$densCS,list(dens$classdist,dens$annee),function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2)
tableau3<-t(me)
for (i in 1:nrow(me)){
	for (j in 1:ncol(me)){
		tableau3[j,i]<-str_c(me[i,j],"(",iconf[i,j],")")
	}
}
tableau3<-as.data.frame(tableau3)
tableau3$annee<-dimnames(tableau3)[[1]]
tableau3<-tableau3[,c(4,1,2,3)]
xtableau3<-xtable(tableau3,
		label="tableau3_dens_dist_annee",
		caption=c(str_c("Densités moyennes en anguilles (en anguille.m$^{-2}$) en fonction de la distance + - intervalles de confiance à 0.05.")
				,"Densité et distance.") )
print(xtableau3,file="data/tableau3_dens_dist_annee.tex",
		table.placement="htbp",
		caption.placement = "top",
		NA.string = "",
		include.rownames=FALSE,
		sanitize.colnames.function = identity)


###################################################
### code chunk number 11: figurebiom_annee
###################################################
########################################
#  Figure  Tendance des biomasses en fonction des années
########################################
pdf(file="image/biom_annee.pdf",height=5,width=7)
g<-ggplot(dens,aes(x=annee,y=biomCS))
g+geom_boxplot(aes(fill=annee))+
		
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(biomasse~g.m-2)))#+
		#annotate("text",x=1,y=0.25,label="cible de gestion",col="red")+
		#ggtitle("Tendance des biomasses en fonction des années")
dev.off()


###################################################
### code chunk number 12: figure_biom_annee_dist
###################################################
########################################
#  Figure  Tendance des biomasses en fonction des années et de la distance mer
########################################
pdf(file="image/biom_annee_dist.pdf",height=5,width=7)
g<-ggplot(dens,aes(x=annee,y=biomCS))
g+geom_boxplot(aes(fill=annee))+
		facet_grid(~ classdist)+
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(biomasse~g.m-2)))+
		#annotate("text",x=1,y=0.25,label="cible de gestion",col="red")+
		#ggtitle("Tendance des biomasses en fonction des années")+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


###################################################
### code chunk number 13: table_biom_annee_dist
###################################################
# biomasseCS et IC Ã  0.05
me<-round(tapply(dens$biomCS,list(dens$classdist,dens$annee),mean),2)
iconf<-round(tapply(dens$biomCS,list(dens$classdist,dens$annee),function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2)
tableau4<-t(me)
for (i in 1:nrow(me)){
	for (j in 1:ncol(me)){
		tableau4[j,i]<-str_c(me[i,j],"(",iconf[i,j],")")
	}
}
tableau4<-as.data.frame(tableau4)
tableau4$annee<-dimnames(tableau4)[[1]]
tableau4<-tableau4[,c(4,1,2,3)]
xtableau4<-xtable(tableau4,
		label="table_biom_dist_annee",
		caption=c(str_c("Biomasses moyennes en anguilles (en g.m$^{-2}$) en fonction de la distance + - intervalles de confiance à 0.05.")
				,"Biomasse et distance.") )
print(xtableau4,file="data/table_biom_dist_annee.tex",
		table.placement="htbp",
		caption.placement = "top",
		NA.string = "",
		include.rownames=FALSE,
		sanitize.colnames.function = identity)


###################################################
### code chunk number 14: figure_densite_age_annee
###################################################
########################################
#  Figure Tendance des densités en fonction de l'âge
########################################
data_age<-melt(dens[,c("Opcod","annee","classdist","age0","age1","age2","age3","age4")],id.vars=c("Opcod","annee","classdist"),variable_name = "age")
colnames(data_age)<-c("Opcod","annee","classdist","age","densiteCS")
data_age$age<-as.factor(data_age$age)
levels(data_age$age)<-c("0","1","2","3","4+")
pdf(file="image/dens_age_annee.pdf",height=5,width=7)
g<-ggplot(data_age,aes(x=age,y=densiteCS))
g+geom_boxplot(aes(fill=annee))+		
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(densite~ang.m^-2)))#+
		#annotate("text",x=1,y=0.25,label="cible de gestion",col="red")+
		#ggtitle("Tendance des densités en fonction de l'âge")
dev.off()


###################################################
### code chunk number 15: table_densite_age_annee
###################################################
# densitéCS et IC Ã  0.05
me<-round(tapply(data_age$densiteCS,list(data_age$age,data_age$annee),mean),2)
iconf<-round(tapply(data_age$densiteCS,list(data_age$age,data_age$annee),function(X) t.test(X)$conf.int[2]-t.test(X)$estimate),2)
tableau5<-t(me)
for (i in 1:nrow(me)){
	for (j in 1:ncol(me)){
		tableau5[j,i]<-str_c(me[i,j],"(",iconf[i,j],")")
	}
}
tableau5<-as.data.frame(tableau5)
tableau5$annee<-dimnames(tableau5)[[1]]
tableau5<-tableau5[,c(6,1,2,3,4,5)]
xtableau5<-xtable(tableau5,
		label="table_densite_age_annee",
		caption=c(str_c("Densité moyennes en anguilles (en anguille.m$^{-2}$) en fonction de la distance à la mer et de l'âge + - intervalles de confiance à 0.05.")
				,"Densité âge et distance.") )
print(xtableau5,file="data/table_densite_age_annee.tex",
		table.placement="htbp",
		caption.placement = "top",
		NA.string = "",
		include.rownames=FALSE,
		sanitize.colnames.function = identity)


###################################################
### code chunk number 16: figure_age_annee_dist
###################################################
########################################
# Figure  Tendance des densités en fonction de l'âge et de la distance mer
########################################
pdf(file="image/dens_age_annee_dist.pdf",height=5,width=7)
g<-ggplot(data_age,aes(x=age,y=densiteCS))
g<-g+
		geom_hline(yintercept=0.3,col="red")+
		geom_boxplot(aes(fill=annee))+	
		facet_grid(classdist~. )+
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(densite~ang.m^-2)),limits=c(0,1.5))#+
		#ggtitle("Tendance des densités en fonction de l'âge et de la distance à la mer")

print(g)
dev.off()


###################################################
### code chunk number 17: figure_anx_densite_annee_riviere
###################################################
########################################
# Figure (annexe ?) Tendance des densités en fonction des rivières
########################################
pdf(file="image/dens_annee_riviere.pdf",height=6,width=8)
g<-ggplot(dens,aes(x=annee,y=densCS))
g+geom_boxplot(aes(fill=annee))+	
		facet_wrap(~ riviere,scales="free_y")+
		scale_fill_brewer(name="group",palette="Set3")+
		scale_y_continuous(expression(paste(densite~ang.m-2)))+
		#ggtitle("Tendance des densités sur les différents affluents")+
		theme(axis.text.x=element_text(angle=90, hjust=0.5 )) 
dev.off()


###################################################
### code chunk number 18: marquage
###################################################
marq<-killfactor(read.table(file="data/marq.csv",header=TRUE,sep=";"))
marq<-marq[,-match("X",colnames(marq))]
marq$date<-dmy(marq$date)
marq$annee<-year(marq$date)
marq$recapt<-FALSE
marq<-marq[order(marq$date),]
marqr<-marq$marque[duplicated(marq$marque)]
marq$action<-"pose"
marq[duplicated(marq$marque),"action"]<-"lecture"
sum(marq$marque%in%marqr) #64
marq2<-marq[marq$marque%in%marqr,]
marq2<-marq2[order(marq2$marque,marq2$date),]
ta1<-table(marq$annee,marq$action)
ta2<-table(marq2$annee,marq2$action)
xta1<-xtable(ta1,
		label="tablemr",
		caption = c(
				str_c("marquage et recaptures"),
				str_c("marquage et recaptures")))
digits(xta1)<-c(0,0,0)
align(xta1) <- c("l","r","r")
print(xta1,
		file="data/tableaumr1.tex",
		table.placement="htbp",
		caption.placement = "top",
		NA.string = "",
		hline.after=c(-1,0,nrow(xta1))
		)
pdf(file="image/croissance.pdf",height=6,width=6)
ggplot(marq2)+geom_point(aes(y=taille,x=date,col=marque))+geom_path(aes(y=taille,x=date,col=marque))+theme_bw()+theme(legend.position = "none")+
		ylab("taille (mm)")
dev.off()
#table(marq$stade)
# table(marq$stade,marq$recapt) pas de recapture d'argentée
croissance<-dplyr::select(marq2,date,taille,marque,station)%.%group_by(marque)%.%
		summarize(station=unique(station),taille0=min(taille),croissance_mm=max(taille)-min(taille),duree_an=as.numeric(difftime(max(date),min(date),units="days")/365))%.%
		mutate(croissance_annuelle_mm=croissance_mm/duree_an,duree_an_round=as.factor(round(duree_an)))%.%
		arrange(desc(duree_an))
pdf(file="image/croissance2.pdf",height=6,width=6)
ggplot(croissance)+geom_point(aes(x=taille0,y=croissance_annuelle_mm,shape=duree_an_round),col="black",size=3.5)+
		geom_point(aes(x=taille0,y=croissance_annuelle_mm,shape=duree_an_round,col=station),size=2.5)+
		ylab("croissance en mm")+xlab("taille au moment du marquage (mm)")+
		scale_colour_brewer("station",palette="Paired")+
		scale_shape_manual("Nb années",values=c(16,17))+
		theme_bw()
dev.off()
t_test_croissance<-t.test(x=croissance$croissance_annuelle_mm[croissance$duree_an_round==2],
		y=croissance$croissance_annuelle_mm[croissance$duree_an_round==4])
#round(t_test_croissance$p.value,2)
mean(croissance$croissance_annuelle_mm)
TempSUP13<-79# (948/12)
Ratiodistsea<-0.75
# modèle femelle Daverat pour trois tailles d'anguilles
(G = c(exp(4.2299082
						+ 0.0075774*TempSUP13
						- 0.2506349*Ratiodistsea
						- 0.3139456), # 5
		exp(4.2299082
						+ 0.0075774*TempSUP13
						- 0.2506349*Ratiodistsea
						- 0.5395996), #10
		exp(4.2299082
						+ 0.0075774*TempSUP13
						- 0.2506349*Ratiodistsea
						- 0.9999416))) # 20
#mounaix fluvial
Linf=843
k=0.03
t0=-9.8
t<-c(1:20)
L=Linf*(1-exp(-k*(t-t0)))
L[2:20]-L[1:19]
#mounaix estuaire
Linf=1346
k=0.08
t0=-0.26
t<-c(1:20)
L=Linf*(1-exp(-k*(t-t0)))
L[2:20]-L[1:19]

txrecapt<-round(100*sum(ta1[2:nrow(ta1),1])/sum(ta1[1:(nrow(ta1)-1),2]))


###################################################
### code chunk number 19: brouillon (eval = FALSE)
###################################################
## # pour une rivière donnée
## denstrev<- dens[dens$riviere=="TREVELO",]
## x11()
## g<-ggplot(denstrev,aes(x=annee,y=densCS))
## g+geom_point(col="grey40",size=1.8)+
## 		geom_boxplot(aes(fill=annee),alpha=0.5)+
## 		scale_fill_brewer(name="group",palette="Set3")+
## 		scale_y_continuous(expression(paste("densité ang."," m-^²")))+
## 		opts(title="Tendance des densités sur le Trévelo entre 1998 et 2011")+
## 		opts(axis.text.x=theme_text(angle=90, hjust=0.5 ))
## 
## # densitéCS
## round(tapply(dens$densCS,list(dens$riviere,dens$annee),mean),2)
## 
## # Sauvegarde du data.frame dens
## save(dens, file="X:/Migrateur/Pechelec/2011/dens2013.Rdata")
## 
## write.table(pec,"data/pec.csv",sep=";")
## write.table(dens,"data/dens.csv",sep=";")


###################################################
### code chunk number 20: survies (eval = FALSE)
###################################################
## # *******************************************
## # Figure 7 SURVIE DES CIVELLES A PARTIR DES SUIVIS ARZAL
## # anal_gestion.R Ã  lancer pour le graphique de la relation recrutement-stock
## # préparation d'un tableau de synthèse ci dessous
## #******************************************
## # 
## # il est possible de relancer le script seulement Ã  partir de lÃ  en décommentant les lignes ci-dessous.
## 
## #pec<-read.table("pechelec/pec.csv",sep=";")
## #dens<-read.table("pechelec/dens.csv",sep=";")
## load(file="X:/Migrateur/Pechelec/2011/dens2011.Rdata")
## ges<-read.table("export_gestion.csv",sep=";",header=TRUE)
## # graphique qui ne presente aucun doute
## colnames(ges)
## #[1] "season" "year"      "rec"       "cpec"      "pass"      "sed"       "ech"      
## #[7] "tard"      "sr"        "eff"       "tx"        "trans"     "Netestu"  
## #[13] "passem"    "pecxp"     "eclm"      "recfluciv" "aj"        "ajcoh"  
## plot(ges$aj*10,type="b",col="blue")
## 
## # Calcul de survies probables pour différents scénarios de mortalité au stade civelle
## mortpas=0
## mortxp=mortpas
## 
## # In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
## ges$recr_flu_aj_m1=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000
## 
## mortxp=(1+mortpas)/2
## 
## # In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
## ges$recr_flu_aj_m2=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000
## 
## mortxp=1
## 
## # In the following we assume the same mortality rate of glass eel from the trapping ladder and of those coming form the sluice
## ges$recr_flu_aj_m3=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000
## 
## mortxp=1
## mortpas=1
## ges$recr_flu_aj_m4=(ges$passem*(1-mortpas)+ges$pecxp*(1-mortxp)+ges$eclm*(1-mortpas))*1e6+ges$aj*1000
## library(ggplot2)
## sumplussd=function(df,var) mean(df$"densCS",na.rm=TRUE)+sd(df$"densCS",na.rm=TRUE) 
## summinussd=function(df) mean(df$"densCS",na.rm=TRUE)-sd(df$"densCS",na.rm=TRUE)
## meanspl=function(df) mean(df$"densCS",na.rm=TRUE)
## meanspl1=function(df) mean(df[,c("1")],na.rm=TRUE)
## sumplussd1=function(df,var) mean(df$"1",na.rm=TRUE)+sd(df$"1",na.rm=TRUE) 
## summinussd1=function(df) mean(df$"1",na.rm=TRUE)-sd(df$"1",na.rm=TRUE)
## spl=ddply(dens,.variables=c("annee","classdist"),.fun=sumplussd)
## smi=ddply(dens,.variables=c("annee","classdist"),.fun=summinussd)
## colnames(spl)[3]<-"sumplussd"
## spl$summinussd=smi[,3]
## spl$denscs=ddply(dens,.variables=c("annee","classdist"),.fun=meanspl)[,3]
## spl$denscs1=ddply(dens,.variables=c("annee","classdist"),.fun=meanspl1)[,3]
## spl$summinussd1=ddply(dens,.variables=c("annee","classdist"),.fun=summinussd1)[,3]
## spl$sumplussd1=ddply(dens,.variables=c("annee","classdist"),.fun=sumplussd1)[,3]
## 
## save(spl,file="pechelec/spl.Rdata")
## 
## load("spl.Rdata") # spl issued from Traitement densite
## # for downstream sector
## spld<-spl[spl$classdist=="<50rkm",] # downstream
## spld<-spld[order(spld$annee),]
## gesflu=ges[,c("year","passem","pecxp","eclm","recfluciv","aj","recr_flu_aj_m1","recr_flu_aj_m2","recr_flu_aj_m3")] # fluvial data
## # replacing missing values for electrofishing data
## flu<-merge(gesflu,spld,by.x="year",by.y="annee",all.x=TRUE)
## # having missing data I will only assume that the densities were the mean of previous and next year
## for (i in c(9,11,13,15)){
## 	flu[i,"denscs"]<-mean(c(flu[i-1,"denscs"],flu[i+1,"denscs"]))
## }
## 
## scale=function(vec){
## 	vec/max(vec,na.rm=TRUE)
## }
## # CORRIGER POUR LES ANNEES OU IL N'Y A PAS DE DONNEES DE SUIVI
## ccf(flu$denscs[3:16],flu$recr_flu_aj_m1[1:14]) # 2 year lag 
## cor(flu$denscs[3:16],flu$recr_flu_aj_m1[1:14]) # 0.73
## cor(flu$denscs[3:16],flu$recr_flu_aj_m2[1:14]) # 0.77
## cor(flu$denscs[3:16],flu$recr_flu_aj_m3[1:14]) # 0.78
## ##########################
## # graphique du rapport
## ##########################
## require(grDevices)
## X11()
## par("mar"=c(5.1,5.1,4.1,5.1))
## plot(1996:2011,ges$recr_flu_aj_m3,xlab="annee",xaxt="n",ylab=expression("Civelles x " * 1000),type="b",yaxt="n",frame = FALSE,col="grey60",lwd=2)
## at<-seq(0,2.5,by=0.5)
## at3<-at*10^6
## at2<-at*10^3
## at1<-at3/25
## axis(1, at = 1996:2011)
## axis(2, at = at3, labels = formatC(at2, big.mark = " ", format = "d"), 
## 		las = 3)
## axis(side=4,at=at3,labels=at1)
## mtext("Anguilles jaunes",4,line=3)
## abline(h=at3,col="grey70")
## points(1996:2011,ges$recr_flu_aj_m3,lty=2,type="b",col="black",lwd=2)
## symbols(x=2007,y=(ges$eclm*10^6)[12],squares=0.3,add=TRUE,inches=F,bg="grey")
## points(1996:2011,ges$recr_flu_aj_m4*25,lty=2,pch=17,type="b",col="black")
## symbols(x=(1996:2011)[ges$pecxp>0],y=(ges$pecxp*10^6)[ges$pecxp>0],
## 		stars=matrix(rep(c(0.5,0.1,0.5,0.1,0.5,0.1,0.5,0.1,0.5,0.1)/2,7),7,10),add=TRUE,inches=F,bg="grey")
## dev.off()
## 
## x11()
## y <- sort(10*runif(10))
## z <- runif(10)
## z3 <- cbind(z, 2*runif(10), runif(10))
## symbols(x, y, thermometers = cbind(.5, 1, z), inches = .5, fg = 1:10)
## symbols(x, y, thermometers = z3, inches = FALSE)
## text(x,y, apply(format(round(z3, digits=2)), 1, paste, collapse = ","),
## 		adj = c(-.2,0), cex = .75, col = "purple", xpd = NA)
## 
## 
## 
## barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2011,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2")
## rect(xleft=c(3.8,5,7.4,8.6,9.8,11.01,18.2), ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10,16)], xright=c(4.3,5.5,7.9,9.1,10.3,11.5,18.7), ytop=(ges$recr_flu_aj_m2/1e6)[c(4,5,7,8,9,10,16)],col="slateblue1",lty=2) 
## rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5,18.7), ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10,16)], xright=c(4.8,6,8.4,9.6,10.8,12,19.2), ytop=(ges$recr_flu_aj_m1/1e6)[c(4,5,7,8,9,10,16)],col="skyblue",lty=2) 
## 
## #barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
## #barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
## barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2011,col="red",add=TRUE)
## points((c(0.5:13.5)+0.2*(c(1:14)))[c(1:8,10,12,14)],(flu$denscs[3:16])[-c(9,11,13)],pch=19,col="green",type="b")
## points((c(0.5:13.5)+0.2*(c(1:14)))[c(1:8,10,12,14)],(flu$denscs[3:16])[-c(9,11,13)],pch=19,col="violetred4",type="p",cex=1.8)
## points((c(0.5:13.5)+0.2*(c(1:14)))[c(1:8,10,12,14)],(flu$denscs[3:16])[-c(9,11,13)],pch=19,col="limegreen",type="p",cex=1.2)
## 
## legend("topright",legend=iconv(c("recrutement fluvial passe",
## 						"recrutement fluvial transport, survie 50% / passe",
## 						"recrutement fluvial transport, survie 100%",
## 						"recrutement fluvial anguilles jaunes"),"utf8"),
## 		fill=c("blue","slateblue1","skyblue","red"),				
## 		bty="n")
## legend(6,2,legend=iconv("densités en pêche électrique Y+2","utf8"),col="limegreen",pch=19,bty="n")
## ########################################
## ## premier graphique de l'animation
## ##########################################
## ## les fichiers .png permettent de gérer la transparence
## #png(filename = "C:/Documents and Settings/cedric/Mes documents/Migrateur/programmes/workspace3.5/various/transport1.png", width = 800, height = 600, bg = "transparent")
## #par(fg="white",col.axis="white",col.lab="white",col.main="white",cex=2)
## #barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
## #rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5)-0.3, ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12)-0.2, ytop=(ges$recr_flu_aj_m1/1e6)[c(4,5,7,8,9,10)],col="skyblue") 
## ##barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
## ##barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
## #barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
## #points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
## #points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
## #par(fg="black",col.axis="black",col.lab="black",col.main="black",cex=1)
## #dev.off()
## ## returns to default setting
## ########################################
## ## deuxième graphique de l'animation
## ##########################################
## ## les fichiers .png permettent de gérer la transparence
## #png(filename = "C:/Documents and Settings/cedric/Mes documents/Migrateur/programmes/workspace3.5/various/transport2.png", width = 800, height = 600, bg = "transparent")
## #par(fg="white",col.axis="white",col.lab="white",col.main="white",cex=2)
## #barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
## #rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5)-0.3, ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12)-0.2, ytop=(ges$recr_flu_aj_m2/1e6)[c(4,5,7,8,9,10)],col="skyblue") 
## ##barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
## ##barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
## #barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
## #points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
## #points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
## #par(fg="black",col.axis="black",col.lab="black",col.main="black",cex=1)
## #dev.off()
## ## returns to default setting
## ########################################
## ## troisième graphique de l'animation
## ##########################################
## ## les fichiers .png permettent de gérer la transparence
## #png(filename = "C:/Documents and Settings/cedric/Mes documents/Migrateur/programmes/workspace3.5/various/transport3.png", width = 800, height = 600, bg = "transparent")
## #par(fg="white",col.axis="white",col.lab="white",col.main="white",cex=2)
## #barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,col="blue",ylab="recrutement fluvial net (millions), densite anguille.m-2",ylim=c(0,1.4))
## ##rect(xleft=c(4.3,5.5,7.9,9.1,10.3,11.5)-0.3, ybottom=(ges$recr_flu_aj_m3/1e6)[c(4,5,7,8,9,10)], xright=c(4.8,6,8.4,9.6,10.8,12)-0.2, ytop=(ges$recr_flu_aj_m2/1e6)[c(4,5,7,8,9,10)],col="skyblue") 
## ##barplot(ges$recr_flu_aj_m2/1e6,names.arg=1996:2009,add=T,col="slateblue1")
## ##barplot(ges$recr_flu_aj_m3/1e6,names.arg=1996:2009,add=T,col="blue")
## #barplot(ges$recr_flu_aj_m4/1e6,names.arg=1996:2009,col="red",add=TRUE)
## #points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="green",type="b")
## #points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
## #par(fg="black",col.axis="black",col.lab="black",col.main="black",cex=1)
## #dev.off()
## ## returns to default setting
## ## analyse des 0 et 1
## ## La correlation croisée est maximale avec 1 an de délai pour les zero un an (bonne nouvelle !)
## #ccf(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m1[2:13][c(1:8,10,12,14)]) # 1 year lag 
## #cor(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m1[2:13][-c(7,9,11)]) # 0.6
## #cor(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m2[2:13][-c(7,9,11)]) # 0.53
## #cor(flu$denscs1[3:14][-c(7,9,11)],flu$recr_flu_aj_m3[2:13][-c(7,9,11)]) # 0.44
## #
## #plot(flu$year[1:14],(flu$denscs1[1:14]),pch=19,col="green",type="b")
## #plot(flu$year[2:13][-c(9,11)],(flu$denscs1[3:14]/100)[-c(9,11)],pch=19,col="black",type="b")
## #
## #points((c(0.5:11.5)+0.2*(c(1:12)))[c(1:8,10,12)],(flu$denscs[3:14]/100)[-c(9,11)],pch=19,col="darkgreen",type="p")
## #


