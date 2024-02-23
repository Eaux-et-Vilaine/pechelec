# 
# modifications du 27 octobre 2010
# Author: cedric et Gaëlle
###############################################################################
#Traitement des données de pêche électrique
#Etablissement de la relation EPA - Densités
# gaelle
#setwd("C:\\Users/B.G.M/Documents/ANGUILLE/IA/Relation EPA-Densite")
# la librairie FSA est pour l'instant à aller chercher à la main et il faut aussi installer
# les dépendances une par une.
# compatibilité R 2.11.1 seulement
# cedric 
library(stacomirtools)
require(stringr)
setwd("F:/workspace/p")
datawd<-"F:/workspace/pdata/pechelec/data/"
# récupération des données
library(FSA)
dens=read.csv(str_c(datawd,"RelationEPA_Densite_V3.csv"),sep=";")
dens<-dens[,-(match("surf_reel_2",colnames(dens)))] # la deuxième surface est liée à des arrondis sur la largeur et la longueur
dens$DensCSold<-dens$DensCS
####################################
# CALCUL DES DENSITES DE CARLE ET STRUBB
######################################
#Confidence intervals are computed using standard large-sample normal distribution theory. 
# Note that the confidence intervals for the 2- and 3-pass special cases are only approximately correct if the estimated population size is greater than 200. If the estimated population size is between 50 and 200 then a 95% CI behaves more like a 90% CI.

dens$NCS<-NA
dens$LCI<-NA
dens$UCI<-NA
dens$p<-NA
dens$Eff3<-NA
dens$Densp1<-dens$Eff1/dens$surf_reel
for (i in 1:nrow(dens)){
	if (!is.na(dens$Eff2[i])){
		if (is.na(dens$Eff3[i])){
			if (dens$Eff2[i]<dens$Eff1[i]){
				p<-removal(c(dens$Eff1[i],dens$Eff2[i]),type="Zippin")
				dens$Nzip[i]<-p$est["No"]
				dens$pzip[i]<-p$est["p"]
				dens$LCIzip[i]<-confint(p)[1,1]
				dens$UCIzip[i]<-confint(p)[1,2]	
			} else {
				dens$Nzip[i]<-NA
			}
		} else{
			p<-removal(c(dens$Eff1[i],dens$Eff2[i],dens$Eff3[i]),type="Zippin")
			dens$Nzip[i]<-p$est["No"]
			dens$pzip[i]<-p$est["p"]
			dens$LCIzip[i]<-confint(p)[1,1]
			dens$UCIzip[i]<-confint(p)[1,2]
		}		
	}
}
for (i in 1:nrow(dens)){
	if (!is.na(dens$Eff2[i])){
		p<-removal(c(dens$Eff1[i],dens$Eff2[i]),type="CarleStrub")
		dens$NCS[i]<-p$est["No"]
		dens$p[i]<-p$est["p"]
		dens$LCI[i]<-confint(p)[1,1]
		dens$UCI[i]<-confint(p)[1,2]
		
	} else {dens$NCS[i]<-NA
	}
}

plot(dens$Nzip,dens$NCS)

dens$DensCS<-100*dens$NCS/dens$surf_reel
dens$DensZip<-100*dens$Nzip/dens$surf_reel
#write.table(dens,file="RelationEPA_Densite_V4.csv",sep=";",row.names=FALSE,col.names=TRUE)
########################
# Analyse des données
#######################
# il semble y avoir un sérieux pb de densités.
dens=read.csv("RelationEPA_Densite_V4.csv",sep=";")
dens$DensCS=100*dens$DensCS
plot(dens$DensCSold,dens$DensCS)
abline(a=0,b=1)
points(dens$DensCSold,dens$NCS,col="red")
# les anciennes densités étaient des effectifs
print(cbind(100*dens$DensCS,dens$DensCSold))

#p2 <- removal(c(dens$Eff1[1],dens$Eff2[1]),type="CarleStrub")$est["No","Estimate"]
#summary(p2)
#confint(p2)

# Source.1 > données par association pour faire des statistiques détaillées notamment concernant la surface
# Source ("Bretagne","IAV","LOGRAMI")
# densp1 densite premier passage=Nb anguilles / surface de la station
# densCS densités calculées par la méthode de Carle et Strubb
# Surf réel surface de la station de pêche
# Surf théorique nbpoint * (1.5)²
# EPA 

summary(dens$EPA)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1200  0.3600  0.7392  0.8450  6.9200 
boxplot(dens$EPA) # clairement pas une distribution normale
dens=subset(dens,!is.na(dens$DensCS)&!is.na(dens$EPA))
#dens.IAV=subset(dens,dens$Source=="IAV"&dens$DensCS<150)
#dens.Bret=subset(dens,dens$Source=="Bretagne"&dens$DensCS<150)    
#dens.LOGRAMI=subset(dens,dens$Source=="LOGRAMI"&dens$DensCS<150)
# statistiques pour la publi (données nulles inclues)
nrow(dens)
length(dens$"Fishing_method"[dens$Source=="IAV"])
round(mean(dens$surf_reel[dens$Source=="IAV"]))
round(sd(dens$surf_reel[dens$Source=="IAV"]))
mean(dens$largeur..m.[dens$Source=="IAV"])
mean(dens$longueur..m.[dens$Source=="IAV"])
mean(dens$profondeur..cm.[dens$Source=="IAV"])/100
length(dens$"Fishing_method"[dens$Source!="IAV"])
round(mean(dens$surf_reel[dens$Source!="IAV"]))
round(sd(dens$surf_reel[dens$Source!="IAV"]))
mean(dens$largeur..m.[dens$Source!="IAV"])
mean(dens$longueur..m.[dens$Source!="IAV"])
mean(dens$profondeur..cm.[dens$Source!="IAV"])/100

# statistiques pour la publi (données non nulles inclues)
dens$lDensCS=log(dens$DensCS)
dens$lEPA=log(dens$EPA)

denslog=subset(dens,!is.infinite(dens$lDensCS)&!is.infinite(dens$lEPA))
length(denslog$"Source"[denslog$Source=="IAV"])
length(denslog$"Source"[denslog$Source!="IAV"])

# Pour la publi, graphique avec les données correctes...
dens$rap_surf=dens$surf_reel/dens$surf_theo
dens$Surface<-"chevauchement"
dens$Surface[dens$rap_surf>1.4]<-"sans chevauchement"
dens$"Fishing_method"=as.character(dens$Source)
dens$"Fishing_method"[dens$Source=="SD22"]<-"EPA-2p"
dens$"Fishing_method"[dens$Source=="LOGRAMI"]<-"EPA-2p"
dens$"Fishing_method"[dens$Source=="Bretagne"]<-"EPA-2p"
dens$"Fishing_method"[dens$Source=="IAV"]<-"EPA+CC-2p"
nrow(dens) # 83
ex(dens)
##################################
#Analyse de l'efficacité de pêche
#################################"
x11()
dens$Efficacite=dens$Eff1/dens$NCS
summary(dens$Efficacite)
library(ggplot2)
g<-ggplot(data=dens)
g<-g+stat_boxplot(aes(x=Source,y=Efficacite))
g+geom_point(aes(x=Source,y=Efficacite))
tapply(dens$Efficacite,dens$Source,function(X)median(X,na.rm=TRUE))
tapply(dens$Efficacite,dens$Source,function(X)median(X,na.rm=TRUE))
# les medianes des efficacités de pêche sont 0.45,0.5,0.58,0.6, et 1 respectivement
#FD22    FD29     IAV LOGRAMI    SD22 
# 0.585   0.450   0.500   1.000   0.600 
# elles sont correlées avec les densités sur la station
g<-ggplot(data=dens)
g+stat_boxplot(aes(x=Source,y=Efficacite))+geom_point(aes(x=Source,y=Efficacite),alpha=0.5)
g<-ggplot(data=dens)
g+geom_point(aes(x=Efficacite,y=DensCS,colour=Fishing_method),alpha=1)
cor.test(x=dens$Efficacite,y=dens$DensCS,alternative = "less")
#Pearson's product-moment correlation
#		
#		data:  dens$Efficacite and dens$DensCS 
#		t = -4.5663, df = 77, p-value = 9.263e-06
#		alternative hypothesis: true correlation is less than 0 
#		95 percent confidence interval:
#		-1.0000000 -0.3010585 
#		sample estimates:
#		cor 
#		-0.4616144 

# il existe une correlation significative et positive entre l'échappement moyen et la densite,
# plus la densité sera forte plus on aura d'anguilles lors du second passage en proportion...
g<-ggplot(data=dens)
g<-g+stat_boxplot(aes(x=Fishing_method,y=Efficacite))
g+geom_point(aes(x=Fishing_method,y=Efficacite))+xlab("Methode")

tapply(dens$Efficacite,dens$Fishing_method,function(X)mean(X,na.rm=TRUE))
tapply(dens$Efficacite,dens$Fishing_method,function(X)var(X,na.rm=TRUE))
effEPA2<-dens$Efficacite[dens$Fishing_method=="EPA-2p"] # length 50
effEPADC2<-c(dens$Efficacite[dens$Fishing_method=="EPA+CC-2p"],rep(NA,17)) 
t.test(effEPA2,effEPADC2,alternative="greater")
#
#Welch Two Sample t-test
#
#data:  effEPA2 and effEPADC2 
#t = 3.1422, df = 73.113, p-value = 0.001211
#alternative hypothesis: true difference in means is greater than 0 
#95 percent confidence interval:
#		0.08690698        Inf 
#sample estimates:
#		mean of x mean of y 
#0.7310734 0.5460883 


#######################
#Analyse des données
######################
#######################
# première regression (à éviter les données doivent être log transformées
# les données de la régression (résidus sont OK) mais la régression est tirée par quelques points 
# que l'on voit dans la distance de cook.
# en plus la courbe quantile quantile.
######################
rownames(dens)<-1:nrow(dens)
lmall <- lm(DensCS ~ EPA -1,data=dens)
cooks.distance (lmall)[cooks.distance(lmall)>0.1]
hist(dens$DensCS,30)
hist(dens$lDensCS,30)
hist(dens$EPA,30)
shapiro.test(lmall$residuals) # p<0.001

#Shapiro-Wilk normality test
#
#data:  lmall$residuals 
#W = 0.6998, p-value = 9.746e-12 pas OK


#summary(lmall)
#Call:
#		lm(formula = DensCS ~ EPA - 1, data = dens)
#
#Residuals:
#		Min       1Q   Median       3Q      Max 
#-1.04607 -0.07976 -0.02300  0.00000  1.35166 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#EPA  0.38879    0.02184    17.8   <2e-16 ***
#		---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Residual standard error: 0.2622 on 82 degrees of freedom
#Multiple R-squared: 0.7944,	Adjusted R-squared: 0.7919 
#F-statistic: 316.8 on 1 and 82 DF,  p-value: < 2.2e-16 

plot(lmall)
dens[c(40,46,49,52),]
dens[dens$DensCS>150,]
#In statistics, Cook's distance is a commonly used estimate of the influence of a data point when doing least squares regression.
#Cook's distance measures the effect of deleting a given observation. 
# Data points with large residuals (outliers) and/or high leverage may distort 
#the outcome and accuracy of a regression. 
#Points with a Cook's distance of 1 or more are considered to merit closer examination in the analysis.
lmall2<- update(lmall, subset=-c(40,46,49,52,51,53,27))
summary(lmall2)
plot(lmall2)
############################
# TENTATIVE SUR LES LOG = le bon modèle
##########################"

denslog=subset(dens,!is.infinite(dens$lDensCS)&!is.infinite(dens$lEPA))
nrow(denslog) # 76
rownames(denslog)<-as.character(1:nrow(denslog))
tapply(denslog$lEPA,denslog$Fishing_method,function(X)sum(!is.na(X)))
#EPA-2p EPA+CC-2p 
#46             30 
lmalllog0=lm(lDensCS~lEPA,data=denslog)
summary(lmalllog0)
plot(lmalllog0) 
denslog[21,]
cooks.distance(lmalllog0)[21]
denslog=denslog[-21,] # c'est celui que l'on retire
lmalllog=lm(lDensCS~lEPA,data=denslog)
plot(lmalllog) # pas de pb avec les résidus ou la distance de cook
shapiro.test(lmalllog$residuals)# p=0.82

summary(lmalllog)
#Call:
#		lm(formula = lDensCS ~ lEPA, data = denslog)
#
#Residuals:
#		Min       1Q   Median       3Q      Max 
#-1.46155 -0.40752 -0.02092  0.42168  1.38518 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.25850    0.09148  -13.76   <2e-16 ***
#		lEPA         1.27119    0.05551   22.90   <2e-16 ***
#		---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Residual standard error: 0.6206 on 73 degrees of freedom
#Multiple R-squared: 0.8778,	Adjusted R-squared: 0.8762 
#F-statistic: 524.5 on 1 and 73 DF,  p-value: < 2.2e-16 
g<-ggplot(data=denslog,aes(x=lEPA,y=lDensCS))
g +geom_point(aes(colour=Fishing_method))+geom_smooth(aes(x=lEPA,y=lDensCS), method="lm",fullrange=TRUE,formula=y ~ x )+
		scale_y_continuous(name="log(estimated density eel .m-2)")+scale_x_continuous(name="log(mean number by EPA)")+
		scale_colour_brewer(name="Fishing method","Set1")
coefficients(lmalllog)
#(Intercept)        lEPA 
#-1.258501    1.271189 
#ln(DensCS)=A*ln(EPA)+B
#DensCS=exp(A*ln(EPA)+ B)
# ci dessous je trace une courbe avec une règle de décision différente pour zéro et pour
# des EPA positives.
EPAgr=c(0,seq(0.2,7,by=0.2)) # EPA pour le graphique
DensCS=c(0,exp(coefficients(lmalllog)[2]*log(seq(0.2,7,by=0.2))+coefficients(lmalllog)[1]))
plot(x=EPAgr,y=DensCS) 
points(0,0,col="red")# hé c'est pas trop mal, on voit pas trop le décrochement à zéro
lm(DensCS~EPAgr-1) # uh ? 0.44
# la valeur est très forte mais on garde tout le monde et les grandes valeurs...
points(dens$EPA,dens$DensCS,col="green")
# Mise en évidence d'une différence de regression en fonction des organismes et méthodes
rm(DensCS,EPAgr)
########################""
# graphes par utilisateur deprecated
#####################################"

library(ggplot2)
g<-ggplot(data=dens,aes(x=EPA,y=DensCS))
g +geom_point(aes(colour=Source))+geom_smooth(aes(y=DensCS,x=EPA,group=Source,colour=Source), method="lm",formula=y ~ x -1)+
		scale_y_continuous(name="Densite estimee")+scale_x_continuous(name="nb moyen par EPA")
# pointwise CI
g +geom_point(aes(colour=Source))+geom_smooth(aes(y=DensCS,x=EPA,group=Source,colour=Source), method="lm",fullrange=TRUE,formula=y ~ x -1)+
		scale_y_continuous(name="Densite estimee")+scale_x_continuous(name="nb moyen par EPA")

g +geom_point(aes(colour=Fishing_method))+geom_smooth(aes(y=DensCS,x=EPA,group=Fishing_method,colour=Fishing_method), method="lm",fullrange=TRUE,formula=y ~ x -1)+
		scale_y_continuous(name="Densite estimee")+scale_x_continuous(name="nb moyen par EPA")



########################################
# Analyse des surfaces (deux graphiques) 
#########################################

plot(dens$rap_surf)
abline(1,0)
x11()
g<-ggplot(data=dens)
g+geom_boxplot(aes(x=Fishing_method,y=rap_surf,fill=Fishing_method))+geom_point(aes(x=Fishing_method,y=rap_surf))+ 
		geom_hline(yintercept=1.7,col="red")+geom_hline(yintercept=1.4,col="orange")+
		ylab("Rapport des surfaces")+xlab(iconv("Méthode de pêche","UTF8"))+
		coord_flip()
#surface réelle mesurée
# surface théorique = surface pour 3m autour de l'électrode
##########################################"
# ANALYSE DES SURFACES REELES ET THEORIQUES
############################################"
colSums(confint(lm(denslog$rap_surf~denslog$Fishing_method)))
tapply(denslog$rap_surf,denslog$Fishing_method, summary)
tapply(denslog$rap_surf,denslog$Fishing_method, sd)
tapply(denslog$rap_surf,denslog$Fishing_method, confint)
rapsurfEPA2<-dens$rap_surf[dens$Fishing_method=="EPA-2p"] # length 50
rapsurfEPADC2<-c(dens$rap_surf[dens$Fishing_method=="EPA+CC-2p"],rep(NA,17)) 
t.test(rapsurfEPA2,rapsurfEPADC2,alternative="less")

# EPA-2p (1.29)
#################################################################################"
# ANALYSE DES SURFACES
# calcul du seuil à partir duquel il n'y a plus de différence dans la régression
################################################################################"""
res=vector()
nbr=vector()
for (i in 1:length(seq(from=2.5,to=0.5,by=-0.1))){
	sequence=seq(2.5,0.5,by=-0.1)
	denssurfok=subset(dens,dens$rap_surf>=(sequence[i]-0.5)& dens$rap_surf<sequence[i]+0.5)
	res[i]<-summary(lm(DensCS ~ EPA -1,data=denssurfok))$coefficients[1]
	nbr[i]=nrow(denssurfok)
}
res	
nbr
windows()
plot(x=seq(from=2.5,to=0.5,by=-0.1),y=res,type="p",
		pch=19,xlab=expression("surface observee/surface theorique"),ylab=iconv("pente de la regression","UTF8"),xaxt="n",ylim=c(15,60))
points(x=seq(from=2.5,to=0.5,by=-0.1),nbr,col="darkblue",type="h",lwd=2)
text(x=seq(from=2.5,to=0.5,by=-0.1),y=nbr+5,label=nbr,cex=0.8)
at<-seq(from=0.5, to=2.5, by=0.5)
lab<-paste("[",at-0.5,"-",at+0.5,"[",sep="")
axis(side=1,at=at,labels=lab)
abline(v=1.25, col="orange")
abline(v=1.75, col="red")
legend("topright",legend=iconv(c("nombre des stations","pente de la régression"),"UTF8"),col=c("blue","black"),lty=c(1,-1),pch=c(-1,20))
rm(res,nbr)




###################
# Graphiques et résulats finaux
####################
# Un modèle
denslog=subset(dens,!is.infinite(dens$lDensCS)&!is.infinite(dens$lEPA))
nrow(denslog) # 76
rownames(denslog)<-as.character(1:nrow(denslog))
tapply(denslog$lEPA,denslog$Fishing_method,function(X)sum(!is.na(X)))
#EPA-2p EPA+CC-2p 
#46             30 
lmalllog0=lm(lDensCS~lEPA,data=denslog)
summary(lmalllog0)
plot(lmalllog0) 
denslog[21,]
cooks.distance(lmalllog0)[21]
denslog=denslog[-21,] # c'est celui que l'on retire
lmalllog=lm(lDensCS~lEPA,data=denslog)
plot(lmalllog) # pas de pb avec les résidus ou la distance de cook
shapiro.test(lmalllog$residuals)# p=0.82
# les deux séparés
denslogEPA2<-denslog[denslog$Fishing_method=="EPA-2p",]
denslogEPA_DC2<-denslog[denslog$Fishing_method=="EPA+CC-2p",]
lmEPA2=lm(lDensCS~lEPA,data=denslogEPA2)
summary(lmEPA2)
plot(lDensCS~lEPA,data=denslogEPA2,ylab="log(densite)",xlab="log(EPA)",col="red")
points(denslogEPA2$lEPA,predict(lmEPA2,type="response"),col="red",type="l")
#Call:
#		lm(formula = lDensCS ~ lEPA, data = denslogEPA2)
#
#Residuals:
#		Min       1Q   Median       3Q      Max 
#-0.97995 -0.43842 -0.05488  0.45184  1.45391 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.28235    0.12288  -10.44 1.76e-13 ***
#		lEPA         1.31058    0.06218   21.07  < 2e-16 ***
#		---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Residual standard error: 0.6199 on 44 degrees of freedom
#Multiple R-squared: 0.9099,	Adjusted R-squared: 0.9078 
#F-statistic: 444.2 on 1 and 44 DF,  p-value: < 2.2e-16 
lmEPA_DC2=lm(lDensCS~lEPA,data=denslogEPA_DC2)
summary(lmEPA_DC2)
#plot(lmEPA_DC2)
points(lDensCS~lEPA,data=denslogEPA_DC2,pch=20,col="blue")
points(denslogEPA_DC2$lEPA,predict(lmEPA_DC2,type="response"),col="blue",pch=20,type="l")

plot(DensCS~EPA,data=denslogEPA_DC2)
points(denslogEPA_DC2$EPA,exp(predict(lmEPA_DC2,type="response")),col="red")
lm(exp(predict(lmEPA_DC2,type="response"))~denslogEPA_DC2$EPA-1) #~24

lmsupsurf=lm(lDensCS~lEPA-1,data=subset(denslog,dens$Surface=="chevauchement"))
#exp(0.1098) 1.11
summary(lmsupsurf)
plot(lmsupsurf)
lminfsurf=lm(lDensCS~lEPA-1,data=subset(denslog,dens$Surface=="sans chevauchement"))
#exp(-0.1696) 0.84
summary(lminfsurf)
plot(lminfsurf)


#Call:
#		lm(formula = lDensCS ~ lEPA, data = denslogEPA_DC2)
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-1.4779 -0.2156  0.0233  0.2915  0.9575 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -1.3787     0.1233 -11.177 1.24e-11 ***
#		lEPA          0.8372     0.1353   6.187 1.29e-06 ***
#		---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Residual standard error: 0.5273 on 27 degrees of freedom
#Multiple R-squared: 0.5864,	Adjusted R-squared: 0.5711 
#F-statistic: 38.28 on 1 and 27 DF,  p-value: 1.294e-06 
# densité en fonction des méthodes

#TODO continuer ici pour les graphes en dessous juste copié, il faut un graphique avec les deux régressions et leurs équations.
windows()
g<-ggplot(data=denslog,aes(x=lEPA,y=lDensCS))
g +geom_point(aes(colour=Fishing_method))+geom_smooth(aes(x=lEPA,y=lDensCS), method="lm",fullrange=TRUE,formula=y ~ x )+
		scale_y_continuous(name="log(estimated density eel .m-2)")+scale_x_continuous(name="log(mean number by EPA)")+
		scale_colour_brewer(name="Fishing method","Set1")
g<-ggplot(data=dens150,aes(x=EPA,y=DensCS))
g +geom_point(aes(colour=Fishing_method))+
		geom_smooth(aes(y=DensCS,x=EPA,group=Fishing_method,colour=Fishing_method,fill=Fishing_method),
				method="lm",fullrange=TRUE,formula=y ~ x -1)+
		scale_y_continuous(name="Densite estimee")+
		scale_x_continuous(name="nb moyen par EPA")+
		scale_colour_brewer("Source","YlGnBu")+scale_fill_brewer("Source","YlGnBu")
# Séparation des données en deux groupes les trop sérrés les pas trop sérrés
# Ci dessous la méthode pour créer plusieurs graphes en un seul avec ggplot
####################################################################################"
##################################################################################
windows()
vplayout <- function(x, y) { viewport(layout.pos.row = x, layout.pos.col = y)   }
grid.newpage()
pushViewport(viewport(layout = grid.layout(6,1,just="center")))
require(grid)
# no limits
#g<-ggplot(data=dens,aes(x=EPA,y=DensCS))
#g<-g +geom_smooth(aes(y=DensCS,x=EPA,lty=Surface,colour=Surface), method="lm",fullrange=TRUE,formula=y ~ x-1)+
#		geom_point(aes(pch=Fishing_method,colour=Surface))+
#		scale_y_continuous(name="Estimated density")+
#		scale_x_continuous(name="Mean number of eel per electrofishing sampling point")+
#		scale_colour_brewer("Surface","Set1")
#print(g, vp=vplayout(1:4,1))
# with limits
g<-ggplot(data=dens,aes(x=EPA,y=DensCS))
g<-g +		geom_point(aes(pch=Fishing_method,colour=Surface))+
		scale_y_continuous(name="Densite estimee (ang.100 m-2)",limits=c(0,300))+
		scale_x_continuous(name="Nombre d'anguille par point",limits=c(0,5))+
		geom_smooth(aes(y=DensCS,x=EPA,colour=Surface,fill=Surface), method="lm",fullrange=TRUE,formula=y ~ x-1)+
		scale_colour_manual("Methode",values = c("chevauchement" = "black","sans chevauchement" = "grey50"))+
		scale_fill_manual("Methode",values = c("chevauchement" = "grey50","sans chevauchement" = "grey80"))+
		theme_bw() +		
		annotate("text",x=3.2, y=180, label="Dens=85.4*EPA", color="grey20",size=3,angle=35) +
		annotate("text",x=3.2, y=80, label="Dens=36.3*EPA", color="grey50",size=3,angle=18)+
		annotate("text", x = 0, y = 280, label = "A",size=5)
print(g, vp=vplayout(1:4,1))
# grid.ls()
# grid.remove("GRID.cellGrob.3711") # must be in last position
# pour la légende :
# one point (6.8,750) is outside of graphical range
# DC density current ... CR crenelated current ... 1 / 2 nb of pass.
g1<-ggplot(data=dens)
g1<-g1+geom_boxplot(aes(x=Surface,y=rap_surf,fill=Surface))+
		geom_point(aes(x=Surface,y=rap_surf))+
		scale_y_continuous(name="Station surface /sum(point surface)")+
		theme_bw() +
		geom_hline(yintercept=1.4,col="black")+
		annotate("text", x = 2, y = 6, label = "B",size=5)+
		scale_fill_manual("Surface",values = c("chevauchement" = "grey50","sans chevauchement" = "grey80"))+
		coord_flip()

print(g1, vp=vplayout(5:6,1))

### reste à faire le graphique des observées et des prédites....####
denssurfok=subset(dens,dens$rap_surf>1.4 )#& dens$DensCS<150)
lmdensurfok <- lm(DensCS ~ EPA -1,data=denssurfok)
denssurfok$pred=predict(lmdensurfok,type="response")
denssurfok$"outlined_operations"<-FALSE
denssurfok$pred
denssurfok[c(1,26),"outlined_operations"]<-TRUE
breaks=as.vector(c(1, 2, 5) %o% 10^(-1:3))
g<-ggplot(data=denssurfok,aes(x=DensCS,y=pred))+geom_point(aes(shape=Fishing_method,colour=outlined_operations))+geom_abline() + scale_shape(name="Electrofishing \nmethod") +
		scale_x_log10(name="observed density DC2",breaks = breaks, labels = breaks)+
		scale_y_log10(name="predicted density EPA",breaks = breaks, labels = breaks)+
		scale_colour_manual("outlined operations",values = c("TRUE" = "grey50","FALSE" = "grey80"))

print(g)


#### ci dessous des scories #########

# statistiques
mean(dens$surf_reel, na.rm=TRUE)
sd(dens$surf_reel,na.rm=TRUE)
summary(dens$surf_reel)



rownames(dens.IAV)<-1:nrow(dens.IAV)
lmIAV <- lm(DensCS ~ EPA -1,data=dens.IAV)
summary(lmIAV)
plot(lmIAV)
dens.IAV[c(19,22,26,27),] # outliers
library(aplpack)
slider.bootstrap.lm.plot(dens.IAV$DensCS, dens.IAV$EPA)

statit=lm(DensCS ~ EPA -1,data=dens.IAV)
boot(data=dens.IAV, statistic=statit,R=100)



dens.Bret=subset(dens150,dens150$Source=="MP2")
rownames(dens.Bret)<-1:nrow(dens.Bret)
lmBret <- lm(DensCS ~ EPA -1,data=dens.Bret)
summary(lmBret)
plot(lmBret)
dens.Bret[c(11,15,17),] #outliers
coefficients(summary(lmBret))
B0.Bret.ass=coefficients(summary(lmBret))[1]
# On pourrait voir simplement avec les coefficients et les écarts types
coefficients(summary(lmBret))[1]-coefficients(summary(lmBret))[2]>
		coefficients(summary(lmIAV))[1]-coefficients(summary(lmIAV))[2]		
# TRUE
# test de la différence significative entre les deux régressions pour avoir la p
summary( lm.result <- lm(DensCS + B0.Bret.ass*EPA~EPA-1,data=dens.IAV) )
#Deuxième méthode An offset is a term to be added to a linear predictor, such as in
#a generalised linear model, with known coefficient 1 rather than
#an estimated coefficient.
summary( lm.result <- lm(DensCS ~EPA+offset(B0.Bret.ass*EPA),data=dens.IAV) )

# conclusion les pentes des régressions sont significativement différentes p<0.001

# intégration des distances de cook dans la régression
#example(influence.measures)

dens150[dens150$Source=="MPH2","Dist_Cook"]=cooks.distance(lmIAV)
dens150[dens150$Source=="MP2","Dist_Cook"]=cooks.distance(lmBret)
g<-ggplot(data=dens150,aes(x=EPA,y=DensCS))
g<-g+geom_point(data=subset(dens150,dens150$DensCS>20&dens150$EPA<0.2),col="grey10",alpha=0.4)
g<-g +geom_point(aes(colour=Source,size=Dist_Cook))+geom_smooth(aes(y=DensCS,x=EPA,group=Source,colour=Source), method="lm",formula=y ~ x -1)+
		scale_y_continuous(name="Densite estimee")+scale_x_continuous(name="nb moyen par EPA")
#x11(9,5)
print(g)

dens150$Source=as.factor(dens150$Source)
plot(Efficacite ~ Source, data = dens150)


plot(lmdensurfok)
library(aplpack)
slider.bootstrap.lm.plot(denssurfok$DensCS, denssurfok$EPA)
library(boot)
r<-boot(data=denssurfok, statistic=function(data=denssurfok,i){
			statist=lm(DensCS ~ EPA -1,data=denssurfok[i,])
			coefficients(summary(statist))},
		R=100)
boot.ci(r)		# 95% ci 33.5 => 40
## Essai de régression log
denslogsurfok=subset(dens,!is.infinite(dens$lDensCS)&!is.infinite(dens$lEPA)&dens$rap_surf>1.4)
nrow(denslogsurfok) # 51 données
lmsurfok=lm(lDensCS~lEPA,data=denslogsurfok)
summary(lmsurfok)
plot(lmsurfok) # pas de pb avec les résidus ou la distance de cook
coefficients(lmsurfok)
# (Intercept)        lEPA 
# 3.678249    1.050658
#ln(DensCS)=A*ln(EPA)+B
#DensCS=exp(A*ln(EPA)+ B)
# ci dessous je trace une courbe avec une règle de décision différente pour zéro et pour
# des EPA positives.
EPAgr=c(0,seq(0.2,7,by=0.2)) # EPA pour le graphique
DensCS=c(0,exp(1.050658 *log(seq(0.2,7,by=0.2))+ 3.678249 ))
plot(x=EPAgr,y=DensCS) 
points(0,0,col="red")# hé c'est pas trop mal, on voit pas trop le décrochement à zéro
lm(DensCS~EPAgr-1) # uh ? 48.29
abline(a=0,b=42.98,col="brown")
# Cette valeur est en dehors de l'intervalle de confiance de la régression linéaire sur des valeurs non
# log transformée. Le gros pb c'est qu'on ne force pas les données à passer par zéro et probablement
# les modifications de la transformation log ne tiennent pas compte de la réalité.
points(dens$EPA,dens$DensCS,col="green")
# Mise en évidence d'une différence de regression en fonction des organismes et méthodes
rm(DensCS,EPAgr)
# régression des données avec chevauchement
denssurfpasok=subset(dens,dens$rap_surf<=1.4 )
lmdensurfpasok <- lm(DensCS ~ EPA -1,data=denssurfpasok)
summary(lmdensurfpasok) # 36.3
plot(lmdensurfpasok)




