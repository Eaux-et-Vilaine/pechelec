# DEPRECATED voir main_pechelec
# 
# Author: cedric
###############################################################################

setwd("E:/workspace/prog")
library(ggplot2)
library(splines)
densites=read.table("pechelec/pechelec9809.csv",sep=";",header=TRUE)
colnames(densites)
killfactor=function(df){
	for (i in 1:ncol(df))
	{
		if(is.factor(df[,i])) df[,i]=as.character(df[,i])
	}
	return(df)
}
densites=killfactor(densites)
densites$"denscs"=as.numeric(densites$denscs)
#[1] "annee"            "secteur"          "densite.CS"       "ecart.type"      
#[5] "interv.conf.0.05"

tapply(densites$densdelury, densites$annee,function(X)round(max(X,na.rm=TRUE)))
tapply(densites$densdelury, densites$annee,function(X)round(min(X,na.rm=TRUE)))
tapply(densites$denscs, densites$annee,function(X) round(max(X,na.rm=TRUE)))
tapply(densites$denscs, densites$annee,function(X)round(min(X,na.rm=TRUE)))

densites$typedist=as.factor(densites$typedist)
library(ggplot2)
g<-ggplot(data=densites,aes(x=annee,y=denscs))
g+geom_point(aes(col=typedist))+geom_line(aes(col=typedist))

g<-ggplot(data=densites,aes(x=annee,y=densdelury))
g+geom_point(aes(col=typedist))+geom_line(aes(col=typedist))

densites9809=subset(densites,densites$Continuite==1 &densites$annee>=1998)

tapply(densites9809$denscs, densites9809$annee,function(X) round(max(X,na.rm=TRUE)))
tapply(densites9809$denscs, densites9809$annee,function(X)round(min(X,na.rm=TRUE)))
g<-ggplot(data=densites9809,aes(x=annee,y=denscs))
sumplussd=function(df,var) mean(df$"denscs",na.rm=TRUE)+sd(df$"denscs",na.rm=TRUE) 
summinussd=function(df) mean(df$"denscs",na.rm=TRUE)-sd(df$"denscs",na.rm=TRUE)
meanspl=function(df) mean(df$"denscs",na.rm=TRUE)



meanspl1=function(df) mean(df[,c("un")],na.rm=TRUE)
sumplussd1=function(df,var) mean(df$"un",na.rm=TRUE)+sd(df$"un",na.rm=TRUE) 
summinussd1=function(df) mean(df$"un",na.rm=TRUE)-sd(df$"un",na.rm=TRUE)

spl=ddply(densites9809,.variables=c("annee","typedist"),.fun=sumplussd)
smi=ddply(densites9809,.variables=c("annee","typedist"),.fun=summinussd)

colnames(spl)[3]<-"sumplussd"
spl$summinussd=smi[,3]
spl$denscs=ddply(densites9809,.variables=c("annee","typedist"),.fun=meanspl)[,3]
spl$denscs1=ddply(densites9809,.variables=c("annee","typedist"),.fun=meanspl1)[,3]
spl$summinussd1=ddply(densites9809,.variables=c("annee","typedist"),.fun=summinussd1)[,3]
spl$sumplussd1=ddply(densites9809,.variables=c("annee","typedist"),.fun=sumplussd1)[,3]

save(spl,file="pechelec/spl.Rdata")



# un premier graphe
g<-ggplot(data=densites9809,aes(x=annee,y=denscs))
(g<-g+geom_point(aes(col=typedist),alpha=0.8)+
		stat_summary(fun.y = mean, geom="line",aes(col=typedist),legend=FALSE)+
		#stat_smooth(method="lm",formula=y ~ ns(x,4), aes(colour=typedist,fill=typedist),lty=2, size=0.8,alpha=0.3,legend = FALSE) +
         scale_colour_brewer(name="distance",palette="Set1")+
		 xlab("Annee")+
         ylab("Densite (ang.100m2)") )
g<-g%+%spl
(g1<- g+geom_ribbon(aes(ymin=summinussd,ymax=sumplussd,colour=typedist,fill=typedist),size=0.5,alpha=0.2))
g1+facet_wrap(~typedist)
# j'essaye dans l'autre sens
densites9809$summinussd<-NA
densites9809$sumplussd<-NA
g<-ggplot(data=spl,aes(x=annee,y=denscs))
g<- g+geom_ribbon(aes(ymin=summinussd,ymax=sumplussd,colour=typedist,fill=typedist),size=0.5,alpha=0.2)
g<-g%+%densites9809
(g<-g+geom_point(aes(col=typedist),alpha=0.8)+
			stat_summary(fun.y = mean, geom="line",aes(col=typedist),legend=FALSE)+
			#stat_smooth(method="lm",formula=y ~ ns(x,4), aes(colour=typedist,fill=typedist),lty=2, size=0.8,alpha=0.3,legend = FALSE) +
			scale_colour_brewer(name="distance",palette="Set1")+
			xlab("Annee")+
			ylab("Densite (ang.100m2)") )


colnames(densites)
# Graphique en fonction de l'âge
den<-densites[densites$Continuite==1,c("typedist","zero","un","deux","trois","quatreplus","annee")]
den$id=1:nrow(den)
den1=melt.data.frame(den, id.vars=c(1,7), measure.vars=c(2:6),variable_name = "age")
colnames(den1)[colnames(den1)=="value"]<-"densite"
den1=den1[den1$annee!=1981,]
den1$typedist=factor(den1$typedist,level=c("<50","[50-100[",">=100"))
g<-ggplot(data=den1,aes(x=annee,y=densite))
(g1<-g+geom_point(aes(col=age),alpha=0.8)+
			stat_summary(fun.y = mean, geom="line",aes(col=age),legend=FALSE)+
			#stat_smooth(method="lm",formula=y ~ ns(x,4), aes(colour=typedist,fill=typedist),lty=2, size=0.8,alpha=0.3,legend = FALSE) +
			scale_colour_brewer(name="age",palette="Set1")+
			xlab("Annee")+
			ylab("Densite par age (ang.100m2)") + 
		    scale_y_sqrt()) 

(g2<-g1+facet_grid(~typedist))


g<-ggplot(data=den1,aes(x=age,y=densite))
(g1<-g+	geom_boxplot(aes(col=age))+
			#stat_smooth(method="lm",formula=y ~ ns(x,4), aes(colour=typedist,fill=typedist),lty=2, size=0.8,alpha=0.3,legend = FALSE) +
			scale_colour_brewer(name="age",palette="Set1")+		
			xlab("Annee")+
			ylab("Densite par age (ang.100m2)") )
(g2<-g1+facet_grid(typedist~annee))
