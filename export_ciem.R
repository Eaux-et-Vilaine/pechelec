library(stringr)
library(tidyverse)
setwd("C:/workspace/p/pechelec")
datawd <- getwd()
CY<-2021

datawd<-"C:/workspace/pdata/pechelec/data/"
datawdy=str_c(datawd,CY,"/")
load(file=str_c(datawdy,"dens.Rdata"))
load(file=str_c(datawdy,"sauvbase.Rdata"))
load(file=str_c(datawdy,"sta_ope_pois.csv"))
load(file=str_c(datawdy,"pec.Rdata"))
load(file=str_c(datawdy,"InfoAnguille.Rdata"))
# calcul des densites de Carle et Strubb
stationsrepetees<-as.character(pec$Id_PtPrelMetier)[strftime(pec$Op_Dtd,"%Y")==2007|strftime(pec$Op_Dtd,"%Y")>=2014]
pec<-pec[,]
str(sta_ope_pois)
sta_ope_pois_s <- sta_ope_pois %>% 
		filter(Id_PtPrelMetier%in%stationsrepetees) %>%
		left_join(InfoAnguille, by = c('Id_Poisson'= 'If_Id_Poisson' ))
pds <- sta_ope_pois_s$Po_Pds
pds[sta_ope_pois_s$Po_Lot!="N"]<-NA	

tw <-		data.frame(
ser_nameshort='VilY',	
fi_date=sta_ope_pois_s$Op_Dtd ,
fi_year=lubridate::year(sta_ope_pois_s$Op_Dtd),	
fi_comment = NA,	
lengthmm=sta_ope_pois_s$Po_Lmi,	
weightg=pds,	
ageyear = NA,	
eye_diam_meanmm = apply(rbind(sta_ope_pois_s$If_Diam_OculaireDH, sta_ope_pois_s$If_Diam_OculaireDV), 2, mean, na.rm=TRUE),	
pectoral_lengthmm = sta_ope_pois_s$If_Long_Pectorale,	
is_female_1= NA,	
is_differentiated = NA,	
anguillicola_presence = NA,	
anguillicola_intensity = NA,	
muscle_lipid_fatmeter_percv = NA,	
muscle_lipid_gravimeter_percv = NA,	
sum_6_pcb = NA,	
teq	= NA,
evex_presence = NA,	
hva_presence = NA,	
pb = NA,	
hgv = NA,	
cd = NA)

plot(tw$lengthmm,tw$weightg)
summary(tw$lengthmm)
summary(tw$weightg)
tw$weightg[tw$weightg==-9] <- NA
tw$weightg[tw$weightg==0] <- NA
tw$eye_diam_meanmm[tw$eye_diam_meanmm==0] <- NA
tw$pectoral_lengthmm[tw$pectoral_lengthmm==0] <- NA
tw$pectoral_lengthmm[tw$pectoral_lengthmm==1] <-13
tw$weightg[tw$weightg==0] <- NA
tw$lengthmm[tw$lgl]
tw$lgl<- log(tw$lengthmm)
tw$lgw <- log(tw$weightg)
tw$ID <- seq_along(tw[,1])
tw1 <- tw %>% select(ID, lgl, lgw) %>% filter(complete.cases(.))
rq <- quantreg::rq(lgw~lgl,tau = .5, data = tw1)
tw1$res <- resid(rq)
ggplot(tw1) + geom_point(aes(x=lgl,y=lgw,col=res)) +
		scale_colour_gradient2(low="red", mid="green", high="blue")

tw[tw$ID %in% tw1[abs(tw1$res)>0.9,"ID"],c("fi_date","lengthmm", "weightg")]
tw1$lgw[abs(tw1$res)>0.9] <-NA

tw <- tw %>% select(-lgl,-lgw) %>% left_join(tw1)
tw$weightg[is.na(tw$lgw)] <- NA

ggplot(tw) + geom_point(aes(x=lengthmm,y=weightg,col=res)) +
		scale_colour_gradient2(low="red", mid="green", high="blue")

# plante
#library(xlsx)
#write.xlsx(tw, "C:/temp/Eel_Data_Call_2022_Annex2_Vilaine.xlsx", sheetName="new_individual_metrics")
# plante aussi library(readODS) # marche pas plus
# write_ods(tw,"~/Eel_Data_Call_2022_Annex2_Vilaine.ods")

# le seul qui marche mais pas de fioritures... (par exemple named region pas réussi...)
library("XLConnect")
tw <- tw %>% select(-lgl, -lgw,-res)
wb <- loadWorkbook("C:/temp/test.xlsx", create = TRUE)
createSheet(wb,"tw")
writeWorksheet(wb,tw,"tw")
saveWorkbook(wb)