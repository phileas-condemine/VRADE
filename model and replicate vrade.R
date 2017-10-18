library(data.table)
library(dplyr)
library(gbm)

setwd("/Users/p-condemine/Documents/Claims/Wreck VGE/Fichiers 2015-01-10/")
library(bit64)
col=colnames(fread("BASEVEI 24 mois.csv",nrows = 2))

vei=fread("BASEVEI 24 mois.csv")



varmod=c("IDMIS","PUIS_REE", "PRIXACT", "iiactu", "coeff", "pmvkm","km","ENERGIE","VIT_MAXI","CYLINDRE","VRADE","BT","GENRE","mois")

varmiss=c("IDMIS","PUIS_REE","VIT_MAXI","CYLINDRE","CDENTITE","DISTRIB","NB_PLACE","SERILIM",
          "TYP_ALIM","BOIT_VIT","PTAC","PROPULS","ROUES_MT","ANTIVOL",
          "NBRE_PTE","HTRISQ","TYPREMP","VERS_VEH","NB_CYL","COUPLE_MOT_MAX","REG_COUPLE_MAX","TRANS",
          "SUSPEN","FREIN","CHARG_UTIL","DIR_ASS","CTRL_DYN_STAB","EMISSION_CO2")



complete=fread("BASEVEI 24 mois.csv",select=varmiss)

db=fread("tableBTVEI.csv")

db=merge(db,complete,by="IDMIS",all.x=T,all.y=F)

# db=subset(db,db$BT>100)

db=subset(db,db$VRADE>100)

db=subset(db,db$km<1000000)

# db$ratio=db$VRADE/db$BT

# db=subset(db,!is.na(db$ratio))

db$claimtime=12*(as.numeric(db$yclaimdate-2013))+as.numeric(db$mclaimdate)

db$lastprice=12*(as.numeric(db$ylastprice-1989))+as.numeric(db$mlastprice)


str(db)

colnames(db)

db=select(db,-V1,-yclaimdate,-ylastprice,-mclaimdate,-mlastprice,-VERS_VEH)

db$dtlastprice=as.numeric(substr(db$DTCREDTE,7,10))*12+as.numeric(substr(db$DTCREDTE,4,5))-
  as.numeric(substr(db$DTDERPRI_D,7,10))*12-as.numeric(substr(db$DTDERPRI_D,4,5))
db$dtlastprice=ifelse(db$dtlastprice>240,240,db$dtlastprice)
db$dtlastprice=ifelse(db$dtlastprice<1,1,db$dtlastprice)


db$ENERGIE=as.factor(db$ENERGIE)

db$GENRE=as.factor(db$GENRE)

db=sample(db)

db$BOIT_VIT=as.factor(db$BOIT_VIT)

db$FREIN=as.factor(db$FREIN)

db$CTRL_DYN_STAB=as.factor(db$CTRL_DYN_STAB)

db$SERILIM=as.factor(db$SERILIM)

db$DIR_ASS=as.factor(db$DIR_ASS)

db$CARROSS=as.factor(db$CARROSS)

db$TRANS=as.factor(db$TRANS)

db$table=as.factor(db$table)

db$ctte=as.factor(db$ctte*1)


save(list="db",file="db.RData")





param=c(ntree=250,depth=30,shrinkage=0.01,train.fraction=0.8)
modelb=gbm(VRADE~PUIS_REE+PRIXACT+iiactu+coeff+pmvkm+km+VIT_MAXI+CYLINDRE+mois+ENERGIE+
             GENRE+EMISSION_CO2+BOIT_VIT+FREIN+CTRL_DYN_STAB+SERILIM+
             NBRE_PTE+DIR_ASS+VIT_MAXI+ctte+CARROSS+TRANS+COUPLE_MOT_MAX+CYLINDRE+table,
           data=db,distribution = "gaussian",n.trees = param[1],interaction.depth = param[2],
           shrinkage = param[3],train.fraction = param[4],verbose = T)

pred1=predict(modelb,db,type="response")

db$pred=pred1
write.csv(data.frame(VRADE=db$VRADE,pred=pred1,prix=db$PRIXORI,IDMIS=db$IDMIS),"predVRADE.csv")

db2=subset(db,abs(db$pred-db$VRADE)<5000)
param=c(ntree=250,depth=30,shrinkage=0.01,train.fraction=0.8)
modelb=gbm(VRADE~PUIS_REE+PRIXACT+iiactu+coeff+pmvkm+km+VIT_MAXI+CYLINDRE+mois+ENERGIE+
             GENRE+EMISSION_CO2+BOIT_VIT+FREIN+CTRL_DYN_STAB+SERILIM+
             NBRE_PTE+DIR_ASS+VIT_MAXI+ctte+CARROSS+TRANS+COUPLE_MOT_MAX+CYLINDRE+table,
           data=db2,distribution = "gaussian",n.trees = param[1],interaction.depth = param[2],
           shrinkage = param[3],train.fraction = param[4],verbose = T)

pred2=predict(modelb,db2,type="response")
err=pred2-db2$VRADE
hist(err[abs(err)<5000],breaks=100)
mean(err[abs(err)<5000])
sd(err)
db2$pred=pred2
write.csv(data.frame(VRADE=db2$VRADE,pred=pred2,prix=db2$PRIXORI,IDMIS=db2$IDMIS),"predVRADE2.csv")

save(list = "db2",file = "db2.RData")

plot(quantile(db$VRADE/pred1,1:99/100),type="l")
lines(seq(from=1,to=1,length.out = 99),col="red")
lines(seq(from=0.9,to=0.9,length.out = 99),col = "green")
lines(seq(from=1.1,to=1.1,length.out = 99),col="green")


# Valeur résiduelle : 
  


dbres=fread("BASEVEI 24 mois.csv",select=c("VRADE","VALRES_CESQ","NOM_COM","IDMIS","DTCESSION","DTCREDTE","DTCIR001","CDENTITE","CDREGION",
                                           "CLASSE","CODEXP","KILOMETR","COUT_ACTU_OPTIQUE","COUT_ACTU_PBRISE","COUT_ACTU_PIECE",
                                           "COUT_CHOC_ARR_EU","COUT_CHOC_AVT_EU","ENERGIE","SINISTRE","NMCNT","PRIXACT","GENRE"))


dbres$mclaimdate=as.numeric(substr(x = dbres$DTCREDTE,4,5))
dbres$yclaimdate=as.numeric(substr(x = dbres$DTCREDTE,7,10))
dbres$mcirc=as.numeric(substr(x = dbres$DTCIR001,4,5))
dbres$ycirc=as.numeric(substr(x = dbres$DTCIR001,7,10))
dbres$mois=dbres$mclaimdate+12*dbres$yclaimdate-dbres$mcirc-12*dbres$ycirc
dbres$mois=ifelse(dbres$mois<1,1,ifelse(dbres$mois>240,240,dbres$mois))
# library(dplyr)
dbres=select(dbres,-mclaimdate,-yclaimdate,-mcirc,-ycirc)

# dbres$mclaimdate=as.numeric(substr(x = dbres$DTCREDTE,4,5))
# dbres$dclaimdate=as.numeric(substr(x = dbres$DTCREDTE,1,2))
# dbres$yclaimdate=as.numeric(substr(x = dbres$DTCREDTE,7,10))
# 
# dbres$mcession=as.numeric(substr(x = dbres$DTCESSION,4,5))
# dbres$dcession=as.numeric(substr(x = dbres$DTCESSION,1,2))
# dbres$ycession=as.numeric(substr(x = dbres$DTCESSION,7,10))
# 
# dbres$dureeprocess=-30*(12*dbres$yclaimdate+dbres$mclaimdate+dbres$dclaimdate/30-12*dbres$ycession-dbres$mcession-dbres$dcession/30)
# plot(quantile(dbres$dureeprocess,1:99/100,na.rm=T))
# dbres$dureeprocess=ifelse(dbres$mois<1,1,ifelse(dbres$mois>240,240,dbres$mois))

str(dbres)

dbres$CDENTITE=as.factor(dbres$CDENTITE)

dbres$NOM_COM=as.factor(dbres$NOM_COM)

dbres$GENRE=as.factor(dbres$GENRE)

dbres$CLASSE=as.factor(dbres$CLASSE)

dbres$ENERGIE=as.factor(dbres$ENERGIE)

dbres=subset(dbres,!dbres$VALRES_CESQ==0&!dbres$VALRES_CESQ/dbres$VRADE==0.25)

dbres=subset(dbres,dbres$VALRES_CESQ>500)

plot(quantile(dbres$VRADE/dbres$VALRES_CESQ,1:98/100))

param=c(ntree=250,depth=20,shrinkage=0.008,train.fraction=0.8)
modelres=gbm(VALRES_CESQ~.-IDMIS-CODEXP-SINISTRE-NMCNT-DTCESSION-DTCREDTE-DTCIR001,
           data=dbres,distribution = "gaussian",n.trees = param[1],interaction.depth = param[2],
           shrinkage = param[3],train.fraction = param[4],verbose = T)

save(list = "dbres",file = "modelres.RData")

summary(modelres)
predres=predict(modelres,newdata=dbres,type="response")
err=predres-dbres$VALRES_CESQ
hist(err[abs(err)<2000],breaks=100)
mean(err[abs(err)<2000])
sd(err)
sd(dbres$VALRES_CESQ)/mean(dbres$VALRES_CESQ)


BT=fread("tableBT.csv",select=c("IDMIS","BT"))
sinmis=fread("BASE_REPARATIONS_24mois.csv",select=c("IDMIS","sinistre"))
BT=merge(BT,sinmis,by="IDMIS",all.x=T,all.y=F)
BTVEI=fread("tableBTVEI.csv",select=c("IDMIS","BT","VRADE"))
sinmis=fread("BASEVEI 24 mois.csv",select=c("IDMIS","SINISTRE"))
BTVEI=merge(BTVEI,sinmis,by="IDMIS",all.x=T,all.y=F)

BCA=fread("RAPPORT_BCA2.csv")
setnames(BCA,"numsin","SINISTRE")
setnames(BT,"sinistre","SINISTRE")
BT2=merge(BT,BCA,by="SINISTRE",all.x=F,all.y=F)

str(BT)
str(BCA)


# IDEE : recup adresse garage, assuré, sinistre pour capter le coté pauvre de l'assuré et donc cession vs autre ?

Montrer que le problème est mal posé
Erreur trop importante
Volume mal calculés
Loi des grands nombres non atteinte (gros couts rares plutot que petits couts fréquents, c'est précisément ce qu'on modélise mal)
% de dépassement de seul ?

Part des VEI vs Rep

Volumes associés en termes de coût pour AXA : sum(mtfactu) et sum( )

% de BT proches de % VRADE ? quantiles de distance

% de valeur prédites proches de % VRADE ? quantiles de distance

% d'additifs mtfactu/MTHTRER par tranche de MTHTRER ? vs BT ? vs pred ? quantiles d'additif

% scrapper données renault des occasions ? En tenant compte qu'ils valorisent leur marché donc trop cher.

Scénarios : si j'ai VRADE que fais-je ?

Si j'ai risque d'additif que fais-je ?

Si j'ai Valeur résiduelle ? Valeur de réparation ?

Est-ce que je peux déterminer les "fausses" réparations ?

Les "fausses" épaves ?

