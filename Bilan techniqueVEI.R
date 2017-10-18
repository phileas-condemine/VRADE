
setwd("/Users/p-condemine/Documents/Claims/Wreck VGE/Fichiers 2015-01-10/")
library(bit64)
library(data.table)
col=colnames(fread("BASEVEI 24 mois.csv",nrows=2))
selected=c("IDMIS","DTCREDTE","DTCIR001","age","MARQUE","MODELE","NOM_COM","ENERGIE",
           "PRIXACT","PRIXORI","GENRE","DTDERPRI_D","CARROSS","VRADE","VRESID","VALRES_CESQ")


# "DT_ANTIVOL_ACTU"   "DT_ANTIVOL_ORIG"   "DT_COUT_ORIG"      "DT_FAB_D"          "DT_FAB_F"         
# "DT_TARIF_ACTU"     "DT_TARIF_ORIG"     "DTCESSION"         "DTCIR001"          "DTCREDTE"          
# "DTDERPRI_D"        "DTFAN"             "DTFRS"             "DTMAJ"


sinistres=fread("BASEVEI 24 mois.csv",select=selected)
sinistres=subset(sinistres,sinistres$ENERGIE%in%c("ES","GO"))

# CONSTRUIRE L'AGE EN MOIS AVEC LES BONNES VARIABLES. SINISTRE ET MISCIR ?

sinistres$agemois=as.numeric(substr(sinistres$DTCREDTE,7,10))*12+as.numeric(substr(sinistres$DTCREDTE,4,5))-as.numeric(substr(sinistres$DTCIR001,7,10))*12-as.numeric(substr(sinistres$DTCIR001,4,5))
sinistres$agemois=ifelse(sinistres$agemois>240,240,sinistres$agemois)
sinistres$agemois=ifelse(sinistres$agemois<1,1,sinistres$agemois)

plot(quantile(sinistres$agemois,1:9/10))

# sinistres[which(!as.factor(sinistres$MARQUE)%in%as.factor(asso$V1))]$MARQUE

sinistres$Vsix=grepl("V6",sinistres$MODELE)|grepl("v6",sinistres$MODELE)

sinistres$Vdouze=grepl("V12",sinistres$MODELE)|grepl("v12",sinistres$MODELE)

sinistres$cab=grepl("CAB",sinistres$CARROSS)

sinistres$coupe=grepl("CPE",sinistres$CARROSS)

sinistres$amg=grepl("AMG",sinistres$MODELE)

sinistres$jts=grepl("JTS",sinistres$MODELE)

sinistres$rs=grepl("RS",sinistres$MODELE)

sinistres$evolution=grepl("evolution",tolower(sinistres$MODELE))

sinistres$mv6=grepl("mv6",tolower(sinistres$MODELE))

sinistres$pickup=grepl("pick up",tolower(sinistres$MODELE))

sinistres$vr6=grepl("vr6",tolower(sinistres$MODELE))

sinistres$w8=grepl("w8",tolower(sinistres$MODELE))



library(dplyr)
  vehiculier=select(sinistres,MARQUE,NOM_COM,Vsix,Vdouze,cab,coupe,amg,jts,rs,evolution,mv6,pickup,vr6,w8)
  
vehiculier=unique(vehiculier)
vehiculier=vehiculier[order(vehiculier$NOM_COM)]
vehiculier=vehiculier[order(vehiculier$MARQUE)]

# write.csv(vehiculier,"vehiculierSimple2vei.csv")

vehiculier=select(sinistres,MARQUE,NOM_COM,Vsix,Vdouze,cab,coupe,amg,jts,rs,evolution,mv6,pickup,vr6,w8)
assoTable=fread("vehiculierSimple2.csv")
assoDeprec=fread("association véhicules V2.csv")

for (i in colnames(vehiculier)[3:14]){
  set(x=vehiculier,j=i,value = 1*vehiculier[,i,with=F])
}
vehiculier$num=1:nrow(vehiculier)

vehiculier=merge(vehiculier,assoTable,by=colnames(vehiculier)[1:14],all.x=T,all.y=F)
vehiculier=vehiculier[order(vehiculier$num)]

sinistres$assotable=as.numeric(vehiculier$Ligne)-1
sinistres=select(sinistres,-Vsix,-Vdouze,-cab,-coupe,-amg,-jts,-rs,-evolution,-mv6,-pickup,-vr6,-w8)
sinistres$diesel=(sinistres$ENERGIE=="GO")
sinistres$ctte=(sinistres$GENRE=="U")
library(pbapply)
options("pbapply.pb"="tk")
system.time(sinistres$table<-unlist(pbsapply(1:nrow(sinistres),FUN=function(i){assoDeprec[sinistres$assotable[i],
  colnames(assoDeprec)[5+2*sinistres$diesel[i]+4*sinistres$ctte[i]],with=F]})))
system.time(sinistres$tablekm<-unlist(pbsapply(1:nrow(sinistres),FUN=function(i){assoDeprec[sinistres$assotable[i],
  colnames(assoDeprec)[6+2*sinistres$diesel[i]+4*sinistres$ctte[i]],with=F]})))

# summary(select(sinistres,table,tablekm))
# str(select(sinistres,table,tablekm))

sinistre=sinistres


deprec=fread("Depreciation.csv")
setnames(sinistres,c("agemois"),c("mois"))
setnames(deprec,"Table","table")
setnames(deprec,colnames(deprec)[2],c("coeff"))
setnames(deprec,colnames(deprec)[3:5],c("refkm","pvkm","mvkm"))

deprec$coeff=as.numeric(gsub(",",".",as.character(deprec$coeff)))
deprec$refkm=as.numeric(gsub(" ","",as.character(deprec$refkm)))
deprec$pvkm=as.numeric(gsub(",",".",as.character(deprec$pvkm)))
deprec$mvkm=as.numeric(gsub(",",".",as.character(deprec$mvkm)))

# setnames(deprec,c("tablekm"),c("table"))
sinistres=left_join(sinistres,select(deprec,table,mois,coeff))
setnames(deprec,c("table"),c("tablekm"))
sinistres=left_join(sinistres,select(deprec,-coeff))

km=fread("KILOMETR.csv")
setnames(km,"KILOMETR","km")
sinistres=left_join(sinistres,km)

# sinistres=sinistre
#Disons que la variable de km sera dans sinistres et s'appellera km

sinistres$pmvkm=1*(sinistres$km>sinistres$refkm)


sinistres$mclaimdate=as.numeric(substr(x = sinistres$DTCREDTE,4,5))
sinistres$yclaimdate=as.numeric(substr(x = sinistres$DTCREDTE,7,10))
sinistres$mlastprice=as.numeric(substr(x = sinistres$DTDERPRI_D,4,5))
sinistres$ylastprice=as.numeric(substr(x = sinistres$DTDERPRI_D,7,10))

# SI >10 ans alors fixer ? 10 ans
# si >150k km alors plus de d?valuation


insee=fread("Indice_insee2.csv")
insee$indice=as.numeric(gsub(",",".",as.character(insee$indice)))
setnames(insee,c("annee","mois"),c("ylastprice","mlastprice"))
sinistres=left_join(sinistres,insee)
setnames(sinistres,"indice","iilastprice")
setnames(insee,c("ylastprice","mlastprice"),c("yclaimdate","mclaimdate"))
sinistres=left_join(sinistres,insee)
setnames(sinistres,"indice","iiclaimdate")

sinistres$iiactu=sinistres$iiclaimdate/sinistres$iilastprice


sinistres$BT=floor(sinistres$PRIXACT*sinistres$iiactu*sinistres$coeff+ifelse(sinistres$pmvkm==1,sinistres$pvkm,sinistres$mvkm)*(sinistres$km-sinistres$refkm))
sinistres$BT=(sinistres$BT>0)*sinistres$BT


# plot(quantile(sinistres$BT,1:9/10,na.rm=T))


cor(sinistres$BT,sinistres$VRADE,use="complete.obs")
sinistres$ratioVRADEBT=sinistres$VRADE/sinistres$BT
hist(sinistres$VRADE/sinistres$BT)

library(Hmisc)

ratioVRADEBT=quantile(sinistres$VRADE/sinistres$BT,1:99/100,na.rm=T)

plot(x=ratioVRADEBT)


write.csv(sinistres,"tableBTVEI.csv")

write.csv(select(sinistres,BT,IDMIS),"BT-MISVEI.csv")





# Variables pour expliquer l'écart VRADE/BT : puissance fiscale, prix du neuf, energie, kilometrage, age du véhicule, 






varmod=c("IDMIS","PUIS_REE", "PRIXACT", "iiactu", "coeff", "pmvkm","km","ENERGIE","VIT_MAXI","CYLINDRE","VRADE","BT","GENRE","mois")

varmiss=c("IDMIS","PUIS_REE","VIT_MAXI","CYLINDRE")


complete=fread("BASEVEI 24 mois.csv",select=varmiss)

db=fread("tableBTVEI.csv")

db=merge(db,complete,by="IDMIS",all.x=T,all.y=F)

db=db[,varmod,with=F]

db=subset(db,db$BT>100)

db=subset(db,db$VRADE>100)

db=subset(db,db$km<1000000)

db$ratio=db$VRADE/db$BT

db=subset(db,!is.na(db$ratio))

db$ENERGIE=as.factor(db$ENERGIE)

db$GENRE=as.factor(db$GENRE)



library(gbm)
quant=data.table(q=0:100/100)
for (x in varmod){
  if (is.numeric(db[[x]])){
  quant=data.table(quant, x=quantile(db[[x]], 0:100/100))
  setnames(quant,"x",x)
  }
}


PUIS_REE+PRIXACT+iiactu+coeff+pmvkm+km+VIT_MAXI+CYLINDRE+mois


db=sample(db)
train=db[1:12000,]
test=db[12001:nrow(db)]

param=c(ntree=200,depth=20,shrinkage=0.01,train.fraction=0.8)
modelb=gbm(VRADE~PUIS_REE+PRIXACT+iiactu+coeff+pmvkm+km+VIT_MAXI+CYLINDRE+mois,
          data=train,distribution = "gaussian",n.trees = param[1],interaction.depth = param[2],
          shrinkage = param[3],train.fraction = param[4],verbose = T)
pred=predict(modelb,test,type="response")

roc=data.frame(pred=pred,target=test$VRADE)
roc=roc[order(pred,decreasing=T),]
roc$sum=cumsum(roc$target)/sum(roc$target)
roc$num1=1:nrow(roc)/nrow(roc)
roc$num2=cumsum(roc$pred)/sum(roc$pred)
# library(ggplot2)
gg<-ggplot()+geom_line(data=data.frame(x=c(0,1),y=c(0,1)),aes(x=x,y=y,color="reference"))+
  geom_line(data=roc,aes(x=num2,y=sum,color="gbm"))
roc=data.frame(pred=test$BT,target=test$VRADE)
roc=roc[order(pred,decreasing=T),]
roc$sum=cumsum(roc$target)/sum(roc$target)
roc$num1=1:nrow(roc)/nrow(roc)
roc$num2=cumsum(roc$pred)/sum(roc$pred)
gg<-gg+geom_line(data=roc,aes(x=num2,y=sum,color="VRADE"))

# library(glmulti)
# model=glm(VRADE~(PUIS_REE+PRIXACT+iiactu+coeff+pmvkm+km+VIT_MAXI+CYLINDRE+mois)^2,
#           data=train,family=gaussian)
# step(model)

modell=glm(formula = VRADE ~ PUIS_REE + PRIXACT + iiactu + coeff + pmvkm + 
      km + VIT_MAXI + CYLINDRE + mois + PUIS_REE:PRIXACT + PUIS_REE:iiactu + 
      PUIS_REE:coeff + PUIS_REE:km + PUIS_REE:VIT_MAXI + PUIS_REE:CYLINDRE + 
      PUIS_REE:mois + PRIXACT:coeff + PRIXACT:pmvkm + PRIXACT:km + 
      PRIXACT:VIT_MAXI + PRIXACT:CYLINDRE + PRIXACT:mois + iiactu:pmvkm + 
      iiactu:CYLINDRE + coeff:pmvkm + coeff:km + coeff:VIT_MAXI + 
      coeff:mois + pmvkm:km + km:CYLINDRE + VIT_MAXI:CYLINDRE + 
      VIT_MAXI:mois, family = gaussian, data = train)
pred=predict(modell,newdata=test,type="response")

sum((predict(modell,newdata=test,type="response")-test$VRADE)^2>(predict(modelb,newdata=test,type="response")-test$VRADE)^2)/nrow(test)


roc=data.frame(pred=pred,target=test$VRADE)
roc=roc[order(pred,decreasing=T),]
roc$sum=cumsum(roc$target)/sum(roc$target)
roc$num1=1:nrow(roc)/nrow(roc)
roc$num2=cumsum(roc$pred)/sum(roc$pred)
gg<-gg+geom_line(data=roc,aes(x=num2,y=sum,color="glm"))
gg





# lines(quantile(test$VRADE/predict(modell,newdata=test,type="response"),1:99/100))

plot(quantile(roc$target/roc$pred,1:99/100))
lines()
predict(model)