require(foreign)
library(foreign)
#install.packages('ordinal') 
library(ordinal) 
#install.packages("AER")
library(AER)
#install.packages('erer')
library(erer)
require(Hmisc)
require(MASS)
library(MASS)
require(nnet)
#install.packages("mlogit")
require(mlogit)
options(scipen=99)
#install.packages('janitor')
library(janitor)
install.packages("pastecs")
library(pastecs)
install.packages("frequency")
library(frequency)
install.packages("mfx")
library(mfx)
install.packages("betareg")
library(betareg)
install.packages("Formula")
library(betareg)
install.packages("bbmle")
library(bbmle)
install.packages("fitdistrplus")
library(fitdistrplus)
install.packages("car")
library(car)
library(pastecs)
install.packages("QuartileS")
install.packages("modEvA")
library(modEvA)
library(performance)
install.packages("performance")
library(msme)
install.packages("msme")

##################
##input the data##
##################

Multilane<-read.csv("data.csv")

######################
####Data Processing###
######################
#Creating dummy observations considering the year##
Multilane13<- Multilane[c(1:32)]
Multilane13$Year<- 2013 

Multilane144<-Multilane[c(1:30)]
Multilane1444<-Multilane[c(33:34)]
Multilane14<- cbind(Multilane144, Multilane1444)
Multilane14$Year<- 2014

Multilane155<-Multilane[c(1:30)]
Multilane1555<-Multilane[c(35:36)]
Multilane15<- cbind(Multilane155, Multilane1555)
Multilane15$Year<- 2015

Multilane166<-Multilane[c(1:30)]
Multilane1666<-Multilane[c(37:38)]
Multilane16<- cbind(Multilane166, Multilane1666)
Multilane16$Year<- 2016

Multilane177<-Multilane[c(1:30)]
Multilane1777<-Multilane[c(39:40)]
Multilane17<- cbind(Multilane177, Multilane1777)
Multilane17$Year<- 2017

colnames(Multilane13)=colnames(Multilane14)=colnames(Multilane15)=colnames(Multilane16)=colnames(Multilane17)
Multilane<- rbind (Multilane13, Multilane14, Multilane15, Multilane16, Multilane17)



Multilane=Multilane[Multilane$Lat!="Not Showing",]
Multilane=Multilane[Multilane$Lat!="",]
Multilane=Multilane[Multilane$Lat!="NA",]
Multilane=Multilane[Multilane$Long!="NA",]
Multilane=Multilane[Multilane$ShoulderWidthInside_oneside!="",]
Multilane=Multilane[Multilane$ShoulderWidthInside_oneside!="No",]
Multilane=Multilane[Multilane$Shouldertypeinside!="",]
Multilane=Multilane[Multilane$Shouldertypeinside!="Soil (Dirt)",]
Multilane=Multilane[Multilane$ShoulderWidthOutside_oneside!="NA",]
Multilane=Multilane[Multilane$Shouldertypeoutside!="",]
Multilane=Multilane[Multilane$Spd.Limit!="NA",]
Multilane=Multilane[Multilane$Feat.Width!="No",]
Multilane=Multilane[Multilane$Feat.Width!="",]
Multilane=Multilane[Multilane$Feature.Composition!="RIVER",]
Multilane=Multilane[Multilane$Feature.Composition!="No",]
Multilane=Multilane[Multilane$Feature.Composition!="",]
Multilane=Multilane[Multilane$Lanewidth!="",]
Multilane=Multilane[Multilane$AADT_2017!="NA",]
Multilane=Multilane[Multilane$AADT_2017!="0",]



Terrainnew=Multilane$ELM*0
Terrainnew[Multilane$Terrain=="1-FLAT"]=1
Terrainnew[Multilane$Terrain=="2-ROLLING"]=2
Terrainnew[Multilane$Terrain=="3-MOUNT."]=3
Multilane$Terrainnew<-Terrainnew
Multilane$Terrainnew=factor(Terrainnew,levels = c(1,2,3))
tabyl(Multilane$Terrain)
tabyl(Multilane$Terrainnew)

LandUsenew=Multilane$ELM*0
LandUsenew[Multilane$Land.Use=="0-RURAL"]=1
LandUsenew[Multilane$Land.Use=="2-COMMERCIAL"]=2
LandUsenew[Multilane$Land.Use=="4-FRINGE (MIX RES. COMM.)"]=2
# LandUsenew[Multilane$Land.Use=="5-INDUSTRIAL (FACTORIES, WAREHOUSES)"]=4
LandUsenew[Multilane$Land.Use=="7-RESIDENTIAL"]=3
# LandUsenew[Multilane$Land.Use=="9-PUBLIC USE (PARKS, SCHOOLS)"]=6
Multilane$LandUsenew=LandUsenew
Multilane$LandUsenew=as.factor(Multilane$LandUsenew)
table(Multilane$LandUsenew)


Illumnew=Multilane$ELM*0
Illumnew[Multilane$Illum=="No"]=0
Illumnew[Multilane$Illum=="YES"]=1
Illumnew[Multilane$Illum=="N"]=2
Multilane$Illumnew=Illumnew
Multilane$Illumnew=factor(Illumnew,levels = c(0,1,2))
tabyl(Multilane$Illum)
tabyl(Multilane$Illumnew)


AccCtrlnew=Multilane$ELM*0
AccCtrlnew[Multilane$Acc..Ctrl.=="0-NONE"]=0
AccCtrlnew[Multilane$Acc..Ctrl.=="1-PARTIAL"]=1
AccCtrlnew[Multilane$Acc..Ctrl.=="2-FULL"]=2
Multilane$AccCtrlnew=AccCtrlnew
Multilane$AccCtrlnew=factor(AccCtrlnew,levels = c(0,1,2))
tabyl(Multilane$Acc..Ctrl.)
tabyl(Multilane$AccCtrlnew)


Shouldertypeinsidenew=Multilane$ELM*0
Shouldertypeinsidenew[Multilane$Shouldertypeinside=="AC"]=0
Shouldertypeinsidenew[Multilane$Shouldertypeinside=="Asphalt Concrete"]=0
Shouldertypeinsidenew[Multilane$Shouldertypeinside=="PC Concrete"]=1
# Shouldertypeinsidenew[Multilane$Shouldertypeinside=="Portland Cement Concrete"]=0
Shouldertypeinsidenew[Multilane$Shouldertypeinside=="Gravel"]=1
Shouldertypeinsidenew[Multilane$Shouldertypeinside=="NIL"]=1
Multilane$Shouldertypeinsidenew=Shouldertypeinsidenew
Multilane$Shouldertypeinsidenew=factor(Shouldertypeinsidenew,levels = c(0,1))
tabyl(Multilane$Shouldertypeinside)
tabyl(Multilane$Shouldertypeinsidenew)


Shouldertypeoutsidenew=Multilane$ELM*0
Shouldertypeoutsidenew[Multilane$Shouldertypeoutside=="Asphalt Concrete"]=0
Shouldertypeoutsidenew[Multilane$Shouldertypeoutside=="PC Concrete"]=1
Shouldertypeoutsidenew[Multilane$Shouldertypeoutside=="Gravel"]=2
Shouldertypeoutsidenew[Multilane$Shouldertypeoutside=="gravel"]=2
Shouldertypeoutsidenew[Multilane$Shouldertypeoutside=="Soil"]=2
Multilane$Shouldertypeoutsidenew=Shouldertypeoutsidenew
Multilane$Shouldertypeoutsidenew=factor(Shouldertypeoutsidenew,levels = c(0,1,2))
tabyl(Multilane$Shouldertypeoutside)
tabyl(Multilane$Shouldertypeoutsidenew)



Feature.Compositionnew=Multilane$ELM*0
#BARRIER
Feature.Compositionnew[Multilane$Feature.Composition=="CONCRETE BARRIER"]=1
Feature.Compositionnew[Multilane$Feature.Composition=="OTHER BARRIER"]=1
# #MOUNTABLE
# Feature.Compositionnew[Multilane$Feature.Composition=="MOUNTABLE (CONCRETE)"]=2
# Feature.Compositionnew[Multilane$Feature.Composition=="OTHER MOUNTABLE"]=2
#GRASS
Feature.Compositionnew[Multilane$Feature.Composition=="GRASS PLOT"]=2
Feature.Compositionnew[Multilane$Feature.Composition=="GRASS PLOT W/ CABLE BARRIER LT."]=2
Feature.Compositionnew[Multilane$Feature.Composition=="GRASS PLOT W/ CABLE BARRIER RT."]=2
Feature.Compositionnew[Multilane$Feature.Composition=="GRASS PLOT W/ CABLE BARRIER RT. & LT."]=2
#PAINTED
Feature.Compositionnew[Multilane$Feature.Composition=="PAINTED"]=3
#OTHER
Feature.Compositionnew[Multilane$Feature.Composition=="NON-MOUNTABLE (CONCRETE)"]=4
Feature.Compositionnew[Multilane$Feature.Composition=="RIVER OR STREAM"]=4
Feature.Compositionnew[Multilane$Feature.Composition=="PRIVATE LAND IN MEDIAN"]=4
Feature.Compositionnew[Multilane$Feature.Composition=="MOUNTABLE (CONCRETE)"]=4
Feature.Compositionnew[Multilane$Feature.Composition=="OTHER MOUNTABLE"]=4
Multilane$Feature.Compositionnew=Feature.Compositionnew
Multilane$Feature.Compositionnew=factor(Feature.Compositionnew,levels = c(1,2,3,4))
tabyl(Multilane$Feature.Composition)
tabyl(Multilane$Feature.Compositionnew)


# RSOutsidenew=Multilane$ELM*0
# RSOutsidenew[Multilane$RSOutside=="Yes"]=1
# RSOutsidenew[Multilane$RSOutside=="No"]=0
# RSOutsidenew[Multilane$RSOutside==""]=0
# Multilane$RSOutsidenew=RSOutsidenew
# table(RSOutsidenew)
# 
# RSInsidenew=Multilane$ELM*0
# RSInsidenew[Multilane$RSInside=="Yes"]=1
# RSInsidenew[Multilane$RSInside=="No"]=0
# RSInsidenew[Multilane$RSInside==""]=0
# RSInsidenew[Multilane$RSInside=="12"]=0
# Multilane$RSInsidenew=RSInsidenew
# table(RSInsidenew)


Multilane$RS=factor(Multilane$RS,levels = c(0,1))
tabyl(Multilane$RS)


# RSCenternew=Multilane$ELM*0
# RSCenternew[Multilane$RSCenter=="Y"]=1
# RSCenternew[Multilane$RSCenter=="N"]=0
# Multilane$RSCenternew=RSCenternew
# Multilane$RSCenternew=as.factor(Multilane$RSCenternew)
# table(RSCenternew)


# Lanewidth=(Multilane$PavementWidth)/(Multilane$Nooflanes)
# Multilane$Lanewidth=Lanewidth
# table(Lanewidth)



Acordeclane=Multilane$ELM*0
for (i in 1:length(Multilane$ELM)){
  if (Multilane$Acelerationordecelerationlane[i]=="Yes") {
    Acordeclane[i]=1
    } else {
      Acordeclane[i]=0
    }
  }
Multilane$Acordeclane=Acordeclane
Multilane$Acordeclane=factor(Acordeclane,levels = c(0,1))
tabyl(Multilane$Acordeclane)



# RightturnorLeftturnlanenew=Multilane$ELM*0
# for (i in 1:length(Multilane$ELM)){
#   if (Multilane$RightturnorLeftturnlane[i]=="N") {
#     RightturnorLeftturnlanenew[i]=0
#   } else {
#     RightturnorLeftturnlanenew[i]=1
#   }
# }
# Multilane$RightturnorLeftturnlanenew=RightturnorLeftturnlanenew
# Multilane$RightturnorLeftturnlanenew=as.factor(Multilane$RightturnorLeftturnlanenew)
# table(RightturnorLeftturnlanenew)


# Bikelanenew=Multilane$ELM*0
# for (i in 1:length(Multilane$ELM)){
#   if (Multilane$Bikelane[i]=="N") {
#     Bikelanenew[i]=0
#   }else if(Multilane$Bikelane[i]=="No")
#     {
#     Bikelanenew[i]=0
#   }
#   else {
#     Bikelanenew[i]=1
#   }
# }
# Multilane$Bikelanenew=Bikelanenew
# Multilane$Bikelanenew=as.factor(Multilane$Bikelanenew)
# table(Bikelanenew)



Multilane$AADT_2017=log(Multilane$AADT_2017) 
Multilane$Segmentlength=log(Multilane$Segmentlength)


Multilane$ShoulderWidthInside_oneside=as.numeric(as.character(Multilane$ShoulderWidthInside_oneside))
Multilane$ShoulderWidthOutside_oneside=as.numeric(as.character(Multilane$ShoulderWidthOutside_oneside))
Multilane$ShoulderWidth=(Multilane$ShoulderWidthInside_oneside+Multilane$ShoulderWidthOutside_oneside)*0.5
stat.desc(Multilane$ShoulderWidth, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)

Multilane$SU.Truck..=as.numeric(as.character(Multilane$SU.Truck..))
Multilane$MU.Truck..=as.numeric(as.character(Multilane$MU.Truck..))
Multilane$truckshare <- Multilane$SU.Truck..+Multilane$MU.Truck..
tabyl(Multilane$truckshare)
stat.desc(Multilane$truckshare, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)



# Multilane$Lat=as.numeric(as.character(Multilane$Lat))
# Multilane$Long=as.numeric(as.character(Multilane$Long))
Multilane$Spd.Limit=as.numeric(as.character(Multilane$Spd.Limit))
Multilane$ShoulderWidth=as.numeric(as.character(Multilane$ShoulderWidth))
Multilane$Feat.Width=as.numeric(as.character(Multilane$Feat.Width))
Multilane$Lanewidth=as.numeric(as.character(Multilane$Lanewidth))
Multilane$AADT_2017=as.numeric(as.character(Multilane$AADT_2017))
Multilane$Segmentlength=as.numeric(as.character(Multilane$Segmentlength))
Multilane$Total2017=as.numeric(as.character(Multilane$Total2017))


#Checking for missing values
Multilane <- Multilane[Multilane$truckshare!= "NA",]


write.csv(Multilane, "Multilane.csv")


# install.packages("pastecs")
# library(pastecs)
stat.desc(Multilane$Segmentlength, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
stat.desc(Multilane$AADT_2017, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)


###############################
##Poisson and NB Model#########
###############################
####Pisson
# poisson = glm (Total2017 ~ Segmentlength + AADT_2017+ Spd.Limit + Feat.Width + AADT_2017 +
#                  Illumnew + AccCtrlnew + Shouldertypeinsidenew + Shouldertypeoutsidenew +
#                  Feature.Compositionnew + RSpresence + Acordeclane + ShoulderWidth +truckshare
#                  ,family= "poisson" , data = Multilane)
# summary(poisson)
# 
# 
# poissonmfx(formula= Total2017 ~ Segmentlength + AADT_2017+ Spd.Limit + Feat.Width + AADT_2017 +
#              Illumnew + AccCtrlnew + Shouldertypeinsidenew + Shouldertypeoutsidenew +
#              Feature.Compositionnew + RSpresence + Acordeclane + ShoulderWidth +truckshare ,  data = Multilane, atmean = FALSE)
# 
# 
# #removing insignificant variables
# poisson = glm (Total2017 ~ AADT_2017+length+SpdLimit + Terrainnew + LandUsenew +AccCtrlnew 
#                 + Shouldertypeinsidenew +Shouldertypeoutsidenew +
#                  MedianWidth1 + Feature.Compositionnew+RSpresencenew
#                +Lanewidth+Acordeclane ,family= "poisson" , data = Multilane)
# summary(poisson)
# ctable = coef(summary(poisson))
# write.csv(ctable,file = "Poisson Estimate.csv")
# 
# poissonmfx(Total2017 ~ AADT_2017+length+SpdLimit + Terrainnew + LandUsenew +AccCtrlnew 
#            + Shouldertypeinsidenew +Shouldertypeoutsidenew +
#              MedianWidth1 + Feature.Compositionnew+RSpresencenew
#            +Lanewidth+Acordeclane ,  data = Multilane, atmean = TRUE)



####negative.binomial()
neg.bin = glm.nb (Total2017 ~ Terrainnew+ Segmentlength + Spd.Limit + Feat.Width + AADT_2017 +
                    Illumnew + AccCtrlnew + Shouldertypeinsidenew + Shouldertypeoutsidenew +
                    Feature.Compositionnew + RS +LandUsenew+ Acordeclane + ShoulderWidth
                  +truckshare+Feature.Compositionnew+Lanewidth, data = Multilane)
summary(neg.bin)


negbinmfx(Total2017 ~ Terrainnew+ Segmentlength + Spd.Limit + Feat.Width + AADT_2017 +
            Illumnew + AccCtrlnew + Shouldertypeinsidenew + Shouldertypeoutsidenew +
            Feature.Compositionnew + RS +LandUsenew+ Acordeclane + ShoulderWidth
          +truckshare+Feature.Compositionnew+Lanewidth, 
          data = Multilane, atmean = TRUE)


#removing insignificant variables
neg.bin = glm.nb (Total2017 ~ AADT_2017+Segmentlength+Spd.Limit+Lanewidth+Feat.Width+
                    Feature.Compositionnew+LandUsenew+AccCtrlnew+RS+truckshare ,
                  data = Multilane)
summary(neg.bin)

neg.bin = glm.nb (Total2017 ~ AADT_2017+Segmentlength, data = Multilane)
summary(neg.bin)

negbinmfx(Total2017 ~ AADT_2017+Segmentlength+Spd.Limit+Lanewidth+Feat.Width+
            Feature.Compositionnew+LandUsenew+AccCtrlnew+RS+truckshare,data = Multilane, atmean = TRUE)

negbinmfx(Total2017 ~ AADT_2017+Segmentlength,data = Multilane, atmean = TRUE)




#loglikelihood, chi2, and AIC for Poisson
Loglike= sum((-poisson$fitted.values)+(Multilane$Total2017*log(poisson$fitted.values))-
               log(factorial(Multilane$Total2017)))

Chi2= sum((((Multilane$Total2017)-(poisson$fitted.values))^2)/poisson$fitted.values)

aic=2*length(poisson$coefficients)-2*Loglike

#Prob
t=as.data.frame(table(Multilane$Total2017))
t$prob=(t$Freq)/sum(t$Freq)
Multilane$probe=Multilane$ELM*0
for(i in 1:length(Multilane$Total2017))
{
  for(j in 1:length(t$Var1)){
    if(Multilane$Total2017[i]==t$Var1[j])
    {Multilane$probe[i]=t$prob[j]}else 
    {j=j+1}
  }
}



#loglikelihood, AIC, and chi2 for NB

Yi=Multilane$Total2017
a=1/(neg.bin$theta)
M=neg.bin$fitted.values
logj=Multilane$ELM*0

logi=Multilane$ELM*0
for(i in 1:length(Multilane$Total2017)){
  
  if (Multilane$Total2017[i]==0) 
  {sumloggj=0}
  else { 
    for(j in 1:Multilane$Total2017[i])
      
    {
      logj=log((j-1)+(a^-1))
      if (j==1) {sumloggj=logj} else {sumloggj=logj+sumloggj}
    }
  }
  
  logi[i]=sumloggj-log(factorial(Yi[i]))-((Yi[i]+a^-1)*log(1+(a*M[i])))+(Yi[i]*log(a))+(Yi[i]*log(M[i]))
}
logi<- as.data.frame(logi)

loglike=sum(logi)

Chi2= sum((((Multilane$Total2017)-(neg.bin$fitted.values))^2)/neg.bin$fitted.values)

aic=2*length(neg.bin$coefficients)-2*loglike



#Pssudo-r2 and overdispersion 
RsqGLM(model = neg.bin)
check_overdispersion(neg.bin)
P__disp(neg.bin)


#get mean at base
####get the means at base############
modeldata=model.frame(neg.bin)
for(i in 1:ncol(modeldata)){
  print(i)
  var=modeldata[,i]
  attr=as.character(as.data.frame(table(var))[,1])
  m=which(modeldata[,i]==attr[1])
  x=table(modeldata$Total2017[m])/length(m)
  if(i==1){
    out_mean=x
  }else{
    out_mean=rbind(out_mean,x)
  }
}
x=cbind(colnames(modeldata),out_mean)



##########################
#########GTWnNBR##########
##########################
gtwr=Multilane
#Calculating distances from Lat and Long, in miles
earthDist = function (X){
  rad <- pi/180
  a1 <- X[2] * rad
  a2 <- X[1] * rad
  b1 <- X[4] * rad
  b2 <- X[3] * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 3959
  d <- R * c
  return(d)
} 

table(Multilane$Year)
# table(crash$CrashMonth)
# table(crash$CrashDate)
# table(crash$CrashTime)
# datenew=paste(substr(crash$CrashDate,1,2),substr(crash$CrashDate,3,5),substr(crash$CrashDate,6,9),sep = " ")
# datenew=strptime(datenew,  "%d %b %Y")
# crash=cbind(crash,datenew)
basetime=2013
datefrombase=gtwr$Year-basetime
gtwr$datefrombase=datefrombase
# crash=cbind(crash,datefrombase)
#time distance function
timedist = function(x){
  diff=abs(x[,1]-x[,2])
  return(diff)
}

#Sampling local observations
k= 850 #kernel bandwith
pb=txtProgressBar(min = 0, max = nrow(gtwr), style = 3)
for (i in 1:nrow(gtwr))
{
  fordist=cbind(matrix(gtwr$Long[i],nrow(gtwr)),matrix(gtwr$Lat[i],nrow(gtwr)),gtwr$Long,gtwr$Lat)
  
  dist=apply(fordist,1,earthDist)
  wgt1=(1-(dist/max(dist))^2)^2 #adaptive bi-square kernel weighting for spatial
  
  fortimedist=cbind(matrix(gtwr$datefrombase[i],nrow(gtwr)),gtwr$datefrombase) #such time distance is just for date
  timediff=timedist(fortimedist)
  wgt2=(1-(timediff/max(timediff))^2)^2 #adaptive bi-square kernel weighting for temporal
  
  wgt=wgt1*wgt2 #combing all weights
  
  neighbor=order(-wgt)[1:k] 
  a=as.data.frame(neighbor)
  m=which(neighbor==i)
  dist_subset=dist[neighbor]
  timediff_subset=timediff[neighbor]
  
  reg_subset=gtwr[neighbor,]
  
  #re-weighting
  wgt1=(1-(dist_subset/max(dist_subset))^2)^2 #adaptive bi-square kernel weighting for spatial
  wgt2=(1-(timediff_subset/max(timediff_subset))^2)^2 #adaptive bi-square kernel weighting for temporal
  wgt_model=wgt1*wgt2 #combing all weights
  
  # neg.bin = glm.nb (Total2017 ~ AADT_2017+Segmentlength+Spd.Limit+Lanewidth+Feat.Width+
  #                     Feature.Compositionnew+LandUsenew+AccCtrlnew+RS+truckshare ,
  #                   data = reg_subset,weights = wgt_model)
  neg.bin = glm.nb (Total2017 ~ AADT_2017+Segmentlength,data = reg_subset,weights = wgt_model)
  
  ctable <- coef(summary(neg.bin))
  # p <- round((pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2), 7)
  # ctable <- cbind(ctable, "p value" = p)
  est = ctable
  se=est[,2]
  coefficient = est[,1]
  tm=which(is.na(coefficient)==T)
  est[tm,]=0
  est[,3]=abs(est[,3])
  
  if (is.na(mean(est[,1]))|max(est[,2])>20)
  {
    output_final=rbind(output_final,matrix(NA,1,ncol=ncol(output_final))) 
    fitvalue=rbind(fitvalue,Multilane$probe)
  }else{
    
    output1=est
    output1=as.matrix(output1)
    row_change=output1[,1]
    z_value = output1[,3]
    if (i==1){
      output_final=row_change
      output_z = z_value
      fitvalue=neg.bin$fitted.values[m] 
    }else{
      output_final=rbind(output_final,row_change)
      output_z = rbind(output_z,z_value)
      fitvalue=rbind(fitvalue,neg.bin$fitted.values[m])
    }
  }
  setTxtProgressBar(pb, i)
  print(i)
  print(fitvalue[i])
}
output_final1=cbind(gtwr,output_final,fitvalue)
colnames(output_final1)[colnames(output_final1)=="1"] <- "fitvalue"
write.csv(output_final1,file="gtwnbr_result_forplot_new2.csv",row.names = F)
write.csv(output_z,file="output_z.csv",row.names = F)
save(output_final1,file="gtwnbr_output_all.RData")
save.image(file="C:/Users/amoham17/Google Drive (amoham17@vols.utk.edu)/RA/Safety Function/Data/Rural muti-nanes highways/Input data/Safety function/image_gtwnbr_output_all.RData")
load("C:/Users/amoham17/Google Drive (amoham17@vols.utk.edu)/RA/Safety Function/Data/Rural muti-nanes highways/Input data/Safety function/image_gtwnbr_output_all.RData")


###loglikelihood, chi2, and AIC for GTWNBR###

Yi=Multilane$Total2017
a=1/(neg.bin$theta)
M=output_final1$fitvalue
logj=Multilane$ELM*0

logi=Multilane$ELM*0
for(i in 1:length(Multilane$Total2017)){
  
  if (Multilane$Total2017[i]==0) 
  {sumloggj=0}
  else { 
    for(j in 1:Multilane$Total2017[i])
      
    {
      logj=log((j-1)+(a^-1))
      if (j==1) {sumloggj=logj} else {sumloggj=logj+sumloggj}
    }
  }
  
  logi[i]=sumloggj-log(factorial(Yi[i]))-((Yi[i]+a^-1)*log(1+(a*M[i])))+(Yi[i]*log(a))+(Yi[i]*log(M[i]))
}
loglike=sum(logi)

Chi2= sum((((Multilane$Total2017)-(output_final1$fitvalue))^2)/output_final1$fitvalue)

aic=2*length(neg.bin$coefficients)-2*loglike




##Stationary test
stat.desc(output_final1[,46:48], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)


IQR(output_final1[,46], na.rm = FALSE, type = 7)

x <- rnorm(100)
quantile(output_final1[,48], probs = c(0.25,0.75))


##tWNBR##
TWNBR<-output_final1[,67:82]
TWNBR$year <- output_final1$Year
TWNBR<-aggregate(TWNBR, by = list(TWNBR$year), FUN = "mean")
write.csv(TWNBR, file = "TNBR.csv")


stat.desc(Multilane$length, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)

max(output_z[,13])




##############################
#########GTW-poisson##########
##############################
gtwr=Multilane
#Calculating distances from Lat and Long, in miles
earthDist = function (X){
  rad <- pi/180
  a1 <- X[2] * rad
  a2 <- X[1] * rad
  b1 <- X[4] * rad
  b2 <- X[3] * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 3959
  d <- R * c
  return(d)
} 

table(Multilane$Year)
# table(crash$CrashMonth)
# table(crash$CrashDate)
# table(crash$CrashTime)
# datenew=paste(substr(crash$CrashDate,1,2),substr(crash$CrashDate,3,5),substr(crash$CrashDate,6,9),sep = " ")
# datenew=strptime(datenew,  "%d %b %Y")
# crash=cbind(crash,datenew)
basetime=2013
datefrombase=gtwr$Year-basetime
gtwr$datefrombase=datefrombase
# crash=cbind(crash,datefrombase)
#time distance function
timedist = function(x){
  diff=abs(x[,1]-x[,2])
  return(diff)
}

#Sampling local observations
k= 950 #kernel bandwith
pb=txtProgressBar(min = 0, max = nrow(gtwr), style = 3)
for (i in 1:nrow(gtwr)){
  fordist=cbind(matrix(gtwr$Long[i],nrow(gtwr)),matrix(gtwr$Lat[i],nrow(gtwr)),gtwr$Long,gtwr$Lat)
  
  dist=apply(fordist,1,earthDist)
  wgt1=(1-(dist/max(dist))^2)^2 #adaptive bi-square kernel weighting for spatial
  
  fortimedist=cbind(matrix(gtwr$datefrombase[i],nrow(gtwr)),gtwr$datefrombase) #such time distance is just for date
  timediff=timedist(fortimedist)
  wgt2=(1-(timediff/max(timediff))^2)^2 #adaptive bi-square kernel weighting for temporal
  
  wgt=wgt1*wgt2 #combing all weights
  
  neighbor=order(-wgt)[1:k] 
  a=as.data.frame(neighbor)
  m=which(neighbor==i)
  dist_subset=dist[neighbor]
  timediff_subset=timediff[neighbor]
  
  reg_subset=gtwr[neighbor,]
  
  #re-weighting
  wgt1=(1-(dist_subset/max(dist_subset))^2)^2 #adaptive bi-square kernel weighting for spatial
  wgt2=(1-(timediff_subset/max(timediff_subset))^2)^2 #adaptive bi-square kernel weighting for temporal
  wgt_model=wgt1*wgt2 #combing all weights
  
  resu = glm (Total2017 ~ AADT_2017+length+SpdLimit + Terrainnew + LandUsenew +AccCtrlnew 
              + Shouldertypeinsidenew +Shouldertypeoutsidenew +
                MedianWidth1 + Feature.Compositionnew+RSpresencenew
              +Lanewidth+Acordeclane ,family= "poisson",
              data = reg_subset,weights = wgt_model)
  
  
  summary(resu)
  
    ctable <- coef(summary(resu))
    # p <- round((pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2), 7)
    # ctable <- cbind(ctable, "p value" = p)
    est = ctable
    se=est[,2]
    coefficient = est[,1]
    tm=which(is.na(coefficient)==T)
    est[tm,]=0
    est[,3]=abs(est[,3])
  # }
  
  if (is.na(mean(est[,1]))|ifmax(est[,2])>20){
    output_final=rbind(output_final,matrix(NA,1,ncol=ncol(output_final))) 
    fitvalue=rbind(fitvalue,Multilane$probe)
  }else{
    
    #logresu=glm(injury ~ crashtypenew + YOUNG + ELDERLY + dui + pedbike + light + badroadcon
    #+ speednew, weights = wgt,
    #family = binomial("logit"),data=reg_subset)
    #logest=summary(logresu)$coefficients
    output1=est
    #colnames(output1)=c("Estimate" ,"Std. Error", "t value", "Pr(>|t|)") #to note the output information
    output1=as.matrix(output1)
    row_change=output1[,1]
    if (i==1){
      output_final=row_change  
      fitvalue=resu$fitted.values[m] 
    }else{
      output_final=rbind(output_final,row_change)
      fitvalue=rbind(fitvalue,resu$fitted.values[m])
    }
  }
  setTxtProgressBar(pb, i)
  # if(i%%1000==0){
  # prog_x=as.character(i)
  # spacename=paste("C:/Users/amoham17/Google Drive (amoham17@vols.utk.edu)/RA/Safety Function/Data/Rural muti-nanes highways/Input data/Safety function",prog_x,'.RData',sep="")
  # save(fitvalue,file = spacename)
  # save.image("C:/Users/amoham17/Google Drive (amoham17@vols.utk.edu)/RA/Safety Function/Data/Rural muti-nanes highways/Input data/Safety function")
  # }
}

tabyl(Multilane$Lanewidth)
output_final1=cbind(gtwr,output_final,fitvalue)
colnames(output_final1)[colnames(output_final1)=="1"] <- "fitvalue"
write.csv(output_final1,file="gtwpr_result_forplot_new.csv",row.names = F)
save(output_final1,file="gtwr_output_all.RData")
save.image(file="C:/Users/amoham17/Google Drive (amoham17@vols.utk.edu)/RA/Safety Function/Data/Rural muti-nanes highways/Input data/Safety function/image_gtwr_output_all.RData")
load("C:/Users/amoham17/Google Drive (amoham17@vols.utk.edu)/RA/Safety Function/Data/Rural muti-nanes highways/Input data/Safety function/image_gtwr_output_all.RData")


#loglikelihood, chi2, and AIC

Loglike= sum((-output_final1$fitvalue)+(Multilane$Total2017*log(output_final1$fitvalue))-
               log(factorial(Multilane$Total2017)))

Chi2= sum((((Multilane$Total2017)-(output_final1$fitvalue))^2)/output_final1$fitvalue)

aic=2*length(poisson$coefficients)-2*Loglike



##Stationary test
stat.desc(output_final1[,66:88], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)

IQR(output_final1[,81], na.rm = FALSE, type = 7)

x <- rnorm(100)
quantile(output_final1[,81], probs = c(0.25,0.75))


