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

library(pastecs)

library(frequency)

library(mfx)

library(betareg)

library(betareg)

library(bbmle)

library(fitdistrplus)

library(car)
library(pastecs)

library(modEvA)
library(performance)

library(msme)





##########################
#########GTWnNBR##########
##########################
gtwr=Dis_safety_df2000
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


library(MASS)
Dis_safety_df2000$probe=Dis_safety_df2000$SumDis*0

#Sampling local observations
k= 100 #kernel bandwith
pb=txtProgressBar(min = 0, max = nrow(gtwr), style = 3)
for (i in 1:nrow(gtwr))
{
  fordist=cbind(matrix(gtwr$Long[i],nrow(gtwr)),matrix(gtwr$Lat[i],nrow(gtwr)),gtwr$Long,gtwr$Lat)
  
  dist=apply(fordist,1,earthDist)
  wgt1=(1-(dist/max(dist))^2)^2 #adaptive bi-square kernel weighting for spatial
  
 # fortimedist=cbind(matrix(gtwr$datefrombase[i],nrow(gtwr)),gtwr$datefrombase) #such time distance is just for date
  #timediff=timedist(fortimedist)
  #wgt2=(1-(timediff/max(timediff))^2)^2 #adaptive bi-square kernel weighting for temporal
  
  #wgt=wgt1*wgt2 #combing all weights
  
  neighbor=order(-wgt1)[1:k] 
  a=as.data.frame(neighbor)
  m=which(neighbor==i)
  dist_subset=dist[neighbor]
 # timediff_subset=timediff[neighbor]
  
  reg_subset=gtwr[neighbor,]
  
  #re-weighting
  wgt1=(1-(dist_subset/max(dist_subset))^2)^2 #adaptive bi-square kernel weighting for spatial
  #wgt2=(1-(timediff_subset/max(timediff_subset))^2)^2 #adaptive bi-square kernel weighting for temporal
 # wgt_model=wgt1*wgt2 #combing all weights
  
  # neg.bin = glm.nb (Total2017 ~ AADT_2017+Segmentlength+Spd.Limit+Lanewidth+Feat.Width+
  #                     Feature.Compositionnew+LandUsenew+AccCtrlnew+RS+truckshare ,
  #                   data = reg_subset,weights = wgt_model)
 
  require(pscl)
  zinb = zeroinfl(Fatcrash ~ EnviroDis+Resilien_1+HealthDis+Transpor_1+Shape_Area+Highschool+College+PT+Taxi | Pop, dist = "negbin", data = reg_subset,weights = wgt1)
  
  summary(zinb)
          
   neg.bin = glm.nb (Fatcrash ~ EnviroDis+Resilien_1+HealthDis+Transpor_1+Pop+Shape_Area+Highschool+College+PT+Taxi, data = reg_subset,weights = wgt1)
  
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
    fitvalue=rbind(fitvalue,Dis_safety_df2000$probe)
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

