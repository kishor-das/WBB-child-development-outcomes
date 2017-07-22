#---------------------------
# tabs1-adherence.R
#
# Kishor Das (kishorisrt@gmail.com)
# estimate the effects of water
# sanitation intervention on easq
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
# output files:
#            table-s1.Rdata
#---------------------------

#---------------------------
# preamble
#---------------------------
rm(list = ls())

library(foreign) # for read.dta() function
library(plyr)    # for rename() function 
library(washb)   # https://ben-arnold.github.io/washb/

# function formatting results
dformat<-function(x,d){
  x<-round(x,d)+0.000 # removing signed zero
  x<-formatC(x,format="f",digits = d)
  return(x)
}


#-------------------------------
# load the analysis dataset
#-------------------------------

uptake<-  read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-uptake.dta")
arm<- read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-arm.dta")
uptake<-merge(uptake,arm,by="dataid")

group<-c("","Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N")

# number of clusters
#n_clust[""]<-"No. of clusters"
#for(arm in group[2:8]){
#  ind<-enrol$arm==arm
#  n_clust[arm]<-length(unique(enrol[ind,]$cluster))
#}
#dformat(sum(!is.na(easq[easq$arm==arm,]$com)),0)

# Stored drinking water
storewat_b<-NULL
storewat_y1<-NULL
storewat_y2<-NULL
storewat_b[""]<-"Baseline"
storewat_y1[""]<- "Year 1"
storewat_y2[""]<- "Year 2"
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==0 
  storewat_b[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$storewat)),0)
  storewat_b[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$storewat,na.rm =TRUE)*100,1)
  if (storewat_b[[paste(arm,"_%",sep="")]]=="NaN") storewat_b[[paste(arm,"_%",sep="")]]<-"--"
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==1 
  storewat_y1[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$storewat)),0)
  storewat_y1[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$storewat,na.rm =TRUE)*100,1)
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==2 
  storewat_y2[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$storewat)),0)
  storewat_y2[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$storewat,na.rm =TRUE)*100,1)
}


#Stored drinking water has detectable free chlorine
freechl_b<-NULL
freechl_y1<-NULL
freechl_y2<-NULL
freechl_b[""]<-  "Baseline"
freechl_y1[""]<- "Year 1"
freechl_y2[""]<- "Year 2"
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==0 
  freechl_b[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$freechl)),0)
  freechl_b[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$freechl,na.rm =TRUE)*100,1)
  if (freechl_b[[paste(arm,"_%",sep="")]]=="NaN") freechl_b[[paste(arm,"_%",sep="")]]<-"--"
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==1 
  freechl_y1[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$freechl)),0)
  freechl_y1[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$freechl,na.rm =TRUE)*100,1)
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==2 
  freechl_y2[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$freechl)),0)
  freechl_y2[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$freechl,na.rm =TRUE)*100,1)
}

#Latrine with a functional water seal
latseal_b<-NULL
latseal_y1<-NULL
latseal_y2<-NULL
latseal_b[""] <- "Baseline"
latseal_y1[""]<- "Year 1"
latseal_y2[""]<- "Year 2"

for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==0 
  latseal_b[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$latseal)),0)
  latseal_b[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$latseal,na.rm =TRUE)*100,1)
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==1 
  latseal_y1[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$latseal)),0)
  latseal_y1[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$latseal,na.rm =TRUE)*100,1)
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==2 
  latseal_y2[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$latseal)),0)
  latseal_y2[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$latseal,na.rm =TRUE)*100,1)
}

#No visible feces on latrine slab
latfeces_b<-NULL
latfeces_y1<-NULL
latfeces_y2<-NULL
latfeces_b[""]<- "Baseline"
latfeces_y1[""]<- "Year 1"
latfeces_y2[""]<- "Year 2"
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==0 
  latfeces_b[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$latfeces)),0)
  latfeces_b[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$latfeces,na.rm =TRUE)*100,1)
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==1 
  latfeces_y1[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$latfeces)),0)
  latfeces_y1[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$latfeces,na.rm =TRUE)*100,1)
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==2 
  latfeces_y2[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$latfeces)),0)
  latfeces_y2[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$latfeces,na.rm =TRUE)*100,1)
}


#Handwashing location has water and soap
hwsws_b<-NULL
hwsws_y1<-NULL
hwsws_y2<-NULL
hwsws_b[""]<-  "Baseline"
hwsws_y1[""]<- "Year 1"
hwsws_y2[""]<- "Year 2"
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==0 
  hwsws_b[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$hwsws)),0)
  hwsws_b[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$hwsws,na.rm =TRUE)*100,1)
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==1
  hwsws_y1[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$hwsws)),0)
  hwsws_y1[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$hwsws,na.rm =TRUE)*100,1)
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==2 
  hwsws_y2[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$hwsws)),0)
  hwsws_y2[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$hwsws,na.rm =TRUE)*100,1)
}

#LNS sachets consumed
rlnsp_b<-NULL
rlnsp_y1<-NULL
rlnsp_y2<-NULL
rlnsp_b[""]<-  "Baseline"
rlnsp_y1[""]<- "Year 1"
rlnsp_y2[""]<- "Year 2"

for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==0 
  rlnsp_b[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$rlnsp)),0)
  rlnsp_b[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$rlnsp,na.rm =TRUE)*100,1)
  if(rlnsp_b[[paste(arm,"_%",sep="")]]=="NaN") rlnsp_b[[paste(arm,"_%",sep="")]]<-"--"
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==1 
  rlnsp_y1[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$rlnsp)),0)
  rlnsp_y1[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$rlnsp,na.rm =TRUE)*100,1)
  if (rlnsp_y1[[paste(arm,"_%",sep="")]]=="NaN") rlnsp_y1[[paste(arm,"_%",sep="")]]<-"--"
}
for(arm in group[2:8]){
  ind<-uptake$arm==arm & uptake$svy==2 
  rlnsp_y2[[paste(arm,"_N",sep="")]]<- dformat(sum(!is.na(uptake[ind,]$rlnsp)),0)
  rlnsp_y2[[paste(arm,"_%",sep="")]]<- dformat(mean(uptake[ind,]$rlnsp,na.rm =TRUE)*100,1)
  if (rlnsp_y2[[paste(arm,"_%",sep="")]]=="NaN") rlnsp_y2[[paste(arm,"_%",sep="")]]="--" 
}


table_s1<-rbind(
  # Stored drinking water
  storewat_b,
  storewat_y1,
  storewat_y2,
  #Stored drinking water has detectable free chlorine
  freechl_b,
  freechl_y1,
  freechl_y2,
  #Latrine with a functional water seal
  latseal_b,
  latseal_y1,
  latseal_y2,
  #No visible feces on latrine slab
  latfeces_b,
  latfeces_y1,
  latfeces_y2,
  #Handwashing location has water and soap
  hwsws_b,
  hwsws_y1,
  hwsws_y2,
  #LNS sachets consumed
  rlnsp_b,
  rlnsp_y1,
  rlnsp_y2  
)
save(table_s1,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\table-s1.Rdata")
