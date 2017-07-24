#---------------------------
# tab3-easq.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# intervention on easq
# unadjusted analysis
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
# output files:
#            easq.Rdata
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

easq<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-easq-year2.dta")

easq<- subset(easq,
              select=c("dataid","childid","tchild","clusterid",
                       "block","arm","svydate","sex","dob","agedays",
                       "ageyears","agegroup","fracode","res","care","resage",
                       "z_com","z_motor","z_personal","z_combined" ))  

easq<- rename(easq,
              replace=c("z_com"="com",
                        "z_motor"="motor",
                        "z_personal"="personal",
                        "z_combined"="combined"
              )) 

group<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N")

#-----------------------------------------
# analysis for easq-communication z-score
#-----------------------------------------
#column 1
com_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
com_col_2<-NULL
for(arm in group){
  com_col_2[arm]<-dformat(sum(!is.na(easq[easq$arm==arm,]$com)),0)
  
}
#column 3
com_col_3<-NULL
for(arm in group){
  ind<-easq$arm==arm
  mean<-  mean(easq[ind,]$com,na.rm =TRUE)
  SD<-  sd(easq[ind,]$com,na.rm =TRUE)
  com_col_3[arm]<-  paste(dformat(mean,2)," (",dformat(SD,2),")",sep="")
}
#column 4
com_col_4<-NULL
com_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)
  com_col_4[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}
#column 5
com_col_5<-NULL
com_col_5["Control"]<-     ""
com_col_5["Water"]<-       ""
com_col_5["Sanitation"]<-  ""
com_col_5["Handwashing"]<- ""
com_col_5["WSH"]<-         "Ref"
com_col_5["Nutrition"]<-   ""
for(arm in group[7]){
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("WSH",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  com_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="") 
}
#column 6
com_col_6<-NULL
com_col_6["Control"]<-     ""
com_col_6["Water"]<-       ""
com_col_6["Sanitation"]<-  ""
com_col_6["Handwashing"]<- ""
com_col_6["WSH"]<-         ""
com_col_6["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("Nutrition",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  com_col_6[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}

#-----------------------------------------
# analysis for easq-gross-motor z-score
#-----------------------------------------
#column 1
mot_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
mot_col_2<-NULL
for(arm in group){
  mot_col_2[arm]<-dformat(sum(!is.na(easq[easq$arm==arm,]$motor)),0)
  
}
#column 3
mot_col_3<-NULL
for(arm in group){
  ind<-easq$arm==arm
  mean<-  dformat(mean(easq[ind,]$motor,na.rm =TRUE),2)
  SD<-    dformat(sd(easq[ind,]$motor,na.rm =TRUE),2)
  mot_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
mot_col_4<-NULL
mot_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  mot_col_4[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#column 5
mot_col_5<-NULL
mot_col_5["Control"]<-     ""
mot_col_5["Water"]<-       ""
mot_col_5["Sanitation"]<-  ""
mot_col_5["Handwashing"]<- ""
mot_col_5["WSH"]<-         "Ref"
mot_col_5["Nutrition"]<-   ""
for(arm in group[7]){
  reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("WSH",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  mot_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")   
}
#column 6
mot_col_6<-NULL
mot_col_6["Control"]<-     ""
mot_col_6["Water"]<-       ""
mot_col_6["Sanitation"]<-  ""
mot_col_6["Handwashing"]<- ""
mot_col_6["WSH"]<-         ""
mot_col_6["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("Nutrition",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  mot_col_6[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}


#-------------------------------------------
# analysis for easq-personal-social z-score
#-------------------------------------------
#column 1
per_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
per_col_2<-NULL
for(arm in group){
  per_col_2[arm]<-dformat(sum(!is.na(easq[easq$arm==arm,]$personal)),0)
  
}
#column 3
per_col_3<-NULL
for(arm in group){
  ind<-easq$arm==arm
  mean<-  dformat(mean(easq[ind,]$personal,na.rm =TRUE),2)
  SD<-    dformat(sd(easq[ind,]$personal,na.rm =TRUE),2)
  per_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
per_col_4<-NULL
per_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  per_col_4[arm]<-paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#column 5
per_col_5<-NULL
per_col_5["Control"]<-     ""
per_col_5["Water"]<-       ""
per_col_5["Sanitation"]<-  ""
per_col_5["Handwashing"]<- ""
per_col_5["WSH"]<-         "Ref"
per_col_5["Nutrition"]<-   ""
for(arm in group[7]){
  reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("WSH",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)   
  per_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#column 6
per_col_6<-NULL
per_col_6["Control"]<-     ""
per_col_6["Water"]<-       ""
per_col_6["Sanitation"]<-  ""
per_col_6["Handwashing"]<- ""
per_col_6["WSH"]<-         ""
per_col_6["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("Nutrition",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  per_col_6[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#------------------------------------
# analysis for easq-combined z-score
#------------------------------------
#column 1
combined_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
combined_col_2<-NULL
for(arm in group){
  combined_col_2[arm]<-dformat(sum(!is.na(easq[easq$arm==arm,]$combined)),0)
  
}
#column 3
combined_col_3<-NULL
for(arm in group){
  ind<-easq$arm==arm
  mean<-  dformat(mean(easq[ind,]$combined,na.rm =TRUE),2)
  SD<-    dformat(sd(easq[ind,]$combined,na.rm =TRUE),2)
  combined_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
combined_col_4<-NULL
combined_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  combined_col_4[arm]<-paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#column 5
combined_col_5<-NULL
combined_col_5["Control"]<-     ""
combined_col_5["Water"]<-       ""
combined_col_5["Sanitation"]<-  ""
combined_col_5["Handwashing"]<- ""
combined_col_5["WSH"]<-         "Ref"
combined_col_5["Nutrition"]<-   ""
for(arm in group[7]){
  reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("WSH",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)   
  combined_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#column 6
combined_col_6<-NULL
combined_col_6["Control"]<-     ""
combined_col_6["Water"]<-       ""
combined_col_6["Sanitation"]<-  ""
combined_col_6["Handwashing"]<- ""
combined_col_6["WSH"]<-         ""
combined_col_6["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,contrast=c("Nutrition",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  combined_col_6[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")  
}

#----------------------------------------------------
# combining all the results into a single data frame
#----------------------------------------------------

com_df<-data.frame(a=com_col_1,b=com_col_2,c=com_col_3,d=com_col_4,e=com_col_5,f=com_col_6)
mot_df<- data.frame(a=mot_col_1,b=mot_col_2,c=mot_col_3,d=mot_col_4,e=mot_col_5,f=mot_col_6)
per_df<-data.frame(a=per_col_1,b=per_col_2,c=per_col_3,d=per_col_4,e=per_col_5,f=per_col_6)
combined_df<-data.frame(a=combined_col_1,b=combined_col_2,c=combined_col_3,d=combined_col_4,e=combined_col_5,f=combined_col_6)

com_glue<-data.frame(a=c("\u005ctextbf{Communication z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""))
mot_glue<-data.frame(a=c("\u005ctextbf{Gross Motor  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""))
per_glue<-data.frame(a=c("\u005ctextbf{Personal-social  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""))
combined_glue<-data.frame(a=c("\u005ctextbf{Combined z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""))

easq<-rbind(com_glue,com_df,mot_glue,mot_df,per_glue,per_df,combined_glue,combined_df)

save(easq,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\easq.Rdata")


  