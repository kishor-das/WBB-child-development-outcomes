#-----------------------------------------
# tab4-cdi.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH 
# interventions on Communication
# development inventory  (CDI) and
# executive functions (A not B and Tower)
#-----------------------------------------

#----------------------------
# input files :
#            washb-bangladesh-cdi-year2.dta
#            washb-bangladesh-efanotb-year2.dta
#            washb-bangladesh-eftower-year2.dta
#
# output files:
#            table-4.Rdata
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

cdi<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-cdi-year2.dta")
anotb<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-efanotb-year2.dta")  
tower<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-eftower-year2.dta") 

cdi<-subset(cdi,
            select=c("dataid","childid","tchild","clusterid",
                     "block","arm","svydate","sex","dob","agedays",
                     "ageyears","agegroup","fracode","res","care","resage",
                     "z_endline_CDI_understand","z_endline_CDI_say")
)
cdi<-rename(cdi,
            replace=c("z_endline_CDI_understand"="und","z_endline_CDI_say"="say" )
)
anotb<-subset(anotb,
              select=c("dataid","childid","tchild","clusterid",
                       "block","arm","svydate","sex","dob","agedays",
                       "ageyears","agegroup","fracode","res","care","resage",
                       "z_endline_A_not_B_score")
)
anotb<-rename(anotb,
              replace=c("z_endline_A_not_B_score"="anotb")
)
tower<-subset(tower,
              select=c("dataid","childid","tchild","clusterid",
                       "block","arm","svydate","sex","dob","agedays",
                       "ageyears","agegroup","fracode","res","care","resage",
                       "z_endline_tower_test")
) 
tower<-rename(tower,
              replace=c("z_endline_tower_test"="tower")
)

group<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N")
#-------------------------------
#CDI understand analysis
#-------------------------------
#column 1
und_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
und_col_2<-NULL
for(arm in group){
  und_col_2[arm]<-dformat(sum(!is.na(cdi[cdi$arm==arm,]$und)),0)
}

#column 3
und_col_3<-NULL
for(arm in group){
  ind<-cdi$arm==arm
  mean<-dformat(mean(cdi[ind,]$und,na.rm =TRUE),2)
  SD<-  dformat(sd(cdi[ind,]$und,na.rm =TRUE),2)
  und_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
und_col_4<-NULL
und_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=cdi$und,tr=cdi$arm,pair=cdi$block,id=cdi$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  und_col_4[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")
}
#column 5
und_col_5<-NULL
und_col_5["Control"]<-     ""
und_col_5["Water"]<-       ""
und_col_5["Sanitation"]<-  ""
und_col_5["Handwashing"]<- ""
und_col_5["WSH"]<-         "Ref"
und_col_5["Nutrition"]<-   ""
for(arm in group[7]){
  reg<- washb_glm(Y=cdi$und,tr=cdi$arm,pair=cdi$block,id=cdi$block,contrast=c("WSH",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)   
  und_col_5[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}
#column 6
und_col_6<-NULL
und_col_6["Control"]<-     ""
und_col_6["Water"]<-       ""
und_col_6["Sanitation"]<-  ""
und_col_6["Handwashing"]<- ""
und_col_6["WSH"]<-         ""
und_col_6["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- washb_glm(Y=cdi$und,tr=cdi$arm,pair=cdi$block,id=cdi$block,contrast=c("Nutrition",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)   
  und_col_6[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}
#-------------------------------
#CDI Say analysis
#-------------------------------
#column 1
say_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
say_col_2<-NULL
for(arm in group){
  say_col_2[arm]<-dformat(sum(!is.na(cdi[cdi$arm==arm,]$say)),0)
}
#column 3
say_col_3<-NULL
for(arm in group){
  ind<-cdi$arm==arm
  mean<-  dformat(mean(cdi[ind,]$say,na.rm =TRUE),2)
  SD<-  dformat(sd(cdi[ind,]$say,na.rm =TRUE),2)
  say_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
say_col_4<-NULL
say_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=cdi$say,tr=cdi$arm,pair=cdi$block,id=cdi$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  say_col_4[arm]<-paste(dif," (",lcb,", ",ucb,")",sep="") 
}
#column 5
say_col_5<-NULL
say_col_5["Control"]<-     ""
say_col_5["Water"]<-       ""
say_col_5["Sanitation"]<-  ""
say_col_5["Handwashing"]<- ""
say_col_5["WSH"]<-         "Ref"
say_col_5["Nutrition"]<-   ""
for(arm in group[7]){
  reg<- washb_glm(Y=cdi$say,tr=cdi$arm,pair=cdi$block,id=cdi$block,contrast=c("WSH",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)   
  say_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#column 6
say_col_6<-NULL
say_col_6["Control"]<-     ""
say_col_6["Water"]<-       ""
say_col_6["Sanitation"]<-  ""
say_col_6["Handwashing"]<- ""
say_col_6["WSH"]<-         ""
say_col_6["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- washb_glm(Y=cdi$say,tr=cdi$arm,pair=cdi$block,id=cdi$block,contrast=c("Nutrition",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  say_col_6[arm]<-paste(dif," (",lcb,", ",ucb,")",sep="")   
}
#-----------------------------------------------
# EF tower test analysis
#-----------------------------------------------
#column 1
tower_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
tower_col_2<-NULL
for(arm in group){
  tower_col_2[arm]<-dformat(sum(!is.na(tower[tower$arm==arm,]$tower)),0)
}
#column 3
tower_col_3<-NULL
for(arm in group){
  ind<-tower$arm==arm
  mean<-  dformat(mean(tower[ind,]$tower,na.rm =TRUE),2)
  SD<-  dformat(sd(tower[ind,]$tower,na.rm =TRUE),2)
  tower_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
tower_col_4<-NULL
tower_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=tower$tower,tr=tower$arm,pair=tower$block,id=tower$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  tower_col_4[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#column 5
tower_col_5<-NULL
tower_col_5["Control"]<-     ""
tower_col_5["Water"]<-       ""
tower_col_5["Sanitation"]<-  ""
tower_col_5["Handwashing"]<- ""
tower_col_5["WSH"]<-         "Ref"
tower_col_5["Nutrition"]<-   ""
for(arm in group[7]){
  reg<- washb_glm(Y=tower$tower,tr=tower$arm,pair=tower$block,id=tower$block,contrast=c("WSH",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  tower_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#column 6
tower_col_6<-NULL
tower_col_6["Control"]<-     ""
tower_col_6["Water"]<-       ""
tower_col_6["Sanitation"]<-  ""
tower_col_6["Handwashing"]<- ""
tower_col_6["WSH"]<-         ""
tower_col_6["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- washb_glm(Y=tower$tower,tr=tower$arm,pair=tower$block,id=tower$block,contrast=c("Nutrition",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  tower_col_6[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}

#-----------------------------------------------
#EF A not B test analysis
#-----------------------------------------------
#column 1
anotb_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
anotb_col_2<-NULL
for(arm in group){
  anotb_col_2[arm]<-dformat(sum(!is.na(anotb[anotb$arm==arm,]$anotb)),0)
}
#column 3
anotb_col_3<-NULL
for(arm in group){
  ind<-anotb$arm==arm
  mean<-  dformat(mean(anotb[ind,]$anotb,na.rm =TRUE),2)
  SD<-  dformat(sd(anotb[ind,]$anotb,na.rm =TRUE),2)
  anotb_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
anotb_col_4<-NULL
anotb_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=anotb$anotb,tr=anotb$arm,pair=anotb$block,id=anotb$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)   
  anotb_col_4[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="") 
}
#column 5
anotb_col_5<-NULL
anotb_col_5["Control"]<-     ""
anotb_col_5["Water"]<-       ""
anotb_col_5["Sanitation"]<-  ""
anotb_col_5["Handwashing"]<- ""
anotb_col_5["WSH"]<-         "Ref"
anotb_col_5["Nutrition"]<-   ""
for(arm in group[7]){
  reg<- washb_glm(Y=anotb$anotb,tr=anotb$arm,pair=anotb$block,id=anotb$block,contrast=c("WSH",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  anotb_col_5[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="") 
}
#column 6
anotb_col_6<-NULL
anotb_col_6["Control"]<-     ""
anotb_col_6["Water"]<-       ""
anotb_col_6["Sanitation"]<-  ""
anotb_col_6["Handwashing"]<- ""
anotb_col_6["WSH"]<-         ""
anotb_col_6["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- washb_glm(Y=anotb$anotb,tr=anotb$arm,pair=anotb$block,id=anotb$block,contrast=c("Nutrition",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)   
  anotb_col_6[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="") 
}
#---------------------------------------------------
# combining all the results into a single data frame
#---------------------------------------------------
und_df<- data.frame(a=und_col_1,b=und_col_2,c=und_col_3,d=und_col_4,e=und_col_5,f=und_col_6)
say_df<- data.frame(a=say_col_1,b=say_col_2,c=say_col_3,d=say_col_4,e=say_col_5,f=say_col_6)
tower_df<- data.frame(a=tower_col_1,b=tower_col_2,c=tower_col_3,d=tower_col_4,e=tower_col_5,f=tower_col_6)
anotb_df<- data.frame(a=anotb_col_1,b=anotb_col_2,c=anotb_col_3,d=anotb_col_4,e=anotb_col_5,f=anotb_col_6)

head_df<-data.frame(a=c("", "Outcome, Arm"),b=c("","N"),c=c("","Mean (SD)"),d=c("Mean Difference","vs. Control (95% CI)"),e=c("Mean Difference","vs. WSH (95% CI)"),f=c("Mean Difference","vs. Nutrition (95% CI)"))
und_glue<-data.frame(a=c("\u005ctextbf{CDI$\u005cdagger$- understand z-score} "),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""))
say_glue<-data.frame(a=c("\u005ctextbf{CDI$\u005cdagger$- say z-score} "),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""))
tower_glue<-data.frame(a=c("\u005ctextbf{EF$\u005cddagger$ Tower test z-score} "),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""))
anotb_glue<-data.frame(a=c("\u005ctextbf{EF$\u005cddagger$ A not B z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""))

table_4<- rbind(
  und_glue,
  und_df,
  say_glue,
  say_df,
  tower_glue,
  tower_df,
  anotb_glue,
  anotb_df
  )
save(table_4,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\table-4.Rdata")