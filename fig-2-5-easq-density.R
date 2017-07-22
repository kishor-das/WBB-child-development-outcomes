#---------------------------
# fig-2-5-easq-density.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# kernel density plot of easq scores
# comparing control group with
# each intervention arms
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
# output files:
#            plot_communication.pdf
#            plot_motor.pdf
#            plot_personal.pdf
#            plot_combined.pdf
#---------------------------

#---------------------------
# preamble
#---------------------------
rm(list = ls())

library(foreign) # for read.dta() function
library(plyr)    # for rename() function 

# source the density plot function
source("C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\analysis script\\functions\\washb-density-function.R")

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

#----------------------------------------------------
# kernel density plot for easq-communication z-score
#----------------------------------------------------
pdf("C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\plot_communication.pdf",width=8.5, height=11)
par(oma=c(3,10,3,10),family="serif")
nf <- layout(matrix(1:8,nrow=4,byrow=T))
# Water v. control
plotDensity(score=easq$com,
            arm=easq$arm,
            block=easq$block,
            color=c("black","aquamarine4"),
            pnum="a",
            contrast=c("Control","Water"),
            ylab="Kernel Density",
            xlab="Communication z-score",
            rplot=T)
# sanitation v. control
plotDensity(score=easq$com,
            arm=easq$arm,
            block=easq$block,
            color=c("black","bisque3"),
            pnum="b",
            contrast=c("Control","Sanitation"),
            ylab="Kernel Density",
            xlab="Communication z-score",
            rplot=F)
# handwashing v. control
plotDensity(score=easq$com,
            arm=easq$arm,
            block=easq$block,
            color=c("black","cadetblue4"),
            pnum="c",
            contrast=c("Control","Handwashing"),
            ylab="Kernel Density",
            xlab="Communication z-score",
            rplot=T)
# WSH v. control
plotDensity(score=easq$com,
            arm=easq$arm,
            block=easq$block,
            color=c("black","red"),
            pnum="d",
            contrast=c("Control","WSH"),
            ylab="Kernel Density",
            xlab="Communication z-score",
            rplot=F)
#Nutrition v. control
plotDensity(score=easq$com,
            arm=easq$arm,
            block=easq$block,
            color=c("black","darkgoldenrod1"),
            pnum="e",
            contrast=c("Control","Nutrition"),
            ylab="Kernel Density",
            xlab="Communication z-score",
            rplot=T)
#WASH+N v. control
plotDensity(score=easq$com,
            arm=easq$arm,
            block=easq$block,
            color=c("black","darkslategrey"),
            pnum="f",
            contrast=c("Control","WSH+N"),
            ylab="Kernel Density",
            xlab="Communication z-score",
            rplot=F)
#WSH v. WASH+N
plotDensity(score=easq$com,
            arm=easq$arm,
            block=easq$block,
            color=c("yellowgreen","turquoise3"),
            pnum="g",
            contrast=c("WSH","WSH+N"),
            ylab="Kernel Density",
            xlab="Communication z-score",
            rplot=T)
#Nutrition v. WASH+N
plotDensity(score=easq$com,
            arm=easq$arm,
            block=easq$block,
            color=c("deepskyblue3","steelblue1"),
            pnum="h",
            contrast=c("Nutrition","WSH+N"),
            ylab="Kernel Density",
            xlab="Communication z-score",
            rplot=F)

mtext("Figure 2: Caption on the next page.",side=1,outer=T,cex=1.5)
dev.off()
#----------------------------------------------------
# kernel density plot for easq-gross motor z-score
#----------------------------------------------------
pdf("C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\plot_motor.pdf",width=8.5, height=11)
par(oma=c(3,10,3,10),family="serif")
nf <- layout(matrix(1:8,nrow=4,byrow=T))
# Water v. control
plotDensity(score=easq$motor,
            arm=easq$arm,
            block=easq$block,
            color=c("black","aquamarine4"),
            pnum="a",
            contrast=c("Control","Water"),
            ylab="Kernel Density",
            xlab="Gross motor z-score",
            rplot=T)
# sanitation v. control
plotDensity(score=easq$motor,
            arm=easq$arm,
            block=easq$block,
            color=c("black","bisque3"),
            pnum="b",
            contrast=c("Control","Sanitation"),
            ylab="Kernel Density",
            xlab="Gross motor z-score",
            rplot=F)
# handwashing v. control
plotDensity(score=easq$motor,
            arm=easq$arm,
            block=easq$block,
            color=c("black","cadetblue4"),
            pnum="c",
            contrast=c("Control","Handwashing"),
            ylab="Kernel Density",
            xlab="Gross motor z-score",
            rplot=T)
# WSH v. control
plotDensity(score=easq$motor,
            arm=easq$arm,
            block=easq$block,
            color=c("black","red"),
            pnum="d",
            contrast=c("Control","WSH"),
            ylab="Kernel Density",
            xlab="Gross motor z-score",
            rplot=F)
#Nutrition v. control
plotDensity(score=easq$motor,
            arm=easq$arm,
            block=easq$block,
            color=c("black","darkgoldenrod1"),
            pnum="e",
            contrast=c("Control","Nutrition"),
            ylab="Kernel Density",
            xlab="Gross motor z-score",
            rplot=T)
#WASH+N v. control
plotDensity(score=easq$motor,
            arm=easq$arm,
            block=easq$block,
            color=c("black","darkslategrey"),
            pnum="f",
            contrast=c("Control","WSH+N"),
            ylab="Kernel Density",
            xlab="Gross motor z-score",
            rplot=F)
#WSH v. WASH+N
plotDensity(score=easq$motor,
            arm=easq$arm,
            block=easq$block,
            color=c("yellowgreen","turquoise3"),
            pnum="g",
            contrast=c("WSH","WSH+N"),
            ylab="Kernel Density",
            xlab="Gross motor z-score",
            rplot=T)
#Nutrition v. WASH+N
plotDensity(score=easq$motor,
            arm=easq$arm,
            block=easq$block,
            color=c("deepskyblue3","steelblue1"),
            pnum="h",
            contrast=c("Nutrition","WSH+N"),
            ylab="Kernel Density",
            xlab="Gross motor z-score",
            rplot=F)

mtext("Figure 3: Caption on the next page.",side=1,outer=T,cex=1.5)
dev.off()
#----------------------------------------------------
# kernel density plot for easq-personal-social z-score
#----------------------------------------------------
pdf("C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\plot_personal.pdf",width=8.5, height=11)
par(oma=c(3,10,3,10),family="serif")
nf <- layout(matrix(1:8,nrow=4,byrow=T))
# Water v. control
plotDensity(score=easq$personal,
            arm=easq$arm,
            block=easq$block,
            color=c("black","aquamarine4"),
            pnum="a",
            contrast=c("Control","Water"),
            ylab="Kernel Density",
            xlab="Personal-social z-score",
            rplot=T)
# sanitation v. control
plotDensity(score=easq$personal,
            arm=easq$arm,
            block=easq$block,
            color=c("black","bisque3"),
            pnum="b",
            contrast=c("Control","Sanitation"),
            ylab="Kernel Density",
            xlab="Personal-social z-score",
            rplot=F)
# handwashing v. control
plotDensity(score=easq$personal,
            arm=easq$arm,
            block=easq$block,
            color=c("black","cadetblue4"),
            pnum="c",
            contrast=c("Control","Handwashing"),
            ylab="Kernel Density",
            xlab="Personal-social z-score",
            rplot=T)
# WSH v. control
plotDensity(score=easq$personal,
            arm=easq$arm,
            block=easq$block,
            color=c("black","red"),
            pnum="d",
            contrast=c("Control","WSH"),
            ylab="Kernel Density",
            xlab="Personal-social z-score",
            rplot=F)
#Nutrition v. control
plotDensity(score=easq$personal,
            arm=easq$arm,
            block=easq$block,
            color=c("black","darkgoldenrod1"),
            pnum="e",
            contrast=c("Control","Nutrition"),
            ylab="Kernel Density",
            xlab="Personal-social z-score",
            rplot=T)
#WASH+N v. control
plotDensity(score=easq$personal,
            arm=easq$arm,
            block=easq$block,
            color=c("black","darkslategrey"),
            pnum="f",
            contrast=c("Control","WSH+N"),
            ylab="Kernel Density",
            xlab="Personal-social z-score",
            rplot=F)
#WSH v. WASH+N
plotDensity(score=easq$personal,
            arm=easq$arm,
            block=easq$block,
            color=c("yellowgreen","turquoise3"),
            pnum="g",
            contrast=c("WSH","WSH+N"),
            ylab="Kernel Density",
            xlab="Personal-social z-score",
            rplot=T)
#Nutrition v. WASH+N
plotDensity(score=easq$personal,
            arm=easq$arm,
            block=easq$block,
            color=c("deepskyblue3","steelblue1"),
            pnum="h",
            contrast=c("Nutrition","WSH+N"),
            ylab="Kernel Density",
            xlab="Personal-social z-score",
            rplot=F)

mtext("Figure 4: Caption on the next page.",side=1,outer=T,cex=1.5)
dev.off()
#----------------------------------------------------
# kernel density plot for easq-combined z-score
#----------------------------------------------------
pdf("C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\plot_combined.pdf",width=8.5, height=11)
par(oma=c(3,10,3,10),family="serif")
nf <- layout(matrix(1:8,nrow=4,byrow=T))
# Water v. control
plotDensity(score=easq$combined,
            arm=easq$arm,
            block=easq$block,
            color=c("black","aquamarine4"),
            pnum="a",
            contrast=c("Control","Water"),
            ylab="Kernel Density",
            xlab="Combined z-score",
            rplot=T)
# sanitation v. control
plotDensity(score=easq$combined,
            arm=easq$arm,
            block=easq$block,
            color=c("black","bisque3"),
            pnum="b",
            contrast=c("Control","Sanitation"),
            ylab="Kernel Density",
            xlab="Combined z-score",
            rplot=F)
# handwashing v. control
plotDensity(score=easq$combined,
            arm=easq$arm,
            block=easq$block,
            color=c("black","cadetblue4"),
            pnum="c",
            contrast=c("Control","Handwashing"),
            ylab="Kernel Density",
            xlab="Combined z-score",
            rplot=T)
# WSH v. control
plotDensity(score=easq$combined,
            arm=easq$arm,
            block=easq$block,
            color=c("black","red"),
            pnum="d",
            contrast=c("Control","WSH"),
            ylab="Kernel Density",
            xlab="Combined z-score",
            rplot=F)
#Nutrition v. control
plotDensity(score=easq$combined,
            arm=easq$arm,
            block=easq$block,
            color=c("black","darkgoldenrod1"),
            pnum="e",
            contrast=c("Control","Nutrition"),
            ylab="Kernel Density",
            xlab="Combined z-score",
            rplot=T)
#WASH+N v. control
plotDensity(score=easq$combined,
            arm=easq$arm,
            block=easq$block,
            color=c("black","darkslategrey"),
            pnum="f",
            contrast=c("Control","WSH+N"),
            ylab="Kernel Density",
            xlab="Combined z-score",
            rplot=F)
#WSH v. WASH+N
plotDensity(score=easq$combined,
            arm=easq$arm,
            block=easq$block,
            color=c("yellowgreen","turquoise3"),
            pnum="g",
            contrast=c("WSH","WSH+N"),
            ylab="Kernel Density",
            xlab="Combined z-score",
            rplot=T)
#Nutrition v. WASH+N
plotDensity(score=easq$combined,
            arm=easq$arm,
            block=easq$block,
            color=c("deepskyblue3","steelblue1"),
            pnum="h",
            contrast=c("Nutrition","WSH+N"),
            ylab="Kernel Density",
            xlab="Combined z-score",
            rplot=F)

mtext("Figure 5: Caption on the next page.",side=1,outer=T,cex=1.5)
dev.off()


