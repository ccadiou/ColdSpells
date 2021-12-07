library(ggplot2)
library(dplyr)
library(plyr)
library(gridExtra)

source("fct_plot.R")

########## CHARGEMENT DES DONNÉES ##########
#importance sampling
path <- "~/Documents/These/Data/Winter/WEGE/"
fnames <- list.files(path)

name <- "TX-m12d1_UNCLE-min-animsa_cal3_TX0.5meth1-897290-1951-2021.Rdat"
ymin <- 1951
ymax <- 2020
mstart <- 12
dstart <- 01


plot_SWG <- function(path,fname,yymin,yymax,mo.start,day.start){
        load(paste(path,fname,sep=""))
        rangeplot=round(range(c(unlist(simu.sta$l.T.mean),
                               unlist(simu.dyn$l.X.mean)),na.rm=TRUE))
        ylab=expression(paste("t2m (",degree,"C)"))
        plot(c(yymin,yymax),rangeplot,type="n",xlab="Years",
             ylab=ylab)
        boxplot(simu.sta$l.X.mean,at=c(yymin:yymax)-0.2,
                add=TRUE,axes=FALSE,col="blue")
        boxplot(simu.dyn$l.X.mean,at=c(yymin:yymax)+0.2,
                add=TRUE,axes=FALSE,col="red")
        lines(c(yymin:yymax),unlist(simu.sta$l.T.mean))
        legend("topleft",lwd=c(1,5,5),col=c("black","blue","red"),
               legend=c("Obs.","Static","Dynamic"),bty="n")
        legend("bottomleft",
               legend=paste("France","t start = ",mo.start,"/",day.start),
               bty="n")
}

plot_SWG(path,fnames[[4]],ymin,ymax,mstart,dstart)

load(paste(path,name,sep=""))
rangeplot=round(range(c(unlist(simu.sta$l.T.mean),
                        unlist(simu.dyn$l.X.mean)),na.rm=TRUE))
ylab=expression(paste("t2m (",degree,"C)"))
plot(c(ymin,ymax),rangeplot,type="n",xlab="Years",
     ylab=ylab)
boxplot(simu.sta$l.X.mean,at=c(ymin:ymax)-0.2,
        add=TRUE,axes=FALSE,col="blue")
boxplot(simu.dyn$l.X.mean,at=c(yymin:yymax)+0.2,
        add=TRUE,axes=FALSE,col="red")
lines(c(ymin:ymax),unlist(simu.sta$l.T.mean))
legend("topleft",lwd=c(1,5,5),col=c("black","blue","red"),
       legend=c("Obs.","Static","Dynamic"),bty="n")
legend("bottomleft",
       legend=paste("France","t start = ",mstart,"/",dstart),
       bty="n")

# pdf(filout)
# par(mar=c(4,5,1,1))
##    expression(paste("Temperature [",degree,"C]")),
dev.off()

rangeplot=round(range(c(unlist(simu.sta$l.T.mean),
                        unlist(simu.dyn$l.X.mean)),na.rm=TRUE))
ylab=expression(paste("t2m (",degree,"C)"))
plot(c(yymin,yymax),rangeplot,type="n",xlab="Years",
     ylab=ylab)
boxplot(simu.sta$l.X.mean,at=c(yymin:yymax)-0.2,
        add=TRUE,axes=FALSE,col="blue")
boxplot(simu.dyn$l.X.mean,at=c(yymin:yymax)+0.2,
        add=TRUE,axes=FALSE,col="red")
lines(c(yymin:yymax),unlist(simu.sta$l.T.mean))
legend("topleft",lwd=c(1,5,5),col=c("black","blue","red"),
       legend=c("Obs.","Static","Dynamic"),bty="n")
legend("bottomleft",
       legend=paste("France","t start = ",mo.start,"/",day.start),
       bty="n")
# legend("topleft","c",bty="n")




#### 2010 - alpha.cal=3 ####
load("../../Data/Winter/TG-France-Y2010m12d1_HW-animpsa_cal3_TG0meth1-860981.Rdat")
simu.dyn.2010.3 <-  adply(simu.dyn$l.X$`2010`, 1)
simu.dyn.2010.3$t.sim <- as.Date(as.character(simu.dyn.2010.3$t.sim),format="%Y%m%d")

simu.dyn.2010.3.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`2010`,1))
colnames(simu.dyn.2010.3.Xmean) <- c("nsim","y")
simu.dyn.2010.3.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`2010`,1))
colnames(simu.dyn.2010.3.Tmean) <- c("nsim","y")


#### 2010 - alpha.cal=6 ####
load("../../Data/Winter/TG-France-Y2010m12d1_HW-animpsa_cal6_TG0meth1-860981.Rdat")
simu.dyn.2010.6 <-  adply(simu.dyn$l.X$`2010`, 1)
simu.dyn.2010.6$t.sim <- as.Date(as.character(simu.dyn.2010.6$t.sim),format="%Y%m%d")

simu.dyn.2010.6.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`2010`,1))
colnames(simu.dyn.2010.6.Xmean) <- c("nsim","y")
simu.dyn.2010.6.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`2010`,1))
colnames(simu.dyn.2010.6.Tmean) <- c("nsim","y")


#### 1962 - alpha.cal=3 ####
load("../../Data/Winter/TG-France-Y1962m12d1_HW-animpsa_cal3_TG0meth1-860981.Rdat")
simu.dyn.1962.3 <-  adply(simu.dyn$l.X$`1962`, 1)
simu.dyn.1962.3$t.sim <- as.Date(as.character(simu.dyn.1962.3$t.sim),format="%Y%m%d")

simu.dyn.1962.3.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`1962`,1))
colnames(simu.dyn.1962.3.Xmean) <- c("nsim","y")
simu.dyn.1962.3.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`1962`,1))
colnames(simu.dyn.1962.3.Tmean) <- c("nsim","y")


#### 1962 - alpha.cal=6 ####
load("../../Data/Winter/TG-France-Y1962m12d1_HW-animpsa_cal6_TG0meth1-860981.Rdat")
simu.dyn.1962.6 <-  adply(simu.dyn$l.X$`1962`, 1)
simu.dyn.1962.6$t.sim <- as.Date(as.character(simu.dyn.1962.6$t.sim),format="%Y%m%d")

simu.dyn.1962.6.Xmean <- as.data.frame(adply(simu.dyn$l.X.mean$`1962`,1))
colnames(simu.dyn.1962.6.Xmean) <- c("nsim","y")
simu.dyn.1962.6.Tmean <- as.data.frame(adply(simu.dyn$l.T.mean$`1962`,1))
colnames(simu.dyn.1962.6.Tmean) <- c("nsim","y")

#### plot ####
p1 <- plot_box(simu.dyn.2010.3.Xmean,dfval=simu.dyn.2010.3.Tmean,title="2010 - alpha.cal=3")
p2 <- plot_box(simu.dyn.2010.6.Xmean,dfval=simu.dyn.2010.6.Tmean,title="2010 - alpha.cal=6")
p3 <- plot_box(simu.dyn.1962.3.Xmean,dfval=simu.dyn.1962.3.Tmean,title="1962 - alpha.cal=3")
p4 <- plot_box(simu.dyn.1962.6.Xmean,dfval=simu.dyn.1962.6.Tmean,title="1962 - alpha.cal=6")
grid.arrange(p1, p2, p3, p4, ncol=2, nrow = 2)

