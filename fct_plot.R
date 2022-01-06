####################
# Plots of time series ___________________________________________________________________________________________________________________________
####################

plot_serie_temp <- function(df,title="",xlegend="",ylegend="",line=TRUE,date_low="1944-02-14",date_high="2023-02-14",trend=FALSE,n.breaks=10){
  p <- ggplot(df,aes(x=df[,1],y=df[,2]))
  if (line) {p <- p+geom_line()}
  if (trend) {p <- p+geom_smooth(method=lm,se=FALSE)}
  return(
    p + geom_point(shape = 19, aes(color="")) +
      scale_color_manual(values = c("black")) +
      labs(titles = title,x = xlegend,y = ylegend) +
      # theme_classic()+
      theme_linedraw()+
      theme(legend.position = "None",legend.title = element_blank())+
      scale_x_date(date_breaks = paste(n.breaks," year",sep=""),date_minor_breaks = "1 year",date_labels = "%Y",limits=c(as.Date(date_low),as.Date(date_high)))+
      scale_y_continuous(breaks=seq(floor(min(df[,2])),ceiling(max(df[,2])),by=1),minor_breaks = seq(floor(min(df[,2])),ceiling(max(df[,2])),by=0.5),
                         limits=c(floor(min(df[,2])),ceiling(max(df[,2]))))
  )
}

# plot simultaneously of two time series from two dataframes
plot_2y <- function(df1,df2,factor,title="",xlegend="",ylegend1="",ylegend2=""){
  p <- ggplot()
  return(
    p + geom_line(data=df1, aes(x=df1[,1], y=df1[,2]), color="red") +
      geom_line(data=df2, aes(x=df2[,1], y=df2[,2]/factor), color="blue")+
      # geom_point()+
      scale_y_continuous(limits=c(0, 8), 
                         sec.axis = sec_axis(~ . *factor, name = ylegend2))+
      labs(titles = title,x=xlegend, y=ylegend1)+
      # geom_point(shape = 19, aes(color="")) +
      scale_color_manual(values = c("red")) +
      # theme_classic()+
      theme(axis.title.y.left=element_text(colour="red"),
            axis.text.y.left=element_text(colour="red"),
            axis.title.y.right=element_text(colour="blue"),
            axis.text.y.right=element_text(colour="blue"),
            legend.position = "None",legend.title = element_blank())
  )
}

####################
# Plot SWG
####################
plot_SWG <- function(path,fname,yymin,yymax,mo.start,day.start,title=""){
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
         legend=paste("France - ","t_start=",mo.start,"/",day.start,sep=""),
         bty="n")
  legend("bottomright",legend=title)
}
####################
# Stats plots __________________________________________________________________________________________________________________________________
####################

# Histogramme of df[,2]
plot_histo <- function(df,title="",xlegend="",ylegend="",y.n.breaks=NULL,y.expand=c(0.05,0),y.limits=NULL){
  p <- ggplot(df,aes(x=df[,2]))
  return(
    p + geom_histogram(color="black", fill="snow3") +
      labs(titles = title,x = xlegend,y = ylegend) +
      theme_linedraw()+
      theme(legend.position = "None",legend.title = element_blank())+
      scale_x_continuous(breaks=seq(floor(min(df[,2])),ceiling(max(df[,2],by=1))))+
      scale_y_continuous(n.breaks=y.n.breaks,minor_breaks = NULL,expand=y.expand,limits=y.limits)
  )
}

#Box plot of df with a red point for dfval value
plot_box <- function(df,dfval,title="",xlabel="",ylabel=""){
  p <- ggplot(df, aes(x=factor(0),y)) + 
    geom_boxplot()+
    # expand_limits(y=0)+
    stat_summary(fun=mean, geom="point",shape=18, size=3)+
    labs(title = title, x = xlabel, y = ylabel)+
    theme_classic()+
    scale_y_continuous(limits=c(0.5,7))+
    geom_point(data=dfval,color="red2")
  return(p)
}
# #Box plot obs et ana, un seul data frame en entrée
# plot_box <- function(df,title="",xlabel="",ylabel=""){
#   p <- ggplot(df, aes(x=factor(0),y)) + 
#     geom_boxplot()+
#     #stat_summary(fun=mean, colour="darkred", geom="point", 
#     # shape=18, size=3)+
#     labs(title = title,x = xlabel, y = ylabel)+
#     theme_classic()
#   return(p)
# }

####################
# List plots ? ____________________________________________________________________________________________________________________________________
####################
plot_list <- function(dfList,title="",xlabel="",ylabel=""){
  require("dplyr")
  p <- ggplot(bind_rows(dfList,.id="sim"),aes(x=date,y=temp,group=sim))+
    geom_line()+
    theme_classic()
  return(p)
}

plot_list_obs <- function(df,title="",xlabel="",ylabel=""){
  require("dplyr")
  p <- ggplot(df,aes(x=date,y=temp,color=source,group=sim))+
    geom_line()+
    scale_color_manual(values=c("black","red"))+
    theme_classic()
  return(p)
}


####################
# Plots of severall submeans with same time scale _________________________________________________________________________________________________
####################
# with bar plot
plot_submean <- function(df,title="",xlabel="",ylabel="",legend_title=""){
  p <- ggplot(df,aes(x=date,y=var,color=n_days,fill=n_days))+
    # geom_bar(stat="identity",width=1,alpha=period)+
    geom_col(width=0.6,position="identity")+
    # scale_x_date(limits=c(as.Date("1949-01-01",format="%Y-%m-%d"),as.Date("2021-02-28",format="%Y-%m-%d")),date_breaks = "5 years", date_minor_breaks = "1 year",date_labels = "%Y")+
    scale_x_continuous(limits=c(1950,2021),breaks = seq(1950,2021,5),minor_breaks = seq(1950, 2021, 1))+
    scale_fill_manual(values=c("blue","red","darkgreen","orange"))+scale_color_manual(values=c("blue","red","darkgreen","orange"))+     #palette="Set1"
    scale_alpha_manual(values=c(1,0.5))+
    theme_linedraw()+ labs(title = title, x = xlabel, y = ylabel,color=legend_title,fill=legend_title)
    # ylim(min(df$var),3.206)
  return(p)
}

# with bubble plot
plot_submean_bubble <- function(df,title="",xlabel="",ylabel="",legend_title=""){
  df$n_days <- factor(df$n_days, levels=c(3,10,30,90))
  cc <- scales::seq_gradient_pal("steelblue1","darkblue","Lab")(seq(0,1,length.out=4))
  ggplot(df, aes(x=date, y=var, size = n_days,color=n_days)) +
    geom_point(alpha=0.8)+
    scale_x_continuous(limits=c(1950,2021),breaks = seq(1950,2021,5),minor_breaks = seq(1950, 2021, 1))+
    scale_size_manual(values=c(1,3,5,7))+
    # scale_color_brewer(palette="Blues")+
    scale_color_manual(values=c("blue","red","green4","orange"))+
    # scale_color_manual(values=cc)+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
    theme_linedraw()+ labs(title = title, x = xlabel, y = ylabel,color=legend_title,size=legend_title)
}


#######################
# plot maps of nc file ___________________________________________________________________________________________________________________________
#######################

nc_to_array <- function(path,fname,var){
  require(ncdf4)
  ncname <- paste(path,fname,sep="")
  # open a netCDF file
  ncin <- nc_open(ncname)
  lon <- ncvar_get(ncin, "lon")
  lat <- ncvar_get(ncin, "lat", verbose = F)
  nc_array <- ncvar_get(ncin, var)
  nc_close(ncin)
  return(nc_array)
}

# plots the anomalies and the gopt lines on the same graph
plot_ano.c <- function(lon,lat,array,array_anomalie,min,max,title=""){
  image.cont.ano(lon,lat,array_anomalie,mar=c(1,1,1,1),titre=title,legend=FALSE,transpose = FALSE,zlev=seq(min,max,length=11))#+legend("bottomright",legend=title)
  image.cont.c(lon,lat,array,mar=c(1,1,1,1),transpose = FALSE,add=TRUE,titre=title) 
}


#############
# Multiple plot function _________________________________________________________________________________________________________________________
#############
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

