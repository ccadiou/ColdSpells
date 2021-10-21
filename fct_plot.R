plot_serie_temp <- function(df,title="",xlegend="",ylegend=""){
  p <- ggplot(df,aes(x=df[,1],y=df[,2]))
  return(
    p + geom_line()+
    geom_point(shape = 19, aes(color="")) +
      scale_color_manual(values = c("black")) +
      labs(
        titles = title,
        x = xlegend,y = ylegend
      ) +
      # theme_classic()+
      theme(legend.position = "None",legend.title = element_blank())
  )
}

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

#Box plot obs et ana
plot_box <- function(df,dfval,title="",xlabel="",ylabel=""){
  p <- ggplot(df, aes(x=factor(0),y)) + 
    geom_boxplot()+
    expand_limits(y=0)+
    stat_summary(fun=mean, geom="point",shape=18, size=3)+
    labs(title = title, x = xlabel, y = ylabel)+
    theme_classic()+
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

plot_submean <- function(df,title="",xlabel="",ylabel="",legend_title=""){
  p <- ggplot(df,aes(x=index,y=temp,color=n_days,fill=n_days))+
    # geom_bar(stat="identity",width=1,alpha=period)+
    geom_col(width=1)+
    scale_x_date(limits=c(as.Date("1949-01-01",format="%Y-%m-%d"),as.Date("2021-02-28",format="%Y-%m-%d")),date_breaks = "5 years", date_minor_breaks = "1 year",date_labels = "%Y")+
    scale_fill_manual(values=c("blue","red","darkgreen","orange"))+scale_color_manual(values=c("blue","red","darkgreen","orange"))+     #palette="Set1"
    scale_alpha_manual(values=c(1,0.5))+
    theme_linedraw()+ labs(title = title, x = xlabel, y = ylabel,color=legend_title,fill=legend_title)
  return(p)
}

