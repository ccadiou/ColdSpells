plot_serie_temp <- function(df,title="",xlegend="",ylegend=""){
  p <- ggplot(df,aes(x=df[,1],y=df[,2]))
  return(
    p + geom_line()+
    geom_point(shape = 19, aes(color="")) +
      scale_color_manual(values = c("red")) +
      labs(
        titles = title,
        x = xlegend,y = ylegend
      ) +
      theme_classic()+
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
      theme_classic()+
      theme(axis.title.y.left=element_text(colour="red"),
            axis.text.y.left=element_text(colour="red"),
            axis.title.y.right=element_text(colour="blue"),
            axis.text.y.right=element_text(colour="blue"),
            legend.position = "None",legend.title = element_blank())
  )
}


