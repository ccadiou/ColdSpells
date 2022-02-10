library(ggplot2)
library(extRemes)
library(igraph)     # for the running mean
library(gridExtra) # for multiplot


load("./data/era5_t2m_daily_fr.RData")

#remove 29feb
df_t2m_daily <- df_t2m_daily[!(format(df_t2m_daily$date,"%m") == "02" & format(df_t2m_daily$date, "%d") == "29"), , drop = FALSE]
df_t2m_daily <- df_t2m_daily[df_t2m_daily$date > "1950-08-01", , drop = FALSE]
# linear trend over winters
df_t2m_winter <- df_t2m_daily[as.numeric(strftime(df_t2m_daily$date, "%m")) %in% c(1:2,12),]
df_t2m_winter$year <- as.numeric(format(df_t2m_winter$date,"%Y"))
df_t2m_winter$year[as.numeric(strftime(df_t2m_winter$date, "%m"))==12] <- df_t2m_winter$year[as.numeric(strftime(df_t2m_winter$date, "%m"))==12]+1
df_winter_mean <- aggregate(df_t2m_winter$t2m, by=list(df_t2m_winter$year), FUN=mean)
colnames(df_winter_mean) <- c("date","t2m")

reg <- lm(df_winter_mean$t2m~c(1:length(df_winter_mean$t2m)))
alpha <- reg[[1]][2]
beta <- reg[[1]][1]

df_t2m_detrended <- df_t2m_winter
df_t2m_detrended$t2m <- df_t2m_detrended$t2m-rep(alpha*c(1:length(df_winter_mean$date)),90)



# calcul des courbes de retour
nd <- 60      #durée du minimum considéré
df_t2m_daily_rm <- df_t2m_detrended
df_t2m_daily_rm$t2m <- c(rep(NA,nd%/%2),running_mean(df_t2m_daily_rm$t2m,nd),rep(NA,nd%/%2+nd%%2-1))
df_t2m_winter_rm <- df_t2m_daily_rm[as.numeric(strftime(df_t2m_daily_rm$date, "%m")) %in% c(1:2,12),]

# série temporelle hiver
# ggplot(df_t2m_daily_rm,aes(x=date,y=t2m))+geom_line()+ theme_linedraw()+
  # scale_x_date(date_breaks = "10 years",date_minor_breaks = "5 years",date_labels = "%Y")#,limits=c(as.Date(date_low),as.Date(date_high)))


# Calculate min for each winter
df_min <- na.omit(aggregate(df_t2m_winter_rm$t2m, by=list(format(df_t2m_winter_rm$date,"%Y")), FUN=min))
colnames(df_min) <- c("date","t2m")

# fit GEV model
yy <- 0
df_min_rc <- df_min[df_min$date!=yy,] #filter to keep all years except the one investigated
val_yy <- df_min[df_min$date==yy,"t2m"] #retrieve value of this specific year
df_min_rc[,2] <- -df_min_rc[,2]       # take oposite to consider minimum
fit <-fevd(df_min_rc[,2],df_min_rc,units="deg C")
plot(fit, type = "rl",main=paste(nd,"days min"))    #use plot function from package
df_min_rc[,2] <- -df_min_rc[,2]



## Manual plot #####
# calculate return levels
rp <- exp(seq(0.1,log(100),length.out=100))
rls <- as.data.frame(cbind(rp,rl=-return.level(fit,return.period = rp,method="MLE")))
rls <- as.data.frame(cbind(rp,-ci(fit,return.period = rp)))
colnames(rls) <- c("rp","lower_ci","rl","upper_ci")

# Add empiricam return level from observation
rls_empiric <-  sort(df_min[,2])#,decreasing=TRUE)
ind <- c(1:length(rls_empiric))
rls_emp <- as.data.frame(cbind(length(ind)/ind,rls_empiric))
colnames(rls_emp) <- c("rp","rl")

p <- ggplot()+
  geom_line(data=rls,aes(x=rp,y=lower_ci),linetype=2)+
  geom_line(data=rls,aes(x=rp,y=upper_ci),linetype=2)+
  geom_line(data=rls,aes(x=rp,y=rl),linetype=1)+
  # geom_hline(yintercept=val_yy)+
# geom_ribbon(data=rls,aes(x=rp,y=rl,ymin=lower_ci,ymax=upper_ci),linetype=2,fill=NA)+
  geom_point(data=rls_emp,aes(x=rp,y=rl),shape=1)+scale_x_continuous(trans='log10')+
  theme_linedraw()+labs(x="Return period (years)",y="Return level (°C)",title=paste(nd,"-days mean",sep=""))
p

# p_90 <- p
# p_60 <- p
# p_30 <- p
# p_10 <- p
# p_3 <- p

dev.off()
gridExtra::grid.arrange(p_60,p_30,p_10,p_3, ncol = 4 )



