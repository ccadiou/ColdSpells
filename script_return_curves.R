library(ggplot2)
library(extRemes)

load("./data/era5_t2m_daily_fr.RData")
nd <- 3
df_t2m_daily_rm <- df_t2m_daily
df_t2m_daily_rm$t2m <- c(rep(NA,nd%/%2),running_mean(df_t2m_daily$t2m,nd),rep(NA,nd%/%2+nd%%2-1))
df_t2m_winter_rm <- df_t2m_daily_rm[as.numeric(strftime(df_t2m_daily_rm$date, "%m")) %in% c(1:2,12),]

# ggplot(df_t2m_winter_rm,aes(x=date,y=t2m))+geom_line()+ theme_linedraw()+
  # scale_x_date(date_breaks = "10 years",date_minor_breaks = "5 years",date_labels = "%Y")#,limits=c(as.Date(date_low),as.Date(date_high)))


# Calculate min for each winter
df_min <- na.omit(aggregate(df_t2m_winter_rm$t2m, by=list(format(df_t2m_winter_rm$date,"%Y")), FUN=min))
colnames(df_min) <- c("date","t2m")
# df_min$date <- as.Date(df_min$date)

# fit GEV model
df_min[,2] <- -df_min[,2]       # take oposite to considere minimum
fit <-fevd(df_min[,2],df_min,units="deg C")
plot(fit, type = "rl",main=paste(nd,"days min"))    #use plot function from package
df_min[,2] <- -df_min[,2]


# plot_60 <- plot(fit, type = "rl")
# plot_30 <- plot(fit, type = "rl")
# plot_10 <- plot(fit, type = "rl")
# plot_3 <- plot(fit, type = "rl")
dev.off()
par(mfcol=c(1,4))


## Manual plot #####
# calculate return levels
rp <- exp(seq(0.1,log(1000),length.out=100))
rls <- as.data.frame(cbind(rp,rl=-return.level(fit,return.period = rp,method="MLE")))

# Add empiricam return level from observation
rls_empiric <-  sort(df_min[,2])#,decreasing=TRUE)
ind <- c(1:length(rls_empiric))
rls_emp <- as.data.frame(cbind(length(ind)/ind,rls_empiric))
colnames(rls_emp) <- c("rp","rl")

p <- ggplot()+
  geom_line(data=rls,aes(x=rp,y=rl))+
  geom_point(data=rls_emp,aes(x=rp,y=rl))+scale_x_continuous(trans='log10')+
  theme_linedraw()
p

####### old code, on winter mean, no valid
df_t2m <- df_t2m[df_t2m$date!=as.Date("1963-02-14"),]

load("./data/era5_t2m_DJFmean_fr.RData")
df_t2m <- df_t2m[df_t2m$dat<as.Date("1980-02-14"),]

df_t2m[,2] <- -df_t2m[,2]
fit <-fevd(df_t2m[,2],df_t2m,units="deg C")
plot(fit)

df_t2m[,2] <- -df_t2m[,2]
# seq(0,log(500),length.out=100)
rp <- exp(seq(0.1,log(1000),length.out=100))
rls <- as.data.frame(cbind(rp,rl=-return.level(fit,return.period = rp,method="MLE")))

load("./data/era5_t2m_DJFmean_fr.RData")

rls_empiric <-  sort(df_t2m[,2])#,decreasing=TRUE)
ind <- c(1:length(rls_empiric))
rls_emp <- as.data.frame(cbind(length(ind)/ind,rls_empiric))
colnames(rls_emp) <- c("rp","rl")

p <- ggplot()+
  geom_line(data=rls,aes(x=rp,y=rl))+
  geom_point(data=rls_emp,aes(x=rp,y=rl))+scale_x_continuous(trans='log10')+
  theme_linedraw()
p

ggplot(df_t2m,aes(x=date,y=t2m))+geom_point()+geom_line()
fevd(df_t2m)
