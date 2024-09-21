library(rstudioapi)
library(zoo)
library(stringr)
library(stringi)
library(readtext)
library(writexl)

rm(list=ls()); gc(); graphics.off(); cat("\f")
main <- dirname(getActiveDocumentContext()$path)
source(file.path(main,'scripts','00 Q_Indicadores.R'))
#Realizar reglas de intercambio
reglas <- read.csv(file.path(main,'scripts','REGLAS_HRUFULL.csv'))
ordenes <- sort(unique(reglas$ORDEN))

#ejecutar prms!
{
ruta <- main
out_dir <- ruta
ruta <- str_replace_all(ruta,"/","\\\\")
system("cmd.exe",input= paste("cd",ruta,"&& nogui.bat"))
nelm <- as.numeric(readLines(file.path(out_dir,'output','POCURO.statvar'))[1])
datos_salida <- read.table(file.path(out_dir,'output','POCURO.statvar'),
                           skip=1+nelm,sep=" ")
variables <- stri_read_lines(file.path(main,'output','POCURO.statvar'))[1:nelm+1]
aca_sim <- which(str_detect(variables,"seg_outflow"))
aca_obs <- which(str_detect(variables,"runoff"))
gross_qres <- as.data.frame(matrix(NA,
                                   ncol=10,nrow=length(ordenes)))
colnames(gross_qres) <- c("Estacion","R2_daily","bR2_daily","KGE_daily",
                          "NSE_daily","R2_monthly","br2_monthly",
                          "KGE_monthly","NSE_monthly","PBias_%")

#crear carpeta de resultados
out_gral <- file.path(out_dir,'output','graphics and indicators')
dir.create(out_gral,showWarnings = F)

i=1
for (i in 1:length(ordenes)){
  where <- which(reglas$ORDEN==ordenes[i])
  obs <- mean(reglas$OBS[where])
  seg <- mean(reglas$SEG[where])
  faju <- mean(reglas$QF[where])
  estacion <- reglas$ESTAC[where][1]
  local_out <- file.path(out_gral,estacion)
  dir.create(local_out,showWarnings = F)
  gross_qres$Estacion[i] <- estacion
  donde_obs <- 7+aca_obs[obs]
  donde_sim <- 7+aca_sim[seg]
  resumen <- datos_salida[,c(2,3,4,donde_sim,donde_obs)]
  colnames(resumen) <- c("year","month","day","sim","obs")
  min_anho <- min(resumen$year)
  max_anho <- max(resumen$year)
  resumen <- resumen[resumen$year>min_anho,]
  resumen$sim <- round(faju*resumen$sim/35.315,digits=1)
  resumen$obs <- round((resumen$obs/35.315),digits=1)
  resumen[resumen<0] <- NA
  rec <- resumen
  rec <- rec[which(rec$year%%2!=0),] # calibration with odd-number years
  rec <- na.omit(rec)
  rev <- resumen
  rev <- rev[which(rev$year%%2==0),] # calibration with even-number years
  rev <- na.omit(rev)
  rec$date <- as.Date(paste0(rec$year,"-",rec$month,"-15"))
  rev$date <- as.Date(paste0(rev$year,"-",rev$month,"-15"))
  rc_m <- aggregate(rec[c('sim','obs')],list(rec$date),mean)
  rv_m <- aggregate(rev[c('sim','obs')],list(rev$date),mean)
  #daily indicators for calibration/validation
  r2.c.d <- f.r2(x.sim=rec$sim,x.obs=rec$obs)
  br2.c.d <- f.br2(x.sim=rec$sim,x.obs=rec$obs)
  kge.c.d <- f.kge(x.sim=rec$sim,x.obs=rec$obs)
  nse.c.d <- f.nse(x.sim=rec$sim,x.obs=rec$obs)
  
  r2.v.d <- f.r2(x.sim=rev$sim,x.obs=rev$obs)
  br2.v.d <- f.br2(x.sim=rev$sim,x.obs=rev$obs)
  kge.v.d <- f.kge(x.sim=rev$sim,x.obs=rev$obs)
  nse.v.d <- f.nse(x.sim=rev$sim,x.obs=rev$obs)
  
  #monthly indicators for calibration/validation
  r2.c.m <- f.r2(x.sim=rc_m$sim,x.obs=rc_m$obs)
  br2.c.m <- f.br2(x.sim=rc_m$sim,x.obs=rc_m$obs)
  kge.c.m <- f.kge(x.sim=rc_m$sim,x.obs=rc_m$obs)
  nse.c.m <- f.nse(x.sim=rc_m$sim,x.obs=rc_m$obs)
  
  r2.v.m <- f.r2(x.sim=rv_m$sim,x.obs=rv_m$obs)
  br2.v.m <- f.br2(x.sim=rv_m$sim,x.obs=rv_m$obs)
  kge.v.m <- f.kge(x.sim=rv_m$sim,x.obs=rv_m$obs)
  nse.v.m <- f.nse(x.sim=rv_m$sim,x.obs=rv_m$obs)
  
  #colors for analysis - keeps it simple
  sim.rgb <- rgb(.333,.678,.537)
  sim.alfa <- rgb(.333,.678,.537, alpha=0.45)
  obs.rgb <- rgb(.733,.463,.576)
  obs.alfa <- rgb(.733,.463,.576, alpha=0.45)
  cal.rgb <- rgb(0,.447,.698)
  cal.alfa <- rgb(0,.447,.698, alpha=0.45)
  val.rgb <- rgb(.835,.369,0)
  val.alfa <- rgb(.835,.369,0, alpha=0.45)
  
  # 1 - Daily Series
  resumen$date <- as.Date(paste(resumen$year,resumen$month,resumen$day,sep="-"))
  max_qd <- max(c(resumen$sim,resumen$obs),na.rm = T)
  
  png(file.path(out_gral,estacion,'001 - Daily Streamflow Series.png'),
      width=1600,height=900,pointsize = 18)
  plot(resumen$obs~resumen$date,ylim=c(0,1.25*max_qd),
       col=NA,xaxt='n',xlab=NA,ylab='[m3/s]',
       main=c('Daily Streamflow Series',estacion))
  rect(xleft = par()$usr[1], ybottom = par()$usr[3],
       xright = par()$usr[2], ytop = par()$usr[4],
       col=rgb(.95,.95,.95))
  grid(nx=NA,ny=NULL,lty=2,col="black")
  axis(1,as.Date(paste0(seq(min_anho+1,max_anho+1,1),"-01-01")),
       format(as.Date(paste0(seq(min_anho+1,max_anho+1,1),"-01-01")),"%Y"),las=2)
  lines(resumen$sim~resumen$date,lwd=5,col=sim.rgb)
  lines(resumen$obs~resumen$date,lwd=2,lty=2,col=obs.rgb)
  legend('top',legend=c('Sim','Obs'),lwd=c(5,2),lty=c(1,2),
         col=c(sim.rgb,obs.rgb))
  legend('topleft',title = 'Calibration (odd years)',pch=20,cex=0.85,
         legend=c(paste("R2:",r2.c.d),
                  paste("bR2:",br2.c.d),
                  paste("KGE:",kge.c.d),
                  paste("NSE:",nse.c.d)))
  legend('topright',title = 'Validation (even years)',pch=20,cex=0.85,
         legend=c(paste("R2:",r2.v.d),
                  paste("bR2:",br2.v.d),
                  paste("KGE:",kge.v.d),
                  paste("NSE:",nse.v.d)))
  box(lwd=3)
  dev.off()
  
  png(file.path(out_gral,estacion,'002 - Daily Streamflow Comparison.png'),
      width=900,height=900,pointsize = 18)
  plot(1,xlim=c(0,1.1*max_qd),ylim=c(0,1.1*max_qd),col=NA,
       main=c('Daily Streamflow Comparison',estacion),
       xlab='Simulated [m3/s]',ylab='Observed [m3/s]')
  rect(xleft = par()$usr[1], ybottom = par()$usr[3],
       xright = par()$usr[2], ytop = par()$usr[4],
       col=rgb(.95,.95,.95))
  grid(nx=NULL,ny=NULL,lty=2,col="gray")
  abline(a=0,b=1,col='black',lty=2,lwd=2)
  points(rec$obs~rec$sim,pch=4,col=cal.rgb,cex=2,lwd=2)
  points(rev$obs~rev$sim,pch=5,col=val.rgb,cex=2,lwd=2)
  legend('topright',pch=c(4,5),pt.lwd=2,col=c(cal.rgb,val.rgb),
         legend=c('Calibration Period','Validation Period'))
  box(lwd=2)
  dev.off()
  
  # 2 - Monthly Series
  
  res_monthly <- resumen
  res_monthly$tsm <- substr(as.Date(paste(
                                          res_monthly$year,
                                          res_monthly$month,
                                          res_monthly$day,sep="-")),1,7)
  res_monthly <- aggregate(res_monthly[,c(4,5)],
                           list(res_monthly$tsm),
                           mean)
  res_monthly$Group.1 <- as.Date(paste0(res_monthly$Group.1,"-15"))
  colnames(res_monthly)[1] <- 'ts'
  max_qm <- max(res_monthly[,-1],na.rm = T)
  
  png(file.path(out_gral,estacion,'003 - Monthly Streamflow Series.png'),
      width=1600,height=900,pointsize = 18)
  plot(res_monthly$sim~res_monthly$ts
       ,col=NA,xlab=NA,
       xaxt='n',ylim=c(0,1.2*max_qm),
       ylab="[m3/s]",main=c("Monthly Streamflow Series",estacion))
  axis(1,as.Date(paste0(seq(min_anho+1,max_anho+1,1),"-01-01")),
       format(as.Date(paste0(seq(min_anho+1,max_anho+1,1),"-01-01")),"%Y"),las=2)
  rect(xleft = par()$usr[1], ybottom = par()$usr[3],
       xright = par()$usr[2], ytop = par()$usr[4],
       col=rgb(.95,.95,.95))
  grid(nx=NA,ny=NULL,lty=2,col="black")
  lines(res_monthly$sim~res_monthly$ts,lwd=6,col=sim.alfa)
  lines(res_monthly$obs~res_monthly$ts,lwd=3,col=obs.rgb ,lty=2)
  legend('top',legend=c('Sim','Obs'),lwd=c(5,2),lty=c(1,2),
         col=c(sim.alfa,obs.rgb))
  legend('topleft',title = 'Calibration (odd years)',pch=20,cex=0.85,
         legend=c(paste("R2:",r2.c.m),
                  paste("bR2:",br2.c.m),
                  paste("KGE:",kge.c.m),
                  paste("NSE:",nse.c.m)))
  legend('topright',title = 'Validation (even years)',pch=20,cex=0.85,
         legend=c(paste("R2:",r2.v.m),
                  paste("bR2:",br2.v.m),
                  paste("KGE:",kge.v.m),
                  paste("NSE:",nse.v.m)))
  box(lwd=3)
  dev.off()
  
  png(file.path(out_gral,estacion,'004 - Monthly Streamflow Comparison.png'),
      width=900,height=900,pointsize = 18)
  plot(1,xlim=c(0,1.1*max_qm),ylim=c(0,1.1*max_qm),col=NA,
       main=c('Monthly Streamflow Comparison',estacion),
       xlab='Simulated [m3/s]',ylab='Observed [m3/s]')
  rect(xleft = par()$usr[1], ybottom = par()$usr[3],
       xright = par()$usr[2], ytop = par()$usr[4],
       col=rgb(.95,.95,.95))
  grid(nx=NULL,ny=NULL,lty=2,col="gray")
  abline(a=0,b=1,col='black',lty=2,lwd=2)
  points(rc_m$obs~rc_m$sim,pch=4,col=cal.rgb,cex=2,lwd=2)
  points(rv_m$obs~rv_m$sim,pch=5,col=val.rgb,cex=2,lwd=2)
  legend('topright',pch=c(4,5),pt.lwd=2,col=c(cal.rgb,val.rgb),
         legend=c('Calibration Period','Validation Period'))
  box(lwd=2)
  dev.off()
  
  # Monthly Variation Series
  
  cve_gral <- as.data.frame(matrix(0,ncol=7,nrow=12))
  colnames(cve_gral) <- c('month','sim15','sim50','sim85',
                          'obs15','obs50','obs85')
  cve_gral$month <- seq(1:12)
  cve_cal <- cve_val <- cve_gral
  res_monthly$month <- as.numeric(substr(res_monthly$ts,6,7))
  rc_m$month <- as.numeric(substr(rc_m$Group.1,6,7))
  rv_m$month <- as.numeric(substr(rv_m$Group.1,6,7))
  
  for (t in 1:12){
    sub_gral <- subset(res_monthly,res_monthly$month==t)
    sub_cal <- subset(rc_m,rc_m$month==t)
    sub_val <- subset(rv_m,rv_m$month==t)
    
    cve_gral$sim15[t] <- quantile(sub_gral$sim,0.15)
    cve_gral$sim50[t] <- quantile(sub_gral$sim,0.5)
    cve_gral$sim85[t] <- quantile(sub_gral$sim,0.85)
    cve_gral$obs15[t] <- quantile(sub_gral$obs,0.15,na.rm = T)
    cve_gral$obs50[t] <- quantile(sub_gral$obs,0.5,na.rm = T)
    cve_gral$obs85[t] <- quantile(sub_gral$obs,0.85,na.rm = T)
    
    cve_cal$sim15[t] <- quantile(sub_cal$sim,0.15)
    cve_cal$sim50[t] <- quantile(sub_cal$sim,0.5)
    cve_cal$sim85[t] <- quantile(sub_cal$sim,0.85)
    cve_cal$obs15[t] <- quantile(sub_cal$obs,0.15,na.rm = T)
    cve_cal$obs50[t] <- quantile(sub_cal$obs,0.5,na.rm = T)
    cve_cal$obs85[t] <- quantile(sub_cal$obs,0.85,na.rm = T)
    
    cve_val$sim15[t] <- quantile(sub_val$sim,0.15)
    cve_val$sim50[t] <- quantile(sub_val$sim,0.5)
    cve_val$sim85[t] <- quantile(sub_val$sim,0.85)
    cve_val$obs15[t] <- quantile(sub_val$obs,0.15,na.rm = T)
    cve_val$obs50[t] <- quantile(sub_val$obs,0.5,na.rm = T)
    cve_val$obs85[t] <- quantile(sub_val$obs,0.85,na.rm = T)
    
  }
  
  mas <- na.omit(res_monthly)
  mcl <- na.omit(rc_m)
  mvl <- na.omit(rv_m)
  gral_bias <- round(100*mean(mas$sim)/mean(mas$obs) - 100,digits=2)
  cal_bias <- round(100*mean(mcl$sim)/mean(mcl$obs) - 100,digits=2)
  val_bias <- round(100*mean(mvl$sim)/mean(mvl$obs) - 100,digits=2)
  
  png(file.path(out_gral,estacion,'005 - Monthly Variation Series.png'),
      width=1600,height=900,pointsize = 18)
  plot(1,col=NA,xlim=c(1,12),ylim=c(0,1.2*max(cve_gral[,-1])),
       xlab=NA, ylab="Average [m3/s]",xaxt='n',
       main=c("Monthly Variation Series",estacion))
  axis(1,at=seq(1,12,1),labels=seq(1,12,1))
  rect(xleft = par()$usr[1], ybottom = par()$usr[3],
       xright = par()$usr[2], ytop = par()$usr[4],
       col=rgb(.95,.95,.95))
  grid(nx=NA,ny=NULL,lty=2,col="black")
  polygon(x=c(seq(1,12,1),seq(12,1,-1)),
          y=c(cve_gral$sim85,rev(cve_gral$sim15)),
          col=sim.alfa ,border=NA)
  polygon(x=c(seq(1,12,1),seq(12,1,-1)),
          y=c(cve_gral$obs85,rev(cve_gral$obs15)),
          col=obs.alfa ,border=NA)
  lines(cve_gral$sim50~cve_gral$month,lwd=3,col=sim.rgb )
  lines(cve_gral$obs50~cve_gral$month,lwd=3,col=obs.rgb )
  legend("topleft",cex=1,border=NA,
         fill=c(sim.rgb,obs.rgb),legend=c("Sim","Obs"))
  legend("topright",pch=19,
         legend=paste0('Bias: ',gral_bias," %"))
  box(lwd=3)
  dev.off()
  
  png(file.path(out_gral,estacion,'006a - Monthly Variation Series - CALIBRATION.png'),
      width=1600,height=900,pointsize = 18)
  plot(1,col=NA,xlim=c(1,12),ylim=c(0,1.2*max(cve_gral[,-1])),
       xlab=NA, ylab="Average [m3/s]",xaxt='n',
       main=c("Monthly Variation Series (Calibration Period)",estacion))
  axis(1,at=seq(1,12,1),labels=seq(1,12,1))
  rect(xleft = par()$usr[1], ybottom = par()$usr[3],
       xright = par()$usr[2], ytop = par()$usr[4],
       col=rgb(.95,.95,.95))
  grid(nx=NA,ny=NULL,lty=2,col="black")
  polygon(x=c(seq(1,12,1),seq(12,1,-1)),
          y=c(cve_cal$sim85,rev(cve_cal$sim15)),
          col=sim.alfa ,border=NA)
  polygon(x=c(seq(1,12,1),seq(12,1,-1)),
          y=c(cve_cal$obs85,rev(cve_cal$obs15)),
          col=cal.alfa ,border=NA)
  lines(cve_cal$sim50~cve_cal$month,lwd=3,col=sim.rgb )
  lines(cve_cal$obs50~cve_cal$month,lwd=3,col=cal.rgb )
  legend("topleft",cex=1,border=NA,
         fill=c(sim.rgb,cal.rgb),legend=c("Sim","Obs (C)"))
  legend("topright",pch=19,
         legend=paste0('Bias: ',cal_bias," %"))
  box(lwd=3)
  dev.off()
  
  png(file.path(out_gral,estacion,'006b - Monthly Variation Series - VALIDATION.png'),
      width=1600,height=900,pointsize = 18)
  plot(1,col=NA,xlim=c(1,12),ylim=c(0,1.2*max(cve_gral[,-1])),
       xlab=NA, ylab="Average [m3/s]",xaxt='n',
       main=c("Monthly Variation Series (Validation Period)",estacion))
  axis(1,at=seq(1,12,1),labels=seq(1,12,1))
  rect(xleft = par()$usr[1], ybottom = par()$usr[3],
       xright = par()$usr[2], ytop = par()$usr[4],
       col=rgb(.95,.95,.95))
  grid(nx=NA,ny=NULL,lty=2,col="black")
  polygon(x=c(seq(1,12,1),seq(12,1,-1)),
          y=c(cve_val$sim85,rev(cve_val$sim15)),
          col=sim.alfa ,border=NA)
  polygon(x=c(seq(1,12,1),seq(12,1,-1)),
          y=c(cve_val$obs85,rev(cve_val$obs15)),
          col=val.alfa ,border=NA)
  lines(cve_val$sim50~cve_cal$month,lwd=3,col=sim.rgb )
  lines(cve_val$obs50~cve_cal$month,lwd=3,col=val.rgb )
  legend("topleft",cex=1,border=NA,
         fill=c(sim.rgb,val.rgb),legend=c("Sim","Obs (V)"))
  legend("topright",pch=19,
         legend=paste0('Bias: ',val_bias," %"))
  box(lwd=3)
  dev.off()
  

}

}

