library(rstudioapi)
library(zoo)
library(stringr)
library(stringi)
library(readtext)

rm(list=ls()); gc(); graphics.off(); cat("\f")

#script para calibrar parametros
main <- dirname(getActiveDocumentContext()$path)
prms_dir <- main
source(file.path(main,'scripts','00 Q_Indicadores.R'))
#Realizar reglas de intercambio
reglas <- read.csv(file.path(main,'scripts','REGLAS_HRU.csv'))
#en caso de querer todas las hru calibradas, cambiar por 'REGLAS_HRU_FULL.csv
ordenes <- sort(unique(reglas$ORDEN))

atv <- 2012
intervalos <- 25
sd.precision <- 1/100 #precisión para evitar que calibre demás
out_gral <- file.path(prms_dir,'output','graphics and indicators')
dir.create(out_gral,showWarnings = F)

#llamar archivo parametros
par.file <- readLines(file.path(prms_dir,'input',
                                "POCURO_initial.params"))
parlist <- read.csv(file.path(main,'scripts','PARAMETERS.csv'))
parlist$optimal <- c(NA)
parlist$invar <- "SI"

for (k in 1:1){
  dbs <- which(reglas$ORDEN<=length(ordenes))

for (unidad in 1:1){#Datos del SC
hru.level <- reglas$HRU[dbs]
obs.par <- mean(reglas$OBS[dbs])
seg.par <- mean(reglas$SEG[dbs])
est.par <- reglas$ESTAC[dbs][1]
qfa.par <- mean(reglas$QF[dbs],na.rm=T)
local_out <- file.path(out_gral,est.par)
dir.create(local_out,showWarnings = F)
parlist$invar <- "SI"
#print(c(paste("HRU:",hru.level),paste("Estacion:",est.par)),
#      quote=F)
Sys.sleep((1))

#prueba de concepto (para ver si todo esta ok)
ruta <- file.path(prms_dir)
ruta <- str_replace_all(ruta,"/","\\\\")
system("cmd.exe",input= paste("cd",ruta,"&& nogui.bat"))
#shell("cmd",file.path(main,"nogui.bat"))

#calibration process
for (iteracion in 1:3){
  for (z in which(parlist$invar=="SI")){
  #parametro, HRU a modificar y variables
  param <- parlist[z,1]
  min.val <- parlist$min[z]
  max.val <- parlist$max[z]
  delta.val <- (max.val - min.val)/(intervalos-1)
  rank <- seq(min.val,max.val,delta.val)
  
  line.param <- which(par.file==param)
  val.line <- line.param+4+hru.level
  
  indicadores <- as.data.frame(matrix(0,ncol=5, nrow=length(rank)))
  colnames(indicadores) <- c("Parameter Value","Daily Ef","Monthly Ef",
                             "Daily Validation","Monthly Validation")
  indicadores[,1] <- rank
  nelm <- 1+ as.numeric(readLines(file.path(prms_dir,'output','POCURO.statvar'))[1])
  
  for (i in 1:length(rank)){
    value <- rank[i]
    cat("\f"); print(paste0(param,": ",value),quote=F); Sys.sleep((1))
    par.file[val.line] <- value
    stri_write_lines(as.vector(par.file),
                     file.path(prms_dir,'input',"POCURO_initial.params"),
                     encoding = "UTF-8")
    system("cmd.exe",input= paste("cd",ruta,"&& nogui.bat")); cat("\f")
    #shell("cmd","nogui.bat",intern = F)
    variables <- stri_read_lines(file.path(prms_dir,'output','POCURO.statvar'))[2:nelm]
    aca_sim <- which(str_detect(variables,"seg_outflow"))
    aca_obs <- which(str_detect(variables,"runoff"))
    datos_salida <- read.table(file.path(prms_dir,'output','POCURO.statvar'),
                               skip=nelm,sep=" ")
    donde_obs <- 7+aca_obs[obs.par]
    donde_sim <- 7+aca_sim[seg.par]
    resumen <- datos_salida[,c(2,3,4,donde_sim,donde_obs)]
    colnames(resumen) <- c("year","month","day","sim","obs")
    min_anho <- min(resumen$year)
    resumen <- resumen[resumen$year>min_anho,]
    resumen$sim <- qfa.par*resumen$sim/35.315
    resumen$obs <- (resumen$obs/35.315)
    resumen[resumen<0] <- NA
    
    rec <- resumen
    rec <- rec[rec$year<=atv,]
    rec <- na.omit(rec)
    rev <- resumen
    rev <- rev[rev$year>atv,]
    rev <- na.omit(rev)
    rec$date <- as.Date(paste0(rec$year,"-",rec$month,"-15"))
    rev$date <- as.Date(paste0(rev$year,"-",rev$month,"-15"))
    rc_m <- aggregate(rec[c('sim','obs')],list(rec$date),mean)
    rv_m <- aggregate(rev[c('sim','obs')],list(rev$date),mean)
    #daily indicators for calibration/validation
    ef.c.d <- f.kge(x.sim=rec$sim,x.obs=rec$obs)
    ef.v.d <- f.kge(x.sim=rev$sim,x.obs=rev$obs)
    #monthly indicators for calibration/validation
    ef.c.m <- f.kge(x.sim=rc_m$sim,x.obs=rc_m$obs)
    ef.v.m <- f.kge(x.sim=rv_m$sim,x.obs=rv_m$obs)
    
    indicadores$`Daily Ef`[i] <- ef.c.d
    indicadores$`Monthly Ef`[i] <- ef.c.m
    indicadores$`Daily Validation`[i] <- ef.v.d
    indicadores$`Monthly Validation`[i] <- ef.v.m
  };{
    
    dfm <- max(indicadores$`Daily Ef`)
    mfm <- max(indicadores$`Monthly Ef`)
    VX <- indicadores[which(indicadores$`Daily Ef`==max(indicadores$`Daily Ef`))[1],1]
    ved <- indicadores$`Daily Validation`[which(indicadores$`Daily Ef`==max(indicadores$`Daily Ef`))[1]]
    vem <- indicadores$`Monthly Validation`[which(indicadores$`Daily Ef`==max(indicadores$`Daily Ef`))[1]]
    
    nsal <- paste0("RANGO_CALIB - ",param,".png")
    png(file.path(local_out,nsal),width=720,height=720,pointsize = 18)
    plot(c(1),col=NA,xlim=c(min.val, max.val),
         ylim=c(0,1),
         xlab=colnames(indicadores)[1],ylab="Indicator",
         main=paste("Indicator Variation:",param,"/ Step",iteracion) )
    rect(xleft = par()$usr[1], ybottom = par()$usr[3],
         xright = par()$usr[2], ytop = par()$usr[4],
         col=rgb(.95,.95,.95))
    grid(lty=2,col="black")
    rect(xleft = par()$usr[1], ybottom = dfm-sd.precision,
         xright = par()$usr[2], ytop = dfm+sd.precision,
         col=rgb(.388,.533,.706,alpha=0.4),border = NA)
    rect(xleft = par()$usr[1], ybottom = mfm-sd.precision,
         xright = par()$usr[2], ytop = mfm+sd.precision,
         col=rgb(1,.682,.204,alpha=0.4),border = NA)
    lines(indicadores$`Daily Ef`~indicadores$`Parameter Value`,
          lwd=3,col=rgb(.388,.533,.706))
    lines(indicadores$`Monthly Ef`~indicadores$`Parameter Value`,
          lwd=3,col=rgb(1,.682,.204) )
    legend("bottomleft",lwd=4,col=c(rgb(.388,.533,.706),rgb(1,.682,.204)),
           legend=c("Daily KGE (c)","Monthly KGE (c)"))
    points(vem~VX,pch=24,cex=3,bg=rgb(1,.682,.204))
    points(ved~VX,pch=24,cex=3,bg=rgb(.388,.533,.706))
    box(lwd=3)
    dev.off()
    
    dfm <- max(indicadores$`Daily Ef`)
    mfm <- max(indicadores$`Monthly Ef`)
    
    plot(c(1),col=NA,xlim=c(min.val, max.val),
         ylim=c(0,1),
         xlab=colnames(indicadores)[1],ylab="Indicator",
         main=paste("Indicator Variation:",param,"/ Step",iteracion) )
    rect(xleft = par()$usr[1], ybottom = par()$usr[3],
         xright = par()$usr[2], ytop = par()$usr[4],
         col=rgb(.95,.95,.95))
    grid(lty=2,col="black")
    rect(xleft = par()$usr[1], ybottom = dfm-sd.precision,
         xright = par()$usr[2], ytop = dfm+sd.precision,
         col=rgb(.388,.533,.706,alpha=0.4),border = NA)
    rect(xleft = par()$usr[1], ybottom = mfm-sd.precision,
         xright = par()$usr[2], ytop = mfm+sd.precision,
         col=rgb(1,.682,.204,alpha=0.4),border = NA)
    lines(indicadores$`Daily Ef`~indicadores$`Parameter Value`,
          lwd=3,col=rgb(.388,.533,.706))
    lines(indicadores$`Monthly Ef`~indicadores$`Parameter Value`,
          lwd=3,col=rgb(1,.682,.204) )
    points(vem~VX,pch=24,lwd=5,col=rgb(1,.682,.204))
    points(ved~VX,pch=24,lwd=5,col=rgb(.388,.533,.706))
    box(lwd=2)
  }
  
  VX <- indicadores[which(indicadores$`Daily Ef`==max(indicadores$`Daily Ef`))[1],1]
  if (sd(indicadores$`Daily Ef`,na.rm = T)<sd.precision){
     VX <- parlist$default[z] }
  if (sd(indicadores$`Daily Ef`,na.rm = T)<sd.precision){
     parlist$invar[z] <- "NO"}
  parlist$optimal[z] <- VX
  par.file[val.line] <- VX
  stri_write_lines(as.vector(par.file),
                   file.path(prms_dir,'input',"POCURO_initial.params"),
                   encoding = "UTF-8")
  system("cmd.exe",input= paste("cd",ruta,"&& nogui.bat")); cat("\f")
  #shell("cmd","nogui.bat",intern = F)
  variables <- stri_read_lines(file.path(prms_dir,'output','POCURO.statvar'))[2:nelm]
  aca_sim <- which(str_detect(variables,"seg_outflow"))
  aca_obs <- which(str_detect(variables,"runoff"))
  datos_salida <- read.table(file.path(prms_dir,'output','POCURO.statvar'),
                             skip=nelm,sep=" ")
  donde_obs <- 7+aca_obs[obs.par]
  donde_sim <- 7+aca_sim[seg.par]
  resumen <- datos_salida[,c(2,3,4,donde_sim,donde_obs)]
  colnames(resumen) <- c("year","month","day","sim","obs")
  min_anho <- min(resumen$year)
  max_anho <- max(resumen$year)
  resumen <- resumen[resumen$year>min_anho,]
  resumen$sim <- qfa.par*resumen$sim/35.341
  resumen$obs <- (resumen$obs/35.341)
  resumen[resumen<0] <- NA
  grm <- resumen
  grm$date <- as.Date(paste(grm$year,
                            grm$month,
                            grm$day, sep="-"))
  grm$date <- substr(grm$date,1,7)
  grm$date <- as.Date(paste0(grm$date,"-15"))
  grm <- aggregate(grm[,c(4,5)],list(grm$date),mean,na.action=na.omit)
  };{
    resumen$qm <- substr(as.Date(paste(resumen$year,
                                       resumen$month,
                                       resumen$day,sep="-")),1,7)
    resumen$ts <- as.Date(paste0(resumen$qm,"-",resumen$day))
    plot(grm$sim*(1/1)~grm[,1],col=NA,xlab=NA,xaxt='n',
         main=c("Caudales Mensuales",paste(est.par,"/ Step",iteracion)),
         ylab="[m3/s]",ylim=c(0,max(c(grm$sim,grm$obs)/1,na.rm=T)))
    rect(xleft = par()$usr[1], ybottom = par()$usr[3],
         xright = par()$usr[2], ytop = par()$usr[4],
         col=rgb(.95,.95,.95))
    grid(nx=NA,ny=NULL,lty=2,col="black")
    axis.Date(1,at=as.Date(paste0(seq(min_anho+1,max_anho+1),"-01-01")),
              format="%m/%y",las=2)
    lines(grm$sim*(1/1)~grm[,1],lwd=5,col=rgb(.333,.678,.537,alpha=0.6))
    lines(grm$obs*(1/1)~grm[,1],lwd=2,col=rgb(.733,.463,.576),lty=2)
    legend("topright",pch=19,
           legend=c(paste0("KGE Monthly (c): ",max(indicadores$`Monthly Ef`)),
                    paste0("KGE Daily (c): ",max(indicadores$`Daily Ef`))))
    legend("topleft",lwd=4,col = c(rgb(.333,.678,.537),rgb(.733,.463,.576)),
           legend=c("Sim","Obs"))
    box(lwd=3)
    
    nsal <- paste0("QM_",est.par,".png")
    png(file.path(local_out,nsal),width=1280,height=720,pointsize=16)
    plot(grm$sim*(1/1)~grm[,1],col=NA,xlab=NA,xaxt='n',
         main=c("Caudales Mensuales",paste(est.par,"/ Step",iteracion)),
         ylab="[m3/s]",ylim=c(0,max(c(grm$sim,grm$obs)/1,na.rm=T)))
    rect(xleft = par()$usr[1], ybottom = par()$usr[3],
         xright = par()$usr[2], ytop = par()$usr[4],
         col=rgb(.95,.95,.95))
    grid(nx=NA,ny=NULL,lty=2,col="black")
    axis.Date(1,at=as.Date(paste0(seq(min_anho+1,max_anho+1),"-01-01")),
              format="%m/%y",las=2)
    lines(grm$sim*(1/1)~grm[,1],lwd=5,col=rgb(.333,.678,.537,alpha=0.6))
    lines(grm$obs*(1/1)~grm[,1],lwd=2,col=rgb(.733,.463,.576),lty=2)
    legend("topright",pch=19,
           legend=c(paste0("KGE Monthly (c): ",max(indicadores$`Monthly Ef`)),
                    paste0("KGE Daily (c): ",max(indicadores$`Daily Ef`))))
    legend("topleft",lwd=4,col = c(rgb(.333,.678,.537),rgb(.733,.463,.576)),
           legend=c("Sim","Obs"))
    box(lwd=3)
    dev.off()
  }
}
}
}
#source(file.path(main,'PRMS + GRAFICAS.R'))
