library(rstudioapi)
library(zoo)
library(stringr)
library(stringi)
library(readtext)

rm(list=ls()); gc(); graphics.off(); cat("\f")

#script para calibrar parametros
main <- dirname(getActiveDocumentContext()$path)
source(file.path(main,'scripts','00 Q_Indicadores.R'))
ruta <- file.path(main)
ruta <- str_replace_all(ruta,"/","\\\\")
#datos sobre hru y parametro a calibrar
hru <- c(3,4,5)
parameter <- "dprst_depth_avg"
lower_value <- 1
upper_value <- 500
default_value <- 132
#datos de paso y precision
intervals <- 50
sd.precision <- 0.01
value_dx <- (upper_value - lower_value)/(intervals-1)
value_rank <- seq(lower_value,upper_value,value_dx)
#elementos comparativos
obs.par <- 1
seg.par <- 2
qfa.par <- 1
#archivo_params
par.file <- readLines(file.path(main,'input',
                                "POCURO_initial.params"))
nelm <- 1+ as.numeric(readLines(file.path(main,'output','POCURO.statvar'))[1])

indicadores <- data.frame(value=round(value_rank,digits=3),
                          eff.d=NA,
                          eff.m=NA)
line.param <- which(par.file==parameter)
val.line <- line.param+4+hru

for(i in 1:nrow(indicadores)){
  x.val <- indicadores$value[i]
  cat("\f"); print(paste0(parameter,": ",x.val),quote=F); Sys.sleep((1))
  par.file[val.line] <- x.val
  stri_write_lines(as.vector(par.file),
                   file.path(main,'input',"POCURO_initial.params"),encoding = "UTF-8")
  system("cmd.exe",input= paste("cd",ruta,"&& nogui.bat")); cat("\f")
  variables <- stri_read_lines(file.path(main,'output','POCURO.statvar'))[2:nelm]
  aca_sim <- which(str_detect(variables,"seg_outflow"))
  aca_obs <- which(str_detect(variables,"runoff"))
  datos_salida <- read.table(file.path(main,'output','POCURO.statvar'),
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
  grm <- resumen
  grm$date <- as.Date(paste(grm$year,
                            grm$month,
                            grm$day, sep="-"))
  grm$date <- substr(grm$date,1,7)
  grm$date <- as.Date(paste0(grm$date,"-15"))
  grm <- aggregate(grm[,c(4,5)],list(grm$date),mean,na.action=na.omit)
  resumen <- na.omit(resumen)
  resumen$qm <- substr(as.Date(paste(resumen$year,
                                     resumen$month,
                                     resumen$day,sep="-")),1,7)
  qd <- f.kge(x.sim=resumen$sim,x.obs=resumen$obs)
  
  am <- aggregate(resumen[,c(4,5)],list(resumen$qm),mean)
  qm <- f.kge(x.sim=am$sim,x.obs=am$obs)
  
  indicadores$eff.d[i] <- qd
  indicadores$eff.m[i] <- qm
};{
  dfm <- max(indicadores$eff.d)
  mfm <- max(indicadores$eff.m)
  plot(c(1),col=NA,xlim=c(lower_value, upper_value),
       ylim=c(0,1),
       xlab=colnames(indicadores)[1],ylab="Indicator",
       main=paste("Indicator Variation:",parameter,"| HRU No.",hru) )
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
  lines(indicadores$eff.d~indicadores$value,
        lwd=3,col=rgb(.388,.533,.706))
  lines(indicadores$eff.m~indicadores$value,
        lwd=3,col=rgb(1,.682,.204) )
  box(lwd=2)
}
#parte manual
par.file[val.line] <- 1
stri_write_lines(as.vector(par.file),
                 file.path(main,'input',"POCURO_initial.params"),encoding = "UTF-8")
system("cmd.exe",input= paste("cd",ruta,"&& nogui.bat")); cat("\f")

