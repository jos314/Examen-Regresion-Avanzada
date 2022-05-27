library(R2jags)
library(dplyr)
wdir<-"/Users/joss/Documents/R"
setwd(wdir)

prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

mortalidad<-read.csv("http://allman.rhon.itam.mx/~lnieto/index_archivos/Bioassay.csv",header=TRUE)

n <-nrow(mortalidad)
plot(mortalidad)
plot (mortalidad[,1] , mortalidad[,3], xlab = "LogDose", ylab = "No. Muertes", main = "Muertes por dosificacion (log)")
mortalidad <- mutate(mortalidad, Dose = exp(mortalidad$LogDose))
plot (mortalidad[,4] , mortalidad[,3], xlab = "Dose", ylab = "No. Muertes", main = "Muertes por dosificacion" )


#DATA
nef<-c(4)
xf<-c(4)

data<-list("n"=n,"ne"=mortalidad[,2],"y"=mortalidad[,3],"x"=mortalidad[,1],"nef"=nef,"xf"=xf)

inits<-function(){list(beta=rep(0,2),yf1=rep(1,n))}

par<-c("beta","p","yf1","yf2")


#Modelo
ej.sim<-jags(data,inits,par,model.file="EjExamen.txt",
               n.iter=50000,n.chains=2,n.burnin=5000,n.thin=1)

#Outs
out<-ej.sim$BUGSoutput$sims.list

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)


z<-out$beta
par(mfrow=c(1,1))
plot(z, xlab = "alpha", ylab = "beta")

# Resumen de los estimadores

out.sum<-ej.sim$BUGSoutput$summary

#Tabla resumen
out.sum.t<-out.sum[grep("beta",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$beta,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
dimnames(out.sum.t)[[1]][1]<-"alpha"
dimnames(out.sum.t)[[1]][2]<-"beta"
LD50 <- - out.sum.t[1,] / out.sum.t[2,]
out.sum.t <- rbind(out.sum.t, LD50)
dimnames(out.sum.t)[[1]][3]<-"LD50"

print(out.sum.t)

#DIC

out.dic<-ej.sim$BUGSoutput$DIC
print(out.dic)



### INCISO C ###

#ReadingDATA
mortalidad<-read.csv("http://allman.rhon.itam.mx/~lnieto/index_archivos/Bioassay.csv",header=TRUE)
n <-nrow(mortalidad)
mortalidad <- mutate(mortalidad, Dose = exp(mortalidad$LogDose))

#DATA
xf<-c(4)
nef<-c(4)

data<-list("n"=n,"ne"=mortalidad[,2],"y"=mortalidad[,3],"x"=mortalidad[,1],"nef"=nef,"xf"=xf)

inits<-function(){list(beta=rep(0,2),yf1=rep(1,n))}

par<-c("beta","p","yf1","yf2")

#Modelo
ej.sim<-jags(data,inits,par,model.file="EjExamen.txt",
             n.iter=50000,n.chains=2,n.burnin=5000,n.thin=1)

# Resumen de los estimadores

out.sum<-ej.sim$BUGSoutput$summary

#Tabla resumen
out.sum.t<-out.sum[grep("beta",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$beta,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
dimnames(out.sum.t)[[1]][1]<-"alpha"
dimnames(out.sum.t)[[1]][2]<-"beta"
LD50 <- - out.sum.t[1,] / out.sum.t[2,]
out.sum.t <- rbind(out.sum.t, LD50)
dimnames(out.sum.t)[[1]][3]<-"LD50"

print(out.sum.t)

#DIC

out.dic<-ej.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
or<-order(mortalidad$LogDose)
ymin<-min(mortalidad$No.Muertes,out.yf[,c(1,3,7)])
ymax<-max(mortalidad$No.Muertes,out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(mortalidad$LogDose,mortalidad$No.Muertes,ylim=c(ymin,ymax), xlab = "Log Dose", ylab = "No. Muertes")
lines(mortalidad$LogDose[or],out.yf[or,1],lwd=2,col=2)
lines(mortalidad$LogDose[or],out.yf[or,3],lty=2,col=2)
lines(mortalidad$LogDose[or],out.yf[or,7],lty=2,col=2)

## Inciso D ##

plot (mortalidad[,1] , mortalidad[,3], xlab = "LogDose", ylab = "No. Muertes", main = "Muertes por dosificacion (log)")
par(new = TRUE)
plot()

## Inciso F ##


#DATA
nef<-c(4)
xf<-c(4)

data<-list("n"=n,"ne"=mortalidad[,2],"y"=mortalidad[,3],"x"=mortalidad[,1],"nef"=nef,"xf"=xf)

inits<-function(){list(beta=rep(0,2),yf1=rep(1,n))}

par<-c("beta","p","yf1","yf2")


#Modelo
ej.sim<-jags(data,inits,par,model.file="EjExamenD1.txt",
             n.iter=50000,n.chains=2,n.burnin=5000,n.thin=1)

# Resumen de los estimadores

out.sum<-ej.sim$BUGSoutput$summary

#Tabla resumen
out.sum.t<-out.sum[grep("beta",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$beta,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
dimnames(out.sum.t)[[1]][1]<-"alpha"
dimnames(out.sum.t)[[1]][2]<-"beta"
LD50 <- - out.sum.t[1,] / out.sum.t[2,]
out.sum.t <- rbind(out.sum.t, LD50)
dimnames(out.sum.t)[[1]][3]<-"LD50"

print(out.sum.t)

#DIC

out.dic<-ej.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
or<-order(mortalidad$LogDose)
ymin<-min(mortalidad$No.Muertes,out.yf[,c(1,3,7)])
ymax<-max(mortalidad$No.Muertes,out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(mortalidad$LogDose,mortalidad$No.Muertes,ylim=c(ymin,ymax), xlab = "Log Dose", ylab = "No. Muertes")
lines(mortalidad$LogDose[or],out.yf[or,1],lwd=2,col=2)
lines(mortalidad$LogDose[or],out.yf[or,3],lty=2,col=2)
lines(mortalidad$LogDose[or],out.yf[or,7],lty=2,col=2)

#Modelo 2
ej.sim<-jags(data,inits,par,model.file="EjExamenD2.txt",
             n.iter=50000,n.chains=2,n.burnin=5000,n.thin=1)

# Resumen de los estimadores

out.sum<-ej.sim$BUGSoutput$summary

#Tabla resumen
out.sum.t<-out.sum[grep("beta",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$beta,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
dimnames(out.sum.t)[[1]][1]<-"alpha"
dimnames(out.sum.t)[[1]][2]<-"beta"
LD50 <- - out.sum.t[1,] / out.sum.t[2,]
out.sum.t <- rbind(out.sum.t, LD50)
dimnames(out.sum.t)[[1]][3]<-"LD50"

print(out.sum.t)

#DIC

out.dic<-ej.sim$BUGSoutput$DIC
print(out.dic)
#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
or<-order(mortalidad$LogDose)
ymin<-min(mortalidad$No.Muertes,out.yf[,c(1,3,7)])
ymax<-max(mortalidad$No.Muertes,out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(mortalidad$LogDose,mortalidad$No.Muertes,ylim=c(ymin,ymax), xlab = "Log Dose", ylab = "No. Muertes")
lines(mortalidad$LogDose[or],out.yf[or,1],lwd=2,col=2)
lines(mortalidad$LogDose[or],out.yf[or,3],lty=2,col=2)
lines(mortalidad$LogDose[or],out.yf[or,7],lty=2,col=2)
abline(v = out.sum.t[3,1], col = "blue")

