# marine biology exam project
rm(list=ls())
library(deSolve)

library(ggplot2)
library(data.table)
#install.packages("patchwork")
library("patchwork")
######### baseline pristine !
param=c()
test=c(0,0)
graze=0.7 # grazing pressure
cm=0.06 # 1/yr, coral mortality
crep=0.25045 # 1/yr local coral lava space recruitment
cspawn=0.00503 # 1/yr coral lava from other places recruitment
clgrowth=0.12 # 1/yr lateral coral growth
tagrazem=10 # 1/yr, turf algae grazed max
ctgrowth=0.5 # coral growth turf over relative space
crecturf=0.1 # coral recruit onto turf, relative space
ftgrowth=11 # 1/yr fine turf growth
macgrazem=0.5 # 1/yr max macroalgae grazed
  # old macroalge graze 0.505*tagrazem
  # 0.01 *tagrazem
lmacgrowth=0.225 # 1/yr lateral growth rate macroalgae over sapce
cgrowthinhib=0.55 # coral growth inhibited by local macroalgae
lmacc=0.45 # lateral growth of macroalgae over coral
lmact=0.45 # lateral growth of macroalgae over turf.
parameters=c(graze,cm,crep,cspawn,clgrowth,tagrazem,ctgrowth,crecturf,ftgrowth,macgrazem,lmacgrowth,cgrowthinhib,lmacc,lmact)

stateinit=c(C=0.01,Tu=0.1,M=0.1)


coralgrowth=function(t,state,parameters) {
  C = state[[1]]
  Tu = state[[2]]
  M = state[[3]]
  S=1-C-Tu-M
  #print(S)
  dCdt=(cspawn+crep*C)*(S+crecturf*Tu)+clgrowth*(1-cgrowthinhib*M)*(S+ctgrowth*Tu)*C-cm*C-lmacc*lmacgrowth*M*C
  
  dTudt=ftgrowth*(1-graze)*S-tagrazem*graze*Tu-crecturf*(cspawn+crep*C)*Tu - ctgrowth*clgrowth*(1-cgrowthinhib*M)*Tu*C -lmact*lmacgrowth*M*Tu
  dMdt=lmacgrowth*M*(S+lmacc*C+lmact*Tu)-macgrazem*graze*M
 
  res=c(dCdt,dTudt,dMdt)
  return(list(res))
  
}

timey=400

pristinebaseline=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)

pristinebaselinefish=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)

plot(pristinebaseline[,2],main="coral growth",xlab="time",ylab="percentage",type="s",lwd=4,ylim=range(0,1))
lines(pristinebaseline[,3],col=2,lwd=4)
lines(pristinebaseline[,4],col=3,lwd=4)
legend(80, 0.7, legend=c("coral","turf algae","macroalgae"),
       col=c(1:3),lwd=4, cex=0.8)


##### fishing
cm=0.05 # 1/yr, coral mortality
crep=0.0009 # 1/yr local coral lava space recruitment
cspawn=0.00006 # 1/yr coral lava from other places recruitment
clgrowth=0.2 # 1/yr lateral coral growth
tagrazem=10 # 1/yr, turf algae grazed max
ctgrowth=0.25 # coral growth turf over relative space
crecturf=0.1 # coral recruit onto turf, relative space
ftgrowth=5 # 1/yr fine turf growth
macgrazem=0.5 # 1/yr max macroalgae grazed
lmacgrowth=0.35 # 1/yr lateral growth rate macroalgae over sapce
cgrowthinhib=0.9 # coral growth inhibited by local macroalgae
lmacc=0.1 # lateral growth of macroalgae over coral
lmact=0.9

#### plot test for converg
graze=0.4
stateinit=c(C=0.3,Tu=0.2,M=0.1)
pristinebaseline=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)
stateinit=c(C=0.1,Tu=0.4,M=0.3)
pristinebaselinefish=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)
plot(pristinebaseline[,2],main="coral growth",xlab="time",ylab="percentage",type="s",lwd=4,ylim=range(0,1))
lines(pristinebaseline[,3],col=2,lwd=4)
lines(pristinebaseline[,4],col=3,lwd=4)
points(pristinebaselinefish[,2],col=1)
points(pristinebaselinefish[,3],col=2)
points(pristinebaselinefish[,4],col=3)
legend(300, 0.7, legend=c("coral","turf algae","macroalgae"),
       col=c(1:3),lwd=4, cex=0.8)

data1 <- data.frame(c(0:400),pristinebaseline[,2])
data1$w <-"Coral"
data2 <- data.frame(c(0:400),pristinebaseline[,3])
data2$w <-"Turf algae"
data3 <- data.frame(c(0:400),pristinebaseline[,4])
data3$w <-"Macroalgae"
data4 <- data2                        # Replicate data
colnames(data4) <- colnames(data1)
data5 <- data3                        # Replicate data
colnames(data5) <- colnames(data1)
data <-rbind(data1,data4,data5)
colnames(data)<- c("Years", "Proportion", "Species")

data1 <- data.frame(c(0:400),pristinebaselinefish[,2])
data1$w <-"Coral"
data2 <- data.frame(c(0:400),pristinebaselinefish[,3])
data2$w <-"Turf algae"
data3 <- data.frame(c(0:400),pristinebaselinefish[,4])
data3$w <-"Macroalgae"
data4 <- data2                        # Replicate data
colnames(data4) <- colnames(data1)
data5 <- data3                        # Replicate data
colnames(data5) <- colnames(data1)
data2 <-rbind(data1,data4,data5)
colnames(data2)<- c("Years", "Proportion", "Species")

ggp1 <- ggplot(data, aes(x = Years, y = Proportion, colour = Species), size = 1) +
  geom_point()
ggp2 <- ggplot(data2, aes(x = Years, y = Proportion, colour = Species), size = 1) +
  geom_point()
ggp1 + ggp2 + plot_layout(guides = "collect")


#### end plot test

fishingf=seq(0.2,0.7,length.out=100)
stateinit=c(C=0.3,Tu=0.2,M=0.2)

resultsfforward=data.frame()
for (i in 1:length(fishingf)){
  graze=fishingf[i]
  parameters=c(graze,cm,crep,cspawn,clgrowth,tagrazem,ctgrowth,crecturf,ftgrowth,macgrazem,lmacgrowth,cgrowthinhib,lmacc,lmact)
  sol=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)
  resultsfforward=rbind(resultsfforward,sol[300,])
  stateinit=c(C=resultsfforward[i,2],Tu=resultsfforward[i,3],M=max(resultsfforward[i,4],0.1))
  
  
}
fishingb=rev(fishingf)
stateinit=c(C=0.3,Tu=0.2,M=0.2)

resultsfback=data.frame()
for (i in 1:length(fishingb)){
  graze=fishingb[i]
  parameters=c(graze,cm,crep,cspawn,clgrowth,tagrazem,ctgrowth,crecturf,ftgrowth,macgrazem,lmacgrowth,cgrowthinhib,lmacc,lmact)
  sol=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)
  resultsfback=rbind(resultsfback,sol[300,])
  stateinit=c(C=resultsfback[i,2],Tu=resultsfback[i,3],M=max(resultsfback[i,4],0.1))
  
  
}


data1 <- data.frame(fishingf,resultsfforward[,2])
data1$w <-"Coral"
data2 <- data.frame(fishingf,resultsfforward[,3])
data2$w <-"Turf algae"
data3 <- data.frame(fishingf,resultsfforward[,4])
data3$w <-"Macroalgae"

colnames(data1)<- c("Fishing", "Proportion", "Species")
colnames(data2)<- c("Fishing", "Proportion", "Species")
colnames(data3)<- c("Fishing", "Proportion", "Species")


data12 <- data.frame(fishingb,resultsfback[,2])
data12$w <-"Coral"
data22 <- data.frame(fishingb,resultsfback[,3])
data22$w <-"Turf algae"
data32 <- data.frame(fishingb,resultsfback[,4])
data32$w <-"Macroalgae"

colnames(data12)<- c("Fishing", "Proportion", "Species")
colnames(data22)<- c("Fishing", "Proportion", "Species")
colnames(data32)<- c("Fishing", "Proportion", "Species")

data <-rbind(data1,data2,data3)
data2 <-rbind(data12,data22,data32)


data$Event <- "Before"
data[data$Fishing>=0.45,]$Event <- "After"
data$Event <- factor(data$Event, levels=c("Before","After"))

data2$Event <- "Before"
data2[data2$Fishing>=0.45,]$Event <- "After"
data2$Event <- factor(data2$Event, levels=c("Before","After"))

xmin <- 0.31 # Beginning and end of discontinuity
xmax <- 0.45

ggplot() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.3, col = "grey90") +
  geom_line(data=data,aes(x = Fishing, y = Proportion, colour = Species, group = interaction(Species, Event)), size = 1) + 
  geom_line(data=data2, aes(x = Fishing, y = Proportion, colour = Species, group = interaction(Species, Event)), size = 1) 



#### use stable state from high grazing stage as initial conditions to see whether the system can regenerate
# with higher grazing again thing of course

# test with the given inits, also still need to change the remaining parms



######  nutrification

graze=0.6 # grazing pressure
cm=0.05 # 1/yr, coral mortality
crep=0.0009# 1/yr local coral lava space recruitment
cspawn=0.00006 # 1/yr coral lava from other places recruitment
clgrowth=0.2 # 1/yr lateral coral growth
tagrazem=9 # 1/yr, turf algae grazed max
ctgrowth=0.2 # coral growth turf over relative space
crecturf=0.05 # coral recruit onto turf, relative space
ftgrowth=11 # 1/yr fine turf growth
macgrazem=0.5 # 1/yr max macroalgae grazed
lmacgrowth=0.35 # 1/yr lateral growth rate macroalgae over sapce
cgrowthinhib=0.9 # coral growth inhibited by local macroalgae
lmacc=0.1 # lateral growth of macroalgae over coral
lmact=0.9 # lateral growth of macroalgae over turf.




nutrificationf=seq(1,2,length.out=100)
stateinit=c(C=0.3,Tu=0.2,M=0.2)
resultsnf=data.frame()
for (i in 1:length(nutrificationf)){
  ftgrowth=nutrificationf[i]*11
  lmacc=nutrificationf[i]*0.1
  lmact=nutrificationf[i]*0.9
  lmacgrowth=nutrificationf[i]*0.35
  parameters=c(graze,cm,crep,cspawn,clgrowth,tagrazem,ctgrowth,crecturf,ftgrowth,macgrazem,lmacgrowth,cgrowthinhib,lmacc,lmact)
  sol=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)
  resultsnf=rbind(resultsnf,sol[300,])
  stateinit=c(C=resultsnf[i,2],Tu=resultsnf[i,3],M=max(resultsnf[i,4],0.1))
}

nutrificationb=rev(nutrificationf)
resultsnb=data.frame()
for (i in 1:length(nutrificationb)){
  ftgrowth=nutrificationb[i]*11
  lmacc=nutrificationb[i]*0.1
  lmact=nutrificationb[i]*0.9
  lmacgrowth=nutrificationb[i]*0.35
  parameters=c(graze,cm,crep,cspawn,clgrowth,tagrazem,ctgrowth,crecturf,ftgrowth,macgrazem,lmacgrowth,cgrowthinhib,lmacc,lmact)
  sol=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)
  resultsnb=rbind(resultsnb,sol[300,])
  stateinit=c(C=resultsnb[i,2],Tu=resultsnb[i,3],M=max(resultsnb[i,4],0.1))
}



data1 <- data.frame(nutrificationf,resultsnf[,2])
data1$w <-"Coral"
data2 <- data.frame(nutrificationf,resultsnf[,3])
data2$w <-"Turf algae"
data3 <- data.frame(nutrificationf,resultsnf[,4])
data3$w <-"Macroalgae"

colnames(data1)<- c("Nutrification", "Proportion", "Species")
colnames(data2)<- c("Nutrification", "Proportion", "Species")
colnames(data3)<- c("Nutrification", "Proportion", "Species")


data12 <- data.frame(nutrificationb,resultsnb[,2])
data12$w <-"Coral"
data22 <- data.frame(nutrificationb,resultsnb[,3])
data22$w <-"Turf algae"
data32 <- data.frame(nutrificationb,resultsnb[,4])
data32$w <-"Macroalgae"

colnames(data12)<- c("Nutrification", "Proportion", "Species")
colnames(data22)<- c("Nutrification", "Proportion", "Species")
colnames(data32)<- c("Nutrification", "Proportion", "Species")

data <-rbind(data1,data2,data3)
data2 <-rbind(data12,data22,data32)


data$Event <- "Before"
data[data$Nutrification>=0.45,]$Event <- "After"
data$Event <- factor(data$Event, levels=c("Before","After"))

data2$Event <- "Before"
data2[data2$Nutrification>=0.45,]$Event <- "After"
data2$Event <- factor(data2$Event, levels=c("Before","After"))

xmin <- 1.14 # Beginning and end of discontinuity
xmax <- 1.42

ggplot() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.3, col = "grey90") +
  geom_line(data=data,aes(x = Nutrification, y = Proportion, colour = Species, group = interaction(Species, Event)), size = 1) + 
  geom_line(data=data2, aes(x = Nutrification, y = Proportion, colour = Species, group = interaction(Species, Event)), size = 1) 



##### sedimentation
graze=0.9 # grazing pressure
cm=0.06 # 1/yr, coral mortality
crep=0.01# 1/yr local coral lava space recruitment
cspawn=0.00006 # 1/yr coral lava from other places recruitment
clgrowth=0.2 # 1/yr lateral coral growth
tagrazem=5 # 1/yr, turf algae grazed max
ctgrowth=0.25 # coral growth turf over relative space
crecturf=0.05 # coral recruit onto turf, relative space
ftgrowth=10 # 1/yr fine turf growth
macgrazem=0.2 # 1/yr max macroalgae grazed
lmacgrowth=0.3 # 1/yr lateral growth rate macroalgae over sapce
cgrowthinhib=0.82 # coral growth inhibited by local macroalgae
lmacc=0.01 # lateral growth of macroalgae over coral
lmact=0.9 # lateral growth of macroalgae over turf.


stateinit=c(C=0.3,Tu=0.2,M=0.2)
sedimentmortf=seq(1,2,length.out=100)
sedimenrecf=seq(1,0.4,length.out=100)
sedimentgrowthf=seq(1,0.5,length.out=100)
resultssf=data.frame()
for (i in 1:100){
  cm=sedimentmortf[i]*0.06
  cspawn=sedimenrecf[i]*0.00006
  crep=sedimenrecf[i]*0.01
  crecturf=sedimenrecf[i]*0.05
  clgrowth=sedimentgrowthf[i]*0.2
  ctgrowth=sedimentgrowthf[i]*0.25
  parameters=c(graze,cm,crep,cspawn,clgrowth,tagrazem,ctgrowth,crecturf,ftgrowth,macgrazem,lmacgrowth,cgrowthinhib,lmacc,lmact)
  sol=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)
  resultssf=rbind(resultssf,sol[300,])
  stateinit=c(C=resultssf[i,2],Tu=resultssf[i,3],M=max(resultssf[i,4],0.1))
  
}
stateinit=c(C=0.3,Tu=0.2,M=0.2)
sedimentmortb=rev(sedimentmortf)
sedimenrecb=rev(sedimenrecf)
sedimentgrowthb=rev(sedimentgrowthf)
resultssb=data.frame()
for (i in 1:100){
  cm=sedimentmortb[i]*0.06
  cspawn=sedimenrecb[i]*0.00006
  crep=sedimenrecb[i]*0.01
  crecturf=sedimenrecb[i]*0.05
  clgrowth=sedimentgrowthb[i]*0.2
  ctgrowth=sedimentgrowthb[i]*0.25
  parameters=c(graze,cm,crep,cspawn,clgrowth,tagrazem,ctgrowth,crecturf,ftgrowth,macgrazem,lmacgrowth,cgrowthinhib,lmacc,lmact)
  sol=ode.1D(y=stateinit,func=coralgrowth, times=0:timey,parms=parameters,nspec=1)
  resultssb=rbind(resultssb,sol[300,])
  stateinit=c(C=resultssb[i,2],Tu=resultssb[i,3],M=max(resultssb[i,4],0.1))
  
}


data1 <- data.frame(sedimentmortf,resultssf[,2])
data1$w <-"Coral"
data2 <- data.frame(sedimentmortf,resultssf[,3])
data2$w <-"Turf algae"
data3 <- data.frame(sedimentmortf,resultssf[,4])
data3$w <-"Macroalgae"

colnames(data1)<- c("Sedimentation", "Proportion", "Species")
colnames(data2)<- c("Sedimentation", "Proportion", "Species")
colnames(data3)<- c("Sedimentation", "Proportion", "Species")


data12 <- data.frame(sedimentmortb,resultssb[,2])
data12$w <-"Coral"
data22 <- data.frame(sedimentmortb,resultssb[,3])
data22$w <-"Turf algae"
data32 <- data.frame(sedimentmortb,resultssb[,4])
data32$w <-"Macroalgae"

colnames(data12)<- c("Sedimentation", "Proportion", "Species")
colnames(data22)<- c("Sedimentation", "Proportion", "Species")
colnames(data32)<- c("Sedimentation", "Proportion", "Species")

data <-rbind(data1,data2,data3)
data2 <-rbind(data12,data22,data32)


data$Event <- "Before"
data[data$Sedimentation>=0.45,]$Event <- "After"
data$Event <- factor(data$Event, levels=c("Before","After"))

data2$Event <- "Before"
data2[data2$Sedimentation>=0.45,]$Event <- "After"
data2$Event <- factor(data2$Event, levels=c("Before","After"))

xmin <- 1.13 # Beginning and end of discontinuity
xmax <- 1.42

ggplot() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.3, col = "grey90") +
  geom_line(data=data,aes(x = Sedimentation, y = Proportion, colour = Species, group = interaction(Species, Event)), size = 1) + 
  geom_line(data=data2, aes(x = Sedimentation, y = Proportion, colour = Species, group = interaction(Species, Event)), size = 1) 


