
#clear memory
rm(list=ls(all=TRUE))

#read in leaf functions
setwd("~/Dropbox/Documents/_research/x_archive/2012_Westoby_JTB/model")
source("leaf_v3.0.r")    #load functions
source("params.r")       #default params


#Plot, OPT_1dimising SALA
OPT_1d<-NULL; 
ENV=seq_log(0.1, 3.0, 1.1) #VPD range   
Env$dw = 20*Ksoil(Env)*RALA; #set dw so that root resistenace = 20 

for(i in 1:length(ENV)){
  Env$vpd = ENV[i];	
  #optimise SaLA
  OP = cost_ben.opt(RALA, Leaf, Plant, Env);
  if(OP[8] > 0)  OPT_1d<-rbind(OPT_1d, c(ENV[i], OP[1], OP[4], OP[3]))

}
X<-OPT_1d[,1]
#our model
plot(X, OPT_1d[,3]/380, type='l', ylim=c(0,1), las=1, xaxs="i", yaxs="i", xlab="Vpd", ylab="ci/ca")

#colins mdoel
for(y in seq(0, 5, by=0.5)){
  X2<-y/2
  points(X, y/(y+sqrt(X)), type='l', col="red") 
text(X2, y/(y+sqrt(X2)), y)
}

#Plot with fixed SALA, 1d Optimised SALA, and 2D optimised SALA/RALA

, OPT_1dimising SALA
OPT<-NULL; 
SALA<-1E-4

#Remove leaf reistance and soil resistance
Plant$r.lf <-0;
Env$dw = 0;

ENV=seq_log(0.1, 3.0, 1.1) #VPD range

for(i in 1:length(ENV)){
  Env$vpd = ENV[i];  
  #optimise SLA
  OP = cost_ben.opt(RALA, Leaf, Plant, Env);
  FIX<-cost_ben(1E-4, RALA, Leaf, Plant, Env)
  OP2<-cost_ben.opt2D(Leaf, Plant, Env);
  if(OP[8] > 0)
    OPT<-rbind(OPT, c(ENV[i], OP[1], OP[4], FIX[4], OP2[4]))  
}

X<-OPT[,1]
#our model
plot(X, OPT[,3]/380, type='l', ylim=c(0,1), las=1, xaxs="i", yaxs="i", xlab="Vpd", ylab="ci/ca")

points(X, OPT[,4]/380, type='l', col="red") 
points(X, OPT[,5]/380, type='l', col="blue") 

#colins mdoel
for(y in seq(2.5, 3.5, by=0.1)){
  X2<-y/2
  points(X, y/(y+sqrt(X)), type='l', col="red") 
  text(X2, y/(y+sqrt(X2)), y)
}

