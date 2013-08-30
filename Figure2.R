
pdf("fig2.pdf", height=8,width=12, pointsize=10, paper ="special")

par(mfrow=c(2,3), omi=c(0.2, 0.2, 0.2, 0.2), mai=c(0.5, 0.4, 0.5, 0.1), pty="s")
#top row a-c - Resposne to VPD
	source("params.r")  
	ENV=seq_log(0.1, 3.0, 1.5) #VPD   
	
	#plot a  - Cost benefit curves over VPD, low soil to root resistance
	Env$dw = 20*Ksoil(Env)*RALA; #set dw so that root resistenace = 20 
	fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), max(SALA_axis)),SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L);
	for(i in 1:length(ENV)){
		Env$vpd = ENV[i];	
		CB = cost_ben(SALA.vec, RALA, Leaf, Plant, Env)
    if(i==1)  #only one cost line because all the same
		  points(CB[1,]*1e4, CB[7,]*1e-3, col=1, lwd =LWD, type='l', lty='dashed', cex=cex.cost)  # Invest
		points(CB[1,]*1e4, CB[6,]*1e-3, col=1, lwd =LWD, type='l')  # Revenue
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		points(OP[1,]*1e4, OP[6,]*1e-3, col=1, lwd =LWD, cex=1, pch=19)  # Optimum
  }
  #add text & arrow to show direction of change in ENV
#   text(3.6, 4.3, format(ENV[length(ENV)], digits=1), cex=0.8)
#   text(3.6, 5.6, format(ENV[1], digits=1), cex=0.8)
#  arrows(3.6, 4.5, 3.6, 5.4, lwd=1.5, length=0.15)

	title('a)', adj=0, cex.main = 2.0)	

	#set dw so that root resistenace = 120 
	Env$dw = 1200*Ksoil(Env)*RALA;
	fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), max(SALA_axis)),SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L);
	for(i in 1:length(ENV)){
		Env$vpd = ENV[i];	
		CB = cost_ben(SALA.vec, RALA, Leaf, Plant, Env)
    if(i==1)  #only one cost line because all the same
      points(CB[1,]*1e4, CB[7,]*1e-3, col=1, lwd =LWD, type='l', lty='dashed', cex=cex.cost)  # Invest
		points(CB[1,]*1e4, CB[6,]*1e-3, col=1, lwd =LWD, type='l')  # Revenue
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		points(OP[1,]*1e4, OP[6,]*1e-3, col=1, lwd =LWD, cex=1, pch=19)}  # Optimum
  
#   text(3.6, 2.7, format(ENV[length(ENV)], digits=1), cex=0.8)
#   text(3.6, 5.6, format(ENV[1], digits=1), cex=0.8)
#   arrows(3.6, 2.9, 3.6, 5.4, lwd=1.5, length=0.15)
  title('b)', adj=0, cex.main = 2.0)	
	
#plot c  - SALA optimum vs VPD for high and low soil to root resistance
	OPT_low<-NULL; OPT_high<-NULL; 	
	ENV=seq_log(0.1, 3.0, 1.1) #VPD range   
	for(i in 1:length(ENV)){
		Env$vpd = ENV[i];	
		Env$dw = 20*Ksoil(Env)*RALA; #set dw so that root resistenace = 20 
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		if(OP[8] > 0)  OPT_low<-rbind(OPT_low, c(ENV[i], OP[1], OP[5], OP[3]))
		Env$dw = 1200*Ksoil(Env)*RALA;
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		if(OP[8] > 0)  OPT_high<-rbind(OPT_high, c(ENV[i], OP[1], OP[5], OP[3]))
		}
	#PLOT 
	fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, c(0.0, 2.5), "VPD (kPa)", seq(0.0,2.5, 0.5), seq(0.0,2.5, 0.25), CEX.A, CEX.L)
	points(OPT_low[,1], OPT_low[,2]*1e4, type='l', lwd =LWD)
	points(OPT_high[,1], OPT_high[,2]*1e4, type='l', lwd =LWD, lty='dashed')
	title('c)', adj=0, cex.main = 2.0)	

#top row d-f - Resposne to CO2
	source("params.r")  
	ENV=seq(200, 1000, 200) #VPD   

	#plot d  - Cost benefit curves at low soil to root resistance
	Env$dw = 20*Ksoil(Env)*RALA; #set dw so that root resistenace = 20 
	fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), max(SALA_axis)),SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L);
	for(i in 1:length(ENV)){
		Env$ca = ENV[i];	
		CB = cost_ben(SALA.vec, RALA, Leaf, Plant, Env)
		if(i==1)  #only one cost line because all the same
      points(CB[1,]*1e4, CB[7,]*1e-3, col=1, lwd =LWD, type='l',  lty='dashed',cex=cex.cost)  # Invest
		points(CB[1,]*1e4, CB[6,]*1e-3, col=1, lwd =LWD, type='l')  # Revenue
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		points(OP[1,]*1e4, OP[6,]*1e-3, col=1, lwd =LWD, cex=1, pch=19)}  # Optimum
	title('d)', adj=0, cex.main = 2.0)	

	#set dw so that root resistenace = 1200 
	Env$dw = 1200*Ksoil(Env)*RALA;
	fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), max(SALA_axis)),SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L);
	for(i in 1:length(ENV)){
		Env$ca = ENV[i];	
		CB = cost_ben(SALA.vec, RALA, Leaf, Plant, Env)
		if(i==1)  #only one cost line because all the same
      points(CB[1,]*1e4, CB[7,]*1e-3, col=1, lwd =LWD, type='l', lty='dashed', cex=cex.cost)  # Invest
		points(CB[1,]*1e4, CB[6,]*1e-3, col=1, lwd =LWD, type='l')  # Revenue
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		points(OP[1,]*1e4, OP[6,]*1e-3, col=1, lwd =LWD, cex=1, pch=19)}  # Optimum
	title('e)', adj=0, cex.main = 2.0)	

	#plot c  - SALA optimum vs CO2 for high and low soil to root resistance
	OPT_low<-NULL; OPT_high<-NULL; 	
	ENV=seq(200, 1000, 20) #VPD range   
	for(i in 1:length(ENV)){
		Env$ca = ENV[i];	
		Env$dw = 20*Ksoil(Env)*RALA; #set dw so that root resistenace = 20 
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		if(OP[8] > 0)  OPT_low<-rbind(OPT_low, c(ENV[i], OP[1], OP[5], OP[3]))
		Env$dw = 1200*Ksoil(Env)*RALA;
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		if(OP[8] > 0)  OPT_high<-rbind(OPT_high, c(ENV[i], OP[1], OP[5], OP[3]))
		}
#	print("CO2")
#	print(OPT_low)
#	print(OPT_high)
	#PLOT 
	fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, c(200, 1000), expression(paste("Atmospheric ",CO[2]," (ppm)")), seq(200,1000, 200), seq(200,1000, 100), CEX.A, CEX.L)
	points(OPT_low[,1], OPT_low[,2]*1e4, type='l', lwd =LWD, lty='dashed')
	points(OPT_high[,1], OPT_high[,2]*1e4, type='l', lwd =LWD)
	title('f)', adj=0, cex.main = 2.0)	

 dev.off()

	