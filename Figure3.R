
pdf("fig3.pdf", height=4,width=8, pointsize=10, paper ="special" )
par(mfrow=c(1,2), omi=c(0.2, 0.2, 0.2, 0.2), mai=c(0.5, 0.4, 0.5, 0.1), pty="s")
	source("params.r")  
	#plot a  - Cost benefit curves over soil to root resistance
	ENV=seq(0, 10000, 1e3);
	fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), max(SALA_axis)),SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L);
	for(i in 1:length(ENV)){
		Env$dw = ENV[i]*Ksoil(Env)*RALA; 
		CB = cost_ben(SALA.vec, RALA, Leaf, Plant, Env)
    if(i==1)  #only one cost line because all the same
		  points(CB[1,]*1e4, CB[7,]*1e-3, col=1, lwd =LWD, type='l', cex=cex.cost,  lty='dashed')  # Invest
		points(CB[1,]*1e4, CB[6,]*1e-3, col=1, lwd =LWD, type='l')  # Revenue
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		points(OP[1,]*1e4, OP[6,]*1e-3, col=1, lwd =LWD, cex=1, pch=19)}  # Optimum
	title('a)', adj=0, cex.main = 1.2)	

	#plot b  - SALA optimum vs soil to root resistance
	OPT<-NULL; 	
	ENV=seq(0, 1e4, 100);
	for(i in 1:length(ENV)){
		Env$dw = ENV[i]*Ksoil(Env)*RALA; 
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
		}
	#PLOT 
	fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, c(10, 0), expression(paste("Soil to Root resistance(",10^3," ",MPa," ", m^2," ",s," ",mol^-1, " )")), seq(0, 10, 2), seq(0, 10,1), CEX.A, CEX.L)
	points(OPT[,1]*1e-3, OPT[,2]*1e4, type='l', lwd =LWD)
	title('b)', adj=0, cex.main = 1.2)	

 dev.off()	


