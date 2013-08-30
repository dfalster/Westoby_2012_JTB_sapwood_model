
pdf("fig6.pdf", height=4,width=8, pointsize=10, paper ="special" )
	
par(mfrow=c(1,2), omi=c(0.2, 0.2, 0.2, 0.2), mai=c(0.5, 0.4, 0.5, 0.1), pty="s")
	source("params.r")  
	#plot a  - Cost benefit curves over diff heights at current CO2
	ENV=c(seq(0.5, 60,10))
	fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), max(SALA_axis)),SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L);
	for(i in 1:length(ENV)){
		Plant$height=ENV[i];
		CB = cost_ben(SALA.vec, RALA, Leaf, Plant, Env)
		points(CB[1,]*1e4, CB[7,]*1e-3, col=1, lwd =LWD, type='l',  lty='dashed',cex=cex.cost)  # Invest
		points(CB[1,]*1e4, CB[6,]*1e-3, col=1, lwd =LWD, type='l')  # Revenue
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		points(OP[1,]*1e4, OP[6,]*1e-3, col=1, lwd =LWD, cex=1, pch=19)}  # Optimum
	title('a)', adj=0, cex.main = 1.2)
	#plot b  -  SALA optimum vs height for different co2 values
	fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, c(0, 120), "Height (m)", seq(0.0,120, 20), seq(0.0,120, 10), CEX.A, CEX.L)
	
	ENV=c(seq(0.1, 120,0.5))
	CA=c(seq(200, 1000,200))
	
	for(j in 1:length(CA)){
			OPT<-NULL; 
			Env$ca = CA[j];
			for(i in 1:length(ENV)){
				Plant$height=ENV[i];
				OP = cost_ben.opt(RALA, Leaf, Plant, Env);
				if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
				}		
			points(OPT[,1], OPT[,2]*1e4, type='l', lwd =LWD)
			}
  text(17, 1.48, "200 ppm", cex=0.8)
  text(17, 0.55, "1000 ppm", cex=0.8)
	
title('b)', adj=0, cex.main = 1.2)
dev.off()	

