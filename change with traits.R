
pdf("trait_plots.pdf", height=8,width=12, pointsize=10, paper ="special" )

par(mfrow=c(3,3), oma=c(5, 5, 10, 5), mai=c(1, 1, 0, 0))

LWD =1;  CEX.L =0.9; CEX.A = 1.2;
YLIM = c(0, .002*1e3);  YLAB = expression(paste("Optimal SA:LA (", 10^-4, m^2, m^-2,")"));
#Y_ax =seq(0, .003*1000, 0.5); Y_ax2=seq(0, .003*1000, 0.1);

#plot 1 LMA
	source("params.r")  
	XLAB = "LMA"; ENV=seq(100, 1500, 50); #X_ax =seq(0.0,3, 0.5); X_ax2 =seq(0.0, 3, 0.1); 

	plot(1:2, 1:2, type="n",log="", axes=T,ann=F, xlim = c(min(ENV), max(ENV)), ylim=YLIM, xaxs="i", yaxs="i", las=1) #,family="helvetica")
	mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L); mtext(YLAB, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Plant$LMA = ENV[i];
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);
		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A")
	#PLOT OPTIMUM	
	points(OPT[,1], OPT[,2]*1e4, type='l', lwd=LWD)
	
#plot 2 LLS
	source("params.r")  
	XLAB = "LEAF LIFESPAN"; ENV=seq(0.1,10, 0.1); #X_ax =seq(0.0,3, 0.5); X_ax2 =seq(0.0, 3, 0.1); 

	plot(1:2, 1:2, type="n",log="", axes=T,ann=F, xlim = c(min(ENV), max(ENV)), ylim=YLIM, xaxs="i", yaxs="i", las=1) #,family="helvetica")
	mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L); mtext(YLAB, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Plant$lls= ENV[i];
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);		if(OP[8] > 0) 	OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]));
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A")
	#PLOT OPTIMUM	
	points(OPT[,1], OPT[,2]*1e4, type='l', lwd=LWD)

#plot 3 Nroot
	source("params.r")  
	XLAB = "Root N"; ENV=seq_log(0.0001,0.01, 1.1); #X_ax =seq(0.0,3, 0.5); X_ax2 =seq(0.0, 3, 0.1); 

	plot(1:2, 1:2, type="n",log="", axes=T,ann=F, xlim = c(min(ENV), max(ENV)), ylim=YLIM, xaxs="i", yaxs="i", las=1) #,family="helvetica")
	mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L); mtext(YLAB, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Plant$nmass.root= ENV[i];
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A")
	#PLOT OPTIMUM	
	points(OPT[,1], OPT[,2]*1e4, type='l', lwd=LWD)

#plot 4 Wood desnity
	source("params.r")  
	XLAB = "Wood density"; ENV=seq(0.2e6,1.0e6, 0.1e6); #X_ax =seq(0.0,3, 0.5); X_ax2 =seq(0.0, 3, 0.1); 

	plot(1:2, 1:2, type="n",log="", axes=T,ann=F, xlim = c(min(ENV), max(ENV)), ylim=YLIM, xaxs="i", yaxs="i", las=1) #,family="helvetica")
	mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L); mtext(YLAB, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Plant$den.stem= ENV[i];
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A")
	#PLOT OPTIMUM	
	points(OPT[,1], OPT[,2]*1e4, type='l', lwd=LWD)

#plot 5 Respiration per N
	source("params.r")  
	XLAB = "Respiration per N"; ENV=seq(0.01,0.1, 0.01); #X_ax =seq(0.0,3, 0.5); X_ax2 =seq(0.0, 3, 0.1); 

	plot(1:2, 1:2, type="n",log="", axes=T,ann=F, xlim = c(min(ENV), max(ENV)), ylim=YLIM, xaxs="i", yaxs="i", las=1) #,family="helvetica")
	mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L); mtext(YLAB, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		RESP.coeff = ENV[i];
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A")
	#PLOT OPTIMUM	
	points(OPT[,1], OPT[,2]*1e4, type='l', lwd=LWD)

#plot 6 Photosynth per per N
	source("params.r")  
	XLAB = "Vmax/Jmax per N"; ENV=seq(0.4, 2.0, 0.1); #X_ax =seq(0.0,3, 0.5); X_ax2 =seq(0.0, 3, 0.1); 
	Kj = Leaf$kj; Kv = Leaf$kv;
	
	plot(1:2, 1:2, type="n",log="", axes=T,ann=F, xlim = c(min(ENV), max(ENV)), ylim=YLIM, xaxs="i", yaxs="i", las=1) #,family="helvetica")
	mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L); mtext(YLAB, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Leaf$kj = Kj*ENV[i]; Leaf$kv = Kv*ENV[i];
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A")
	#PLOT OPTIMUM	
	points(OPT[,1], OPT[,2]*1e4, type='l', lwd=LWD)

#plot 7 Ks
	source("params.r")  
	XLAB = "Ks"; ENV=seq_log(50, 1000, 1.1); #X_ax =seq(0.0,3, 0.5); X_ax2 =seq(0.0, 3, 0.1); 

	plot(1:2, 1:2, type="n",log="", axes=T,ann=F, xlim = c(min(ENV), max(ENV)), ylim=YLIM, xaxs="i", yaxs="i", las=1) #,family="helvetica")
	mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L); mtext(YLAB, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Plant$K.stem = ENV[i];
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A")
	#PLOT OPTIMUM	
	points(OPT[,1], OPT[,2]*1e4, type='l', lwd=LWD)
	
#plot 8 Psi crit
	source("params.r")  
	XLAB = "Psi crit"; ENV=seq(2.5, 10, 0.1); #X_ax =seq(0.0,3, 0.5); X_ax2 =seq(0.0, 3, 0.1); 

	plot(1:2, 1:2, type="n",log="", axes=T,ann=F, xlim = c(-min(ENV), -max(ENV)), ylim=YLIM, xaxs="i", yaxs="i", las=1) #,family="helvetica")
	mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L); mtext(YLAB, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Plant$psi.crit = -ENV[i];
		OP =cost_ben.opt(RALA, Leaf, Plant, Env);		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A")
	#PLOT OPTIMUM	
	points(-OPT[,1], OPT[,2]*1e4, type='l', lwd=LWD)

#plot 9 Narea
	source("params.r")  
	XLAB = "Narea"; ENV=seq(0.25, 8, 0.25); #X_ax =seq(0.0,3, 0.5); X_ax2 =seq(0.0, 3, 0.1); 

	plot(1:2, 1:2, type="n",log="", axes=T,ann=F, xlim = c(min(ENV), max(ENV)), ylim=YLIM, xaxs="i", yaxs="i", las=1) #,family="helvetica")
	mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L); mtext(YLAB, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Plant$narea.lf = ENV[i];
		OP = cost_ben.opt(RALA, Leaf, Plant, Env);		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A")
	#PLOT OPTIMUM	
	points(OPT[,1], OPT[,2]*1e4, type='l', lwd=LWD) 

dev.off()
	