
pdf("fig7.pdf", height=12,width=8, pointsize=10, paper ="special" )
par(mfrow=c(3,2), omi=c(0.2, 0.2, 0.2, 0.2), mai=c(0.5, 0.4, 0.5, 0.1), pty="s")

#for max height axis
YLAB.2 = "Max. height (m)"; 
Y_ax.2 =seq(0,100,20); Y_ax2.2=seq(0, 100,10); YLIM.2 = c(0,100); 

#First factor - C02
source("params.r")       	#reset parameters
 	#set range to use in plots
 	ENV =seq(200, 1000, 10); 
	XLAB = expression(paste("Atmospheric ",CO[2]," (ppm)"));  X_ax =seq(200, 1000,200); X_ax2=seq(200, 1000, 100); XLIM = c(200, 1000);   
	
	OPT1<-NULL; OPT2<-NULL;
	for(i in 1:length(ENV)){
		Env$ca= ENV[i];
		OP = height.max(RALA, Leaf, Plant, Env)
		OPT1<-rbind(OPT1, c(ENV[i], OP[1], OP[5], OP[3], OP[9]))
		OP = cost_ben.opt(RALA, Leaf, Plant, Env)
		if(OP[8] > 0)  OPT2<-rbind(OPT2, c(ENV[i], OP[1], OP[5], OP[3]))
		};	
		colnames(OPT1)=c("ENV", "sala", "leafN", "A", "Hmax"); colnames(OPT2)=c("ENV", "sala", "leafN", "A");
	
	fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, XLIM, XLAB, X_ax, X_ax2, CEX.A, CEX.L)
	points(OPT2[,1], OPT2[,2]*1e4, type='l', lwd =LWD)    #plot SALA for otpimising C 
	points(OPT1[,1], OPT1[,2]*1e4, type='l', lwd =LWD, lty="dashed")     #plot SALA for optimising H 
	title('a)', adj=0, cex.main = 1.5)
	fig.setup(YLIM.2, YLAB.2, Y_ax.2, Y_ax2.2, XLIM, XLAB, X_ax, X_ax2, CEX.A, CEX.L)
	points(OPT1[,1], OPT1[,5], type='l', lwd =LWD, lty="dashed")     
	title('b)', adj=0, cex.main = 1.5)
#plot SALA for optimising H 
	print(OPT1)

# VPD
source("params.r")       	#reset parameters
 	#set range to use in plots
	ENV=seq_log(0.1, 3.0, 1.1);
 	XLAB = "VPD  (kPa)"; X_ax =seq(0.0,2.5, 0.5);  X_ax2 =seq(0.0, 2.5, 0.25); XLIM = c(min(X_ax), max(X_ax));
	OPT1<-NULL; OPT2<-NULL;
	for(i in 1:length(ENV)){
		Env$vpd= ENV[i];
		OP = height.max(RALA, Leaf, Plant, Env)
		OPT1<-rbind(OPT1, c(ENV[i], OP[1], OP[5], OP[3], OP[9]))
		
		OP = cost_ben.opt(RALA, Leaf, Plant, Env)
		if(OP[8] > 0)  OPT2<-rbind(OPT2, c(ENV[i], OP[1], OP[5], OP[3]))
		};	
		colnames(OPT1)=c("ENV", "sala", "leafN", "A", "Hmax"); colnames(OPT2)=c("ENV", "sala", "leafN", "A");


	fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, XLIM, XLAB, X_ax, X_ax2, CEX.A, CEX.L)
	points(OPT2[,1], OPT2[,2]*1e4, type='l', lwd =LWD)    #plot SALA for otpimising C 
	points(OPT1[,1], OPT1[,2]*1e4, type='l', lwd =LWD, lty="dashed") #plot SALA for optimising H 
	title('c)', adj=0, cex.main = 1.5)

	fig.setup(YLIM.2, YLAB.2, Y_ax.2, Y_ax2.2, XLIM, XLAB, X_ax, X_ax2, CEX.A, CEX.L)
	points(OPT1[,1], OPT1[,5], type='l', lwd =LWD, lty="dashed")     #plot SALA for optimising H 
	title('d)', adj=0, cex.main = 1.5)

# SOILS PSI
source("params.r")       	#reset parameters
 	#set range to use in plots
 	ENV=-seq(0.01,1.5, 0.1);
	XLAB = "Soil Water Potential (MPa)"; X_ax = seq(0,-1.5, -0.25); X_ax2 = seq(0.0, -1.5, -1.25); XLIM = c(min(X_ax), max(X_ax));
	OPT1<-NULL; OPT2<-NULL;
	for(i in 1:length(ENV)){
		Env$psi.soil= ENV[i];
		OP = height.max(RALA, Leaf, Plant, Env)
		OPT1<-rbind(OPT1, c(ENV[i], OP[1], OP[5], OP[3], OP[9]))
		
		OP = cost_ben.opt(RALA, Leaf, Plant, Env)
		if(OP[8] > 0)  OPT2<-rbind(OPT2, c(ENV[i], OP[1], OP[5], OP[3]))
		};	
		colnames(OPT1)=c("ENV", "sala", "leafN", "A", "Hmax"); colnames(OPT2)=c("ENV", "sala", "leafN", "A");


	fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, XLIM, XLAB, X_ax, X_ax2, CEX.A, CEX.L)
	points(OPT2[,1], OPT2[,2]*1e4, type='l', lwd =LWD)    #plot SALA for otpimising C 
	points(OPT1[,1], OPT1[,2]*1e4, type='l', lwd =LWD, lty="dashed") #plot SALA for optimising H 
	title('e)', adj=0, cex.main = 1.5)

	fig.setup(YLIM.2, YLAB.2, Y_ax.2, Y_ax2.2, XLIM, XLAB, X_ax, X_ax2, CEX.A, CEX.L)
	points(OPT1[,1], OPT1[,5], type='l', lwd =LWD, lty="dashed")     #plot SALA for optimising H 
	title('f)', adj=0, cex.main = 1.5)

 dev.off()

	