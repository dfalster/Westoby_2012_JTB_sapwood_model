
pdf("fig5.pdf", height=4,width=10, pointsize=10, paper ="special" )
par(mfrow=c(1,3), omi=c(0.2, 0.2, 0.2, 0.2), mai=c(0.5, 0.5, 0, 0), pty="s")

#axis labels
Y_ax =c(0.1,1,10); Y_ax2=c(seq(0.1, 1, 0.1), seq(1, 10, 1)); YLIM = c(0.1,10);   
YLAB = expression(paste("Optimal SA:LA (", 10^-4, m^2, m^-2,") or"));
YLAB.2 = expression(paste("RA:LA (", m^2, m^-2,") [log scale]"));

#Part a - plot in relation to VPD
	source("params.r")       	#reset parameters
    ENV=seq(0.1, 2.9, 0.1);  
    XLAB = "VPD (kPa)"; X_ax =seq(0.0,2.5, 1);  X_ax2 =seq(0.0, 2.5, 0.5); XLIM = c(0.0, 2.5);

    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Env$vpd = ENV[i];
		OP = cost_ben.opt2D(Leaf, Plant, Env)
		if(OP[8] > 0) 
			OPT<-rbind(OPT, c(ENV[i], OP[1], OP[9], OP[5], OP[3]))
		}
	colnames(OPT)=c("ENV", "sala", "rala", "leafN", "A")
	
	#PLOT SALA OPTIMUM	
    plot(1:2, 1:2, type="n",log="y", axes=F,ann=F, xlim = XLIM, ylim=YLIM, xaxs="i", yaxs="i", las=1); 	axis(2, at=Y_ax, labels=Y_ax, las=1, tck=0.030, cex.axis=CEX.A, adj = 0.5); axis(2, at=Y_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5); 
    axis(1, at=X_ax, labels=X_ax, las=1, tck=0.03, cex.axis=CEX.A); axis(1, at=X_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5);  
    axis(3, at=X_ax, labels=F, las=1, tck=0.03, cex.axis=CEX.A); axis(3, at=X_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5);  
    axis(4, at=Y_ax, labels=F, las=1, tck=0.030, cex.axis=CEX.A, adj = 0.5); axis(4, at=Y_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5);
	box();  
    mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L) 
    mtext(YLAB, side = 2, line = 5.0, outer = F, at= NA, cex=CEX.L)  
	mtext(YLAB.2, side = 2, line = 3.0, outer = F, at= NA, cex=CEX.L)  
	points(OPT[,2]*1e4~OPT[,1], type = 'l') #PLOT SALA OPTIMUM
	points(OPT[,3]~OPT[,1], type = 'l', lty = "dashed") #PLOT RALA OPTIMUM
	title('a)', adj=0, cex.main = 1.5, line = 1)	

#*****
#Part b - CO2
	source("params.r")       	#reset parameters
    ENV=seq(200, 1000, 25);  
    XLAB = expression(paste("Atmospheric ",CO[2]," (ppm)")); X_ax =seq(200,1000,200);  X_ax2 =seq(200,1000,100); XLIM = c(min(X_ax), max(X_ax));

    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Env$ca = ENV[i];
		OP = cost_ben.opt2D(Leaf, Plant, Env)
		if(OP[8] > 0) 
			OPT<-rbind(OPT, c(ENV[i], OP[1], OP[9], OP[5], OP[3]))
		}
	colnames(OPT)=c("ENV", "sala", "rala", "leafN", "A")
	
	#PLOT SALA OPTIMUM	
    plot(1:2, 1:2, type="n",log="y", axes=F,ann=F, xlim = XLIM, ylim=YLIM, xaxs="i", yaxs="i", las=1); 	axis(2, at=Y_ax, labels=Y_ax, las=1, tck=0.030, cex.axis=CEX.A, adj = 0.5); axis(2, at=Y_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5); 
    axis(1, at=X_ax, labels=X_ax, las=1, tck=0.03, cex.axis=CEX.A); axis(1, at=X_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5);  
    axis(3, at=X_ax, labels=F, las=1, tck=0.03, cex.axis=CEX.A); axis(3, at=X_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5);  
    axis(4, at=Y_ax, labels=F, las=1, tck=0.030, cex.axis=CEX.A, adj = 0.5); axis(4, at=Y_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5);
	box();  
    mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L) 
	points(OPT[,2]*1e4~OPT[,1], type = 'l') #PLOT SALA OPTIMUM
	points(OPT[,3]~OPT[,1], type = 'l', lty = "dashed") #PLOT RALA OPTIMUM
	title('b)', adj=0, cex.main = 1.5, line = 1)	


#*****
#Part c - psi soil
	source("params.r")       	#reset parameters
    ENV=-seq(0.1, 3.9, 0.05);  
    XLAB = "Soil water potential (MPa)"; X_ax =-seq(0.0,2, 0.5);  X_ax2 =-seq(0.0, 2.0, 0.25); XLIM = c(min(X_ax), max(X_ax));

    #CALCULATE OPTIMUM 
	OPT<-NULL; 
	for(i in 1:length(ENV)){
		Env$psi.soil = ENV[i];
		OP = cost_ben.opt2D(Leaf, Plant, Env)
		if(OP[8] > 0) 
			OPT<-rbind(OPT, c(ENV[i], OP[1], OP[9], OP[5], OP[3]))
		}
	colnames(OPT)=c("ENV", "sala", "rala", "leafN", "A")
	
	#PLOT SALA OPTIMUM	
    plot(1:2, 1:2, type="n",log="y", axes=F,ann=F, xlim = XLIM, ylim=YLIM, xaxs="i", yaxs="i", las=1); 	axis(2, at=Y_ax, labels=Y_ax, las=1, tck=0.03, cex.axis=CEX.A, adj = 0.5); axis(2, at=Y_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5); 
    axis(1, at=X_ax, labels=X_ax, las=1, tck=0.03, cex.axis=CEX.A); axis(1, at=X_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5);  
    axis(3, at=X_ax, labels=F, las=1, tck=0.03, cex.axis=CEX.A); axis(3, at=X_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5);  
    axis(4, at=Y_ax, labels=F, las=1, tck=0.03, cex.axis=CEX.A, adj = 0.5); axis(4, at=Y_ax2, labels=F, las=1, tck=0.015, cex.axis=CEX.A, adj = 0.5);
	box();  
    mtext(XLAB, side = 1, line = 3.0, outer = F, at= NA, cex=CEX.L) 
	points(OPT[,2]*1e4~OPT[,1], type = 'l') #PLOT SALA OPTIMUM
	points(OPT[,3]~OPT[,1], type = 'l', lty = "dashed") #PLOT RALA OPTIMUM
	title('c)', adj=0, cex.main = 1.5, line = 1)	


 dev.off()


	