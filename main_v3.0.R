#Mark Westoby, Dan Falster and Will Cornwell
#last modified 2012.02.27

#clear memory
rm(list=ls(all=TRUE))

#read in leaf functions
	source("leaf_v3.0.r")    #load functions
	source("params.r")       #default params

#Revision for JTB 2012.02.27
# - reversed axis direction for VPD in figs 2,5,7
# - set N cost to 0.0 (effectively removed)
# - changed cost lines in 2,3,4, 6 to be dashed

#--------------------------------------------------
# MS FIGURES
#set common axes for cost-benefit and SALA plots
	#for cost-benefit curves
	CB_lab = expression(paste("Revenue or cost (kg C"," ", m^-2,")"));
	CB_axis =seq(0, 8, 1); CB_axis2=seq(0, 8, 0.5);
	SALA_lab = expression(paste("SA:LA (", 10^-4, m^2, m^-2,")"));
 	SALA_axis =seq(0, .0004*1e4, 1); SALA_axis2=seq(0, .0004*1e4, 0.5);
	cex.cost =1.5;

	#for opitmum sala plots
	OPT_lab = expression(paste("Optimal SA:LA (", 10^-4, m^2, m^-2,")"));
	OPT_axis =seq(0, .00025*1e4, 0.5); OPT_axis2=seq(0, .00025*1e4, 0.25);

#make figures
	LWD =1;  CEX.L =0.9; CEX.A = 1.2; #formatting
	source("Figure2.r")
	CEX.L =1.0; CEX.A = 0.9; #formatting
	source("Figure3.r")
	source("Figure4.r")
	CEX.L =0.9; CEX.A = 1.2; #formatting
	source("Figure5.r")
	CEX.L =1.0; CEX.A = 0.9; #formatting
	source("Figure6.r")
	CEX.L =0.9; CEX.A = 1.2; #formatting
	source("Figure7.r")
	source("change with traits.r")

#-------------------------------------------------------------------------------
# EXTRA PLOTS
#-------------------------------------------------------------------------------
#plot Aci curve
   N.alloc=cost_ben(100*SALA.min,RALA, Leaf, Plant, Env)[5]
   gc = stom.func(100*SALA.min, RALA, Plant, Env )
   Plot.Aci.g(Plant$narea.lf, N.alloc, Env.q, gc, Env$ca, Env$temp,Leaf)
   Plot.Aci(Plant$narea.lf, 0.45, Env.q, Env$temp,Leaf)

#------------------------------------------------------------------------------

#2D opimisation
net.Cbal.2D(RALA, Leaf, Plant, Env)
cost_ben.opt2D(Leaf, Plant, Env)

#PLOT OPTIMUM OVER RANGE OF PARAMETER SPACE
	source("params.r")       	#reset parameters
    #set range to use in plots
   # ENV=seq_log(0.1, 3.0, 1.1);  #VPD
    ENV=seq(1, 3.0, 0.1);  XLAB = "Psi Soil";#Psi.soil

    #CALCULATE OPTIMUM
	OPT<-NULL;
	for(i in 1:length(ENV)){
		#Env$vpd = ENV[i];
		Env$psi.soil=-ENV[i]
		OP = cost_ben.opt2D(Leaf, Plant, Env)
		if(OP[8] > 0)
			OPT<-rbind(OPT, c(-ENV[i], OP[1], OP[9], OP[5], OP[3]))
		}
	colnames(OPT)=c("ENV", "sala", "rala", "leafN", "A")

	#PLOT OPTIMUM
	par(mar=c(5,4,4,4))
	plot(OPT[,2]~OPT[,1],pch=1,col=3,xlab=XLAB,ylab="",type="l",log="", ylim=c(0, max(OPT[,2])*1.2))
	mtext("Sapwood area per leaf area",side=2,line=2,col=3)
	mtext("Co-optimization of root area and sapwood area per leaf area", side=3, line =1)
	par(new=T)
	plot(OPT[,3]~OPT[,1],axes=F,xlab="",ylab="",pch=2,col=4 ,type="l",log="", ylim=c(0, max(OPT[,3])*1.2))
	axis(side=4); box();
	mtext("Root area per leaf area",side=4,line=2,col=4)


#-------------------------------------------------------------------------------
#PLOT COST BENEFIT CURVES OVER RANGE OF PARAMETER SPACE
	source("params.r")       #reset parameters
	YLIM = c(0,5000); YLAB = expression(paste("cost or revenue (", gC," ", m^-2,")"));
	XLIM = c(0, SALA.max); XLAB = expression(paste("SA:LA (", m^2, m^-2,")"));
	LWD = 3; CEX =1.5; 	     #line wideth, symbol size
	RALA<-5

#range to use in plots
	ENV=c(-0.5, -1, -1.5,-2,-2.5,-3) #PSI.SOIL
	#ENV=c(-0.05,-1.0,,-1.5,-1.8,-1.9,-2.0,-2.1)#Psi soil

#make plots
	plot(1,1,, type="n", xlim=XLIM, xlab = XLAB, ylim = YLIM, ylab= YLAB);
	for(i in 1:length(ENV)){
	#CHANGE THIS TO PLOT FOR DIFF VARIABLES
		Env$psi.soil = ENV[i];
	#DON'T CHANGE THIS
		CB = cost_ben(SALA.vec, RALA, Leaf, Plant, Env)
		points(CB[1,], CB[7,], col=1, lwd =LWD, type='l')  # Invest
		points(CB[1,], CB[6,], col=i+1, lwd =LWD, type='l')  # Revenue
		OP = cost_ben.opt(RALA, Leaf, Plant, Env)
		points(OP[1,], OP[6,], col=i+1, lwd =LWD, cex=CEX)}  # Optimum

#-------------------------------------------------------------------------------
#PLOT OPTIMUM OVER RANGE OF PARAMETER SPACE
	source("params.r")       	#reset parameters
	LWD =2;
  	YLIM = c(0, .0005*1000);  YLAB = expression(paste("Optimal SA:LA (", 10^-3, m^2, m^-2,")"));

 #set range to use in plots
  #XLAB = "VPD"; ENV=seq(0.1, 3.0, 0.01);
	#XLAB = "Soil Water Potential (-Mpa)"; ENV=seq(0.05,2.1, 0.05)
  	XLAB = expression(CO[2]); ENV =seq(100, 1000, 10);
	#XLAB = "Height"; ENV =seq_log(0.1, 90, 1.05);
  #CALCULATE OPTIMUM
	OPT<-NULL;
	for(i in 1:length(ENV)){
		Env$ca = ENV[i];
		OP = cost_ben.opt(RALA, Leaf, Plant, Env)
		if(OP[8] > 0)  OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3], OP[4]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A", "CI")
	#PLOT OPTIMUM
	plot(OPT[,1], OPT[,2]*1000, type='l', col = 4, lwd =LWD,	xlab = XLAB, xlim=c(min(ENV), max(ENV)),  ylim = YLIM, ylab= YLAB, las =1)

	#PLOT SA:LA, LEAF N AND A IN PANEL
	par(mfrow=c(1,3))
		plot(OPT[,1], OPT[,2]*1000, type='l', col = 4, lwd =LWD, xlab=" ", xlim=c(min(ENV), max(ENV)),  ylim = YLIM, ylab= YLAB, las =1)
	#	plot(OPT[,1], OPT[,3], 		type='l', col = 4, lwd =LWD, xlab = XLAB, xlim=c(min(ENV), max(ENV)),  ylim=c(0,1), ylab= "N allocation (fraction to Vmax)")
		plot(OPT[,1], OPT[,4], 		type='l', col = 4, lwd =LWD, xlab =" ", 	xlim=c(min(ENV), max(ENV)),  ylim=c(0,20), ylab= "Operating Assimilation (ymol/m2/s)")
	plot(OPT[,1], OPT[,5]/Env$ca, 	type='l', col = 4, lwd =LWD, xlab = XLAB, xlim=c(min(ENV), max(ENV)), ylab= "LAMDA", ylim =c(0,1))


#-------------------------------------------------------------------------------
#Maximum height
  source("params.r")       	#reset parameters
	YLAB = "Max physiological height (m)";  YLIM = c(0,30);
 	#set range to use in plots
	XLAB = "CO2"; ENV =seq(100, 1000, 10);

	#CALCULATE maximum
	OPT<-NULL;
	for(i in 1:length(ENV)){
		Env$ca= ENV[i];
		OP = height.max(RALA, Leaf, Plant, Env)
		OPT<-rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3], OP[9]))
		};	colnames(OPT)=c("ENV", "sala", "leafN", "A", "Hmax")
  #PLOT MAXIMUM
	plot(OPT[,1], OPT[,5], type='l', col = 4, lwd =LWD,	xlab = XLAB, xlim=c(min(ENV), max(ENV)),  ylab= YLAB, las =1)

  #PLOT SA:LA, LEAF N AND A IN PANEL
	par(mfrow=c(1,2))
		plot(OPT[,1], OPT[,5],  type='l', col = 4, lwd =LWD, xlab=" ",    xlim=c(min(ENV), max(ENV)),  ylim = YLIM, ylab= YLAB, las =1);		plot(OPT[,1], OPT[,2],	type='l', col = 4, lwd =LWD, xlab = XLAB, xlim=c(min(ENV), max(ENV)),               ylab= "SALA");

#---------------------------------------------------
#create data for 3D plots

Rala <- seq_log(0.001, 10, 1.1)
Sala <- seq_log(0.0001, 0.005,1.1)
setup.3d(Rala,Sala,"temp")

