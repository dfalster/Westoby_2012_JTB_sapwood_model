
seq_log<-function(lower, upper, multiply)
	{i<-1; temp<-NULL;	
	while(i*lower<upper) {temp<-c(temp,lower*i); i=i*multiply}
	return(temp);}
	
fig.setup<-function(ylim, ylab, y_ax, y_ax2, xlim, xlab, x_ax, x_ax2, cex.A, cex.L, log=""){	
		plot(1:2, 1:2, type="n",log=log, axes=F,ann=F, xlim = xlim, ylim=ylim, xaxs="i", yaxs="i", las=1)
		
	axis(2, at=y_ax, labels=y_ax, las=1, tck=0.030, cex.axis=cex.A, adj = 0.5); axis(4, at=y_ax, labels=F,  tck=0.03)
    axis(2, at=y_ax2, labels=F, las=1, tck=0.015, cex.axis=cex.A, adj = 0.5); axis(4, at=y_ax2, labels=F,  tck=0.015)
    axis(1, at=x_ax, labels=x_ax, las=1, tck=0.03, cex.axis=cex.A); axis(3, at=x_ax, labels=F, las=1, tck=0.03)
    axis(1, at=x_ax2, labels=F, las=1, tck=0.015, cex.axis=cex.A, adj = 0.5); axis(3, at=x_ax2, labels=F,  tck=0.015)
    box()  
    mtext(xlab, side = 1, line = 3, outer = F, at= NA, cex=cex.L) 
    mtext(ylab, side = 2, line = 3, outer = F, at= NA, cex=cex.L)  
	}

#predict leaf lifespan from LMA 
pred.LL = function(LMA){ return(10^(-1.376)*LMA^1.147/12)}   #in years

#predict leaf N content from LMA, 
pred.nmass = function(LMA){ return(10^(1.2838)*LMA^(-0.51958)/100)}  #as fraction0-1
  	
#CO2 (Rubsico) limited photosythesis
Av=function(n.area, n.alloc, ci, temp, leaf){  
		Vmax =  n.area*n.alloc*leaf$kv;
		return((Vmax*(ci-leaf$gamma))/(ci+leaf$km)- Rd(n.area, temp))}

#Electron transport (light) limited photosythesis
Aj=function(n.area, n.alloc, ci, q, temp, leaf){
		Jmax =  n.area*(1-n.alloc)*leaf$kj;
		#dependency on light level
		#J = (leaf.alpha*q + Jmax - (leaf.alpha^2*q^2+Jmax^2+ leaf.alpha*q*Jmax*(4-2*leaf.theta))^0.5)/(2*leaf.theta);
		return (Jmax*(ci-leaf$gamma)/(4*(ci+2*leaf$gamma))- Rd(n.area, temp))}
		
#returns minimum of Rubsico and electron tranpsort limited photosyntheis
Amin =function(n.area, n.alloc, ci, q, temp, leaf){
  return(pmin(Aj(n.area, n.alloc,ci, q, temp, leaf), Av(n.area, n.alloc,ci, temp, leaf)));}

#leaf dark respiration as function leaf N and temperature
Rd<-function(n.area, temp)
	{return(n.area*Resp_const(temp));}

#Respiration function from Sitch et al 2003
#Use mean daily temp, const = 0.066 gC gN d-1 at 10∞C************/
Resp_const <-function(temp){		
	resp = RESP.coeff/       #gC /gN / day
        (24*3600)* #convert to per second rate
        (10^6)/12* 	 #convert to umol C02
        exp(308.56*(1/56.02-1/(temp+46.02))); #temperature adjustment     
	return (resp);}              #umol C02 /gN/ s
	
#Create a plot of A-Ci curves showing Aj and Av
Plot.Aci=function(n.area, n.alloc, q, temp, leaf){
	plot(function(x)Aj(x, n.area=n.area, n.alloc=n.alloc, q=q, temp=temp, leaf =leaf), 0, 400, lwd = 2, col = "red",xlab = expression(paste("Ambient ", C0[2], " (ppm)")), xlim=c(0,400), ylim = c(-5,30), ylab= expression(paste("Assimilation rate (", mu, "mol ", m^2, s^-1,")")))
	curve(Av(x, n.area=n.area, n.alloc=n.alloc, temp=temp, leaf =leaf), from =0, to = 400, lwd = 2, col = "blue", add=TRUE)
	curve(0*x,col = "black", add=TRUE, lty =3)}

#Create a plot of A-Ci curves and diffusion limited curve A =g(ca-ci)
Plot.Aci.g=function(n.area, n.alloc, q, gc, ca, temp, leaf){
	Plot.Aci(n.area, n.alloc, q, temp, leaf)
	curve(gc*(ca-x),col = "black", lwd = 2, add=TRUE, xlim=c(0,ca))
	legend(x="topleft",legend=c("Aj","Av", "A=g(ca-ci)"), lty=1, cex=0.8, lwd=c(2,2,1),col=c("red", "blue", "black"))}

Ksoil<-function(env){
	return(env$K.soil.max *	(env$psi.soil.sat/env$psi.soil)^(2+3/env$b))}
#	min(1, (env$psi.soil.sat/env$psi.soil)^(2+3*env$b)))}

#returns stomatal conductance as function of diff parameters
stom.func <-function(sa.la, ra.la,plant, env) {
	r.st <- plant$height/plant$K.stem/sa.la; 
	r.so<- env$dw/Ksoil(env)/ra.la;
	r.rt<- 1/plant$kroot/ra.la;	
	#print(c(r.so,r.rt,r.st))
	g<-1/(1.6*env$vpd/100)*(env$psi.soil - plant$psi.crit - plant$height*998.2*9.8/10^6)/(r.so + r.rt + r.st + plant$r.lf)
	return(g)}

# finds intersection of Av Aj functions for given n alloc
ci.AvAj.intersect=function(n.alloc, leaf){
		return(((1-n.alloc)*leaf$kj*leaf$km-8*n.alloc*leaf$kv*leaf$gamma)/(n.alloc*(4*leaf$kv +leaf$kj)-leaf$kj));}
		
leaf.solve <- function(N, q, gc, ca, temp, leaf){ 
  	ri = 0.066/(24*3600)/(12*10^(-6))*exp(308.56*(1/56.02-1.0/(temp+46.02))); #daytime respiration parameter need temp 
  	kv=leaf$kv; kj=leaf$kj; T=leaf$gamma; Km=leaf$km;  
  	soln<-NULL;
  	for(i in 1:length(gc)){		#do for all values of gc if a vector 
      	n_al<- (4*gc[i]*kj*ca*T-16*gc[i]*kv*T*Km+N*kj^2*T-8*gc[i]*ca*kv*Km+32*gc[i]*kv*T^2+N*kj^2*Km+16*gc[i]*ca*kv*T+4*gc[i]*kj*Km*T			+16*ri*N*kv*T-2*gc[i]*kj*ca*Km-8*ri*N*Km*kv+2*N*kj*Km*kv+4*ri*N*kj*T+8*N*kv*T*kj-2*ri*N*kj*Km-2*gc[i]*kj*Km^2+2*((2*T-Km)^2*(64*gc[i]			^2*kv^2*T^2+8*gc[i]*kv*Km*ri*N*kj+16*gc[i]*ca*kv*ri*N*kj+2*gc[i]*kj^2*Km*ri*N+16*T*gc[i]^2*kv*kj*ca+64*T*gc[i]*kv^2*ri*N+16*T*gc[i]*kv*ri*N*kj
 			+4*T*gc[i]*kv*N*kj^2+ri^2*N^2*kj^2+16*ri^2*N^2*kv^2+gc[i]^2*kj^2*ca^2+gc[i]^2*kj^2*Km^2+16*gc[i]^2*ca^2*kv^2+N^2*kv^2*kj^2+8*gc[i]			^2*ca^2*kv*kj+2*gc[i]^2*kj^2*ca*Km-2*gc[i]*ca*kv*N*kj^2+2*gc[i]*kv*N*kj^2*Km+8*gc[i]^2*ca*kv*kj*Km-8*gc[i]*ca*kv^2*N*kj+64*T*gc[i]^2*ca*kv^2			+16*T*gc[i]^2*kv*kj*Km+32*T*gc[i]*kv^2*N*kj-2*ri*N^2*kv*kj^2+8*ri^2*N^2*kv*kj-8*ri*N^2*kv^2*kj+2*gc[i]*kj^2*ca*ri*N+32*gc[i]			*ca*kv^2*ri*N))^	(1/2))/N/(kj+4*kv)/(12*kv*T+kj*Km+kj*T);
    	ci<- ci.AvAj.intersect(n_al, leaf);
    	A<-  gc[i]*(ca-ci);
    	soln=cbind(soln,rbind(A, ci, n_al));}
  return (soln);}	


#Calculate physiological maximum height
height.max<-function(ra.la, leaf, plant, env){  
	Hmax= uniroot(cost_ben.opt.Cbal.heightwrap, c(1e-2, 150), ra.la=ra.la, leaf=leaf, plant=plant, env=env)$root;
	plant.temp = plant; plant.temp$height=Hmax;
	return(rbind(cost_ben.opt(ra.la, leaf, plant.temp, env), Hmax));}

cost_ben.opt.Cbal.heightwrap <- function(height, ra.la, leaf, plant , env){
	Plant.temp = plant; Plant.temp$height=height;
	return(cost_ben.opt.Cbal(ra.la, leaf, Plant.temp, env))};

cost_ben.opt.Cbal <- function(ra.la, leaf, plant , env){
	optim<-optimise(net.Cbal, c(SALA.min, SALA.max), maximum= T, tol = 0.000001, ra.la=ra.la, leaf=leaf, plant=plant, env=env);
 	return(optim$objective)};

cost_ben.opt <- function(ra.la, leaf, plant , env){
 	optim<-optimise(net.Cbal, c(SALA.min, SALA.max), maximum= T, tol = 0.000001,  ra.la=ra.la, leaf=leaf, plant=plant, env=env);
 	return(cost_ben(optim$maximum, ra.la, leaf, plant, env));}
  
cost_ben.opt2D <- function(leaf, plant, env){
	Rala<-optimise(net.Cbal.2D, c(1e-10, 50), maximum= T, tol = 0.000001, leaf=leaf, plant=plant, env=env);
	Sala<-cost_ben.opt(ra.la=Rala$maximum, leaf=leaf,plant=plant, env=env);
 	return(rbind(Sala, ra.la= Rala$maximum));}
 
#used for optimsation	
net.Cbal.2D<-function(ra.la, leaf, plant, env){
	return(cost_ben.opt.Cbal(ra.la=ra.la, leaf=leaf,plant=plant, env=env));}
 
#used for optimsation	
net.Cbal<-function(sa.la, ra.la, leaf, plant, env){
    return(cost_ben(sa.la,ra.la, leaf, plant, env)[8]);}  

cost_ben <- function(sa.la, ra.la, leaf, plant, env){
	g<-stom.func(sa.la, ra.la,plant, env);    #calculate stomatal conductance as function hydraulic parameters
	leaf.sol = leaf.solve(plant$narea.lf, env$q, g, env$ca, env$temp, leaf); #solve for oeprating A
    Rev <- revenue (leaf.sol[1,], sa.la, ra.la, plant, env);
    Invest<- investment(sa.la, ra.la, plant);
    Net <- Rev - Invest;
    return(rbind(sa.la, g, leaf.sol, Rev, Invest, Net))};

#returns cost of building stem & leaf (construction & resp) as function in gC/m2
investment <-function(sa.la, ra.la, plant) {
	# all costs are per unit area leaf
	leaf.mass <-plant$LMA			      #g/m2
	stem.mass <- sa.la*plant$height  *plant$den.stem        #g/m2  = m2/m2 *m*g/m3  
	root.mass<-  ra.la*plant$root.rad*plant$den.root/2 
	tot.n = plant$narea.lf+stem.mass*plant$nmass.stem+root.mass*plant$nmass.root #g/m2 
	n.cost=0.0; #set to zero during revision for JTB, 2012.02.27    #9.12*tot.n			      #C cost of N 9.12 gC g-1N for fixers from Gutschick 1981
 	return(0.45*(leaf.mass + stem.mass + root.mass) + n.cost)}

#returns lifetime revenue for stem = photsynethesis - respiration
revenue <- function(A, sa.la,ra.la, plant, env){
		leaf.ps = A *
              	(12*60*60)*   		   		#umol/m2/day   = *[(hrs/day)*(mins/hr)*(sec/min)
               	10^(-6)*12*		       		#g/m2/day     = *(mol/umol)*gram C/mol
               	0.5;				      		#discount productivity to account for leaf orientation and leaf ageing
		#Respiration
		stem.n <- sa.la * plant$height * plant$den.stem * plant$nmass.stem;  #gN /m2 leaf 
		stem.resp = stem.n*Resp_const(env$temp)*
                  24*3600*                	#s /day 
                  12/(10^6); 	          	#g C / umol C02
                                          	#= gC/m2 leaf /day
		leaf.night.resp = plant$narea.lf*Resp_const(env$temp)*   #umol C02 /m2/ s
                      12*3600*	            #s /day 
                      12/(10^6); 	          #convert umol to g C
                                              #= gC/m2 leaf /day                                         
        root.n <- ra.la * plant$root.rad * plant$den.root/2 * plant$nmass.root
        root.resp = root.n*Resp_const(env$temp)*
                  24*3600*                #s /day 
                  12/(10^6); 
                                             
		Rev = leaf.ps  - leaf.night.resp - stem.resp -root.resp   #gC/m2/day
		Rev = Rev*(plant$lls*365)	#gC/lls  *[years*(days/year)]
		return(Rev)}
		
#create data for 3D plot in Matlab. Input are vectors of ra.la & sa.la
setup.3d<-function(ra.la,sa.la,name){
	Row=length(ra.la)
	Col=length(sa.la)
	X<-matrix(data=rep(ra.la,Col), nrow=Row, ncol=Col, byrow=F)
	Y<-matrix(data=rep(sa.la,Row), nrow=Row, ncol=Col, byrow=T)
	rev<-matrix(data=NA, nrow=Row, ncol=Col, byrow=T)
	inv<-matrix(data=NA, nrow=Row, ncol=Col, byrow=T)
	net<-matrix(data=NA, nrow=Row, ncol=Col, byrow=T)
	for(i in 1:Row){
		for(j in 1:Col){
			CB =cost_ben(Y[i,j],X[i,j], Leaf, Plant, Env);
			rev[i,j] = CB[6,];
			inv[i,j] = CB[7,];
			net[i,j] = CB[8,];
			}
		}
	write.table(X,file=paste(name,"_X",".txt",sep=""),row.names = FALSE, col.names = FALSE)
	write.table(Y,file=paste(name,"_Y",".txt",sep=""),row.names = FALSE, col.names = FALSE)
	write.table(rev,file=paste(name,"_Rev",".txt",sep=""),row.names = FALSE, col.names = FALSE)
	write.table(inv,file=paste(name,"_Inv",".txt",sep=""),row.names = FALSE, col.names = FALSE)
	write.table(net,file=paste(name,"_Net",".txt",sep=""),row.names = FALSE, col.names = FALSE)
	
	par(mfrow=c(1,3))
	persp(log10(X[,1]), log10(Y[1,]), rev, theta = 30, phi = 30, expand = 0.5, col = "lightblue", xlab= "log RALA", ylab="log SALA", main="Revenue")
	persp(log10(X[,1]), log10(Y[1,]), inv, theta = 30, phi = 30, expand = 0.5, col = "lightblue", xlab= "log RALA", ylab="log SALA", main="Investment")
	persp(log10(X[,1]), log10(Y[1,]), net, theta = 30, phi = 30, expand = 0.5, col = "lightblue", xlab= "log RALA", ylab="log SALA", main="Net Carbon")
	}
   		