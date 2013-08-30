

load_default_parameters <- function(){

  #leaf biochemistry - fixed parameters
  Leaf = list(km= 223 *(1+210000/216000), 	#umol / mol = Kc * (1+oi/Ko)
              gamma=32.6, 		              #umol / mol (= ppm)
              kv=2.0*1.0*33.6,   		# rubsico rate coefficient ymol/gN/s
              kj=2.0*1.0*44.3,       # electron transport rate coefficient ymol/gN/s
              alpha = 0.06,		  #quantum yield
              theta = 0.8)     	#curvature

  #Environmental Parameters
  Env = list(
    #Climate variables
    vpd= 0.6,				#mol/mol
    ca= 380,				#umol/mol
    psi.soil = -0.5,		#Mpa
    temp= 10, #not actually used anywhere at present

    #Soil params - Details on soil hydraulics from Sperry et al 1998. These params for silt loam
    b=4.38, 						#soil texture parameter affects the conductance at a given water pot
    psi.soil.sat= -0.5*0.1^-0.5 /1000,	#Soil water potential under saturating condns, 0.1= GWD = geometric mean particle dimaeter
    K.soil.max=12.1,				#maximum conductance of soil
    dw=0.005)						#Av pathlength for water traveling from soil to root via root induced water potential gradient

  Plant =list(

    #Leaf parameters
    r.lf=	30,				#resistance in petiole/leaf
    psi.crit= -4,		#MPA
    LMA=400, #97,					#g/m2

    lls=pred.LL(400),		#yrs
    narea.lf=1.75,			#g/m2

    #stem
    K.stem=75,        		#mol m /mPA/m2/s   (note this is per m2 sapwood)
    den.stem=0.6e6,			#g/m3
    height = 15,			#m  equivilent to pathlength of xylem right now
    nmass.stem=0.001,		#g/g

    #root
    kroot = 1e8,			#conductance from root surface to stem base -high so that is negligible
    root.rad=0.001,			#m - root radius
    nmass.root=0.005,		#n conc in roots
    den.root=0.4e6) 		#g/m3  specific density root tissue
  list(Leaf = Leaf, Plant = Plant, Env = Env)
}


#predict leaf lifespan from LMA in years
pred.LL = function(LMA){
  return(10^(-1.376)*LMA^1.147/12)
}

#predict leaf N content from LMA, #as fraction0-1
pred.nmass = function(LMA){
  return(10^(1.2838)*LMA^(-0.51958)/100)
}

#CO2 (Rubsico) limited photosythesis
Av=function(n.area, n.alloc, ci, temp, leaf){
  Vmax =  n.area*n.alloc*leaf$kv

  return((Vmax*(ci-leaf$gamma))/(ci+leaf$km)- Rd(n.area, temp))
}

#Electron transport (light) limited photosythesis
Aj=function(n.area, n.alloc, ci, q, temp, leaf){
  Jmax =  n.area*(1-n.alloc)*leaf$kj

  return (Jmax*(ci-leaf$gamma)/(4*(ci+2*leaf$gamma))- Rd(n.area, temp))
}

#returns minimum of Rubsico and electron tranpsort limited photosyntheis
Amin =function(n.area, n.alloc, ci, q, temp, leaf){
  return(pmin(Aj(n.area, n.alloc,ci, q, temp, leaf), Av(n.area, n.alloc,ci, temp, leaf)))
}

#leaf dark respiration as function leaf N and temperature
Rd<-function(n.area, temp){
  return(n.area*Resp_const(temp))
}

#Respiration function from Sitch et al 2003
#Use mean daily temp, const = 0.066 gC gN d-1 at 10∞C************/
Resp_const <-function(temp, RESP.coeff=0.066){
  resp = RESP.coeff/       #gC /gN / day
    (24*3600)* #convert to per second rate
    (10^6)/12* 	 #convert to umol C02
    exp(308.56*(1/56.02-1/(temp+46.02)))
  #temperature adjustment
  return (resp)
  #umol C02 /gN/ s
  }

#Create a plot of A-Ci curves showing Aj and Av
Plot.Aci=function(n.area, n.alloc, q, temp, leaf){
  plot(function(x)Aj(x, n.area=n.area, n.alloc=n.alloc, q=q, temp=temp, leaf =leaf), 0, 400, lwd = 2, col = "red",xlab = expression(paste("Ambient ", C0[2], " (ppm)")), xlim=c(0,400), ylim = c(-5,30), ylab= expression(paste("Assimilation rate (", mu, "mol ", m^2, s^-1,")")))
  curve(Av(x, n.area=n.area, n.alloc=n.alloc, temp=temp, leaf =leaf), from =0, to = 400, lwd = 2, col = "blue", add=TRUE)
  curve(0*x,col = "black", add=TRUE, lty =3)
}

#Create a plot of A-Ci curves and diffusion limited curve A =g(ca-ci)
Plot.Aci.g=function(n.area, n.alloc, q, gc, ca, temp, leaf){
  Plot.Aci(n.area, n.alloc, q, temp, leaf)
  curve(gc*(ca-x),col = "black", lwd = 2, add=TRUE, xlim=c(0,ca))
  legend(x="topleft",legend=c("Aj","Av", "A=g(ca-ci)"), lty=1, cex=0.8, lwd=c(2,2,1),col=c("red", "blue", "black"))
}

Ksoil<-function(env){
  return(env$K.soil.max *	(env$psi.soil.sat/env$psi.soil)^(2+3/env$b))
}

#returns stomatal conductance as function of diff parameters
stom.func <-function(sa.la, ra.la,plant, env) {
  r.st <- plant$height/plant$K.stem/sa.la

  r.so<- env$dw/Ksoil(env)/ra.la

  r.rt<- 1/plant$kroot/ra.la

  #print(c(r.so,r.rt,r.st))
  g<-1/(1.6*env$vpd/100)*(env$psi.soil - plant$psi.crit - plant$height*998.2*9.8/10^6)/(r.so + r.rt + r.st + plant$r.lf)
  return(g)
}

# finds intersection of Av Aj functions for given n alloc
ci.AvAj.intersect=function(n.alloc, leaf){
  return(((1-n.alloc)*leaf$kj*leaf$km-8*n.alloc*leaf$kv*leaf$gamma)/(n.alloc*(4*leaf$kv +leaf$kj)-leaf$kj))
}

leaf.solve <- function(N, q, gc, ca, temp, leaf){
  ri = 0.066/(24*3600)/(12*10^(-6))*exp(308.56*(1/56.02-1.0/(temp+46.02)))
  #daytime respiration parameter need temp
  kv=leaf$kv
  kj=leaf$kj
  T=leaf$gamma
  Km=leaf$km
  soln<-NULL

  for(i in 1:length(gc)){		#do for all values of gc if a vector
    n_al<- (4*gc[i]*kj*ca*T-16*gc[i]*kv*T*Km+N*kj^2*T-8*gc[i]*ca*kv*Km+32*gc[i]*kv*T^2+N*kj^2*Km+16*gc[i]*ca*kv*T+4*gc[i]*kj*Km*T			+16*ri*N*kv*T-2*gc[i]*kj*ca*Km-8*ri*N*Km*kv+2*N*kj*Km*kv+4*ri*N*kj*T+8*N*kv*T*kj-2*ri*N*kj*Km-2*gc[i]*kj*Km^2+2*((2*T-Km)^2*(64*gc[i]			^2*kv^2*T^2+8*gc[i]*kv*Km*ri*N*kj+16*gc[i]*ca*kv*ri*N*kj+2*gc[i]*kj^2*Km*ri*N+16*T*gc[i]^2*kv*kj*ca+64*T*gc[i]*kv^2*ri*N+16*T*gc[i]*kv*ri*N*kj
                                                                                                                                                                                                                                                                 +4*T*gc[i]*kv*N*kj^2+ri^2*N^2*kj^2+16*ri^2*N^2*kv^2+gc[i]^2*kj^2*ca^2+gc[i]^2*kj^2*Km^2+16*gc[i]^2*ca^2*kv^2+N^2*kv^2*kj^2+8*gc[i]			^2*ca^2*kv*kj+2*gc[i]^2*kj^2*ca*Km-2*gc[i]*ca*kv*N*kj^2+2*gc[i]*kv*N*kj^2*Km+8*gc[i]^2*ca*kv*kj*Km-8*gc[i]*ca*kv^2*N*kj+64*T*gc[i]^2*ca*kv^2			+16*T*gc[i]^2*kv*kj*Km+32*T*gc[i]*kv^2*N*kj-2*ri*N^2*kv*kj^2+8*ri^2*N^2*kv*kj-8*ri*N^2*kv^2*kj+2*gc[i]*kj^2*ca*ri*N+32*gc[i]			*ca*kv^2*ri*N))^	(1/2))/N/(kj+4*kv)/(12*kv*T+kj*Km+kj*T)

    ci<- ci.AvAj.intersect(n_al, leaf)

    A<-  gc[i]*(ca-ci)

    soln=cbind(soln,rbind(A, ci, n_al))
  }
  return (soln)
}


#Calculate physiological maximum height
height.max<-function(ra.la, leaf, plant, env){
  Hmax= uniroot(cost_ben.opt.Cbal.heightwrap, c(1e-2, 150), ra.la=ra.la, leaf=leaf, plant=plant, env=env)$root

  plant.temp = plant

  plant.temp$height=Hmax

  return(rbind(cost_ben.opt(ra.la, leaf, plant.temp, env), Hmax))
}

cost_ben.opt.Cbal.heightwrap <- function(height, ra.la, leaf, plant , env){
  Plant.temp = plant
  Plant.temp$height=height

  return(cost_ben.opt.Cbal(ra.la, leaf, Plant.temp, env))
}

cost_ben.opt.Cbal <- function(ra.la, leaf, plant , env, SALA.min = 1E-8, SALA.max = 1E-2){
  optim<-optimise(net.Cbal, c(SALA.min, SALA.max), maximum= T, tol = 0.000001, ra.la=ra.la, leaf=leaf, plant=plant, env=env)

  return(optim$objective)
}

cost_ben.opt <- function(ra.la, leaf, plant , env, SALA.min = 1E-8, SALA.max = 1E-2){
  optim<-optimise(net.Cbal, c(SALA.min, SALA.max), maximum= T, tol = 0.000001,  ra.la=ra.la, leaf=leaf, plant=plant, env=env)

  return(cost_ben(optim$maximum, ra.la, leaf, plant, env))

}

cost_ben.opt2D <- function(leaf, plant, env){
  Rala<-optimise(net.Cbal.2D, c(1e-10, 50), maximum= T, tol = 0.000001, leaf=leaf, plant=plant, env=env)

  Sala<-cost_ben.opt(ra.la=Rala$maximum, leaf=leaf,plant=plant, env=env)

  return(rbind(Sala, ra.la= Rala$maximum))

}

#used for optimsation
net.Cbal.2D<-function(ra.la, leaf, plant, env){
  return(cost_ben.opt.Cbal(ra.la=ra.la, leaf=leaf,plant=plant, env=env))

}

#used for optimsation
net.Cbal<-function(sa.la, ra.la, leaf, plant, env){
  return(cost_ben(sa.la,ra.la, leaf, plant, env)[8])

}

cost_ben <- function(sa.la, ra.la, leaf, plant, env){
  g<-stom.func(sa.la, ra.la,plant, env)
     #calculate stomatal conductance as function hydraulic parameters
  leaf.sol = leaf.solve(plant$narea.lf, env$q, g, env$ca, env$temp, leaf)
  #solve for oeprating A
  Rev <- revenue (leaf.sol[1,], sa.la, ra.la, plant, env)

  Invest<- investment(sa.la, ra.la, plant)

  Net <- Rev - Invest

  return(rbind(sa.la, g, leaf.sol, Rev, Invest, Net))
}

#returns cost of building stem & leaf (construction & resp) as function in gC/m2
investment <-function(sa.la, ra.la, plant) {
  # all costs are per unit area leaf
  leaf.mass <-plant$LMA			      #g/m2
  stem.mass <- sa.la*plant$height  *plant$den.stem        #g/m2  = m2/m2 *m*g/m3
  root.mass<-  ra.la*plant$root.rad*plant$den.root/2
  tot.n = plant$narea.lf+stem.mass*plant$nmass.stem+root.mass*plant$nmass.root #g/m2
  n.cost=0.0
  #set to zero during revision for JTB, 2012.02.27    #9.12*tot.n			      #C cost of N 9.12 gC g-1N for fixers from Gutschick 1981
  return(0.45*(leaf.mass + stem.mass + root.mass) + n.cost)
}

#returns lifetime revenue for stem = photsynethesis - respiration
revenue <- function(A, sa.la,ra.la, plant, env){
  leaf.ps = A *
    (12*60*60)*   		   		#umol/m2/day   = *[(hrs/day)*(mins/hr)*(sec/min)
    10^(-6)*12*		       		#g/m2/day     = *(mol/umol)*gram C/mol
    0.5
 				      		#discount productivity to account for leaf orientation and leaf ageing
  #Respiration
  stem.n <- sa.la * plant$height * plant$den.stem * plant$nmass.stem
   #gN /m2 leaf
  stem.resp = stem.n*Resp_const(env$temp)*
    24*3600*                	#s /day
    12/(10^6)
  	          	#g C / umol C02
  #= gC/m2 leaf /day
  leaf.night.resp = plant$narea.lf*Resp_const(env$temp)*   #umol C02 /m2/ s
    12*3600*	            #s /day
    12/(10^6)
  	          #convert umol to g C
  #= gC/m2 leaf /day
  root.n <- ra.la * plant$root.rad * plant$den.root/2 * plant$nmass.root
  root.resp = root.n*Resp_const(env$temp)*
    24*3600*                #s /day
    12/(10^6)


  Rev = leaf.ps  - leaf.night.resp - stem.resp -root.resp   #gC/m2/day
  Rev = Rev*(plant$lls*365)	#gC/lls  *[years*(days/year)]
  return(Rev)
}
