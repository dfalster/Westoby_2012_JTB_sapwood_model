
#leaf biochemistry - fixed parameters
Leaf = list(km= 223 *(1+210000/216000), 	#umol / mol = Kc * (1+oi/Ko)
              gamma=32.6, 		              #umol / mol (= ppm)
              kv=2.0*1.0*33.6,   		# rubsico rate coefficient ymol/gN/s
              kj=2.0*1.0*44.3,       # electron transport rate coefficient ymol/gN/s
              alpha = 0.06,		  #quantum yield
              theta = 0.8);     	#curvature
        
#------------------------------------------------------------------
#Environmental Parameters	 
Env = list(
	#Climate variables
	vpd= 0.6,				#mol/mol
	ca= 380,				#umol/mol
#	q=  1000,				#PPFD
	psi.soil = -0.5,		#Mpa
	temp= 10, #not actually used anywhere at present
	
	#Soil params - Details on soil hydraulics from Sperry et al 1998. These params for silt loam
	b=4.38, 						#soil texture parameter affects the conductance at a given water pot
	psi.soil.sat= -0.5*0.1^-0.5 /1000,	#Soil water potential under saturating condns, 0.1= GWD = geometric mean particle dimaeter
	K.soil.max=12.1,				#maximum conductance of soil
	dw=0.005); 						#Av pathlength for water traveling from soil to root via root induced water potential gradient

#------------------------------------------------------------------
#Fixed plant parameters
	RESP.coeff=0.066; 		#gC /gN / day
	
Plant =list(	
	#universal
	r.lf=	30,				#resistance in petiole/leaf
	
	#Leaf parameters
	psi.crit= -4,		#MPA
	LMA=400, #97,					#g/m2;
	lls=pred.LL(400),		#yrs
	narea.lf=1.75,			#g/m2
	
	#stem
	K.stem=75,        		#mol m /mPA/m2/s   (note this is per m2 sapwood)
	den.stem=0.6e6,			#g/m3 
	height = 15,			#m; equivilent to pathlength of xylem right now
	nmass.stem=0.001,		#g/g	
	
	#root
	kroot = 1e8,			#conductance from root surface to stem base -high so that is negligible
	root.rad=0.001,			#m - root radius
	nmass.root=0.005,		#n conc in roots
	den.root=0.4e6); 		#g/m3  specific density root tissue

#Variable
    N.alloc = 0.5
  	RALA <- 3 				#m2/m2 (range 0.2-40)
	SALA <- 0.0005
	SALA.min <- 0.00000001
	SALA.max <- 0.005
	SALA.vec=seq_log(SALA.min, SALA.max, 1.01)

