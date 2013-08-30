#Mark Westoby, Dan Falster and Will Cornwell

#clear memory
rm(list=ls(all=TRUE))

#read in leaf functions
source("R/functions-utils.R")    #load functions
source("R/functions-model.R")    #load functions

#--------------------------------------------------
# FIGURES from Westoby et al 2012

#set common axes for cost-benefit and SALA plots
#for cost-benefit curves
CB_lab <- expression(paste("Revenue or cost (kg C"," ", m^-2,")"))
CB_axis <- seq(0, 8, 1)
CB_axis2 <- seq(0, 8, 0.5)
SALA_lab <- expression(paste("SA:LA (", 10^-4, m^2, m^-2,")"))
SALA_axis <- seq(0, .0004*1e4, 1)
SALA_axis2 <- seq(0, .0004*1e4, 0.5)

#for opitmum sala plots
OPT_lab <- expression(paste("Optimal SA:LA (", 10^-4, m^2, m^-2,")"))
OPT_axis <- seq(0, .00025*1e4, 0.5); OPT_axis2=seq(0, .00025*1e4, 0.25)


source("R/figures.r")

to.pdf(figure2(), "output/figure2.pdf", height=8, width=12, pointsize=10)
to.pdf(figure3(), "output/figure3.pdf", height=4, width=8, pointsize=10)
to.pdf(figure4(), "output/figure4.pdf", height=4, width=8, pointsize=10)
to.pdf(figure5(), "output/figure5.pdf", height=4, width=10, pointsize=10)
to.pdf(figure6(), "output/figure6.pdf", height=4, width=8, pointsize=10)
to.pdf(figure7(), "output/figure7.pdf", height=12, width=8, pointsize=10)
to.pdf(figure8(), "output/figure8.pdf", height=8, width=12, pointsize=10)
