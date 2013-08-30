
figure2 <- function(SALA.vec = seq_log(1e-08, 0.005, 1.01), cex.cost = 1.5, CEX.L = 0.9, 
    CEX.A = 1.2, RALA = 3) {
    
    par(mfrow = c(2, 3), omi = c(0.2, 0.2, 0.2, 0.2), mai = c(0.5, 0.4, 0.5, 0.1), 
        pty = "s")
    # top row a-c - Resposne to VPD
    pars <- load_default_parameters()
    ENV <- seq_log(0.1, 3, 1.5)  #VPD
    
    # plot a - Cost benefit curves over VPD, low soil to root resistance
    pars$Env$dw = 20 * Ksoil(pars$Env) * RALA
    # set dw so that root resistenace = 20
    fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), 
        max(SALA_axis)), SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L)
    
    for (i in 1:length(ENV)) {
        pars$Env$vpd = ENV[i]
        
        CB = cost_ben(SALA.vec, RALA, pars$Leaf, pars$Plant, pars$Env)
        if (i == 1) {
            # only one cost line because all the same
            points(CB[1, ] * 10000, CB[7, ] * 0.001, col = 1, type = "l", lty = "dashed", 
                cex = cex.cost)
        }
        points(CB[1, ] * 10000, CB[6, ] * 0.001, col = 1, type = "l")  # Revenue
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        points(OP[1, ] * 10000, OP[6, ] * 0.001, col = 1, cex = 1, pch = 19)  # Optimum
    }
    
    title("a)", adj = 0, cex.main = 2)
    
    # set dw so that root resistenace = 120
    pars$Env$dw = 1200 * Ksoil(pars$Env) * RALA
    
    fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), 
        max(SALA_axis)), SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L)
    
    for (i in 1:length(ENV)) {
        pars$Env$vpd = ENV[i]
        
        CB = cost_ben(SALA.vec, RALA, pars$Leaf, pars$Plant, pars$Env)
        if (i == 1) {
            # only one cost line because all the same
            points(CB[1, ] * 10000, CB[7, ] * 0.001, col = 1, type = "l", lty = "dashed", 
                cex = cex.cost)  # Invest
        }
        points(CB[1, ] * 10000, CB[6, ] * 0.001, col = 1, type = "l")  # Revenue
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        points(OP[1, ] * 10000, OP[6, ] * 0.001, col = 1, cex = 1, pch = 19)
    }  # Optimum
    
    title("b)", adj = 0, cex.main = 2)
    
    # plot c - SALA optimum vs VPD for high and low soil to root resistance
    OPT_low <- NULL
    OPT_high <- NULL
    
    ENV <- seq_log(0.1, 3, 1.1)  #VPD range
    for (i in 1:length(ENV)) {
        pars$Env$vpd = ENV[i]
        
        pars$Env$dw = 20 * Ksoil(pars$Env) * RALA
        # set dw so that root resistenace = 20
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        if (OP[8] > 0) {
            OPT_low <- rbind(OPT_low, c(ENV[i], OP[1], OP[5], OP[3]))
        }
        pars$Env$dw = 1200 * Ksoil(pars$Env) * RALA
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        if (OP[8] > 0) {
            OPT_high <- rbind(OPT_high, c(ENV[i], OP[1], OP[5], OP[3]))
        }
    }
    # PLOT
    fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, c(0, 
        2.5), "VPD (kPa)", seq(0, 2.5, 0.5), seq(0, 2.5, 0.25), CEX.A, CEX.L)
    points(OPT_low[, 1], OPT_low[, 2] * 10000, type = "l")
    points(OPT_high[, 1], OPT_high[, 2] * 10000, type = "l", lty = "dashed")
    title("c)", adj = 0, cex.main = 2)
    
    # top row d-f - Resposne to CO2
    pars <- load_default_parameters()
    ENV <- seq(200, 1000, 200)  #VPD
    
    # plot d - Cost benefit curves at low soil to root resistance
    pars$Env$dw = 20 * Ksoil(pars$Env) * RALA
    # set dw so that root resistenace = 20
    fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), 
        max(SALA_axis)), SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L)
    
    for (i in 1:length(ENV)) {
        pars$Env$ca = ENV[i]
        
        CB = cost_ben(SALA.vec, RALA, pars$Leaf, pars$Plant, pars$Env)
        if (i == 1) {
            # only one cost line because all the same
            points(CB[1, ] * 10000, CB[7, ] * 0.001, col = 1, type = "l", lty = "dashed", 
                cex = cex.cost)  # Invest
        }
        points(CB[1, ] * 10000, CB[6, ] * 0.001, col = 1, type = "l")  # Revenue
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        points(OP[1, ] * 10000, OP[6, ] * 0.001, col = 1, cex = 1, pch = 19)
    }  # Optimum
    title("d)", adj = 0, cex.main = 2)
    
    # set dw so that root resistenace = 1200
    pars$Env$dw = 1200 * Ksoil(pars$Env) * RALA
    
    fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), 
        max(SALA_axis)), SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L)
    
    for (i in 1:length(ENV)) {
        pars$Env$ca = ENV[i]
        
        CB = cost_ben(SALA.vec, RALA, pars$Leaf, pars$Plant, pars$Env)
        if (i == 1) {
            # only one cost line because all the same
            points(CB[1, ] * 10000, CB[7, ] * 0.001, col = 1, type = "l", lty = "dashed", 
                cex = cex.cost)  # Invest
        }
        points(CB[1, ] * 10000, CB[6, ] * 0.001, col = 1, type = "l")  # Revenue
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        points(OP[1, ] * 10000, OP[6, ] * 0.001, col = 1, cex = 1, pch = 19)
    }  # Optimum
    title("e)", adj = 0, cex.main = 2)
    
    # plot c - SALA optimum vs CO2 for high and low soil to root resistance
    OPT_low <- NULL
    OPT_high <- NULL
    
    ENV <- seq(200, 1000, 20)  #VPD range
    for (i in 1:length(ENV)) {
        pars$Env$ca = ENV[i]
        
        pars$Env$dw = 20 * Ksoil(pars$Env) * RALA
        # set dw so that root resistenace = 20
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        if (OP[8] > 0) {
            
            OPT_low <- rbind(OPT_low, c(ENV[i], OP[1], OP[5], OP[3]))
        }
        pars$Env$dw = 1200 * Ksoil(pars$Env) * RALA
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        if (OP[8] > 0) {
            OPT_high <- rbind(OPT_high, c(ENV[i], OP[1], OP[5], OP[3]))
        }
    }
    # PLOT
    fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, c(200, 
        1000), expression(paste("Atmospheric ", CO[2], " (ppm)")), seq(200, 1000, 
        200), seq(200, 1000, 100), CEX.A, CEX.L)
    points(OPT_low[, 1], OPT_low[, 2] * 10000, type = "l", lty = "dashed")
    points(OPT_high[, 1], OPT_high[, 2] * 10000, type = "l")
    title("f)", adj = 0, cex.main = 2)
}

figure3 <- function(SALA.vec = seq_log(1e-08, 0.005, 1.01), cex.cost = 1.5, CEX.L = 1, 
    CEX.A = 0.9, RALA = 3) {
    par(mfrow = c(1, 2), omi = c(0.2, 0.2, 0.2, 0.2), mai = c(0.5, 0.4, 0.5, 0.1), 
        pty = "s")
    pars <- load_default_parameters()
    # plot a - Cost benefit curves over soil to root resistance
    ENV <- seq(0, 10000, 1000)
    
    fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), 
        max(SALA_axis)), SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L)
    
    for (i in 1:length(ENV)) {
        pars$Env$dw = ENV[i] * Ksoil(pars$Env) * RALA
        
        CB = cost_ben(SALA.vec, RALA, pars$Leaf, pars$Plant, pars$Env)
        if (i == 1) {
            # only one cost line because all the same
            points(CB[1, ] * 10000, CB[7, ] * 0.001, col = 1, type = "l", cex = cex.cost, 
                lty = "dashed")
        }
        points(CB[1, ] * 10000, CB[6, ] * 0.001, col = 1, type = "l")  # Revenue
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        points(OP[1, ] * 10000, OP[6, ] * 0.001, col = 1, cex = 1, pch = 19)
    }  # Optimum
    title("a)", adj = 0, cex.main = 1.2)
    
    # plot b - SALA optimum vs soil to root resistance
    OPT <- NULL
    
    ENV <- seq(0, 10000, 100)
    
    for (i in 1:length(ENV)) {
        pars$Env$dw = ENV[i] * Ksoil(pars$Env) * RALA
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    # PLOT
    fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, c(10, 
        0), expression(paste("Soil to Root resistance(", 10^3, " ", MPa, " ", m^2, 
        " ", s, " ", mol^-1, " )")), seq(0, 10, 2), seq(0, 10, 1), CEX.A, CEX.L)
    points(OPT[, 1] * 0.001, OPT[, 2] * 10000, type = "l")
    title("b)", adj = 0, cex.main = 1.2)
    
    
}

figure4 <- function(SALA.vec = seq_log(1e-08, 0.005, 1.01), cex.cost = 1.5, CEX.L = 1, 
    CEX.A = 0.9, RALA = 3) {
    par(mfrow = c(1, 2), omi = c(0.2, 0.2, 0.2, 0.2), mai = c(0.5, 0.4, 0.5, 0.1), 
        pty = "s")
    pars <- load_default_parameters()
    # plot a - Cost benefit curves over soil water pot
    ENV <- seq(pars$Env$psi.soil.sat, -1.5, -0.2)
    fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), 
        max(SALA_axis)), SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L)
    
    for (i in 1:length(ENV)) {
        pars$Env$psi.soil = ENV[i]
        
        CB = cost_ben(SALA.vec, RALA, pars$Leaf, pars$Plant, pars$Env)
        if (i == 1) {
            # only one cost line because all the same
            points(CB[1, ] * 10000, CB[7, ] * 0.001, col = 1, type = "l", cex = cex.cost, 
                lty = "dashed")
        }
        points(CB[1, ] * 10000, CB[6, ] * 0.001, col = 1, type = "l")  # Revenue
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        points(OP[1, ] * 10000, OP[6, ] * 0.001, col = 1, cex = 1, pch = 19)
    }  # Optimum
    title("a)", adj = 0, cex.main = 1.2)
    
    # plot b - SALA optimum vs soil to root resistance
    OPT <- NULL
    
    ENV <- seq(pars$Env$psi.soil.sat, -1.5, -0.02)
    for (i in 1:length(ENV)) {
        pars$Env$psi.soil = ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    # PLOT
    fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, c(-1.5, 
        0), "Soil water Potential (MPa)", seq(0, -1.5, -0.25), seq(0, -1.5, -0.125), 
        CEX.A, CEX.L)
    points(OPT[, 1], OPT[, 2] * 10000, type = "l")
    title("b)", adj = 0, cex.main = 1.2)
    
    
}

figure5 <- function(SALA.vec = seq_log(1e-08, 0.005, 1.01), cex.cost = 1.5, CEX.L = 0.9, 
    CEX.A = 1.2) {
    
    
    par(mfrow = c(1, 3), omi = c(0.2, 0.2, 0.2, 0.2), mai = c(0.5, 0.5, 0, 0), pty = "s")
    
    # axis labels
    Y_ax = c(0.1, 1, 10)
    Y_ax2 = c(seq(0.1, 1, 0.1), seq(1, 10, 1))
    YLIM = c(0.1, 10)
    
    YLAB = expression(paste("Optimal SA:LA (", 10^-4, m^2, m^-2, ") or"))
    
    YLAB.2 = expression(paste("RA:LA (", m^2, m^-2, ") [log scale]"))
    
    
    # Part a - plot in relation to VPD
    pars <- load_default_parameters()  #reset parameters
    ENV <- seq(0.1, 2.9, 0.1)
    
    XLAB = "VPD (kPa)"
    X_ax = seq(0, 2.5, 1)
    X_ax2 = seq(0, 2.5, 0.5)
    XLIM = c(0, 2.5)
    
    
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Env$vpd = ENV[i]
        
        OP = cost_ben.opt2D(pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[9], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "rala", "leafN", "A")
    
    # PLOT SALA OPTIMUM
    plot(1:2, 1:2, type = "n", log = "y", axes = F, ann = F, xlim = XLIM, ylim = YLIM, 
        xaxs = "i", yaxs = "i", las = 1)
    axis(2, at = Y_ax, labels = Y_ax, las = 1, tck = 0.03, cex.axis = CEX.A, adj = 0.5)
    axis(2, at = Y_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    axis(1, at = X_ax, labels = X_ax, las = 1, tck = 0.03, cex.axis = CEX.A)
    axis(1, at = X_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    axis(3, at = X_ax, labels = F, las = 1, tck = 0.03, cex.axis = CEX.A)
    axis(3, at = X_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    axis(4, at = Y_ax, labels = F, las = 1, tck = 0.03, cex.axis = CEX.A, adj = 0.5)
    axis(4, at = Y_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    box()
    
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 5, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB.2, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    points(OPT[, 2] * 10000 ~ OPT[, 1], type = "l")  #PLOT SALA OPTIMUM
    points(OPT[, 3] ~ OPT[, 1], type = "l", lty = "dashed")  #PLOT RALA OPTIMUM
    title("a)", adj = 0, cex.main = 1.5, line = 1)
    
    # ***** Part b - CO2
    pars <- load_default_parameters()  #reset parameters
    ENV <- seq(200, 1000, 25)
    
    XLAB = expression(paste("Atmospheric ", CO[2], " (ppm)"))
    X_ax = seq(200, 1000, 200)
    X_ax2 = seq(200, 1000, 100)
    XLIM = c(min(X_ax), max(X_ax))
    
    
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Env$ca = ENV[i]
        
        OP = cost_ben.opt2D(pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[9], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "rala", "leafN", "A")
    
    # PLOT SALA OPTIMUM
    plot(1:2, 1:2, type = "n", log = "y", axes = F, ann = F, xlim = XLIM, ylim = YLIM, 
        xaxs = "i", yaxs = "i", las = 1)
    axis(2, at = Y_ax, labels = Y_ax, las = 1, tck = 0.03, cex.axis = CEX.A, adj = 0.5)
    axis(2, at = Y_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    axis(1, at = X_ax, labels = X_ax, las = 1, tck = 0.03, cex.axis = CEX.A)
    axis(1, at = X_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    axis(3, at = X_ax, labels = F, las = 1, tck = 0.03, cex.axis = CEX.A)
    axis(3, at = X_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    axis(4, at = Y_ax, labels = F, las = 1, tck = 0.03, cex.axis = CEX.A, adj = 0.5)
    axis(4, at = Y_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    box()
    
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    points(OPT[, 2] * 10000 ~ OPT[, 1], type = "l")  #PLOT SALA OPTIMUM
    points(OPT[, 3] ~ OPT[, 1], type = "l", lty = "dashed")  #PLOT RALA OPTIMUM
    title("b)", adj = 0, cex.main = 1.5, line = 1)
    
    
    # ***** Part c - psi soil
    pars <- load_default_parameters()  #reset parameters
    ENV = -seq(0.1, 3.9, 0.05)
    
    XLAB = "Soil water potential (MPa)"
    X_ax = -seq(0, 2, 0.5)
    X_ax2 = -seq(0, 2, 0.25)
    XLIM = c(min(X_ax), max(X_ax))
    
    
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Env$psi.soil = ENV[i]
        
        OP = cost_ben.opt2D(pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[9], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "rala", "leafN", "A")
    
    # PLOT SALA OPTIMUM
    plot(1:2, 1:2, type = "n", log = "y", axes = F, ann = F, xlim = XLIM, ylim = YLIM, 
        xaxs = "i", yaxs = "i", las = 1)
    axis(2, at = Y_ax, labels = Y_ax, las = 1, tck = 0.03, cex.axis = CEX.A, adj = 0.5)
    axis(2, at = Y_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    axis(1, at = X_ax, labels = X_ax, las = 1, tck = 0.03, cex.axis = CEX.A)
    axis(1, at = X_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    axis(3, at = X_ax, labels = F, las = 1, tck = 0.03, cex.axis = CEX.A)
    axis(3, at = X_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    axis(4, at = Y_ax, labels = F, las = 1, tck = 0.03, cex.axis = CEX.A, adj = 0.5)
    axis(4, at = Y_ax2, labels = F, las = 1, tck = 0.015, cex.axis = CEX.A, adj = 0.5)
    
    box()
    
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    points(OPT[, 2] * 10000 ~ OPT[, 1], type = "l")  #PLOT SALA OPTIMUM
    points(OPT[, 3] ~ OPT[, 1], type = "l", lty = "dashed")  #PLOT RALA OPTIMUM
    title("c)", adj = 0, cex.main = 1.5, line = 1)
    
    
}

figure6 <- function(SALA.vec = seq_log(1e-08, 0.005, 1.01), cex.cost = 1.5, CEX.L = 0.9, 
    CEX.A = 1.2, RALA = 3) {
    
    par(mfrow = c(1, 2), omi = c(0.2, 0.2, 0.2, 0.2), mai = c(0.5, 0.4, 0.5, 0.1), 
        pty = "s")
    pars <- load_default_parameters()
    # plot a - Cost benefit curves over diff heights at current CO2
    ENV = c(seq(0.5, 60, 10))
    fig.setup(c(min(CB_axis), max(CB_axis)), CB_lab, CB_axis, CB_axis2, c(min(SALA_axis), 
        max(SALA_axis)), SALA_lab, SALA_axis, SALA_axis2, CEX.A, CEX.L)
    
    for (i in 1:length(ENV)) {
        pars$Plant$height = ENV[i]
        
        CB = cost_ben(SALA.vec, RALA, pars$Leaf, pars$Plant, pars$Env)
        points(CB[1, ] * 10000, CB[7, ] * 0.001, col = 1, type = "l", lty = "dashed", 
            cex = cex.cost)  # Invest
        points(CB[1, ] * 10000, CB[6, ] * 0.001, col = 1, type = "l")  # Revenue
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        points(OP[1, ] * 10000, OP[6, ] * 0.001, col = 1, cex = 1, pch = 19)
    }  # Optimum
    title("a)", adj = 0, cex.main = 1.2)
    # plot b - SALA optimum vs height for different co2 values
    fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, c(0, 
        120), "Height (m)", seq(0, 120, 20), seq(0, 120, 10), CEX.A, CEX.L)
    
    ENV = c(seq(0.1, 120, 0.5))
    CA = c(seq(200, 1000, 200))
    
    for (j in 1:length(CA)) {
        OPT <- NULL
        
        pars$Env$ca = CA[j]
        
        for (i in 1:length(ENV)) {
            pars$Plant$height = ENV[i]
            
            OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
            
            if (OP[8] > 0) 
                OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
        }
        points(OPT[, 1], OPT[, 2] * 10000, type = "l")
    }
    text(17, 1.48, "200 ppm", cex = 0.8)
    text(17, 0.55, "1000 ppm", cex = 0.8)
    
    title("b)", adj = 0, cex.main = 1.2)
}

figure7 <- function(SALA.vec = seq_log(1e-08, 0.005, 1.01), cex.cost = 1.5, CEX.L = 0.9, 
    CEX.A = 1.2, RALA = 3) {
    par(mfrow = c(3, 2), omi = c(0.2, 0.2, 0.2, 0.2), mai = c(0.5, 0.4, 0.5, 0.1), 
        pty = "s")
    
    # for max height axis
    YLAB.2 = "Max. height (m)"
    
    Y_ax.2 = seq(0, 100, 20)
    Y_ax2.2 = seq(0, 100, 10)
    YLIM.2 = c(0, 100)
    
    
    # First factor - C02
    pars <- load_default_parameters()  #reset parameters
    # set range to use in plots
    ENV = seq(200, 1000, 10)
    
    XLAB = expression(paste("Atmospheric ", CO[2], " (ppm)"))
    X_ax = seq(200, 1000, 200)
    X_ax2 = seq(200, 1000, 100)
    XLIM = c(200, 1000)
    
    
    OPT1 <- NULL
    OPT2 <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Env$ca = ENV[i]
        
        OP = height.max(RALA, pars$Leaf, pars$Plant, pars$Env)
        OPT1 <- rbind(OPT1, c(ENV[i], OP[1], OP[5], OP[3], OP[9]))
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT2 <- rbind(OPT2, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    
    colnames(OPT1) = c("ENV", "sala", "leafN", "A", "Hmax")
    colnames(OPT2) = c("ENV", "sala", "leafN", "A")
    
    
    fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, XLIM, 
        XLAB, X_ax, X_ax2, CEX.A, CEX.L)
    points(OPT2[, 1], OPT2[, 2] * 10000, type = "l")  #plot SALA for otpimising C
    points(OPT1[, 1], OPT1[, 2] * 10000, type = "l", lty = "dashed")  #plot SALA for optimising H
    title("a)", adj = 0, cex.main = 1.5)
    fig.setup(YLIM.2, YLAB.2, Y_ax.2, Y_ax2.2, XLIM, XLAB, X_ax, X_ax2, CEX.A, CEX.L)
    points(OPT1[, 1], OPT1[, 5], type = "l", lty = "dashed")
    title("b)", adj = 0, cex.main = 1.5)
    # plot SALA for optimising H
    print(OPT1)
    
    # VPD
    pars <- load_default_parameters()  #reset parameters
    # set range to use in plots
    ENV <- seq_log(0.1, 3, 1.1)
    
    XLAB = "VPD  (kPa)"
    X_ax = seq(0, 2.5, 0.5)
    X_ax2 = seq(0, 2.5, 0.25)
    XLIM = c(min(X_ax), max(X_ax))
    
    OPT1 <- NULL
    OPT2 <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Env$vpd = ENV[i]
        
        OP = height.max(RALA, pars$Leaf, pars$Plant, pars$Env)
        OPT1 <- rbind(OPT1, c(ENV[i], OP[1], OP[5], OP[3], OP[9]))
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT2 <- rbind(OPT2, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    
    colnames(OPT1) = c("ENV", "sala", "leafN", "A", "Hmax")
    colnames(OPT2) = c("ENV", "sala", "leafN", "A")
    
    
    
    fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, XLIM, 
        XLAB, X_ax, X_ax2, CEX.A, CEX.L)
    points(OPT2[, 1], OPT2[, 2] * 10000, type = "l")  #plot SALA for otpimising C
    points(OPT1[, 1], OPT1[, 2] * 10000, type = "l", lty = "dashed")  #plot SALA for optimising H
    title("c)", adj = 0, cex.main = 1.5)
    
    fig.setup(YLIM.2, YLAB.2, Y_ax.2, Y_ax2.2, XLIM, XLAB, X_ax, X_ax2, CEX.A, CEX.L)
    points(OPT1[, 1], OPT1[, 5], type = "l", lty = "dashed")  #plot SALA for optimising H
    title("d)", adj = 0, cex.main = 1.5)
    
    # SOILS PSI
    pars <- load_default_parameters()  #reset parameters
    # set range to use in plots
    ENV = -seq(0.01, 1.5, 0.1)
    
    XLAB = "Soil Water Potential (MPa)"
    X_ax = seq(0, -1.5, -0.25)
    X_ax2 = seq(0, -1.5, -1.25)
    XLIM = c(min(X_ax), max(X_ax))
    
    OPT1 <- NULL
    OPT2 <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Env$psi.soil = ENV[i]
        
        OP = height.max(RALA, pars$Leaf, pars$Plant, pars$Env)
        OPT1 <- rbind(OPT1, c(ENV[i], OP[1], OP[5], OP[3], OP[9]))
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT2 <- rbind(OPT2, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    
    colnames(OPT1) = c("ENV", "sala", "leafN", "A", "Hmax")
    colnames(OPT2) = c("ENV", "sala", "leafN", "A")
    
    
    
    fig.setup(c(min(OPT_axis), max(OPT_axis)), OPT_lab, OPT_axis, OPT_axis2, XLIM, 
        XLAB, X_ax, X_ax2, CEX.A, CEX.L)
    points(OPT2[, 1], OPT2[, 2] * 10000, type = "l")  #plot SALA for otpimising C
    points(OPT1[, 1], OPT1[, 2] * 10000, type = "l", lty = "dashed")  #plot SALA for optimising H
    title("e)", adj = 0, cex.main = 1.5)
    
    fig.setup(YLIM.2, YLAB.2, Y_ax.2, Y_ax2.2, XLIM, XLAB, X_ax, X_ax2, CEX.A, CEX.L)
    points(OPT1[, 1], OPT1[, 5], type = "l", lty = "dashed")  #plot SALA for optimising H
    title("f)", adj = 0, cex.main = 1.5)
    
}

figure8 <- function(SALA.vec = seq_log(1e-08, 0.005, 1.01), cex.cost = 1.5, CEX.L = 0.9, 
    CEX.A = 1.2, RALA = 3) {
    par(mfrow = c(3, 3), oma = c(5, 5, 10, 5), mai = c(1, 1, 0, 0))
    
    YLIM = c(0, 0.002 * 1000)
    YLAB = expression(paste("Optimal SA:LA (", 10^-4, m^2, m^-2, ")"))
    
    # Y_ax =seq(0, .003*1000, 0.5)
    Y_ax2 = seq(0, 0.003 * 1000, 0.1)
    
    
    # plot 1 LMA
    pars <- load_default_parameters()
    XLAB = "LMA"
    ENV <- seq(100, 1500, 50)
    # X_ax =seq(0.0,3, 0.5)
    X_ax2 = seq(0, 3, 0.1)
    
    
    plot(1:2, 1:2, type = "n", log = "", axes = T, ann = F, xlim = c(min(ENV), max(ENV)), 
        ylim = YLIM, xaxs = "i", yaxs = "i", las = 1)
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Plant$LMA = ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "leafN", "A")
    # PLOT OPTIMUM
    points(OPT[, 1], OPT[, 2] * 10000, type = "l")
    
    # plot 2 LLS
    pars <- load_default_parameters()
    XLAB = "LEAF LIFESPAN"
    ENV <- seq(0.1, 10, 0.1)
    # X_ax =seq(0.0,3, 0.5)
    X_ax2 = seq(0, 3, 0.1)
    
    
    plot(1:2, 1:2, type = "n", log = "", axes = T, ann = F, xlim = c(min(ENV), max(ENV)), 
        ylim = YLIM, xaxs = "i", yaxs = "i", las = 1)
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Plant$lls = ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
        
    }
    colnames(OPT) = c("ENV", "sala", "leafN", "A")
    # PLOT OPTIMUM
    points(OPT[, 1], OPT[, 2] * 10000, type = "l")
    
    # plot 3 Nroot
    pars <- load_default_parameters()
    XLAB = "Root N"
    ENV <- seq_log(1e-04, 0.01, 1.1)
    # X_ax =seq(0.0,3, 0.5)
    X_ax2 = seq(0, 3, 0.1)
    
    
    plot(1:2, 1:2, type = "n", log = "", axes = T, ann = F, xlim = c(min(ENV), max(ENV)), 
        ylim = YLIM, xaxs = "i", yaxs = "i", las = 1)
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Plant$nmass.root = ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "leafN", "A")
    # PLOT OPTIMUM
    points(OPT[, 1], OPT[, 2] * 10000, type = "l")
    
    # plot 4 Wood desnity
    pars <- load_default_parameters()
    XLAB = "Wood density"
    ENV <- seq(2e+05, 1e+06, 1e+05)
    # X_ax =seq(0.0,3, 0.5)
    X_ax2 = seq(0, 3, 0.1)
    
    
    plot(1:2, 1:2, type = "n", log = "", axes = T, ann = F, xlim = c(min(ENV), max(ENV)), 
        ylim = YLIM, xaxs = "i", yaxs = "i", las = 1)
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Plant$den.stem = ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "leafN", "A")
    # PLOT OPTIMUM
    points(OPT[, 1], OPT[, 2] * 10000, type = "l")
    
    # plot 5 Respiration per N
    pars <- load_default_parameters()
    XLAB = "Respiration per N"
    ENV <- seq(0.01, 0.1, 0.01)
    # X_ax =seq(0.0,3, 0.5)
    X_ax2 = seq(0, 3, 0.1)
    
    
    plot(1:2, 1:2, type = "n", log = "", axes = T, ann = F, xlim = c(min(ENV), max(ENV)), 
        ylim = YLIM, xaxs = "i", yaxs = "i", las = 1)
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        RESP.coeff = ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "leafN", "A")
    # PLOT OPTIMUM
    points(OPT[, 1], OPT[, 2] * 10000, type = "l")
    
    # plot 6 Photosynth per per N
    pars <- load_default_parameters()
    XLAB = "Vmax/Jmax per N"
    ENV <- seq(0.4, 2, 0.1)
    # X_ax =seq(0.0,3, 0.5)
    X_ax2 = seq(0, 3, 0.1)
    
    Kj = pars$Leaf$kj
    Kv = pars$Leaf$kv
    
    
    plot(1:2, 1:2, type = "n", log = "", axes = T, ann = F, xlim = c(min(ENV), max(ENV)), 
        ylim = YLIM, xaxs = "i", yaxs = "i", las = 1)
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Leaf$kj = Kj * ENV[i]
        pars$Leaf$kv = Kv * ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "leafN", "A")
    # PLOT OPTIMUM
    points(OPT[, 1], OPT[, 2] * 10000, type = "l")
    
    # plot 7 Ks
    pars <- load_default_parameters()
    XLAB = "Ks"
    ENV <- seq_log(50, 1000, 1.1)
    # X_ax =seq(0.0,3, 0.5)
    X_ax2 = seq(0, 3, 0.1)
    
    
    plot(1:2, 1:2, type = "n", log = "", axes = T, ann = F, xlim = c(min(ENV), max(ENV)), 
        ylim = YLIM, xaxs = "i", yaxs = "i", las = 1)
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Plant$K.stem = ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "leafN", "A")
    # PLOT OPTIMUM
    points(OPT[, 1], OPT[, 2] * 10000, type = "l")
    
    # plot 8 Psi crit
    pars <- load_default_parameters()
    XLAB = "Psi crit"
    ENV <- seq(2.5, 10, 0.1)
    # X_ax =seq(0.0,3, 0.5)
    X_ax2 = seq(0, 3, 0.1)
    
    
    plot(1:2, 1:2, type = "n", log = "", axes = T, ann = F, xlim = c(-min(ENV), -max(ENV)), 
        ylim = YLIM, xaxs = "i", yaxs = "i", las = 1)
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Plant$psi.crit = -ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "leafN", "A")
    # PLOT OPTIMUM
    points(-OPT[, 1], OPT[, 2] * 10000, type = "l")
    
    # plot 9 Narea
    pars <- load_default_parameters()
    XLAB = "Narea"
    ENV <- seq(0.25, 8, 0.25)
    # X_ax =seq(0.0,3, 0.5)
    X_ax2 = seq(0, 3, 0.1)
    
    
    plot(1:2, 1:2, type = "n", log = "", axes = T, ann = F, xlim = c(min(ENV), max(ENV)), 
        ylim = YLIM, xaxs = "i", yaxs = "i", las = 1)
    mtext(XLAB, side = 1, line = 3, outer = F, at = NA, cex = CEX.L)
    mtext(YLAB, side = 2, line = 3, outer = F, at = NA, cex = CEX.L)
    # CALCULATE OPTIMUM
    OPT <- NULL
    
    for (i in 1:length(ENV)) {
        pars$Plant$narea.lf = ENV[i]
        
        OP = cost_ben.opt(RALA, pars$Leaf, pars$Plant, pars$Env)
        if (OP[8] > 0) 
            OPT <- rbind(OPT, c(ENV[i], OP[1], OP[5], OP[3]))
    }
    colnames(OPT) = c("ENV", "sala", "leafN", "A")
    # PLOT OPTIMUM
    points(OPT[, 1], OPT[, 2] * 10000, type = "l")
}

 
