scatter3D_fancy <- function(x, y, z, posx, posy, posz,..., colvar = z) {
  require(plot3D)
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y, z = rep(posz, length(z)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 3, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x = rep(posx, length(x)), y, z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 3, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x, y = rep(posy, length(y)), z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 3, add = TRUE, colkey = FALSE)
  }
  scatter3D(x, y, z, ..., colvar = NULL, col = "black", panel.first=panelfirst,
            colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)) 
}

TCL <- function(nsample, E, SD, fun) {
  dist <- switch(fun, norm = rnorm, lnorm = rlnorm, unif = runif, gamma = rgamma, runif)
  if (fun == "norm") {
    tmp   <- dist(nsample, E, SD)    
  }
  if (fun == "lnorm") {
    sigma <- sqrt(log((SD/E)^2+1))
    mu    <- log(E) - sigma^2/2
    tmp   <- dist(nsample, meanlog = mu, sdlog = sigma)    
  }
  if (fun == "unif") {
    max   <- 3*SD^2/E+E
    min   <- 2*E-max
    tmp   <- dist(nsample, min = min, max = max)
  }
  if (fun == "gamma") {
    shape <- E^2/SD^2
    scale <- SD^2/E
    tmp   <- dist(nsample, shape = shape, scale = scale)
  }
  # if (fun == "beta") {
  #   shape1 <- (1-E)*E^2/SD^2-E
  #   shape2 <- shape1*(1-E)/E
  #   tmp   <- dist(nsample, shape1 = shape1, shape2 = shape2)
  # }
  return(mean(tmp))
}

