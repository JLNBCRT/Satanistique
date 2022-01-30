# Add small dots on basal plane and on the depth plane
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