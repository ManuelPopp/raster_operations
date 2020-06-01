#' Compare neighbouring pixel values of a raster object
#'
#' Compares each pixel of a raster object with its adjacent pixels and replaces differing values by a user defined value.
#' @param x Raster data input.
#' @param mode Number of adjacent pixels to include in the comparison. Either a numeric value (4, 8 or 16) or a character string ("rook", "queen" or "bishop).
#' @param assign Value to assign to a pixel with different value to at least one of its neighbouring pixels.
#' @return A raster with the dimensions of the original raster and either numeric values or NA.
#' @importFrom raster as.matrix
#' @export

cmp.nbs <- function(x, mode = "rook", assign = NA){
  if(is.numeric(mode)){
    dims <- mode}else if(mode == "rook"){
      dims <- 4}else if(mode == "queen"){
        dims <- 8}else if(mode == "bishop"){
          dims <- 16}
  y <- raster::as.matrix(x)
  y0 <- rbind(rep(NA, ncol(y)), y[-nrow(y),]) # top
  y1 <- rbind(y[-1,], rep(NA, ncol(y))) # bottom
  y2 <- cbind(y[,-1], rep(NA, nrow(y))) # left
  y3 <- cbind(rep(NA, nrow(y)), y[,-ncol(y)]) # right
  if(dims == 4){
    x[which(y != y0 | y != y1 | y != y2 | y != y3)] <- assign
    return(x)
  }
  if(dims == 8){
    y4 <- rbind(rep(NA, ncol(y2)), y2[-nrow(y2),]) # topleft
    y5 <- rbind(rep(NA, ncol(y3)), y3[-nrow(y3),]) # topright
    y6 <- rbind(y2[-1,], rep(NA, ncol(y2))) # bottomleft
    y7 <- cbind(y3[-1,], rep(NA, nrow(y3))) # bottomright
    x[which(y != y0 | y != y1 | y != y2 | y != y3 | y != y3 | y != y5 | y != y6 | y != y7)] <- assign
    return(x)
  }
}
