#' Generate autocorrelated data.
#'
#' \code{algo9} generates a valued raster with a specified degree of
#' autocorrelation.
#'
#' @param x an object of class \code{Raster} or a numeric vector depicting the
#' dimensions of a new raster.
#' @param mori a numeric scalar indicating the desired degree of
#' autocorrelation.  Autocorrelation is measured using Moran's I.
#' @param print_out logical indicating whether or not to print the iteration
#' number.  The default is \code{TRUE}.
#'
#' @details Implements the algorithm outlined by Goodchild (1980).
#'
#' @return \code{algo9} returns a \code{Raster} class object representing
#' a randomly generated surface with a specificed degree of autocorrelation.
#'
#' @references Goodchild, Michael.  1980.  "Algorithm 9: Simulation of
#' autocorrelation for aggregate data"  Environment and Planning A 12:
#' 1073-1081.
#'
#' @examples algo9(x = c(50, 50), mori = .3)

algo9 <- function(x, mori, old_r, tol = 1 / 1e2, maxIter = 1e4,
                  print_out = TRUE) {
  if (class(x) == 'numeric') {
    rmat <- matrix(rnorm(x[1] * x[2]), nrow = x[1], ncol = x[2])
    old_r <- raster::raster(rmat)
  }
  else old_r <- x
  iter <- 1
  n <- raster::ncell(old_r)
  if (any(is.na(raster::values(old_r)))) old_r[] <- runif(n,-2,2)
  old.moran <- raster::Moran(old_r)
  if (old.moran > mori - tol) stop('Bad start.  Try again.')
  while (abs(mori - old.moran) > tol & iter <= maxIter) {
    swap <- sample(n, 2)
    new.r <- old_r
    new.r[rev(swap)] <- old_r[swap]
    new.moran <- raster::Moran(new.r)
    if(new.moran > old.moran & new.moran < mori) {
      old.moran <- new.moran
      old_r <- new.r
    }
    if (print_out){
      cat(paste0('Iteration ', iter, ': ', 'I = ', round(old.moran, 4), '\n'))
    }
    iter <- iter + 1
  }
  old_r
}
