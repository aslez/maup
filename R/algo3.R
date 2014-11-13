#' Generate random zones.
#'
#' \code{algo3} generates a random zoning system.  Zones can be output in either
#' raster or vector format.
#'
#' @param x an object of class \code{Raster} or a numeric vector depicting the
#' dimensions of a new raster.
#' @param m a numeric scalar indicating the number of zones to generate.
#' @param ras logical indicating whether output should be in raster or vector
#' format.  The default is \code{FALSE}.
#'
#' @details Implements the algorithm outlined by Openshaw (1977).
#'
#' @return \code{algo3} returns either a \code{Raster} or
#' \code{SpatialPolygons} class object representing a set of randomly generated
#' zones.
#'
#' @references Openshaw, Stan.  1977.  "Algorithm 3: a procedure to
#' generate pseudo-random aggregations of N zones into M regions, where M is
#' less than N."  Environment and Planning A 9: 1423-1428.
#'
#' @examples algo3(x = c(50, 50))

algo3 <- function(x, m, ras = FALSE){
  if (class(x) == 'numeric') {
    r <- raster(matrix(0, nrow = x[1], ncol = x[2]))
  }
  else r <- x
  n <- ncell(r)
  adj <- adjacent(r, 1:n, 4)
  w <- matrix(0, n, n)
  for(i in 1:nrow(adj)){
    w[adj[i, 1], adj[i, 2]] <- 1
  }
  alloc <- rep(NA, n)
  alloc[which(values(is.na(r)))] <- 0
  core.zones <- sample(which(is.na(alloc)), m)
  active.zones <- rep(TRUE, m)
  alloc[core.zones] <- which(active.zones)
  nbr.list <- lapply(seq_len(nrow(w)), function(x) w[x, ])
  while(any(active.zones)){
    wc <- sample(which(active.zones), 1)
    if(sum(active.zones) == 1) wc <- which(active.zones)
    zm <- which(alloc == wc)
    nbrs <- unlist(unique(lapply(nbr.list[zm], function(x) which(x > 0))))
    eligible <- intersect(nbrs, which(is.na(alloc)))
    if(length(eligible) == 1) alloc[eligible] <- wc
    if(length(eligible) > 1){
      new.obs <- sample(eligible, 1)
      alloc[new.obs] <- wc
    }
    if(length(eligible) == 0) active.zones[wc] <- FALSE
  }
  r[] <- alloc
  result <- r
  if(!ras) result <- rasterToPolygons(r, dissolve = TRUE)
  result
}

