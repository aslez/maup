#' Rezone existing data
#'
#' \code{rezone} randomly rezones an existing zoning system, allowing for
#' user-defined constraints.  Zones can be output in either raster or vector
#' format.
#'
#' @param x an object of class \code{Raster}.
#' @param rz_df a three-column data frame.  The firs two columns are used to
#' assign zones to regions by passing values to \code{Reclassify}.  The third
#' column indicates the number of zones to generate in each rezoning region.
#' @param ras logical indicating whether output should be in raster or vector
#' format.  The default is \code{FALSE}.
#'
#' @return \code{rezone} returns either a \code{Raster} or
#' \code{SpatialPolygons} class object representing a new set of randomly
#' generated zones reflecting the constraints placed on the rezoning process.
#'
#' @examples r <- algo3(x = c(20, 20), m = 5, ras = TRUE)
#' rz_vals <- data.frame(1:6, 1:6, c(2, 1, 1, 1, 1, 1))
#' rezone(x = r, rz_df = rz_vals, ras = TRUE)

rezone <- function(x, rz_df, ras = FALSE){
  rzr <- raster::reclassify(x, rz_df[, 1:2])
  r_lst <- lapply(unique(raster::getValues(rzr)), function(z) {
    raster::mask(rzr, rzr == z, maskvalue = 0, updatevalue = NA)
  })
  r_lst_ud <- lapply(r_lst, function(z) {
    z[!is.na(z)] <- 0
    z
  })
  m_df <- unique(rz_df[, 2:3])[unique(raster::getValues(rzr)), ]
  rzz_lst <- lapply(seq_along(r_lst_ud), function(z) {
    algo3(r_lst_ud[[z]], m = m_df[z, 2], ras = TRUE)
  })
  rzz_lst_ud <- lapply(seq_along(rzz_lst), function(z) {
    rzz_lst[[z]][rzz_lst[[z]] == 0] <- NA
    10^nchar(ncell(rzz_lst[[z]])) * m_df[z, 1] + rzz_lst[[z]]
  })
  rzz <- overlay(stack(rzz_lst_ud), fun = function(x) sum(x, na.rm = TRUE))
  result <- rzz
  if(!ras) {
    sp_shp <- raster::rasterToPolygons(rzz)
    result <- rgeos::gUnionCascaded(sp_shp, id = sp_shp@data$layer)
  }
  result
}
