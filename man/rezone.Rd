% Generated by roxygen2 (4.0.2): do not edit by hand
\name{rezone}
\alias{rezone}
\title{Rezone existing data}
\usage{
rezone(x, rz_df, ras = FALSE)
}
\arguments{
\item{x}{an object of class \code{Raster}.}

\item{rz_df}{a three-column data frame.  The firs two columns are used to
assign zones to regions by passing values to \code{Reclassify}.  The third
column indicates the number of zones to generate in each rezoning region.}

\item{ras}{logical indicating whether output should be in raster or vector
format.  The default is \code{FALSE}.}
}
\value{
\code{rezone} returns either a \code{Raster} or
\code{SpatialPolygons} class object representing a new set of randomly
generated zones reflecting the constraints placed on the rezoning process.
}
\description{
\code{rezone} randomly rezones an existing zoning system, allowing for
user-defined constraints.  Zones can be output in either raster or vector
format.
}
\examples{
r <- algo3(x = c(20, 20), m = 5, ras = TRUE)
rz_vals <- data.frame(1:6, 1:6, c(2, 1, 1, 1, 1, 1))
rezone(x = r, rz_df = rz_vals, ras = TRUE)
}

