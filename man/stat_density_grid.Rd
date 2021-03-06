% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-density-grid.R
\docType{data}
\name{stat_density_grid}
\alias{stat_density_grid}
\alias{stat_densitygrid}
\alias{StatDensitygrid}
\title{Generate 2d kernel density estimates}
\usage{
stat_density_grid(mapping = NULL, data = NULL, geom = "tile",
  position = "identity", ..., n = 100, h = NULL, bins = NULL,
  binwidth = NULL, breaks = NULL, na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[=aes]{aes()}} or
\code{\link[=aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[=ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[=fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data.}

\item{geom}{The geometric object to use display the data}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{...}{Other arguments passed on to \code{\link[=layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{n}{Number of grid points in each direction.}

\item{h}{Bandwidth (vector of length two). If \code{NULL}, estimated
using \code{\link[MASS:bandwidth.nrd]{MASS::bandwidth.nrd()}}.}

\item{bins}{Number of bins for discretization. Has priority
over \code{binwidth}.}

\item{binwidth}{Binwidth used during discretization.}

\item{breaks}{Explicit bin boundaries to use for discretization.
Has priority over both \code{bins} and \code{binwidth}.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[=borders]{borders()}}.}
}
\description{
Generate 2d kernel density estimates
}
\section{Computed variables}{

Same as \code{\link[=stat_isolevels]{stat_isolevels()}}, with the addition of:
\describe{
\item{\code{density}}{the density estimate}
\item{\code{ndensity}}{density estimate scaled to maximum of 1}
\item{\code{z}}{the density estimate, identical to \code{density}}
}
}

\examples{
# default uses `geom_tile()` for drawing
ggplot(faithful, aes(eruptions, waiting)) +
  stat_density_grid(aes(fill = stat(density))) +
  geom_point(size = 0.3, color = "white")

# discretized colors
ggplot(faithful, aes(eruptions, waiting)) +
  stat_density_grid(aes(fill = stat(zmin))) +
  geom_point(size = 0.3, color = "white")

# points
ggplot(faithful, aes(eruptions, waiting)) +
  stat_density_grid(
    aes(color = stat(density), size = stat(density)),
    geom = "point", n = 20, stroke = 0
  ) +
  geom_point(size = 0.75, shape = 21, fill = "white", color = "black")

# contour bands
ggplot(faithful, aes(eruptions, waiting)) +
  stat_density_grid(
    aes(fill = stat(zmin)), geom = "isobands",
    color = "gray70"
  ) +
  geom_point(size = 0.3, color = "white")
}
\keyword{datasets}
