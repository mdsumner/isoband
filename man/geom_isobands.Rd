% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw-key.R, R/geom-isobands.R
\docType{data}
\name{draw_key_polypath}
\alias{draw_key_polypath}
\alias{geom_isobands}
\alias{geom_density_bands}
\alias{GeomIsobands}
\title{Draw isoband and isoline contours}
\usage{
geom_isobands(mapping = NULL, data = NULL, stat = "isolevels",
  position = "identity", ..., bins = NULL, binwidth = NULL,
  breaks = NULL, polygon_outline = TRUE, na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE)

geom_density_bands(mapping = NULL, data = NULL, stat = "densitygrid",
  position = "identity", ..., bins = NULL, binwidth = NULL,
  breaks = NULL, polygon_outline = TRUE, na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE)
}
\arguments{
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

\item{mapping}{Set of aesthetic mappings created by \code{\link[=aes]{aes()}} or
\code{\link[=aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{...}{Other arguments passed on to \code{\link[=layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

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

\item{polygon_outlines}{Draw filled polygons with equally colored outlines? The
default is \code{TRUE}, which works well in cases where isobands are drawn without
colored isolines. However, it can create drawing artifacts when used in
combination with alpha transparency.}
}
\description{
Draw isoband and isoline contours
}
\examples{
volcano3d <- reshape2::melt(volcano)
names(volcano3d) <- c("x", "y", "z")

ggplot(volcano3d, aes(x, y, z = z)) +
  geom_isobands(aes(color = stat(zmin)), fill = NA) +
  scale_color_viridis_c() +
  coord_cartesian(expand = FALSE) +
  theme_bw()

ggplot(volcano3d, aes(x, y, z = z)) +
  geom_isobands(aes(fill = stat(zmin)), color = NA) +
  scale_fill_viridis_c(guide = "legend") +
  coord_cartesian(expand = FALSE) +
  theme_bw()

# set polygon_outline = FALSE when drawing filled polygons
# with alpha transparency
ggplot(volcano3d, aes(x, y, z = z)) +
  geom_isobands(
    aes(fill = stat(zmin)), color = NA,
    alpha = 0.5, polygon_outline = FALSE
  ) +
  scale_fill_viridis_c(guide = "legend") +
  coord_cartesian(expand = FALSE) +
  theme_bw()

ggplot(faithful, aes(eruptions, waiting)) +
  geom_density_bands(color = "gray40", alpha = 0.5, size = 0.2) +
  geom_point(size = 0.3) +
  scale_fill_viridis_c(guide = "legend")
}
\keyword{datasets}
