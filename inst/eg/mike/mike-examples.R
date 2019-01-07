library(ggplot2)
library(isoband)

x3d <- as.data.frame(r, xy = TRUE)
names(x3d) <- c("x", "y", "z")

ggplot(x3d, aes(x, y, z = z)) +
  #geom_raster(aes(fill = z)) +
  geom_isobands(aes(fill = stat(zmin)), color = NA, alpha = 0.2) +
  scale_fill_viridis_c(guide = "legend") +
  coord_cartesian(expand = FALSE) +
  theme_bw()




# r <- palr::oisst
r <- raster::raster(system.file("nc/reduced.nc", package = "stars"), varname = "sst")
#r <- lazyraster::as_raster(lazyraster::lazyraster(raadtools::topofile("gebco_14")))
#> Loading required namespace: ncdf4
library(raster)
#> Loading required package: sp
z <- t(as.matrix(flip(r[[1]], "y")))
x <- xFromCol(r)
y <- rev(yFromRow(r))
library(isoband)
lo <- c(0, 4, 8, 12, 16, 20, 24, 28)
l <- isolines(x, y, t(z), lo)
b <- isobands(x, y, t(z),  lo, lo + 4)
na_bind <- function(x) {
  x <- tibble::as_tibble(x)
  id <- x$id
  head(do.call(rbind, lapply(split(x, id)[unique(id)], function(a) rbind(a, NA))), -1)
}

lineplot <- function(x) {
  x <- tibble::as_tibble(x)
  lines(na_bind(x))
}
polyplot <- function(x, ...) {
  x <- tibble::as_tibble(x)
  polypath(na_bind(x))
}




image(x, y, z, useRaster = TRUE, asp = 1)
purrr::walk(l, lineplot)



image(x, y, z, useRaster = TRUE, col = viridis::viridis(100))
vps <- gridBase::baseViewports()
grid::pushViewport(vps$inner, vps$figure, vps$plot)

col <- sample(rainbow(length(b)))
for (i in seq_along(b)) {
  xx <- tibble::as_tibble(b[[i]])
  grid::grid.path(xx$x, xx$y, xx$id, gp = grid::gpar(col = NA, fill = col[i]),
                   default.units = "native", rule = "winding")

}

grid::popViewport(3)
purrr::walk(b, polyplot)

