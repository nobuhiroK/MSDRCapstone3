#' geom_timeline
#' @import ggplot2
#'
#' @param mapping aethetics
#' @param data cleaned significant data
#' @param stat stat transformatiion
#' @param position adjustment
#' @param na.rm remove na
#' @param show.legend show.legend
#' @param inherit.aes override default
#' @param ... ...
#'
#' @return ggplot of a timeline object
#' @description
#'
#' @examples
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' new Geom for plotting a timelne
#'
#'
#' @import  grid
#'
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,

                                 required_aes = c("x"),

                                 default_aes = ggplot2::aes(
                                   colour = "black",
                                   size = 10,
                                   alpha = 0.8,
                                   shape = 21,
                                   fill = "black"
                                 ),

                                 draw_key = ggplot2::draw_key_point,


                                 draw_panel = function(data, panel_scales, coord) {

                                   coords <- coord$transform(data, panel_scales)

                                   Grob_list = grid::gList()

                                   y_country_lines <- unique(coords$y)

                                   Grob_line_temp <- sapply(y_country_lines, grid::linesGrob,
                                                            x = c(min(coords$x), max(coords$x)),
                                                            gp = grid::gpar(alpha = 0.4, lwd=3))

                                   Grob_list = grid::gList(Grob_list, Grob_line_temp)


                                   points <- grid::pointsGrob(
                                     coords$x,
                                     coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(col = coords$colour,
                                                     fill = coords$fill,
                                                     alpha = coords$alpha)
                                   )

                                   grid::gList(Grob_list, points)
                                 }


)

