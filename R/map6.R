#' A timeline plot dor NOAA significant earthquakes data
#'
#' @description a time line of earthquakes ranging from xmin to xmaxdates
#' with a point for each earthquake.
#'
#' @details Optional aesthetics include color, size, and alpha (for transparency).
#' The xaesthetic is a date and an optional
#' y aesthetic is a factor indicating some stratification
#' in which case multiple time lines
#' will be plotted for each level of the factor (e.g. country).
#'
#' @inheritParams ggplot2::geom_point
#'
#' @importFrom ggplot2 layer
#'
#'
#' @examples
#' \dontrun{
#' eq_clean_data() %>%
#'    dplyr::filter(COUNTRY %in% c('USA', 'JAPAN')) %>%
#'    dplyr::filter(DATE > '2000-01-01') %>%
#'    ggplot(aes(x = DATE,
#'               y = COUNTRY,
#'               color = as.numeric(TOTAL_DEATHS),
#'               size = as.numeric(EQ_PRIMARY)
#'    )) +
#'    geom_timeline() +
#'    labs(size = "Richter scale value", color = "# deaths")
#' }
#' @export

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @importFrom ggplot2 aes draw_key_point
#' @importFrom grid pointsGrob linesGrob gList gpar
#' @importFrom scales alpha
#'

GeomTimeline <-
  ggplot2::ggproto(
    "GeomTimeline", ggplot2::Geom,
    required_aes = c("x"),
    default_aes = ggplot2::aes(
      colour = "grey",
      size = 2.0,
      alpha = 0.5,
      shape = 21,
      fill = "grey",
      stroke = 0.5),

    draw_key = ggplot2::draw_key_point,

    draw_panel = function(data, panel_scales, coord) {


      coords <- coord$transform(data, panel_scales)

      GrobPoints <- grid::pointsGrob(
        coords$x,
        coords$y,
        pch = coords$shape,
        size = unit(coords$size / 4, "char"),
        gp = grid::gpar(
          col = scales::alpha(coords$colour, coords$alpha),
          fill = scales::alpha(coords$colour, coords$alpha)
        )
      )
      y_segments <- unique(coords$y)

      GrobLines <- grid::polylineGrob(
        x = unit(rep(c(0, 1), each = length(y_segments)), "npc"),
        y = unit(c(y_segments, y_segments), "npc"),
        id = rep(seq_along(y_segments), 2),
        gp = grid::gpar(col = "grey",
                        lwd = .pt)
      )

      grid::gList(GrobPoints, GrobLines)
    }
  )
