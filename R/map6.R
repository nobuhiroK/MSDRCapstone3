#' A timeline plot for NOAA significant earthquakes data
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


#' @title GeomTimeline
#'
#' @description new Geom for mapping a timeline for NOAA significant data
#'
#' @importFrom ggplot2 aes draw_key_point
#' @importFrom grid pointsGrob linesGrob gList gpar
#' @importFrom scales alpha
#'
#' @format NULL
#' @usage NULL
#' @export

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


#' A timeline plot with label to annotate for NOAA significant earthquakes data
#'
#'
#'
#' @description This geom adds a vertical line to each data point with a text annotation
#' (e.g. the location of the earthquake) attached to each line.
#'
#' @details  There should be an option to subset to n_max number of earthquakes,
#'  where we take the n_max largest (by magnitude) earthquakes.
#'  Aesthetics are x, which is the date of the earthquake
#'  and label which takes the column name from which annotations will be obtained.
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

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @title GeomTimelineLabel
#'
#' @description new Geom for mapping a timeline for NOAA significant data
#' with a label to annotate top_n data points
#'
#' @importFrom ggplot2 aes draw_key_point
#' @importFrom grid grobTree segmentsGrob textGrob
#' @importFrom dplyr top_n group_by_
#'
#' @format NULL
#' @usage NULL
#' @export

GeomTimelineLabel <-
  ggplot2::ggproto(
    "GeomTimelineLabel", ggplot2::Geom,
    required_aes = c("x", "label","magnitude"),
    default_aes = ggplot2::aes(
      n_max = NA),

    # top_n_data = function(data, params){
    #   top_n <- data$n_max[1]
    #   if(is.numeric(top_n)){
    #     dplyr::top_n(dplyr::group_by_(data, "group"), top_n, size)
    #
    #   } else {
    #     data
    #   }
    #
    # }

    draw_key = ggplot2::draw_key_point,

    draw_panel = function(data, panel_scales, coord) {


      n_max <- data$n_max[1]

      # get to n earthquakes by magnitude
      data <- data %>%
        dplyr::mutate(magnitude = magnitude / max(magnitude) * 1.5) %>%
        dplyr::group_by(group) %>%
        dplyr::top_n(n_max, magnitude)


      coords <- coord$transform(data, panel_scales)

      grid::grobTree(
        grid::segmentsGrob(x0 =coords$x, x1=coords$x,
                           y0=coords$y, y1=coords$y + 0.1,
                           gp=grid::gpar()),
        grid::textGrob(
          x=coords$x, y=coords$y + 0.1, label=coords$label,
          rot = 45, hjust = -0.1, vjust = -0.1,
          gp= grid::gpar()
        )

      )


    }
  )
