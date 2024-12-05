#' Radar points for polar plot
#'
#' For continuous scale ensures that first and last points do not share same angular position. For discrete scale it is equivalent to `geom_point`.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and `inherit.aes = TRUE` (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position A position adjustment to use on the data for this layer.
#' @param ... Other arguments passed on to `layer()`'s params argument.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes	If `FALSE`, overrides the default aesthetics, rather than combining with them.
#'
#' @export
#' @importFrom grid pointsGrob gTree gList unit gpar
#' @importFrom scales alpha
#' @importFrom ggplot2 aes layer ggproto GeomPoint
#' @importFrom tidyr nest unnest
#' @importFrom dplyr mutate group_by n bind_rows
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#' @examples
#' example_df <- data.frame(x = factor(c("One", "Two", "Three")),
#'                          y = c(0.2, 0.4, 1.0))
#' ggplot2::ggplot(example_df, ggplot2::aes(x = x, y = y)) +
#'   geom_radar_point() +
#'   ggplot2::coord_polar() +
#'   ggplot2::ylim(0, 1)
geom_radar_point <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", ...,
                             show.legend = NA, inherit.aes = TRUE) {

  GeomRadatPoint <- ggproto(
    "GeomRadatPoint", GeomPoint,

    required_aes = c("x", "y"),

    draw_panel = function(data, panel_params, coord) {
      # are coordinates polar?
      if (!inherits(coord, "CoordPolar")) stop("geom_radar_point() works only for polar coordinates.")

      # are axis discrete?
      axis_is_discrete <- inherits(panel_params$theta.major, "mapped_discrete")
      theta_start <- ifelse(axis_is_discrete, pi / length(panel_params$theta.major), 0)

      # transform coordinates
      coords <- coord$transform(data, panel_params)

      if (!axis_is_discrete) {
        # for continuous case, adjust position of dots, so that first and last items do not overlap
        coords <-
          coords |>
          dplyr::group_by(.data$group) |>
          tidyr::nest() |>
          dplyr::mutate(data = purrr::map(data, ~ . |> dplyr::mutate(theta = seq(0, 2 * pi, length.out = dplyr::n() + 1)[1:dplyr::n()])),
                 data = purrr::map(data, ~dplyr::bind_rows(., .[1, ]))) |>
          tidyr::unnest(cols = c(data)) |>
          dplyr::mutate(x = .data$r * sin(.data$theta + theta_start) + 0.5,
                        y = .data$r * cos(.data$theta + theta_start) + 0.5)
      }

      grid::pointsGrob(
        x = coords$x,
        y = coords$y,
        pch = coords$shape,
        size = unit(coords$size, "mm"),
        gp = grid::gpar(col = alpha(coords$colour, coords$alpha))
      )
    }
  )

  layer(
    geom = GeomRadatPoint,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}
