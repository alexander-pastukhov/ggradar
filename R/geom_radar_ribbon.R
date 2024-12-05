#' Radar ribbon for polar plot
#'
#' Draws a closed ribbon. For continuous scale ensures that first and last points do not share same angular position.
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
#' @importFrom grid polygonGrob gTree gList unit gpar
#' @importFrom scales alpha
#' @importFrom ggplot2 aes layer ggproto GeomPath
#' @importFrom tidyr nest unnest
#' @importFrom dplyr mutate group_by n bind_rows arrange
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#' @examples
#' example_df <- data.frame(x = factor(c("One", "Two", "Three")),
#'                          y = c(0.15, 0.3, 0.75),
#'                          ymin = c(0.1, 0.2, 0.5),
#'                          ymax = c(0.2, 0.4, 1.0))
#' ggplot2::ggplot(example_df, ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
#'   geom_radar_ribbon() +
#'   ggplot2::coord_polar() +
#'   ggplot2::ylim(0, 1)
geom_radar_ribbon <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", ...,
                              show.legend = NA, inherit.aes = TRUE) {

  GeomRadarRibbon <- ggproto(
    "GeomRadarRibbon", GeomPath,

    required_aes = c("x", "ymin", "ymax"),

    default_aes = aes(
      fill = "grey",
      alpha = 1),

    draw_panel = function(data, panel_params, coord) {
      # are coordinates polar?
      if (!inherits(coord, "CoordPolar")) stop("geom_radar_point() works only for polar coordinates.")

      # are axis discrete?
      axis_is_discrete <- inherits(panel_params$theta.major, "mapped_discrete")
      theta_start <- ifelse(axis_is_discrete, pi / length(panel_params$theta.major), 0)

      # special case: discrete axis and ungroup data makes each point its own group
      if (axis_is_discrete & length(unique(data$group)) == nrow(data)) {
        data$group <- 1
      }

      # sort data within each group so that x is ordered (otherwise, it is hard to draw a ribbon)
      data <- dplyr::arrange(data, .data$group, .data$x)

      # transform coordinates
      coords <- coord$transform(data, panel_params)



      # transforming ymin and ymax (need to substitute them into y)
      data_y <- data
      data_y$y <- data$ymin
      coords_y <- coord$transform(data_y, panel_params)
      coords$rmin <- coords_y$r
      data_y$y <- data$ymax
      coords_y <- coord$transform(data_y, panel_params)
      coords$rmax <- coords_y$r

      # adjust position of dots, so that first and last items do not overlap
      coords <-
        coords |>
        dplyr::group_by(.data$group) |>
        tidyr::nest() |>
        dplyr::mutate(data = map(data, ~. |> dplyr::mutate(, theta = seq(0, 2 * pi, length.out = dplyr::n() + 1)[1:dplyr::n()])),
                      data = map(data, ~dplyr::bind_rows(., .[1, ]))) |>
        tidyr::unnest(cols = c(data)) |>
        dplyr::mutate(xmin = .data$rmin * sin(.data$theta + theta_start) + 0.5,
               xmax = .data$rmax * sin(.data$theta + theta_start) + 0.5,
               ymin = .data$rmin  * cos(.data$theta + theta_start) + 0.5,
               ymax = .data$rmax  * cos(.data$theta + theta_start) + 0.5)

      grobs <- lapply(1:(nrow(coords)-1), function(i1) {
        row1 <- coords[i1, ]
        row2 <- coords[i1 + 1, ]

        # Create the polygon grob
        grid::polygonGrob(
          x = c(row1$xmin[1], row1$xmax[1], row2$xmax[1], row2$xmin[1]),
          y = c(row1$ymin[1], row1$ymax[1], row2$ymax[1], row2$ymin[1]),
          gp = grid::gpar(
            fill = scales::alpha(row1$fill[1], row1$alpha[1]),
            col = scales::alpha(row1$fill[1], row1$alpha[1]),
            lwd = 0
          )
        )
      })

      grid::gTree(children = do.call(grid::gList, grobs))
    }
  )

  layer(
    geom = GeomRadarRibbon,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}
