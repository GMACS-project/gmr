#' @title Base theme for plotting with gmr
#'
#' @export
BaseThemeGMR <- function(leg.pos = "bottom",
                               # base_family = "Times New Roman",
                               base_size = 14,
                               x_angle = 0,
                               x_axis_size = 10,
                               y_axis_size = 10,

                               do.axis.line = TRUE,
                               # axis_title_family = "TimesNewRomanPS-BoldMT",
                               axis_title_size = 12,

                               do.ticks = TRUE,
                               do.grid = TRUE,

                               # strip_text_family = "TimesNewRomanPS-BoldMT",
                               strip_text_size = 12,
                               panel_space_x = 1.5,
                               panel_space_y = 1.5,
                               panel_face = "plain",

                               plot_title_size = 16,
                               plot_title_margin = 10,
                               # plot_title_family = "TimesNewRomanPS-BoldMT",

                               subtitle_size = 12,
                               subtitle_margin = 12,
                               # subtitle_family = "TimesNewRomanPS-ItalicMT",
                               subtitle_face = "plain",

                               caption_size = 9,
                               caption_margin = 10,
                               # caption_family = "TimesNewRomanPSMT",

                               tag_size = 12,

                               plot_margin = ggplot2::margin(30, 30, 30, 30),
                               ...) {
  out <- ggplot2::ggplot()

  # Base theme
  out <- ggplot2::theme_minimal(base_size = base_size
                                # base_family = base_family
                                )

  # Legend
  out <-
    out + ggplot2::theme(legend.background = ggplot2::element_blank())
  out <- out + ggplot2::theme(legend.key = ggplot2::element_blank())
  out <- out + ggplot2::theme(legend.position = leg.pos)

  # Axis line
  if (do.axis.line) {
    out <-
      out + ggplot2::theme(axis.line.x = ggplot2::element_line(color = "#2b2b2b", size = 0.20))
    out <-
      out + ggplot2::theme(axis.line.y = ggplot2::element_line(color = "#2b2b2b", size = 0.20))
  } else {
    out <- out + ggplot2::theme(axis.line.x = ggplot2::element_blank())
  }

  # Ticks
  if (do.ticks) {
    out <-
      out + ggplot2::theme(axis.ticks.x = ggplot2::element_line(size = 0.20))
    out <-
      out + ggplot2::theme(axis.ticks.y = ggplot2::element_line(size = 0.20))
    out <-
      out + ggplot2::theme(axis.ticks.length = grid::unit(8, "pt"))
  } else {
    out <- out + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    out <-
      out + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    out <-
      out + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  }


  # Grid
  if (do.grid) {
    out <-
      out + ggplot2::theme(panel.grid = ggplot2::element_line(color = "grey80", size = 0.10))
    out <-
      out + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "grey80", size = 0.10))
    out <-
      out + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = "grey80", size = 0.05))
  } else {
    out <- out + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }


  # Axis text and tittle
  out <-
    out + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 1),
        angle = x_angle,
        size = x_axis_size
      )
    )
  out <-
    out + ggplot2::theme(axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 1, l = 1.5),
                                                             size = y_axis_size))

  out <-
    out + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size
                                                            # family = axis_title_family
                                                            ))
  out <-
    out + ggplot2::theme(axis.title.x = ggplot2::element_text(size = axis_title_size
                                                              # family = axis_title_family
                                                              ))
  out <-
    out + ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        size = axis_title_size,
        # family = axis_title_family,
        margin = ggplot2::margin(r = 3)
      )
    )

  # Strip.text when faceting is used
  out <-
    out + ggplot2::theme(
      strip.text = ggplot2::element_text(
        size = strip_text_size,
        face = "bold"
        # family = strip_text_family
      )
    )

  out <-
    out + ggplot2::theme(strip.text.x = ggplot2::element_text(face = panel_face))
  out <-
    out + ggplot2::theme(strip.text.y = ggplot2::element_text(face = panel_face))
  # Space between panels
  out <-
    out + ggplot2::theme(panel.spacing.x = grid::unit(panel_space_x, "lines"))
  out <-
    out + ggplot2::theme(panel.spacing.y = grid::unit(panel_space_y, "lines"))


  # Titles & caption set up
  out <- out + ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0,
      size = plot_title_size,
      margin = ggplot2::margin(b = plot_title_margin),
      face = "bold"
      # family = plot_title_family
    )
  )

  out <-
    out + ggplot2::theme(
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        size = subtitle_size,
        margin = ggplot2::margin(b = subtitle_margin),
        # family = subtitle_family,
        face = subtitle_face
      )
    )

  out <- out + ggplot2::theme(
    plot.caption = ggplot2::element_text(
      hjust = 1,
      size = caption_size,
      margin = ggplot2::margin(t = caption_margin),
      # family = caption_family
    )
  )

  out <-
    out + ggplot2::theme(plot.tag = ggplot2::element_text(size = tag_size, face = "bold"))

  out <- out + ggplot2::theme(plot.margin = plot_margin)
  return(out)
}
