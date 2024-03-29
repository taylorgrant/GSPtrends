#' Theme for ggplot
#'
#' @param base_family Font family
#' @param base_size base size
#' @param plot_title_family title font family
#' @param plot_title_size title size
#' @param plot_title_face title face
#' @param plot_title_margin title margin
#' @param subtitle_family subtitle family
#' @param subtitle_size subtitle size
#' @param subtitle_face subtitle face
#' @param subtitle_margin subtitle margin
#' @param strip_text_family strip text family
#' @param strip_text_size strip text font size
#' @param strip_text_face strip text font face
#' @param caption_family family
#' @param caption_size size
#' @param caption_face face
#' @param caption_margin margin
#' @param axis_title_family family
#' @param axis_title_size size
#' @param axis_title_face face
#' @param axis_title_just justification
#' @param plot_margin margin
#' @param panel_spacing spacing
#' @param grid_col grid
#' @param grid grid
#' @param axis_col axis
#' @param axis axis
#' @param ticks ticks
#'
#' @return re-themed ggplot graph
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(x = mpg, y = hp)) +
#' geom_point() +
#' theme_xf
#' }
theme_xf <- function(base_family="Open Sans", base_size = 11.5,
                     plot_title_family=base_family, plot_title_size = 16,
                     plot_title_face="plain", plot_title_margin = 4,
                     subtitle_family="Open Sans", subtitle_size = 12,
                     subtitle_face = "plain", subtitle_margin = 15,
                     strip_text_family = base_family, strip_text_size = 12,
                     strip_text_face = "plain",
                     caption_family = "Open Sans", caption_size = 9,
                     caption_face = "italic", caption_margin = 10,
                     axis_title_family = base_family, axis_title_size = 13,
                     axis_title_face = "plain", axis_title_just = "rt",
                     plot_margin = ggplot2::margin(10,10,10,10),
                     panel_spacing = ggplot2::unit(0.5, "lines"),
                     grid_col = "#cccccc", grid = TRUE,
                     axis_col = "#cccccc",axis = FALSE,
                     ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

  ret <- ret + ggplot2::theme(legend.background=ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key=ggplot2::element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + ggplot2::theme(panel.grid=ggplot2::element_line(color=grid_col, linewidth=0.10))
    ret <- ret + ggplot2::theme(panel.grid.major=ggplot2::element_line(color=grid_col, linewidth=0.1))
    ret <- ret + ggplot2::theme(panel.grid.minor=ggplot2::element_line(color=grid_col, linewidth=0.1))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x=ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y=ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x=ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y=ggplot2::element_blank())
    }

  } else {
    ret <- ret + ggplot2::theme(panel.grid=ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line=ggplot2::element_line(color="#2b2b2b", linewidth=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_line(color=axis_col, linewidth=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_line(color=axis_col, linewidth=0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_line(color=axis_col, linewidth=0.15))
      ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_line(color=axis_col, linewidth=0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line=ggplot2::element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_line(linewidth=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_line(linewidth=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_line(linewidth=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + ggplot2::theme(axis.text.x=ggplot2::element_text(margin=ggplot2::margin(t=0)))
  ret <- ret + ggplot2::theme(axis.text.y=ggplot2::element_text(margin=ggplot2::margin(r=0)))
  ret <- ret + ggplot2::theme(axis.title=ggplot2::element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x=ggplot2::element_text(hjust=xj, size=axis_title_size,
                                                                 family=axis_title_family, face=axis_title_face))
  ret <- ret + ggplot2::theme(axis.title.y=ggplot2::element_text(hjust=yj, size=axis_title_size,
                                                                 family=axis_title_family, face=axis_title_face))
  ret <- ret + ggplot2::theme(strip.text=ggplot2::element_text(hjust=0, size=strip_text_size,
                                                               face=strip_text_face, family=strip_text_family))
  ret <- ret + ggplot2::theme(panel.spacing.x=grid::unit(.5, "lines"))
  ret <- ret + ggplot2::theme(panel.spacing.y=grid::unit(.5, "lines"))
  ret <- ret + ggplot2::theme(plot.title=ggplot2::element_text(hjust=0, size=plot_title_size,
                                                               margin=ggplot2::margin(b=plot_title_margin),
                                                               family=plot_title_family, face=plot_title_face))
  ret <- ret + ggplot2::theme(plot.subtitle=ggplot2::element_text(hjust=0, size=subtitle_size,
                                                                  margin=ggplot2::margin(b=subtitle_margin),
                                                                  family=subtitle_family, face=subtitle_face))
  ret <- ret + ggplot2::theme(plot.caption=ggplot2::element_text(hjust=1, size=caption_size,
                                                                 margin=ggplot2::margin(t=caption_margin),
                                                                 family=caption_family, face=caption_face))
  ret <- ret + ggplot2::theme(plot.margin=plot_margin)

  ret <-  ret + ggplot2::theme(panel.spacing=panel_spacing)

  ret

}
