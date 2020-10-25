#' Define Colors
#' @export
mm_colors <- c(
  `teal` = "#146473",
  `yellow` = "#BA931D",
  `blue` = "#252582",
  `orange` = "#BA6A1D",
  `dark_grey` = "#4D5F63",
  `grey` = "#7A8182")


#' Call Colors
#' @export
mm_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (mm_colors)

  mm_colors[cols]
}


#' Define Color Palettes
#' @export
mm_palettes <- list(
  `main`  = mm_cols("teal", "yellow", "blue", "orange"),
  `cool`  = mm_cols("teal", "blue"),
  `hot`   = mm_cols("yellow", "orange"),
  `grey`  = mm_cols("grey", "dark grey")
)


#' Call Color Palettes
#' @export
mm_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- mm_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}


#' Color Scales
#' @export
scale_color_mm <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- mm_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("mm_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Fill Scales
#' @export
scale_fill_mm <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- mm_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("mm_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' mm_light ggplot theme
#' @export
theme_mm_light <- function () {
  theme_minimal(base_size = 12, base_family = "Barlow Condensed") %+replace%
    theme(
      plot.title = element_text(family = "Merriweather",
                                face = "bold",
                                size = 18,
                                hjust = 0,
                                margin = margin(0,0,10,0)),
      plot.title.position = "plot",
      plot.subtitle = element_text(hjust = 0,
                                   margin = margin(0,0,36,0)),
      plot.caption = element_text(size = 10,
                                  color = "#7A8182",
                                  hjust = 1,
                                  margin = margin(10,0,0,0)),
      plot.caption.position = "plot",
      axis.text = element_text(color = "black"),
      axis.title.y = element_text(face = "italic",
                                  angle = 90,
                                  hjust = 1,
                                  size = 10,
                                  color = "#7A8182",
                                  margin = margin(0,10,0,0)),
      axis.title.x = element_text(face = "italic",
                                  hjust = 0,
                                  size = 10,
                                  color = "#7A8182",
                                  margin = margin(10,0,0,0)),
      plot.margin = margin(20,20,20,20)
    )
}
