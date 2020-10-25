#' Define Colors
#' @export
mm_colors <- c(
  `teal` = "#257D94",
  `yellow` = "#E0AA22",
  `purple` = "#480B47",
  `coral` = "#E06A4F",
  `blue` = "#2C4994",
  `green` = "#1E946A",
  `gray` = "#666666",
  `light_gray` = "#A3A3A3",
  `dark_gray` = "#333333",
  `dark_teal` = "#091F25",
  `dark_teal_light` = "#6B797C",
  `dark_teal_dark` = "#051013",
  `teal_light` = "#7CB1BF",
  `teal_dark` = "#133F4A",
  `yellow_light` = "#ECCC7A",
  `yellow_dark` = "#705511",
  `purple_light` = "#916D91",
  `purple_dark` = "#240624",
  `coral_light` = "#ECA695",
  `coral_dark` = "#703528",
  `blue_light` = "#8092BF",
  `blue_dark` = "#16254A",
  `green_light` = "#78BFA6",
  `green_dark` = "#0F4A35",
  `gray_light` = "#A3A3A3",
  `gray_dark` = "#333333")


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
  `main`  = mm_cols("teal", "yellow", "purple", "coral", "blue", "green"),
  `cool`  = mm_cols("teal", "blue", "green"),
  `hot`   = mm_cols("yellow", "purple", "coral"),
  `gray`  = mm_cols("gray_light", "gray", "gray_dark"),
  `mixed_scale`  = mm_cols("teal", "yellow", "coral")
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
                                  color = "gray_dark",
                                  hjust = 1,
                                  margin = margin(10,0,0,0)),
      plot.caption.position = "plot",
      axis.text = element_text(color = "black"),
      axis.title.y = element_text(face = "italic",
                                  angle = 90,
                                  hjust = 1,
                                  size = 10,
                                  color = "gray_dark",
                                  margin = margin(0,10,0,0)),
      axis.title.x = element_text(face = "italic",
                                  hjust = 0,
                                  size = 10,
                                  color = "gray_dark",
                                  margin = margin(10,0,0,0)),
      plot.margin = margin(20,20,20,20)
    )
}
