# Werner's colours palettes for plotting with ggplot2

#' WernerColors palettes for plotting with ggplot2
#'
#' Function for using \code{WernerColors} colors schemes in \code{ggplot2}.
#' For discrete scales use \code{\link{scale_color_werner_d}} and \code{\link{scale_fill_werner_d}}
#' For continuous scales use \code{\link{scale_color_werner_c}} and \code{\link{scale_fill_werner_c}}
#'
#' @param palette_name Name of Palette.
#' @param direction Direction of palette.
#' @param n Number of colors.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @export
scale_color_werner_d <- function(palette_name, direction = 1, n, ...){
  discrete_scale(aesthetics = "colour",
                 scale_name="werner_d",
                 palette= function(n) werner_brewer(name=palette_name, n = n, direction = direction, return_hex = FALSE),
                 ...)
}

#' WernerColors palettes for plotting with ggplot2
#'
#' Function for using \code{WernerColors} colors schemes in \code{ggplot2}.
#' For discrete scales use \code{\link{scale_color_werner_d}} and \code{\link{scale_fill_werner_d}}
#' For continuous scales use \code{\link{scale_color_werner_c}} and \code{\link{scale_fill_werner_c}}
#'
#' @param palette_name Name of Palette.
#' @param direction Direction of palette.
#' @param n Number of colors.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @export
scale_fill_werner_d <- function(palette_name, direction = 1, n, ...){
  discrete_scale(aesthetics = "fill",
                 scale_name="werner_d",
                 palette= function(n) werner_brewer(name=palette_name, n = n, direction = direction, return_hex = FALSE),
                 ...)
}

#' WernerColors palettes for plotting with ggplot2
#'
#' Function for using \code{WernerColors} colors schemes in \code{ggplot2}.
#' For discrete scales use \code{\link{scale_color_werner_d}} and \code{\link{scale_fill_werner_d}}
#' For continuous scales use \code{\link{scale_color_werner_c}} and \code{\link{scale_fill_werner_c}}
#'
#' @param palette_name Name of Palette.
#' @param direction Direction of palette.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @export
scale_color_werner_c <- function(palette_name, direction=1, ...){
  scale_color_gradientn(colors=werner_brewer(name=palette_name, direction=direction), ...)
}


#' WernerColors palettes for plotting with ggplot2
#'
#' Function for using \code{WernerColors} colors schemes in \code{ggplot2}.
#' For discrete scales use \code{\link{scale_color_werner_d}} and \code{\link{scale_fill_werner_d}}
#' For continuous scales use \code{\link{scale_color_werner_c}} and \code{\link{scale_fill_werner_c}}
#'
#' @param palette_name Name of Palette.
#' @param direction Direction of palette.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @export
scale_fill_werner_c <- function(palette_name, direction=1, ...){
  scale_fill_gradientn(colors=werner_brewer(name=palette_name, direction=direction), ...)
}


#' WernerColors palettes for plotting with ggplot2
#'
#' Function for using \code{WernerColors} colors schemes in \code{ggplot2}.
#' For discrete scales use \code{\link{scale_color_werner_d}} and \code{\link{scale_fill_werner_d}}
#' For continuous scales use \code{\link{scale_color_werner_c}} and \code{\link{scale_fill_werner_c}}
#'
#' @param palette_name Name of Palette.
#' @param direction Direction of palette.
#' @param n Number of colors.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @export
scale_colour_werner_d <- scale_color_werner_d

#' WernerColors palettes for plotting with ggplot2
#'
#' Function for using \code{WernerColors} colors schemes in \code{ggplot2}.
#' For discrete scales use \code{\link{scale_color_werner_d}} and \code{\link{scale_fill_werner_d}}
#' For continuous scales use \code{\link{scale_color_werner_c}} and \code{\link{scale_fill_werner_c}}
#'
#' @param palette_name Name of Palette.
#' @param direction Direction of palette.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @export
scale_colour_werner_c <- scale_color_werner_c

