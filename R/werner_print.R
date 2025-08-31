#' Werner's colours Brewer generator and printing
#'
#' This function prints and plots palettes.
#' @param name Name of Palette.
#' @importFrom grDevices rgb
#' @importFrom graphics image par rect text
#' @export

print_werner <- function(name) {
  palette <- WernerPals[[name]][[1]]
  attr(palette,"name") = name
  n <- length(palette)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = palette,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.92, n + 1, 1.08, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(palette, "name"), cex = 2.5, family = "serif")
}
