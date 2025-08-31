#' Werner's colours Brewer generator and printing
#'
#' This function generates colour palettes.
#' @param name Name of Palette.
#' @param n Number of colours.
#' @param type Either a discrete or continuos colour palette
#' @param direction Direction of palette
#' @param return_hex Whether to return a vector with hex values or not.
#' @return Colours of desired palette in hex code or the palette ready for use.
#' @export

werner_brewer <- function(name, n,
                          type = c("discrete", "continuous"),
                          direction = c(1, -1),
                          return_hex=FALSE) {
  palette <- WernerPals[[name]]

  if (is.null(palette)|is.numeric(name)){
    stop("Palette does not exist.")
  }

  if (missing(n)) {
    n <- length(palette[[1]])
  }

  if (missing(direction)) {
    direction <- 1
  }

  if (missing(type)) {
    if(n > length(palette[[1]])){type <- "continuous"}
    else{type <- "discrete"}
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(palette[[1]])) {
    stop("Too many colors for the palette! \n try type 'continuous' instead.")
  }

  continuous <-  if(direction==1){grDevices::colorRampPalette(palette[[1]])(n)
  }else{
    grDevices::colorRampPalette(rev(palette[[1]]))(n)}

  discrete <- if(direction==1){
    palette[[1]]
  } else  rev(palette[[1]])

  out <- switch(type,
                continuous = continuous,
                discrete = discrete
  )
  if(return_hex==TRUE){print(out)}
  structure(out, class = "palette", name = name)
}
