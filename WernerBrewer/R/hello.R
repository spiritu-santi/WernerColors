library(magrittr)
library(tidyverse)
library(palettes)
werner_colours <- read.table(file = "~/Desktop/WernerColors/WernerColors/data-raw/data_raw.csv",sep=",",header=TRUE,comment.char="") %>% as_tibble()
werner_table <- werner_colours
# Full palette ---------------------------------------------------------
#WernerColours <- pal_palette(WernerColours = werner_table$Hex[1:110])
#plot(WernerColours)
werner.brewer <- function(name, n,
                          type = c("discrete", "continuous"), direction = c(1, -1),
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
    stop("Too many colors!")
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
  #print.werner(structure(out, class = "palette", name = name))
}

print.werner <- function(name, ...) {
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

WernerPals <- list(
  Rocks = list(werner_table$Hex[c(35,34,26,37,23)]),
  Plants = list(werner_table$Hex[c(63,57,29,84,41,22)]),
  Birds = list(werner_table$Hex[c(23,20,15,9,8,3,75,77,78,100,102,98)]),
  Bugs = list(werner_table$Hex[c(38,27,29,36,52,58,65,64)]),
  Bugs2 = list(werner_table$Hex[c(43,44,38,41,79,83,80,65)]),
  Firebirds = list(werner_table$Hex[c(5,75,77,78,100,102,98)]),
  Greybirds = list(werner_table$Hex[c(3,8,9,15,20,23)]),
  Seeds = list(werner_table$Hex[c(87,84,81,105,8,61,57,42,22)]),
  Apples = list(werner_table$Hex[c(86,81,85,69,60,61)]),
  Leaves = list(werner_table$Hex[c(55,59,48,47,46)])
)

WernerBasic <- list(
  whites = list(werner_table$Hex[1:8]),
  greys = list(werner_table$Hex[9:16]),
  blacks = list(werner_table$Hex[17:23]),
  blues = list(werner_table$Hex[24:34]),
  purples = list(werner_table$Hex[35:45]),
  greens = list(werner_table$Hex[46:61]),
  yellows = list(werner_table$Hex[62:75]),
  orange = list(werner_table$Hex[76:81]),
  reds = list(werner_table$Hex[82:99]),
  browns = list(werner_table$Hex[100:110])
)
