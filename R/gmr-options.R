#' The color-blind friendly palette with grey
#'
#' see http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
#'
#' @author D'Arcy N. Webber
#' @export
#'
.cbPalette1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#' The color-blind friendly palette with black
#'
#' see http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
#'
#' @author D'Arcy N. Webber
#' @export
#'
.cbPalette2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#' GMR plotting options
#'
#' Parameters to be passed on to gmr plotting functions
#'
#' @details
#' \code{plot_type} is the type of plot to create (png is the only option at the mo),
#' \code{plot_resolution} png resolution
#' \code{plot_size} a vector of length 2 containing the width and height of plot (mm)
#' \code{plot_cols} the colour palette used for plotting (recommend the colour-blind palette cbPalette1 or cdPalette2)
#' @author D'Arcy N. Webber
#' @export
#'
.gmr_options <- list(plot_type = "png", plot_resolution = 400,
                     plot_size = c(100,100), plot_cols = .cbPalette1,
                     thick = 2, thin = 1)

#' Set plotting theme for ggplot2 via gmr
#'
#' Gives user control over plot theme by running ggplot2 functions
#' that do the same. This allows a user to set the theme without
#' independently loading the ggplot2 package.
#'
#' @param theme of desired theme
#' @return Sets ggplot2 theme for current working session
#' @export
set_ggtheme <- function(theme){
  switch(theme,
         bw      = ggtheme <- ggplot2::theme_bw(),
         gray    = ggtheme <- ggplot2::theme_gray(),
         classic = ggtheme <- ggplot2::theme_classic(),
         minimal = ggtheme <- ggplot2::theme_minimal()
  )
  message("The ggplot theme has been set to ", theme, " for this working session")
}


# A simple function for creating transparent colors
# Author: Nathan Stephens (hacks package)
colr <- function(col.pal=1,a=1)
{
  col.rgb<-col2rgb(col.pal)/255
  rgb(t(col.rgb),alpha=a)
}
