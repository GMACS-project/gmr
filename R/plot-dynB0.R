#' Extract spawning stock biomass (ssb) from gmacs run
#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M list object created by read_admb function
#' @return dataframe of spawning biomass
#' @export
#'
.get_dynB0_df <- function(M)
{
    par <- NULL
    n <- length(M)
    mdf <- NULL
    for (i in 1:n)
    {
        A <- M[[i]]
        df <- data.frame(Model = names(M)[i],
                         par = A$fit$names,
                         log_ssb = A$fit$est,
                         log_sd = A$fit$std)
        df      <- subset(df, par == "sd_log_dyn_Bzero")
        df$year <- A$mod_yrs[-1]
        df$ssb  <- exp(df$log_ssb)
        df$lb   <- exp(df$log_ssb - 1.96*df$log_sd)
        df$ub   <- exp(df$log_ssb + 1.96*df$log_sd)
        mdf     <- rbind(mdf, df)
    }
    return(mdf)
}


#' Plot predicted spawning stock biomass (ssb)
#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M List object(s) created by read_admb function
#' @param xlab the x-label of the figure
#' @param ylab the y-label of the figure
#' @param ylim is the upper limit of the figure
#' @param alpha the opacity of the ribbon
#' @return Plot of model estimates of spawning stock biomass
#' @export
#'
plot_dynB0 <- function(M, xlab = "Year", ylab = "RSB (SSB/dB0)", ylim = NULL, alpha = 0.2)
{
    xlab <- paste0("\n", xlab)
    ylab <- paste0(ylab, "\n")

    mdf <- .get_dynB0_df(M)

    p <- ggplot2::ggplot(mdf) + ggplot2::labs(x = xlab, y = ylab)

    if (is.null(ylim))
    {
        p <- p + ggplot2::expand_limits(y = 0)
    } else {
        p <- p + ggplot2::ylim(ylim[1], ylim[2])
    }

    if (length(M) == 1)
    {
        p <- p + ggplot2::geom_line(ggplot2::aes(x = year, y = ssb)) +
            ggplot2::geom_ribbon(ggplot2::aes(x = year, ymax = ub, ymin = lb), alpha = alpha)
    } else {
        p <- p + ggplot2::geom_line(ggplot2::aes(x = year, y = ssb, col = Model)) +
            ggplot2::geom_ribbon(ggplot2::aes(x = year, ymax = ub, ymin = lb, fill = Model), alpha = alpha)
    }

    if(!.OVERLAY) p <- p + ggplot2::facet_wrap(~Model)
    print(p + .THEME)
}
