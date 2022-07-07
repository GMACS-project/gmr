#' Get cpue or other indices
#'
#' @param M List object created by read_admb function
#' @return dataframe of observed and predicted indices and residuals
#' @author SJD Martell, D'Arcy N. Webber
#' @export
#'
.get_cpue_df <- function(M)
{
    n   <- length(M)
    mdf <- NULL
    for (i in 1:n)
    {
        A        <- M[[i]]
        df       <- data.frame(Model = names(M)[i], as.data.frame(A$dSurveyData))
        # colnames(df) <- c("Model","Index","year","seas","fleet","sex","mature","cpue","cv","units")
        if(dim(df)[2]<11) df = data.frame(cbind(df, rep(0,dim(df)[1])))
        colnames(df) <- c("Model","Index","year","seas","fleet","sex","mature","cpue","cv","units","cpue_Timing")
        df$sex   <- .SEX[df$sex+1]
        df$fleet <- .FLEET[df$fleet]
        sd       <- sqrt(log(1 + df$cv^2))
        df$lb    <- exp(log(df$cpue) - 1.96*sd)
        df$ub    <- exp(log(df$cpue) + 1.96*sd)

        df$cvest <- stats::na.exclude(as.vector(t(A$cpue_cv_add)))
        sde      <- sqrt(log(1 + df$cvest^2))
        df$lbe   <- exp(log(df$cpue) - 1.96*sde)
        df$ube   <- exp(log(df$cpue) + 1.96*sde)

        df$pred  <- stats::na.exclude(as.vector(t(A$pre_cpue)))
        df$resd  <- stats::na.exclude(as.vector(t(A$res_cpue)))
        mdf      <- rbind(mdf, df)
    }
    return(mdf)
}


#' Plot cpue or other indices
#'
#' @param M list object created by read_admb function
#' @param subsetby the fleet to subset the data to
#' @param xlab the x-axis label for the plot
#' @param ylab the y-axis label for the plot
#' @param ShowEstErr Shows errorbars from estimated CVs as well
#' @param logy Plot the CPUE in log-space
#' @param slab the sex label for the plot that appears above the key
#' @return plot of all observed and predicted incices
#' @export
#'
plot_cpue <- function(M, subsetby = "", xlab = "Year", ylab = "CPUE", slab = "Sex", ShowEstErr = FALSE, logy = FALSE)
{
    mdf <- .get_cpue_df(M)
    if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)

    if (logy) {
        mdf$cpue <- log(mdf$cpue)
        mdf$lb <- log(mdf$lb)
        mdf$ub <- log(mdf$ub)
        mdf$lbe <- log(mdf$lbe)
        mdf$ube <- log(mdf$ube)
        mdf$pred <- log(mdf$pred)
        ylab <- paste0("log(", ylab, ")")
    }

    xlab <- paste0("\n", xlab)
    ylab <- paste0(ylab, "\n")

    p  <- ggplot2::ggplot(mdf, ggplot2::aes(year, cpue)) +
        ggplot2::expand_limits(y = 0) +
        ggplot2::geom_pointrange(ggplot2::aes(year, cpue, ymax = ub, ymin = lb), col = "black")

    if (ShowEstErr) {
        if (length(M) == 1 && length(unique(mdf$sex)) == 1) {
            p  <- p + ggplot2::geom_pointrange(ggplot2::aes(year, cpue, ymax = ube, ymin = lbe), color = "red", shape = 1, linetype = "dotted", position = ggplot2::position_dodge(width = 1))
        } else if (length(M) != 1 && length(unique(mdf$sex)) == 1) {
            p  <- p + ggplot2::geom_pointrange(ggplot2::aes(year, cpue, ymax = ube, ymin = lbe, col = Model), shape = 1, linetype = "dotted", position = ggplot2::position_dodge(width = 1))
        } else if (length(M) == 1 && length(unique(mdf$sex)) != 1) {
            p  <- p + ggplot2::geom_pointrange(ggplot2::aes(year, cpue, ymax = ube, ymin = lbe, col = sex), shape = 1, linetype = "dotted", position = ggplot2::position_dodge(width = 1))
        } else {
            p  <- p + ggplot2::geom_pointrange(ggplot2::aes(year, cpue, ymax = ube, ymin = lbe, col = Model), shape = 1, linetype = "dotted", position = ggplot2::position_dodge(width = 1))
        }
    }

    if (.OVERLAY) {
        if (length(M) == 1 && length(unique(mdf$sex)) == 1) {
            p <- p + ggplot2::geom_line(data = mdf, ggplot2::aes(year, pred)) +
                ggplot2::facet_wrap(~fleet, scales = "free_y",ncol=1)
        } else if (length(M) != 1 && length(unique(mdf$sex)) == 1) {
            p <- p + ggplot2::geom_line(data = mdf, ggplot2::aes(year, pred, color = Model, linetype = Model)) +
                ggplot2::facet_wrap(~fleet, scales = "free_y",ncol=1)
        } else if (length(M) == 1 && length(unique(mdf$sex)) != 1) {
            p <- p + ggplot2::geom_line(data = mdf, ggplot2::aes(year, pred, color = sex)) + labs(col = slab) +
                ggplot2::facet_wrap(~fleet + sex, scales = "free_y",ncol=1)
        } else {
            p <- p + geom_line(data = mdf, ggplot2::aes(year, pred, color = Model, linetype = Model)) +
                ggplot2::facet_wrap(~fleet + sex, scales = "free_y",ncol=1)
        }
    } else {
        p  <- p + ggplot2::geom_line(data = mdf, aes(year, pred))
        p  <- p + ggplot2::facet_wrap(~fleet + sex + Model, scales = "free_y",ncol=1)
    }

    p  <- p + labs(x = xlab, y = ylab)
    print(p + .THEME + ggplot2::theme(legend.position=c(.7,.85)))
}


#' Plot residuals of cpue or other indices
#'
#' @param M List object created by read_admb function
#' @param subsetby the fleet or fleets to plot
#' @param xlab the x-axis label for the plot
#' @param ylab the y-axis label for the plot
#' @param slab the sex label for the plot that appears above the key
#' @return plot of fit indices residuals
#' @export
#'
plot_cpue_res <- function(M, subsetby = "", xlab = "Year", ylab = "Residual", slab = "Sex")
{
    xlab <- paste0("\n", xlab)
    ylab <- paste0(ylab, "\n")

    mdf <- .get_cpue_df(M)
    if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)

    p  <- ggplot2::ggplot(data = mdf, ggplot2::aes(year, resd)) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = 0))

    if (length(M) == 1 && length(unique(mdf$sex)) == 1)
    {
        p <- p + ggplot2::geom_point(data = mdf, ggplot2::aes(year, resd), position = ggplot2::position_dodge(0.5)) +
          ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = resd), position = ggplot2::position_dodge(0.5)) +
          ggplot2::facet_wrap(~fleet, scales = "free_y")
    } else if (length(M) != 1 && length(unique(mdf$sex)) == 1) {
        p <- p + ggplot2::geom_point(data = mdf, ggplot2::aes(year, resd, color = Model, shape = Model), position = ggplot2::position_dodge(0.5)) +
            ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = resd, color = Model), position = ggplot2::position_dodge(0.5)) +
            ggplot2::facet_wrap(~fleet, scales = "free_y")
    } else if (length(M) == 1 && length(unique(mdf$sex)) != 1) {
        p <- p + ggplot2::geom_point(data = mdf, ggplot2::aes(year, resd, color = sex, shape = sex), position = ggplot2::position_dodge(0.5)) + labs(col = slab) +
          ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = resd, color = sex), position = ggplot2::position_dodge(0.5)) +
            ggplot2::facet_wrap(~fleet + sex, scales = "free_y")
    } else {
        p <- p + ggplot2::geom_point(data = mdf, ggplot2::aes(year, resd, color = Model, shape = Model), position = ggplot2::position_dodge(0.5)) +
          ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = resd, color = Model), position = ggplot2::position_dodge(0.5)) +
            ggplot2::facet_wrap(~fleet + sex, scales = "free_y")
    }

    p  <- p + labs(x = xlab, y = ylab, fill = slab)
    print(p + .THEME)
}
