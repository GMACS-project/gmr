#' Get growth increment data
#'
#' @param M list object(s) created by read_admb function
#' @return dataframe of growth increments
#' @author D'Arcy N. Webber, SJD Martell
#'
.get_gi_df <- function(M)
{
    n  <- length(M)
    mdf <- NULL
    if(length(M[[1]]$iMoltInc)>0)
    {
    for(i in 1:n)
    {
        A <- M[[i]]
        df <- data.frame(Model = names(M)[i],
                         Sex   = as.factor(A$iMoltIncSex),
                         molt_inc   = A$dMoltInc,
                         Premolt  = A$dPreMoltSize,
                         type = 'obs')

        df_2<-data.frame(Model = names(M)[i],
                         Sex  = as.factor(rep(sort(unique(A$iMoltIncSex)),each=length(A$mid_points))),
                         molt_inc = as.vector(t(A$molt_increment)),
                         Premolt = A$mid_points,
                         type = 'pred')

        mdf <- rbind(mdf, df,df_2)
    }
    } else
    {
      for(i in 1:n)
      {
      A <- M[[i]]
      df <- data.frame(Model = names(M)[i],
                       molt_inc = A$molt_increment,
                       mids = A$mid_points,
                       Sex = as.factor(rep(1:(A$nsex),length(A$mid_points))))
      mdf <- rbind(mdf, df)
      }
    }
    return(mdf)
}


#' Plot growth from arbitrary start age
#'
#' @param M list object created by read_admb function
#' @param xlab the x-axis label for the plot
#' @param ylab the y-axis label for the plot
#' @param slab the sex label for the plot that appears above the key
#' @return plot growth increment for given pre-molt size, including model predictions and data
#' @author SJD Martell, D'Arcy N. Webber
#' @export
#'
plot_growth_inc <- function(M, xlab = "Pre-molt size (mm)", ylab = "Molt increment (mm)",
                            slab = "Sex")
{
    xlab <- paste0("\n", xlab)
    ylab <- paste0(ylab, "\n")

    mdf <- .get_gi_df(M)
    obs_in<-dplyr::filter(mdf,type=="obs")
    pred_in<-dplyr::filter(mdf,type=="pred")

    p <- ggplot(obs_in) +
        labs(x = xlab, y=ylab, col = slab) +
        expand_limits(y=0) +
        facet_wrap(~Sex) +
        geom_line(data=pred_in,aes(x=Premolt,y=molt_inc,col=Model)) +
        .THEME

     if(obs_in$molt_inc[1]!=0)
       p <- p+ geom_point(aes(x=Premolt,y=molt_inc,col=Sex))

    #p <- p + geom_point(aes(x = size, y = obs, colour = sex))
    #p <- p + geom_line(aes(x = size, y = pred, colour = sex))
    #if (!length(M) == 1)
    #{
    #    p <- p + facet_wrap(~Model)
    #}
    # if(length(mdf$pred)>0)
    # {
    # if (length(M) == 1 && length(unique(mdf$Sex)) == 1)
    # {
    #     p <- p + geom_line(aes(x = size, y = obs)) +
    #         geom_point(aes(x = size, y = obs))
    # } else if (length(M) != 1 && length(unique(mdf$Sex)) == 1) {
    #     p <- p + geom_line(aes(x = size, y = obs, col = Model)) +
    #         geom_point(aes(x = size, y = obs, col = Model))
    # } else if (length(M) == 1 && length(unique(mdf$Sex)) != 1) {
    #     p <- p + geom_line(aes(x = size, y = obs, linetype = Sex))
    # } else {
    #     p <- p + geom_line(aes(x = size, y = obs, linetype = Sex, col = Model)) +
    #         geom_point(aes(x = size, y = obs, col = Model))
    # }
    # }
    # if(length(mdf$pred)==0)
    # {
    #   if (length(M) == 1 && length(unique(mdf$Sex)) == 1)
    #   {
    #     p <- p + geom_line(aes(x = mids, y = molt_inc)) +
    #       geom_point(aes(x = mids, y = molt_inc))
    #   } else if (length(M) != 1 && length(unique(mdf$Sex)) == 1) {
    #     p <- p + geom_line(aes(x = mids, y = molt_inc, col = Model)) +
    #       geom_point(aes(x = mids, y = molt_inc, col = Model))
    #   } else if (length(M) == 1 && length(unique(mdf$Sex)) != 1) {
    #     p <- p + geom_line(aes(x = mids, y = molt_inc, linetype = Sex))
    #   } else {
    #     p <- p + geom_line(aes(x = mids, y = molt_inc, linetype = Sex, col = Model)) +
    #       geom_point(aes(x = mids, y = molt_inc, col = Model))
    #   }
    # }
    #
     return(p)
}
