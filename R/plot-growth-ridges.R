#'
#' @title Plot growth transition matrices as ridges
#'
#' @description Function to plot growth transition matrices as ridges.
#'
#' @param M - list with model results objects from [read_admb()] as named elements
#'
#' @return ggplot2 object using [ggridges::geom_density_ridges()] to visualize
#' p(post-molt size|pre-molt size).
#'
#' @author WT Stockhausen (ctb)
#'
#' @import ggplot2
#' @importFrom ggridges geom_density_ridges
#' @importFrom reshape2 melt
#' @importFrom stringr str_starts
#'
#' @export
#'
plot_growth_ridges<-function(M,showPlot=FALSE){
  lst = list();
  y <- NULL
  n  <- length(M)
  mdf <- NULL
  if(length(M[[1]]$growth_matrix_1_1)>0) {
    for(i in 1:n) {
      A <- M[[i]]
      gms = names(A)[stringr::str_starts(names(A),"growth_matrix")];
      lstj = list();
      for (j in 1:length(gms)){
        g_mat<-A[[gms[j]]];
        rownames(g_mat)<-A$mid_points
        colnames(g_mat)<-A$mid_points
        in_g<-data.frame(reshape2::melt(g_mat))
        colnames(in_g)<-c("Postmolt","Premolt","Density")

        df =NULL;
        if (!is.null(A$iMoltIncSex))
          df <- data.frame(Model = names(M)[i],
                           Sex      = as.factor(A$iMoltIncSex),
                           molt_inc = A$dMoltInc,
                           Premolt  = A$dPreMoltSize,
                           Postmolt = A$dPreMoltSize + A$dMoltInc,
                           type = 'obs')

        p <- ggplot(in_g) +
               geom_density_ridges(aes(x=Postmolt, y=Premolt, height = Density, group=Premolt,
                                       fill=after_stat(y),alpha=.9999),stat = "identity",scale=3) +
                scale_fill_viridis_c() +
                labs(x="Postmolt size (mm)",y="Premolt size (mm)", subtitle=paste0(names(M)[i],": ",gms[j])) +
                .THEME +
                theme(legend.position = "none",
                      axis.text.x = element_text(angle = 90));
        if (!is.null(df)) p = p+geom_point(data=df,aes(y=Premolt,x=Postmolt));

        if (showPlot) print(p);

        lstj[[gms[j]]] = p;
      }#--j
      if (length(lstj)>0) lst[[names(M)[i]]] = lstj;
    }#--i
  }
  return(lst)
}
