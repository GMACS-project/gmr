plot_growth_ridges<-function(M)
{
  y <- NULL
  n  <- length(M)
  mdf <- NULL
  if(length(M[[1]]$growth_matrix_1_1)>0)
  {
    for(i in 1:n)
    {
      A <- M[[i]]
      g_mat<-A$growth_matrix_1_1
      rownames(g_mat)<-A$mid_points
      colnames(g_mat)<-A$mid_points
      in_g<-data.frame(reshape2::melt(g_mat))
      colnames(in_g)<-c("Postmolt","Premolt","Density")

      df <- data.frame(Model = names(M)[i],
                       Sex   = as.factor(A$iMoltIncSex),
                       molt_inc   = A$dMoltInc,
                       Premolt  = A$dPreMoltSize,
                       Postmolt = A$dPreMoltSize + A$dMoltInc,
                       type = 'obs')

      p <- ggplot(in_g)
      p <- p + geom_density_ridges(ggplot2::aes(x=Postmolt, y=Premolt, height = Density, group=Premolt,
                                       fill=stat(y),alpha=.9999),stat = "identity",scale=3) +
        ggplot2::scale_fill_viridis_c()+
        .THEME +
        ggplot2::theme(legend.position = "none",
              axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::geom_point(data=df,ggplot2::aes(y=Premolt,x=Postmolt))

      print(p)

    }

  }
}
