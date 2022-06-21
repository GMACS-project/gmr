#' Plot growth from arbitrary start age
#'
#' @param M List object created by read_admb function
#' @return Plot natural mortality over time and size
#' @export
#'
plot_growth <- function(M){
  A    <- M
  df   <- data.frame(A$mean_size)
  nclass<-length(df[1,])
  colnames(df) <- 1:nclass
  nrow   <- dim(df)[1]
  # Always saves for both sexes???
  df$sex <- c(rep(1,length=nrow/2),rep(2,length=nrow/2))
  mdf    <- reshape2::melt(df,id=c("sex"))

  p <- ggplot2::ggplot(mdf,ggplot2::aes(x=as.factor(variable),y=value))
  p <- p + ggplot2::geom_line(ggplot2::aes(as.numeric(variable),value),stat="identity")
  p <- p + ggplot2::labs(x="Time (years)",y="Mean size (mm)")
  p <- p + ggplot2::facet_wrap(~sex,scales="free")

  #nyr <- nclass
  #df<- data.frame(A$growth_matrix)
  #df$sex <- c(rep(1,length=nrow/2),rep(2,length=nrow/2))
  #df$time <- 1:nyr
  #mdf    <- melt(df,id=c("sex","time"))
  #p2 <- ggplot(mdf,aes(x=time,y=as.double(variable),z=value))
  #p2 <- p2 + geom_tile(aes(fill = value))
  #p2 <- p2 + stat_contour(geom="polygon", aes(fill=(value)))
  #p2 <- p2 + labs(x="time",y="size bin",fill="Density")
  #p2 <- p2 + facet_wrap(~sex,scale="free")
#
#  plot_multiple(p2,p)

  geom_density_ridges()

  return(p)
}
