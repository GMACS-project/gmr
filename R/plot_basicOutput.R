
#' @title Plotting the basic outputs of GMACS
#'
#' @description Plots the basic outputs of GMACS.
#'
#' @param Stock vector of strings specifying the name(s) of the stock
#' considered in the analysis.
#' @param model_name vector of strings holding the name(s) of the
#' GMACS versions/models that is/are used in the analysis.
#' @param Dir vector of strings containing the directories for all
#' \code{GMACS_version} used in this analysis.
#' @param GMACSFileName name of the \code{gmacs.dat} file.
#' Default \code{"gmacs.dat"}.
#' @param out (logical); if \code{TRUE}; the plot will be saved on the \code{Dir.out}
#' directory.
#' @param Dir.out (character string); path to the directory to save plots
#' @param save.out (character string);
#' @param out.format Device to use to save the plot (currently only "pdf")
#'
#' @export
#
plot_basicOutput <- function(Stock = NULL,
                             Dir = NULL,
                             model_name = NULL,
                             GMACSFileName = "gmacs.dat",
                             out = TRUE,
                             Dir.out = NULL,
                             save.out = NULL,
                             out.format = NULL){

  fn       <- file.path(Dir, "gmacs")
  M        <- lapply(fn, read_admb) #need .prj file to run gmacs and need .rep file here
  names(M) <- ScenarioNames <- model_name

  # Specificities for the plot
  .THEME <<- ggplot2::theme_bw(base_size = 12, base_family = "") +
    ggplot2::theme(strip.text.x = ggplot2::element_text(margin= ggplot2::margin(1,0,1,0)),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   strip.background = ggplot2::element_rect(color="white",fill="white"))

  .OVERLAY  <<- TRUE

  # Management table

  PlotTab<- data.frame(Model=ScenarioNames,
                       MMB=rep(0,length(ScenarioNames)),
                       B35=rep(0,length(ScenarioNames)),
                       F35=rep(0,length(ScenarioNames)),
                       FOFL=rep(0,length(ScenarioNames)),
                       OFL=rep(0,length(ScenarioNames)),
                       Status=rep(0,length(ScenarioNames)))

  for(x in 1:length(M)){
    PlotTab$MMB[x]<-M[[x]]$ssb[length(M[[x]]$ssb)]
    PlotTab$B35[x]<-M[[x]]$spr_bmsy
    PlotTab$F35[x]<-M[[x]]$sd_fmsy[1]
    PlotTab$FOFL[x]<-M[[x]]$sd_fofl[1]
    PlotTab$OFL[x]<-M[[x]]$spr_cofl
    PlotTab$Status[x]<- PlotTab$MMB[x]/PlotTab$B35[x]
  }

  rownames(PlotTab)<- NULL
  PlotTab[,c(2:6)]<-round(PlotTab[,c(2:6)],3)
  knitr::kable(PlotTab[,1:6],split.cells=c(25,rep(7,5)),
               justify=c("left",rep("center",5)),
               caption="\\label{stepchange}Changes in management quantities \n
for each model considered. Reported management quantities are derived
from maximum likelihood estimates.")

  # Read Data file

  GMACSFiles <- readGMACS.dat(path = file.path(Dir[1], GMACSFileName))
  DatFileName <- file.path(Dir[1], GMACSFiles$DatFileName)
  CtlFileName <- file.path(Dir[1], GMACSFiles$CtlFileName)
  PrjFileName <- file.path(Dir[1], GMACSFiles$PrjFileName)
  nyrRetro <- GMACSFiles$N_Year_Retro

  Dat <- readGMACSdat(FileName = DatFileName,verbose = TRUE)
  Ctl <- readGMACSctl(FileName = CtlFileName, DatFile = Dat, nyrRetro = nyrRetro)
  PRJ <- readGMACSprj(FileName = PrjFileName, verbose = TRUE)

  .SEX      <<- c("Aggregate","Male","Female")
  .TYPE     <<- c("Retained","Discarded","Total")
  .SHELL    <<- c("New","Old")
  .MATURITY <<- c("Aggregate","Mature","Immature")

  # .FLEET    <- c(Dat$F_Fleet_names, Dat$Survey_names)
  .FLEET    <<- Dat$F_Fleet_names
  .SEAS   <<- as.character(1:Dat$N_seasons)



  ABC_buffer  <<- PRJ$ABCBuffer
  chosen_ind <<- 1
  OFL_fleet_ind <<-0

  plot.res <- 300
  .height <- 4.2
  .width <- 6.2

  if (out.format != "pdf") {
    .height <- .height * plot.res
    .width <- .width * plot.res
  }

  if (save.out) {
    if (out.format == "pdf") {
      grDevices::cairo_pdf(
        filename = file.path(Dir.out,"GMACS_outputs.pdf"),
        fallback_resolution = plot.res,
        height = .height,
        width = .width, onefile = T
      )

      if(length(Dat$Survey_names)>1 && Dat$Survey_names != "")
      {  for(s in 1:length(Dat$Survey_names)){
        nam <- Dat$Survey_names[s]
        plot_cpue(M, ShowEstErr = TRUE, Dat$Survey_names[s], ylab = "Survey biomass")
      }
      }
      plot_catch(M)
      plot_F(M)
      plot_growth_inc(M)
      plot_molt_prob(M)
      plot_selectivity(M)
      plot_recruitment(M)
      plot_ssb(M)
      plot_recruitment_size(M)
      for(i in 1:length(Dat$F_Fleet_names)){
        plot_kobe(M,fleet_in=Dat$F_Fleet_names[i],ref_ind=1)
      }
      dev.off()
    }
  } else {
    #
  }
  #
  # if(length(Dat$Survey_names)>1 && Dat$Survey_names != "")
  # {  for(s in 1:length(Dat$Survey_names)){
  #   nam <- Dat$Survey_names[s]
  #   plot_cpue(M, ShowEstErr = TRUE, Dat$Survey_names[s], ylab = "Survey biomass")
  # }
  # }
  #
  # plot_catch(M)
  #
  # plot_F(M)
  #
  # plot_growth_inc(M)
  #
  # plot_molt_prob(M)
  #
  # plot_selectivity(M)
  #
  # plot_recruitment(M)
  #
  # plot_ssb(M)
  #
  # plot_recruitment_size(M)
  #
  # for(i in 1:length(Dat$F_Fleet_names)){
  #   plot_kobe(M,fleet_in=Dat$F_Fleet_names[i],ref_ind=1)
  # }
}
