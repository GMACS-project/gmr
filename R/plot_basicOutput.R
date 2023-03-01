
#' @title Plotting the basic outputs of GMACS
#'
#' @description Plots the basic outputs of GMACS.
#'
#' @param Stock (character string)- Specifies the name(s) of the stock
#' considered. if \code{Stock='all'}, then the function will plot the basic
#' outputs for all stocks that are "available" in the \code{"build"} directory.
#' @param model_name (character string)- vector of strings specifying the name(s)
#' of the GMACS version(s)/model(s) that is/are considered for plotting.
#' @param Dir (character string)- vector of folder names of length \code{model_name} for all
#' \code{GMACS_version/model} for which you want to plot the outputs. Each \code{Dir}
#' correspond to the root folder where the \code{"build"} folder is stored.
#' @param out (logical)- if \code{TRUE}; the plots will be saved in the \code{Dir.out}
#' directory.
#' @param Dir.out (character string)- Path directory where to save the plots. If
#' \code{Stock='all'}, \code{Dir.out} is a vector of length equal to the number of
#' stocks "available" in the \code{"build"} directory.
#' (Warning: The function create a "plot" folder where outputs are saved).
#' @param save.out (logical)- if \code{TRUE}, then the plots are saved using the
#' format indicated in the \code{out.format} argument. If \code{FALSE}, then plots
#' will be available in the plot windows of R.
#' @param out.format (character string)- Device to use to save the plot (\code{"pdf"}, \code{"png"}, \code{"jpeg"})
#'
#' @export
#
plot_basicOutput <- function(Stock = NULL,
                             Dir = NULL,
                             model_name = NULL,
                             out = TRUE,
                             Dir.out = NULL,
                             save.out = NULL,
                             out.format = NULL) {


  verbose <- FALSE
  GMACSFileName <- "gmacs.dat"


  # I. Characteristics for the plot ----
  .THEME <<-
    ggplot2::theme_bw(base_size = 12, base_family = "") +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(margin = ggplot2::margin(1, 0, 1, 0)),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(color = "white", fill =
                                                 "white")
    ) +
    ggplot2::theme(legend.position = "bottom")

  .OVERLAY  <<- TRUE

  plot.res <- 300
  .height <- 5.2
  .width <- 6.2

  if (out.format != "pdf") {
    .height <- .height * plot.res
    .width <- .width * plot.res
  } else {
    .height <- 5.2 * 1.3
    .width <- 6.2 * 1.4
  }
  # -----------------------------------------------------------------------

  # II. Set the function to plot ----
  do_plot <- function(Stock = NULL,
                      Dir = NULL,
                      Dir.out = NULL){
    # 1. Define directories - Load results - read GMACS files ----

    # 1.1. Define Stock specific directories
    for(d in 1:length(Dir)){
      Dir[d] <- file.path(Dir[d], "build", Stock)
    }

    # 1.2. Define directory where to save the plots
    Dir.out <- file.path(Dir.out, "plots")
    if (!file.exists(Dir.out))
      dir.create(path = Dir.out, recursive = TRUE)

    # 1.3. Load results
    fn       <- file.path(Dir, "gmacs")
    M        <-
      lapply(fn, read_admb) # need .prj file to run gmacs and need .rep file here
    names(M) <-
      ScenarioNames <-
      model_name

    # 1.4. Read Gmacs files
    GMACSFiles <-
      readGMACS.dat(path = file.path(Dir[1], GMACSFileName),verbose = verbose)
    DatFileName <-
      file.path(Dir[1], GMACSFiles$DatFileName)
    CtlFileName <-
      file.path(Dir[1], GMACSFiles$CtlFileName)
    PrjFileName <-
      file.path(Dir[1], GMACSFiles$PrjFileName)
    nyrRetro <- GMACSFiles$N_Year_Retro

    Dat <-
      readGMACSdat(FileName = DatFileName, verbose = verbose)
    Ctl <-
      readGMACSctl(FileName = CtlFileName,
                   DatFile = Dat,
                   nyrRetro = nyrRetro,
                   verbose = verbose)
    PRJ <-
      readGMACSprj(FileName = PrjFileName, verbose = verbose)
    # -------------------------------------------------------------------------


    # 2. Define variables needed for plotting ----
    .SEX      <<- c("Aggregate", "Male", "Female")
    .TYPE     <<- c("Retained", "Discarded", "Total")
    .SHELL    <<- c("New", "Old")
    .MATURITY <<- c("Aggregate", "Mature", "Immature")

    .All_FLEET    <<- c(Dat$F_Fleet_names, Dat$Survey_names)
    .All_FLEET <<- .All_FLEET[!.All_FLEET==""]
    .FLEET    <<- Dat$F_Fleet_names
    .SEAS   <<- as.character(1:Dat$N_seasons)

    ABC_buffer  <<- PRJ$ABCBuffer
    chosen_ind <<- 1
    OFL_fleet_ind <<- 0
    # -------------------------------------------------------------------------


    # 3. Management table ----
    PlotTab <- data.frame(
      Model = ScenarioNames,
      MMB = rep(0, length(ScenarioNames)),
      B35 = rep(0, length(ScenarioNames)),
      F35 = rep(0, length(ScenarioNames)),
      FOFL = rep(0, length(ScenarioNames)),
      OFL = rep(0, length(ScenarioNames)),
      Status = rep(0, length(ScenarioNames))
    )
    for (x in 1:length(M)) {
      PlotTab$MMB[x] <- M[[x]]$ssb[length(M[[x]]$ssb)]
      PlotTab$B35[x] <- M[[x]]$spr_bmsy
      PlotTab$F35[x] <- M[[x]]$sd_fmsy[1]
      PlotTab$FOFL[x] <- M[[x]]$sd_fofl[1]
      PlotTab$OFL[x] <- M[[x]]$spr_cofl
      PlotTab$Status[x] <-
        PlotTab$MMB[x] / PlotTab$B35[x]
    }
    rownames(PlotTab) <- NULL
    PlotTab[, c(2:6)] <- round(PlotTab[, c(2:6)], 3)
    Mgt_table <- PlotTab

    #   Mgt_table <- knitr::kable(
    #     PlotTab[, 1:6],
    #     split.cells = c(25, rep(7, 5)),
    #     justify = c("left", rep("center", 5)),
    #     caption = "\\label{stepchange}Changes in management quantities \nfor each version/model considered.\nReported management quantities are derived
    # from maximum likelihood estimates."
    #   )
    # -------------------------------------------------------------------------


    # 4. Save plots ----
    if (!save.out || save.out && out.format == "pdf") {
      if (save.out && out.format == "pdf")
        grDevices::cairo_pdf(
          filename = file.path(Dir.out, "GMACS_outputs.pdf"),
          fallback_resolution = plot.res,
          height = .height,
          width = .width,
          onefile = T
        )

      if (length(Dat$Survey_names) > 1 &&
          Dat$Survey_names != "")
      {
        for (s in 1:length(Dat$Survey_names)) {
          nam <- Dat$Survey_names[s]
          plot_cpue(M,
                    ShowEstErr = TRUE,
                    Dat$Survey_names[s],
                    ylab = "Survey biomass")
          plot_cpue_res(M,
                        Dat$Survey_names[s],
                        ylab = "Survey biomass")
        }
      }
      plot_size_comps(M)
      plot_catch(M)
      plot_F(M)
      if (Dat$GrowthObsType != 0)
        plot_growth_inc(M)
      plot_molt_prob(M)
      plot_selectivity(M)
      plot_natural_mortality(M)
      plot_recruitment(M)
      plot_recruitment_size(M)
      plot_ssb(M)
      for (i in 1:length(Dat$F_Fleet_names)) {
        plot_kobe(M,
                  fleet_in = Dat$F_Fleet_names[i],
                  ref_ind = 1)
      }

      if (save.out && out.format == "pdf")
        dev.off()

    } else {
      # Survey data
      if (length(Dat$Survey_names) > 1 &&
          Dat$Survey_names != "")
      {
        for (s in 1:length(Dat$Survey_names)) {
          nam <- Dat$Survey_names[s]
          cpue <- plot_cpue(
            M,
            subsetby = Dat$Survey_names[s],
            ShowEstErr = TRUE,
            ylab = "Survey biomass"
          )
          # cpue <- cpue + BaseThemeGMR()

          cpue_res <- plot_cpue_res(M,
                                    Dat$Survey_names[s],
                                    ylab = "Survey biomass")
          # cpue_res <- cpue_res + BaseThemeGMR()

          ggplot2::ggsave(
            units = "px",
            width = .width * 1.4,
            height = .height,
            dpi = plot.res,
            filename = paste0("cpue_", Dat$Survey_names[s], ".", out.format),
            path = file.path(Dir.out),
            plot = cpue,
            device = out.format
          )
          ggplot2::ggsave(
            units = "px",
            width = .width * 1.4,
            height = .height,
            dpi = plot.res,
            filename = paste0("cpue_res_", Dat$Survey_names[s], ".", out.format),
            path = file.path(Dir.out),
            plot = cpue_res,
            device = out.format
          )
        }
      }

      # Catch
      {
        catch <- plot_catch(M)
        # catch <- catch + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.5,
          height = .height * 1.1,
          dpi = plot.res,
          filename = paste0("catch.", out.format),
          path = file.path(Dir.out),
          plot = catch,
          device = out.format
        )
      }

      # Size composition
      {
        # Size_Comp <- plot_size_comps(M)
        base::invisible(utils::capture.output(Size_Comp <- plot_size_comps(M)))
        for (sc in 1:length(Size_Comp)) {
          # Size_Comp[[sc]] <- Size_Comp[[sc]] + BaseThemeGMR()
          ggplot2::ggsave(
            units = "px",
            width = .width * 1.5,
            height = .height * 1.1,
            dpi = plot.res,
            filename = paste0("Size_Comp_", sc, ".", out.format),
            path = file.path(Dir.out),
            plot = Size_Comp[[sc]],
            device = out.format
          )
        }
      }

      # Fishery
      {
        Fish_F <- plot_F(M)
        # Fish_F <- Fish_F + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.4,
          height = .height * 1.1,
          dpi = plot.res,
          filename = paste0("fishing_mortality.", out.format),
          path = file.path(Dir.out),
          plot = Fish_F,
          device = out.format
        )
      }

      # Selectivity
      {
        sel <- plot_selectivity(M)
        # sel <- sel + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.3,
          height = .height * 1.1,
          dpi = plot.res,
          filename = paste0("selectivity.", out.format),
          path = file.path(Dir.out),
          plot = sel,
          device = out.format
        )
      }

      # nat Mort
      {
        Nat_M <- plot_natural_mortality(M)
        # Nat_M <- Nat_M + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.4,
          height = .height,
          dpi = plot.res,
          filename = paste0("Natural_mortality.", out.format),
          path = file.path(Dir.out),
          plot = Nat_M,
          device = out.format
        )
      }

      # Recruitment
      {
        rec <- plot_recruitment(M)
        # rec <- rec + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.1,
          height = .height,
          dpi = plot.res,
          filename = paste0("recruit.", out.format),
          path = file.path(Dir.out),
          plot = rec,
          device = out.format
        )
      }

      # Recruitment size
      {
        rec_size <- plot_recruitment_size(M)
        # rec_size <- rec_size + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.1,
          height = .height,
          dpi = plot.res,
          filename = paste0("recruit_Size.", out.format),
          path = file.path(Dir.out),
          plot = rec_size,
          device = out.format
        )
      }

      #SSB
      {
        ssb <- plot_ssb(M, alpha = 0.2)
        # ssb <- ssb + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.4,
          height = .height * 1.1,
          dpi = plot.res,
          filename = paste0("ssb.", out.format),
          path = file.path(Dir.out),
          plot = ssb,
          device = out.format
        )
      }

      # Growth increment
      if (Dat$GrowthObsType != 0) {
        Grw_Inc <- plot_growth_inc(M)
        # Grw_Inc <- Grw_Inc + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.4,
          height = .height * 1.1,
          dpi = plot.res,
          filename = paste0("Growth_Inc.", out.format),
          path = file.path(Dir.out),
          plot = Grw_Inc,
          device = out.format
        )
      }

      # Probability of molting
      {
        Molt_prob <- plot_molt_prob(M)
        # Molt_prob <- Molt_prob + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.4,
          height = .height * 1.1,
          dpi = plot.res,
          filename = paste0("Molting_prob.", out.format),
          path = file.path(Dir.out),
          plot = Molt_prob,
          device = out.format
        )
      }

      for (i in 1:length(Dat$F_Fleet_names)) {
        kobe <- NULL
        kobe <- plot_kobe(M,
                          fleet_in = Dat$F_Fleet_names[i],
                          ref_ind = 1)
        # kobe <- kobe + BaseThemeGMR()
        ggplot2::ggsave(
          units = "px",
          width = .width * 1.4,
          height = .height * 1.1,
          dpi = plot.res,
          filename = paste0("Kobe_", Dat$F_Fleet_names[i], ".", out.format),
          path = file.path(Dir.out),
          plot = kobe,
          device = out.format
        )
      }
      dev.off(which = dev.cur())
    }
    # -------------------------------------------------------------------------

    return(Mgt_table)
  }
  # -----------------------------------------------------------------------

  # III. Apply the do_plot function to the Stock variable ----
  Out_table <- NULL

  if(Stock == "all" && length(Stock) == 1 || length(Stock) >1){

    if(Stock == "all"  && length(Stock) == 1){
      namDir <- stringr::str_split(string = Dir, pattern = "build")
      nam <- list.files(file.path(namDir[[1]][1], "build"))
      nam <- nam[!nam%in%c("release","debug")]
    } else {
      nam <- Stock
    }

    for(n in 1:length(nam)){
      cat(paste0("- Plotting :", nam[n]))
      cat("\n")

      out <- do_plot(Stock = nam[n],
                     Dir = Dir,
                     Dir.out = Dir.out[n])
      eval(parse(text = paste(
        "Out_table$",nam[n] ," <- out", sep=""
      )))
    }

  } else {
    cat(paste0("- Plotting :", Stock))
    cat("\n")

    out <- do_plot(Stock = Stock,
                   Dir = Dir,
                   Dir.out = Dir.out)

    eval(parse(text = paste(
      "Out_table$",Stock ," <- out", sep=""
    )))
  }

  Mgt_table <<- Out_table
  cat("\nChanges in management quantities for each version/model considered are stored in the 'Mgt_table' object.\n")
  # -----------------------------------------------------------------------

}
