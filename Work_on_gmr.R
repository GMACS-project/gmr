#  Work on the package

# Ignore specific files
ignores <- c(
  "Work_on_gmr.R",
  "README.Rmd",
  "NEWS.RMD"
)
usethis::use_git_ignore(ignores, directory = ".")


# Build vignette
usethis::use_vignette("001_Use_gmr_to_Run_GMACS")


# Build web site
pkgdown::build_site()
