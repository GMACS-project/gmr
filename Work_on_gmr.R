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


# Use Github Actions
usethis::use_github_actions()

usethis::use_github_action_check_standard()
usethis::use_github_action("check-standard")

usethis::use_github_actions_badge(name = "R-CMD-check.yaml")
