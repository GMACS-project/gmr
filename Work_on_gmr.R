#  Work on the package

# Ignore specific files
ignores <- c(
  "Work_on_gmr.R",
  "README.Rmd",
  "NEWS.RMD"
)
usethis::use_git_ignore(ignores, directory = ".")
