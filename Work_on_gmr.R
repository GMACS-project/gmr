#  Work on the package

# Ignore specific files
ignores <- c(
  "Work_on_gmr.R",
  "README.Rmd",
  "NEWS.RMD"
)
usethis::use_git_ignore(ignores, directory = ".")

# Scan the source files within a package for attributes and generate code as
# required. Generates the bindings required to call C++ functions from R for
# functions adorned with the Rcpp::export attribute.
Rcpp::compileAttributes()

# Document functions
devtools::document()
roxygen2::roxygenise()

# Add vignettes
# usethis::use_vignette(name = "the-basics-optimizing-deb-parameters", title = "The Basics - Optimizing DEB parameters")

# Build articles
pkgdown::build_articles()

# Build site
pkgdown::build_site()

# Use Github Actions
usethis::use_github_actions()

usethis::use_github_action_check_standard()
usethis::use_github_action("check-standard")

usethis::use_github_actions_badge(name = "R-CMD-check.yaml")
