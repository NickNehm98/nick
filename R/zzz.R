#' Package Load Function
#'
#' Automatically installs and loads required packages when the package is loaded into the namespace.
#'
#' @param libname The library path.
#' @param pkgname The package name.
.onLoad <- function(libname, pkgname) {
  required_packages <- c("tidyverse", "psych", "car", "rempsyc", "apa", "report",
                         "apaTables", "datawizard",  "caret", "papaja", "jtools",
                         "patchwork", "scales", "ggforce", "ggthemes", "cowplot")
  # Function to install and load packages
  install_and_load_packages <- function(packages) {
    for (package in packages) {
      if (!require(package, character.only = TRUE)) {
        message(paste("Installing missing package:", package))
        install.packages(package, dependencies = TRUE)
      }
      library(package, character.only = TRUE)
    }
  }

  # Call the utility function
  install_and_load_packages(required_packages)
  packageStartupMessage("Required packages have been installed and loaded.")
}
