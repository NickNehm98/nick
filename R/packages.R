#' Install and Load Required Packages
#'
#' This code checks for the installation of required packages and installs any missing ones.
#' It then loads all the specified packages into the R session.
#'
#' @name install_load_packages
#' @param packages A character vector of package names to check, install, and load.
#' @details If a package is not installed, it will be installed with all its dependencies.
#' Messages will indicate any missing packages that are being installed.
#' @examples
#' packages <- c("ggplot2", "dplyr")
#' for (package in packages) {
#'   if (!require(package, character.only = TRUE)) {
#'     install.packages(package, dependencies = TRUE)
#'   }
#'   library(package, character.only = TRUE)
#' }
NULL

#Install necessary packages
packages = c("tidyverse", "psych", "car", "rempsyc", "apa", "report",
             "apaTables", "datawizard",  "caret", "papaja", "jtools",
             "patchwork", "scales", "ggforce", "ggthemes", "cowplot")

#install and/or load packages
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    message(paste("Installing missing package:", package))
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}
