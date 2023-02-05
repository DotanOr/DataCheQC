message("Starting loading of libraries...")
rm(list = ls(all = TRUE))
lib_list <- c("shiny",
              "devtools",
              "fontawesome",
              "bvpSolve",
              "trust",
              "pROC",
              "ggplot2",
              "broom",
              "haven",
              "cowplot",
              "tidyr",
              "shinydashboard",
              "DT",
              "data.table",
              "ipc",
              "glue",
              "cli",
              "readxl",
              "shinycssloaders",
              "shinyWidgets",
              "shinyjs",
              "shinyBS",
              "shinybusy",
              "dplyr",
              "ggplotify",
              "grDevices",
              "zip",
              "Cairo",
              "magrittr",
              "tools",
              "stringr",
              "purrr",
              "plotly",
              "gridExtra",
              "ggforce",
              "tidyverse",
              "shinyFiles",
              "fontquiver",
              "svglite",
              "officer",
              "flextable",
              "ggsci",
              "promises",
              "future")
lib_vers <- c('1.7.4', '2.2.2', '0.5.0', '1.4.3', '0.1.8', '1.16.1', '3.4.0', '1.0.3', '2.5.1', '1.0.0', '1.3.0', '0.7.1', '0.12', '1.14.6', '0.1.3', '1.6.2', '3.6.0', '1.3.1', '1.0.0', '0.5.7', '2.1.0', '0.61', '0.2.2', '1.1.0', '0.0.5', '3.6.3', '2.1.1', '1.5.11', '2.0.3', '3.6.3', '1.5.0', '1.0.1', '4.10.1', '2.3', '0.3.3', '1.3.2', '0.9.0', '0.2.1', '1.2.3', '0.3.17', '0.6.3', '2.9', '1.2.0.1', '1.31.0')
names(lib_vers) <- lib_list
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
      package_vers <- lib_vers[[package_name]]
      message(paste0("installing package ",package_name,", version ",package_vers))
      if (package_name == "devtools") {
        install.packages("devtools")
      }
      else {
        devtools::install_version(package_name,
                                  version = package_vers,
                                  repos = "https://cran.r-project.org/",
                                  upgrade = "never",
                                  dependencies = T)
      }
    }
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}
load_or_install(lib_list)
message("finished loading libraries...")
