message("Starting loading of libraries...")
rm(list = ls(all = TRUE))
lib_list <- c("shiny",
              "devtools",
              "shinydashboard",
              "IQRtools",
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
              "ggplot2",
              "ggplotify",
              "grDevices",
              "zip",
              "Cairo",
              "magrittr",
              "tools",
              "stringr",
              "purrr",
              "plotly",
              "xgxr",
              "gridExtra",
              "ggforce",
              "tidyverse",
              "shinyFiles",
              "fontquiver",
              "svglite",
              "haven",
              "officer",
              "flextable",
              "ggsci",
              "promises",
              "future")
lib_vers <- c('1.6.0',
              '2.3.2',
              '0.7.1',
              '1.8.0',
              '0.17',
              '1.14.0',
              '0.1.3',
              '1.4.2',
              '3.0.1',
              '1.3.1',
              '1.0.0',
              '0.5.7',
              '2.0.0',
              '0.61',
              '0.2.2',
              '1.0.5',
              '3.3.5',
              '0.0.5',
              '3.6.3',
              '2.1.1',
              '1.5.12.2',
              '2.0.1',
              '3.6.3',
              '1.4.0',
              '0.3.4',
              '4.9.3',
              '1.1.0',
              '2.3',
              '0.3.3',
              '1.3.1',
              '0.9.0',
              '0.2.1',
              '2.0.0',
              '2.3.1',
              '0.3.17',
              '0.6.3',
              '2.9',
              '1.2.0.1',
              '1.21.0')
names(lib_vers) <- lib_list
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
      package_vers <- lib_vers[[package_name]]
      message(paste("installing packge",package_name,"Version",package_vers))
      if (package_name == "IQRtools"){
        source("https://iqrtoolsabc321.intiquan.com/install.R")
        installVersion.IQRtools("1.8.0")
      }
      else if (package_name == "devtools") {
        install.packages("devtools")
      }
      else {
        devtools::install_version(package_name,
                                  version = package_vers,
                         repos = c("http://lib.stat.cmu.edu/R/CRAN",
                                 "https://cran.r-project.org/"),
                         upgrade = "never",
                         dependencies = T)
      }
    }
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}
load_or_install(lib_list)
message("finished loading libraries...")
