message("Starting loading of libraries...")
rm(list = ls(all = TRUE))
lib_list <- c("shiny",
              "devtools",
              "cowplot",
              "tidyr",
              "IQRtools",
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
lib_vers <- c('1.6.0', '2.3.2', '1.1.1', '1.1.3', '1.8.0', '0.7.1', '0.17', '1.14.0', '0.1.3', '1.4.2', '3.0.1', '1.3.1', '1.0.0', '0.5.7', '2.0.0', '0.61', '0.2.2', '1.0.5', '3.3.5', '0.0.5', '3.6.3', '2.1.1', '1.5.12.2', '2.0.1', '3.6.3', '1.4.0', '0.3.4', '4.9.3', '1.1.0', '2.3', '0.3.3', '1.3.1', '0.9.0', '0.2.1', '2.0.0', '2.3.1', '0.3.17', '0.6.3', '2.9', '1.2.0.1', '1.21.0')
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
      if (package_name == "IQRtools"){
        source("https://iqrtoolsabc321.intiquan.com/install.R")
        IQR_dependencies <- c("nlme",
                              "survival",
                              "cluster",
                              "foreign",
                              "nnet",
                              "reshape2",
                              "Rcpp",
                              "foreach",
                              "doParallel",
                              "rootSolve",
                              "deSolve",
                              "numDeriv",
                              "brglm2",
                              "PopED",
                              "pdftools",
                              "fpc",
                              "writexl",
                              "kableExtra",
                              "ResourceSelection")
        lapply(IQR_dependencies[!is_installed(IQR_dependencies)],install.packages)
        installVersion <- package_vers
        # Download IQR Tools
        url <- paste0("http://iqrtoolsabc321.intiquan.com/rrepo/src/contrib/IQRtools_",installVersion,".tar.gz")
        destfile <- paste0("IQRtools_",installVersion,".tar.gz")
        
        doit <- TRUE
        while (doit) {
          cat()
          doit <- tryCatch({
            download.file(url,destfile,mode="wb",method=methodDownload)
            FALSE
          }, error = function(err) {
            TRUE
          })
        }
        
        # Install IQR Tools
        install.packages(destfile, repos=NULL, type="source", clean = TRUE,dependencies = F)
        unlink(destfile)
        
        # Install cOde and dMod if needed
        x <- installed.packages()
        path_cOdedMod <- paste0(x[rownames(x)=="IQRtools",colnames(x)=="LibPath"],"/IQRtools/dependencies")[1]
        cOde <- list.files(path_cOdedMod,pattern = "cOde_*",full.names = TRUE)
        dMod <- list.files(path_cOdedMod,pattern = "dMod_*",full.names = TRUE)
        if (length(cOde) == 1) {
          install.packages(cOde,repos=NULL,type="source")
        }
        if (length(dMod) == 1) {
          install.packages(dMod,repos=NULL,type="source")
        }
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
