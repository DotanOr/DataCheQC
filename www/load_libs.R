lib_list <- c("shiny","shinydashboard","IQRtools","DT","shinycssloaders","shinyWidgets","shinyjs","shinyBS","shinybusy", "dplyr", "ggplot2","ggplotify", "grDevices", "zip", "Cairo", "magrittr","tools","stringr","purrr","plotly","xgxr","gridExtra","ggforce","tidyverse","shinyFiles","fontquiver","svglite","haven","officer","flextable","ggsci","promises","future")
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
      install.packages(package_name, 
                       repos=c("http://lib.stat.cmu.edu/R/CRAN",
                               "https://iqrtools.intiquan.com/rrepo",
                               "https://cran.r-project.org/"))
    }
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}
load_or_install(lib_list)
message("finished loading libraries...")
