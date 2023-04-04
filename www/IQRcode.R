### IQRtools 1.8.0 source code
#### with some alterations to allow repacking using packrat

# resolve conflicts with package
if(any(search()=="package:IQRtools")) detach("package:IQRtools", unload = T)
#'@export
packageR_IQRtools <- function () {
  info <- list()
  info$R$platform <- version$platform
  info$R$version <- paste0(version$major,".",version$minor)
  info$R$packages <- data.frame(
    PACKAGE = utils::installed.packages()[,"Package"],
    VERSION = utils::installed.packages()[,"Version"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  content <- aux_fileread(paste0(getRepos(),"/src/contrib"))
  filenames <- sapply(info$R$packages$PACKAGE, function (pkg) {
    m <- gregexpr(paste0("\"",pkg,"_[^>]+"),content)
    filename <- regmatches(content,m)[[1]]
    suppressWarnings(filename <- max(filename))
    filename <- gsub("\"","",filename)
    filename
  })
  info$R$packages$FILENAME <- filenames
  Probable_RBASE_packages_NA <- info$R$packages[is.na(info$R$packages$FILENAME) & info$R$packages$VERSION == info$R$version,]
  NON_RBASE_packages_NA <- info$R$packages[is.na(info$R$packages$FILENAME) & info$R$packages$VERSION != info$R$version,]
  if (nrow(Probable_RBASE_packages_NA) > 0) {
    cat("The following packages are not available for download (seem to be BASE R packages):\n")
    print(Probable_RBASE_packages_NA)
  }
  if (nrow(NON_RBASE_packages_NA) > 0) {
    cat("The following packages are not available for download (please check it should be OK):\n")
    print(NON_RBASE_packages_NA)
  }
  info$R$packages <- info$R$packages[!is.na(info$R$packages$FILENAME),]
  aux_mkdir("rrepo")
  aux_mkdir("rrepo/src/contrib")
  dummy <- lapply(info$R$packages$FILENAME, function (file) {
    url <- paste0(getRepos(),"/src/contrib/",file)
    utils::download.file(url,destfile = paste0("rrepo/src/contrib/",file),mode = "wb")
  })
  aux_mkdir("rrepo/src/base/")
  url <- paste0(getRepos(),"/src/base/R-",version$major,"/R-",info$R$version,".tar.gz")
  utils::download.file(url,destfile = paste0("rrepo/src/base/","R-",info$R$version,".tar.gz"),mode = "wb")
  url <- paste0(getRepos(),"/src/base/INSTALL")
  utils::download.file(url,destfile = paste0("rrepo/src/base/INSTALL.txt"),mode = "wb")
  aux_mkdir("rrepo/bin/base")
  url <- paste0(getRepos(),"/bin/windows/base/R-",info$R$version,"-win.exe")
  utils::download.file(url,destfile = paste0("rrepo/bin/base/","R-",info$R$version,"-win.exe"),mode = "wb")
  rtoolsFile <- paste0("Rtools",version$major,floor(as.numeric(version$minor)),".exe")
  url <- paste0("https://cran.r-project.org/bin/windows/Rtools/",rtoolsFile)
  aux_mkdir("rrepo/Rtools/")
  utils::download.file(url,destfile = paste0("rrepo/Rtools/",rtoolsFile),mode = "wb")
  versionIQR <- aux_strrep(aux_version("IQRtools"),".9000","")
  url <- paste0("http://iqrtools.intiquan.com/rrepo/src/contrib/IQRtools_",versionIQR,".tar.gz")
  utils::download.file(url,destfile = paste0("rrepo/src/contrib/IQRtools_",versionIQR,".tar.gz"),mode = "wb")
  aux_mkdir("rrepo/dll/")
  file <- paste0("IQRtools_",versionIQR)
  url <- paste0("http://iqrtools.intiquan.com/rrepo/src/contrib/IQRtools_",versionIQR,".tar.gz")
  utils::download.file(paste0("http://iqrtools.intiquan.com/rrepo/dll/",file,".dll"),
                       paste0("rrepo/dll/IQRtools_",versionIQR,".dll"),mode = "wb")
  utils::download.file(paste0("http://iqrtools.intiquan.com/rrepo/dll/",file,".so"),
                       paste0("rrepo/dll/IQRtools_",versionIQR,".so"),mode = "wb")
  utils::download.file(paste0("http://iqrtools.intiquan.com/rrepo/dll/",file,".so_mac"),
                       paste0("rrepo/dll/IQRtools_",versionIQR,".so_mac"),mode = "wb")
  utils::download.file(paste0("http://iqrtools.intiquan.com/rrepo/dll/",file,".so_centos6"),
                       paste0("rrepo/dll/IQRtools_",versionIQR,".so_centos6"),mode = "wb")
  oldpath <- getwd()
  setwd("rrepo/src/contrib/")
  tools::write_PACKAGES(".", type="source")
  setwd(oldpath)
  utils::zip(zipfile = "rrepo.zip",files = "rrepo")
  unlink("rrepo",recursive = TRUE)
}
#' #'@export
#' support <- function () {
#'   utils::browseURL("https://groups.google.com/a/intiquan.com/g/iqr-tools-users-group")
#' }
#'@export
install_MIDDmodule <- function (module_number) {
  if (.Platform$OS.type=="windows") {
    methodDownload = "wininet"
  } else {
    methodDownload = "libcurl"
  }
  url <- paste0("https://training.intiquan.com/MIDDmodules/M",module_number,".zip")
  if (!url_exists(url)) stopIQR("Requested webinar module does not exist on server.")
  aux_mkdir("~/IntiQuan/Webinars")
  setwd("~/IntiQuan/Webinars")
  utils::download.file(url = url,
                       destfile = "webinardownload.zip",
                       mode="wb",
                       method=methodDownload)
  utils::unzip(zipfile = "webinardownload.zip",overwrite = TRUE)
  unlink("webinardownload.zip",force = TRUE)
  message(paste0("The Webinar Module ",module_number," material is installed in '~/IntiQuan/Webinars/",module_number,"'.\n\nEnjoy It!\n"))
}
#'@export
install_workshop <- function (workshop) {
  if (.Platform$OS.type=="windows") {
    methodDownload = "wininet"
  } else {
    methodDownload = "libcurl"
  }
  url <- paste0("https://training.intiquan.com/",workshop,"/material.zip")
  if (!url_exists(url)) stopIQR("Requested workshop does not exist on server.")
  setwd("~")
  utils::download.file(url = url,
                       destfile = "IntiQuan.zip",
                       mode="wb",
                       method=methodDownload)
  utils::unzip(zipfile = "IntiQuan.zip",overwrite = TRUE)
  unlink("IntiQuan.zip",force = TRUE)
  message(paste0("The workshop material is installed in '~/IntiQuan/.\n\nHappy Workshopping!\n"))
}
#' #'@export
#' doc_IQRtools <- function () {
#'   if (file.exists(system.file(package="IQRtools",paste0("docs/book/index.html")))) {
#'     location <- system.file(package="IQRtools",paste0("docs/book/index.html"))
#'   } else {
#'     location <- "https://iqrtools.intiquan.com"
#'   }
#'   utils::browseURL(location)
#' }
#'@export
examples_IQRtools <- function(path="~/IntiQuan"){
  dir <- NULL
  dir1 <- system.file(package="IQRtools","docs/material")
  dir2 <- "/IQDESKTOP/.help/iqrtools/docs/material/"
  if (dir.exists(dir1)) {
    dir <- dir1
  } else if (dir.exists(dir2)) {
    dir <- dir2
  }
  if (is.null(dir)) {
    message("Due to CRAN limitations the material is not available inside the public IQR Tools package.")
    message("To get all the examples in this manner, run IQR Tools from within IQ Desktop (https://iqdesktop.intiquan.com).")
    return()
  }
  if (grepl(" ",path)) {
    stopIQR("The path contains spaces. Choose a different path")
  }
  aux_mkdir(path)
  file.copy(from=dir, to=path,recursive=TRUE)
  setwd(path)
  message(paste0("IQR Tools documentation example material copied to: ",path))
  message(paste0("Working directory set to: ",path))
}
is_installed_PACKAGE <- function (pkg) {
  pkg %in% rownames(utils::installed.packages())
}
showStartupMessage <- function () {
  version <- aux_version("IQRtools")
  if (version=="99.0.0") version <- "PUBLIC"
  message <- paste0("\n",aux_strrep(crayon::bold(crayon::green("IQR"),crayon::silver("tools"))," ",""),": ",
                    "Supporting Efficient Model-Informed Drug Development\n\n             ",
                    crayon::bold("Version:",version),"\n\n","          Open Source (GNU AGPL-3) - Provided to You by IntiQuan\n\n")
  cat(message)
  loadSetupOptions_IQRtools()
  if (.COMPLIANCE_MODE) {
    cat(crayon::bold(crayon::green("          Compliance mode is ON\n\n")))
  } else {
    cat(crayon::bold(crayon::red("          Compliance mode is OFF\n\n")))
  }
  cat(crayon::bold(crayon::cyan("          Support: https://support.intiquan.com/iqrtools\n\n")))
  return(invisible(NULL))
}
#'@export
getRepos <- function () {
  Rversion <- paste0(version$major,".",version$minor)
  repos <- "https://cran.rstudio.com"
  if (Rversion == "3.6.3") repos <- "https://cran.intiquan.com/snapshot/2020-03-15"
  if (Rversion == "3.6.2") repos <- "https://cran.microsoft.com/snapshot/2019-04-15"
  if (Rversion == "3.6.1") repos <- "https://cran.microsoft.com/snapshot/2019-04-15"
  if (Rversion == "3.6.0") repos <- "https://cran.microsoft.com/snapshot/2019-04-15"
  if (Rversion == "3.5.3") repos <- "https://cran.microsoft.com/snapshot/2019-04-15"
  if (Rversion == "3.5.2") repos <- "https://cran.microsoft.com/snapshot/2019-02-01"
  if (Rversion == "3.5.1") repos <- "https://cran.microsoft.com/snapshot/2018-08-01"
  if (Rversion == "3.5.0") repos <- "https://cran.microsoft.com/snapshot/2018-06-01"
  if (Rversion == "3.4.4") repos <- "https://cran.microsoft.com/snapshot/2018-04-01"
  if (Rversion == "3.4.3") repos <- "https://cran.microsoft.com/snapshot/2018-01-01"
  if (Rversion == "3.4.2") repos <- "https://cran.microsoft.com/snapshot/2017-10-15"
  if (Rversion == "3.4.1") repos <- "https://cran.microsoft.com/snapshot/2017-09-01"
  if (Rversion == "3.4.0") repos <- "https://cran.microsoft.com/snapshot/2017-05-01"
  if (Rversion == "3.3.3") repos <- "https://cran.microsoft.com/snapshot/2017-03-15"
  if (Rversion == "3.3.2") repos <- "https://cran.microsoft.com/snapshot/2016-11-01"
  if (Rversion == "3.3.1") repos <- "https://cran.microsoft.com/snapshot/2016-07-01"
  if (Rversion == "3.3.0") repos <- "https://cran.microsoft.com/snapshot/2016-06-01"
  if (Rversion == "3.2.4") repos <- "https://cran.microsoft.com/snapshot/2016-04-01"
  if (repos=="https://cran.rstudio.com") warningIQR("Using official CRAN repo - results might not be reproducible!")
  return(repos)
}
#'@export
setup_IQRtools <- function(local=FALSE){
  if (!local) {
    file.edit(system.file(package="IQRtools","setup_options_IQRtools.R"))
  } else {
    loadSetupOptions_IQRtools()
    if (!.ALLOW_USER_SETTINGS_FILE) {
      stopIQR("The use of a local setup_options_IQRtools.R file has been disabled")
    }
    home__ <- Sys.getenv("HOME")
    file__ <- paste0(home__,"/setup_options_IQRtools.R")
    file.edit(file__)
  }
}
exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
#'@export
test_IQRtools <- function() {
  model <- IQRmodel(system.file(package="IQRtools","models/NovakTyson.txt"))
  sim_IQRmodel(model,seq(0:0.1:1))
}
#'@export
test_IQReport <- function() {
  oldpath <- getwd()
  if (.Platform$OS.type=="windows") {
    folder <- "C:/LOCAL/IQReport/test"
  } else {
    folder <- "~/IntiQuan/IQReport/test"
  }
  aux_mkdir(folder)
  setwd(folder)
  df <- cbind(cars=rownames(datasets::mtcars),datasets::mtcars)
  rmd <- rmdTITLE(template="DefaultStyle.rmdt",title = "TEST") +
    rmdSECTION("A table") +
    rmdTABLEDF(df,"Some table")
  export_IQRrmd(rmd,"test.rmd")
  message("Generating test Word document ...")
  IQReport("test.rmd")
  if (.Platform$OS.type=="windows") {
    system(paste0("openDocx.bat . test"))
  }
  message(paste0("Word document generated in ",getwd()))
  setwd(oldpath)
}
#'@export
test_IQRsbml <- function() {
  message("Importing a larger SBML model ...")
  model <- IQRmodel(system.file(package = "IQRtools","models/model.xml"))
  print(model)
  message("Simulating the model ...")
  res <- sim_IQRmodel(model)
  message("Plotting results ...")
  plotres <- tidyr::gather(res,NAME,VALUE,-TIME)
  IQRggplot(plotres, aes(x=TIME,y=VALUE,GROUP=NAME)) + geom_line()
}
#'@export
getOSIQR <- function(){
  i <- Sys.info()[["sysname"]]
  if (i=="Windows") return("windows")
  if (i=="Darwin") return("mac")
  if (i=="Linux") return("unix")
  stopIQR("Unknown operating system")
}
#'@export
libraryPK_IQRtools <- function(pathname = "Resources") {
  libraryPath <- system.file("docs/material/ModelLibraryPK", package = "IQRtools")
  if (!file.exists(pathname)) dir.create(pathname)
  check <- file.copy(from = libraryPath, to = pathname, recursive = TRUE, overwrite = TRUE)
  if (check) message(paste0("IQR Tools PK model library was copied to ", pathname)) else warningIQR(paste0("IQR Tools PK model library could not be copied to ", pathname))
}
checkPackage_IQRtools <- function(package,requiredVersion=NULL) {
  if (!(package %in% rownames(utils::installed.packages()))) {
    if (is.null(requiredVersion)) {
      stopIQR(paste0("Package '",package,"' is not installed but required for this functionality."))
    } else {
      stopIQR(paste0("Package '",package,"' is not installed but required (Version: '",requiredVersion,"') for this functionality."))
    }
  }
  if (!is.null(requiredVersion)) {
    if (aux_version(package)!=requiredVersion)
      stopIQR(paste0("Package '",package,"' is installed but required version is: '",requiredVersion,"'."))
  }
}
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = TRUE,...) {
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  res <- sHEAD(x, ...)
  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {
    res <- sGET(x, ...)
    if (is.null(res$result)) return(NA) 
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    return(TRUE)
  } else {
    return(TRUE)
  }
}
allowed_spaces_IQR <- function () {
  loadSetupOptions_IQRtools()
  return(.ALLOW_SPACES_IN_PATH)
}
#'@export
getProgramSpecification <- function(folder,
                                    extensions = c(".R", ".txt", ".r"),
                                    recursive = TRUE) {
  keepPattern__ <- paste0("[.]", paste0(gsub("^[.]","",extensions), collapse = "$|[.]"), "$")
  filesFull__ <- list.files(folder, full.names = TRUE, recursive = recursive, pattern = keepPattern__)
  filesRel__  <- list.files(folder, full.names = FALSE, recursive = recursive, pattern = keepPattern__)
  subfolders__ <- aux_fileparts(filesRel__)$pathname
  out__ <- data.frame(
    Source = filesFull__,
    Subfolder = subfolders__,
    Name = NA,
    Type = "program",
    Description = NA,
    defineRMD = NA,
    stringsAsFactors = FALSE
  )
  out__
}
#'@export
getModelSpecification <- function(modelList) {
  purrr::imap_dfr(modelList, function(mod, nam) {
    data.frame(
      Source = mod,
      Name = nam,
      Type = "model",
      Description = NA,
      defineRMD = NA,
      SubFolder = NA,
      stringsAsFactors = FALSE
    )
  })
}
#'@export
IQRoutputValueTable <- function(value,label,description="",path="../Output/00_ValueTable") {
  if (length(value) > 1) stopIQR("Value has to be of length 1")
  if (nchar(gsub("\\W","",label)) != nchar(label)) stopIQR("label argument only allowed to contain characters: A-Z, a-z, 0-9")
  if (file.exists(path)) {
    vTpre <- load_ValueTable(path)
    if (any(grepl(label,vTpre$label))) stopIQR("Label argument is already present in value table elements")
  }
  vTelement <- data.frame(
    label = label,
    value = value,
    description = description,
    stringsAsFactors = FALSE
  )
  filename <- paste0(path,"/vT_",label,".rds")
  if (file.exists(filename)) stopIQR("Valuetable element with same filename already exists.")
  IQRoutputRDS(vTelement,filename)
  filenameVT <- paste0(path,"/TAB01_ValueTable.txt")
  tab <- load_ValueTable(path)
  IQRoutputTable(xtable = tab,xtitle = "Value table for lookup and reporting purposes",filename = filenameVT)
  return(invisible(NULL))
}
load_ValueTable <- function(path="../Output/00_ValueTable") {
  if (!dir.exists(path)) stopIQR("Value table folder does not exist")
  files <- list.files(path,pattern = "*.rds",full.names = TRUE)
  files <- files[!grepl(".rds.log",files)]
  do.call(rbind,lapply(files, function (file) {
    out__ <- readRDS(file)
    logInfo <- parseLogfile(paste0(file,".log"))
    if (!is.null(logInfo)) {
      out__$log <- paste0(logInfo$outputfile,"; ",logInfo$analysisfile,"; ",logInfo$date)
    } else {
      out__$log <- ""
    }
    out__
  }))
}
parseLogfile <- function (path) {
  if (!file.exists(path)) return(NULL)
  content <- aux_fileread(path,collapserows = FALSE)
  list(
    username = aux_strtrim(aux_explode(content[grepl("Username",content)],"\\|")[2]),
    analysisfile = aux_strtrim(aux_explode(content[grepl("Analysis file",content)],"\\|")[2]),
    date = aux_strtrim(aux_explode(content[grepl("Date of creation",content)],"\\|")[2]),
    outputfile = aux_strtrim(aux_explode(content[grepl("File (relative to calling function)",content,fixed = TRUE)],"\\|")[2])
  )
}
#'@export
is_enabled_complianceMode <- function(){
  loadSetupOptions_IQRtools()
  test <- globalenv()$IQRTOOLS_OVERRIDE_SETTING_COMPLIANCE_MODE
  test <- ifelse(is.null(test),FALSE,test)
  if (exists("IQRTOOLS_OVERRIDE_SETTING_COMPLIANCE_MODE")) return(IQRTOOLS_OVERRIDE_SETTING_COMPLIANCE_MODE)
  return(F)
}
#'@export
is_enabled_rdsMode <- function(){
  loadSetupOptions_IQRtools()
  return(F)
}
#'@export
IQRinitCompliance <- function(scriptname){
  e__ <- globalenv()
  e__$COMPLIANCE_MODE_SCRIPT_NAME <- scriptname
}
genComplianceLog <- function(outputfilename, FLAGshort = FALSE, FLAGsession = FALSE) {
  if (!is_enabled_complianceMode()) return()
  if (is.null(outputfilename)) return()
  e__ <- globalenv()
  if (!("COMPLIANCE_MODE_SCRIPT_NAME" %in% ls(e__)))
    stopIQR("Compliance mode is enabled but the COMPLIANCE_MODE_SCRIPT_NAME variable has not\nbeen defined in the global environment (by the user). You can use the function IQRinitCompliance to do so")
  COMPLIANCE_MODE_SCRIPT_NAME <- e__$COMPLIANCE_MODE_SCRIPT_NAME
  logt__ <- "<TT>   File generation log"
  logfilepath__ <- paste(outputfilename,'.log',sep='')
  outputfilename <- gsub('//','/',outputfilename)
  userrow__ <- Sys.info()[['user']]
  timerow__ <- Sys.time()
  short__ <- paste0(COMPLIANCE_MODE_SCRIPT_NAME, " | User: ", userrow__, " | Date: ", timerow__)
  callr__   <- paste('<TR>   Analysis file                       | ',COMPLIANCE_MODE_SCRIPT_NAME,sep='')
  pathrow__ <- paste('<TR>   File (relative to calling function) | ',outputfilename,sep='')
  userrow__ <- paste('<TR>   Username                            | ',userrow__,sep='')
  timerow__ <- paste('<TR>   Date of creation                    | ',timerow__,sep='')
  loglength__ <- max(1,max(nchar(callr__)),nchar(timerow__),nchar(userrow__),nchar(pathrow__))
  logsep__ <- paste(c("       ",rep("=",loglength__-7),"\n"),collapse="",sep='')
  content__ <- c(logt__,logsep__,pathrow__,userrow__,timerow__,callr__)
  if (FLAGshort) content__ <- short__
  if (FLAGsession) return(content__)
  write(content__,logfilepath__,append=FALSE)
}
#'@export
IQRoutputCSV <- function(data,
                         filename,
                         na=".",
                         quote=FALSE,
                         row.names=FALSE,
                         FLAGattributes=TRUE,
                         replaceComma=NULL) {
  if (is.null(filename)){
    warningIQR("filename is NULL - no file written")
    return(data)
  }
  filename.csv__ <- paste0(aux_strrep(filename,".csv",""),".csv")
  IQRsaveCSVdata(data,
                 filename.csv__,
                 na=na,
                 quote=quote,
                 row.names=row.names,
                 FLAGattributes=FLAGattributes,
                 replaceComma=replaceComma)
  genComplianceLog(filename.csv__)
  if (FLAGattributes) {
    filenameATR <- gsub('\\.csv(.gz)?$','.atr', filename)
    if (file.exists(filenameATR)) {
      genComplianceLog(filenameATR)
    }
  }
}
#'@export
IQRoutputFile <- function(text,filename) {
  if (is.null(filename)) return()
  aux_mkdir(aux_fileparts(filename)$pathname)
  aux_filewrite(text,filename)
  genComplianceLog(filename)
}
#'@export
IQRoutputRDS <- function(object,filename) {
  if (is.null(filename)) return()
  filename <- paste0(aux_strrep(filename,".rds",""),".rds")
  aux_mkdir(aux_fileparts(filename)$pathname)
  saveRDS(object,filename)
  genComplianceLog(filename)
}
#'@export
IQRoutputPNG <- function(gr, filename=NULL, res = 300, width = 21/2.54, height = 21/2.54*3/4, scale=1, scaleWidth=1, scaleHeight=1, ...) {
  on.exit({if (!is.null(filename)) aux_closePNGs()})
  if (!is.null(filename)) {
    aux_closePNGs()
    filename <- paste0(aux_strrep(filename,".png",""),".png")
    aux_mkdir(aux_fileparts(filename)$pathname)
    grDevices::png(filename, height = height*scale*scaleHeight, width = width*scale*scaleWidth, bg = "transparent", res = res, units = "in")
    if ("gtable" %in% class(gr))
    {
      print(graphics::plot(gr))
    } else {
      print(gr)
    }
    genComplianceLog(filename)
    aux_closePNGs()
    if (is_enabled_rdsMode()) {
      saveRDS(gr,file=paste0(filename,".rds"))
    }
  }
}
#'@export
IQRoutputPDF <- function(gr,
                         filename=NULL,
                         width = 21/2.54,
                         height = 21/2.54*3/4,
                         scale=1,
                         scaleWidth=1,
                         scaleHeight=1,
                         nrow = 1, ncol = 1,
                         ...) {
  on.exit({if (!is.null(filename)) aux_closePDFs()})
  if (!is.null(filename)) {
    aux_closePDFs()
    filename <- paste0(aux_strrep(filename,".pdf",""),".pdf")
    aux_mkdir(aux_fileparts(filename)$pathname)
    dummy__ <- grDevices::pdf(file = filename,
                              onefile=TRUE,
                              width=width*scale*scaleWidth,
                              height=height*scale*scaleHeight,
                              ...)
    if ("gtable" %in% class(gr)) {
      print(graphics::plot(gr))
    } else {
      if ("ggplot" %in% class(gr)) {
        print(gr)
      } else {
        if ("list" %in% class(gr)& "arrangelist" %in% class(gr[[1]])) {
          plyr::l_ply(gr, print)
        } else {
          if ("list" %in% class(gr)) {
            printGrid(gr, nrow = nrow, ncol = ncol)
          } else {
            if ("IQRslideplot" %in% class(gr)) {
              print(gr)
            } else {
              stopIQR("Unhandled graphics object")
            }
          }
        }
      }
    }
    aux_closePDFs()
    genComplianceLog(filename)
    if (is_enabled_rdsMode()) {
      saveRDS(gr,file=paste0(filename,".rds"))
    }
  }
}
#'@export
IQRoutputPDFstart <- function(filename, width = 21/2.54, height = 21/2.54*3/4, scale=1, scaleWidth=1, scaleHeight=1, ...) {
  aux_closePDFs()
  if (!is.null(filename)) {
    aux_mkdir(aux_fileparts(filename)$pathname)
    if (file.exists(filename)) {
      unlink(filename)
    }
    dummy__ <- grDevices::pdf(file = paste0(aux_strrep(filename,".pdf",""),".pdf"), onefile=TRUE,
                              width=width*scale*scaleWidth,
                              height=height*scale*scaleHeight, ...)
  }
}
#'@export
IQRoutputPDFend <- function(filename) {
  aux_closePDFs()
  if (!is.null(filename)) {
    genComplianceLog(paste0(aux_strrep(filename,".pdf",""),".pdf"))
  }
}
#'@export
IQRoutputTable <- function(xtable=NULL,
                           xfooter=NULL,
                           xtitle=NULL,
                           object=NULL,
                           filename=NULL,
                           report=NULL,
                           na.string=NULL,
                           FLAGreplaceRoundBracketsHeader=TRUE,
                           verbose=NULL) {
  if (!is.null(filename)) {
    filename <- paste0(aux_strrep(filename,".txt",""),".txt")
    aux_mkdir(aux_fileparts(filename)$pathname)
  }
  if (is.null(verbose)) verbose <- TRUE
  if (is.null(xtable) & is.null(object)) stopIQR("Neither xtable nor object defined")
  if (!is.null(xtable) & !is.null(object)) stopIQR("Both xtable and object defined")
  if (!is.null(xtable) & is.null(report)) report <- TRUE
  if (!is.null(xfooter)) {
    xfooter <- paste(xfooter, collapse = "\n")
  }
  if (!is.null(object) & (!"IQRoutputTable" %in% class(object))) stopIQR("Input argument object is not an IQRoutputTable object")
  if (is.null(object)) {
    out__ <- list(
      xtable = xtable,
      xtitle = xtitle,
      xfooter = xfooter,
      report = report,
      filename = filename
    )
  } else {
    out__ <- list(
      xtable = object$xtable,
      xtitle = {
        if (is.null(xtitle)) {
          x__ <- object$xtitle
        } else {
          x__ <- xtitle
        }
        x__
      },
      xfooter =  {
        if (is.null(xfooter)) {
          x__ <- object$xfooter
        } else {
          x__ <- xfooter
        }
        x__
      },
      report =  {
        if (is.null(report)) {
          x__ <- object$report
        } else {
          x__ <- report
        }
        x__
      },
      filename =  {
        if (is.null(filename)) {
          x__ <- object$filename
        } else {
          x__ <- filename
        }
        x__
      }
    )
  }
  class(out__) <- c("IQRoutputTable",class(out__))
  if (!is.null(filename)) {
    aux_filewrite(text=text_IQRoutputTable(out__,
                                           report=report,
                                           na.string=na.string,
                                           FLAGreplaceRoundBracketsHeader=FLAGreplaceRoundBracketsHeader),
                  filename=paste0(aux_strrep(filename,".txt",""),".txt"))
    genComplianceLog(filename)
    if (is_enabled_rdsMode()) {
      saveRDS(out__,file=paste0(filename,".rds"))
    }
  }
  if (is.null(filename) | verbose) {
    return(out__)
  }
}
#'@export
print.IQRoutputTable <- function (x, ...) {
  cat(text_IQRoutputTable(x,report = FALSE))
  cat("\n\nIQRoutputTable object")
}
#'@export
text_IQRoutputTable <- function (object,report=NULL, na.string=NULL,FLAGreplaceRoundBracketsHeader=TRUE) {
  if (!"IQRoutputTable" %in% class(object)) stopIQR("Input argument not an IQRoutputTable object")
  xtable <- object$xtable
  xfooter <- object$xfooter
  xtitle <- object$xtitle
  if (is.null(report)) report <- object$report
  if (FLAGreplaceRoundBracketsHeader) {
    n__ <- names(xtable)
    n__ <- gsub("(","[",n__,fixed = TRUE)
    n__ <- gsub(")","]",n__,fixed = TRUE)
    names(xtable) <- n__
  }
  if (!is.null(xfooter)) {
    if (report) {
      xfooter <- gsub("\n","<br>",xfooter,fixed = TRUE)
      xfooter <- gsub("^\\* ","\\\\* ",xfooter)
      xfooter <- gsub("<br>\\* ","<br>\\\\* ",xfooter)
    } else {
      xfooter <- gsub("<br>","\n",xfooter,fixed = TRUE)
    }
  }
  if (report) {
    trid__ <- "<TR>"
    tfid__ <- "<TF>"
    ttid__ <- "<TT>"
    thid__ <- "<TH>"
  } else {
    trid__ <- ""
    tfid__ <- ""
    ttid__ <- ""
    thid__ <- ""
  }
  tr__ <- format(data.frame(lapply(xtable,function (x) as.character(x)),stringsAsFactors = FALSE),justify="left")
  if (!is.null(na.string))
    tr__ <- data.frame(lapply(tr__, function(x) {x[grepl("^NA$", trimws(x))] <- na.string; x} ))
  if ("matrix" %in% class(xtable)){
    th__ <- paste0("X",c(1:ncol(xtable)))
  }  else{ 
    th__ <- colnames(xtable)
  }
  widths <- rep(1,ncol(xtable))
  for (coli__ in c(1:length(widths))) {
    tr_nchar <- nchar(tr__[,coli__])
    tr_nchar <- ifelse(length(tr_nchar) == 0, 0, tr_nchar)
    th_nchar <- nchar(th__[coli__])
    th_nchar <- ifelse(length(th_nchar) == 0, 0, th_nchar)
    widths[coli__] <- max(1, max(tr_nchar, th_nchar))
  }
  for (coli__ in c(1:length(th__))){
    th__[coli__] <- format(th__[coli__], width = widths[coli__])
  }
  th__ <- paste(th__,collapse= " | ")
  th__ <- paste(thid__," ",th__, collapse= "")
  for (coli__ in c(1:length(widths))){
    tr__[,coli__] <- sprintf(paste('%-',widths[coli__], 's',sep=''),tr__[,coli__])
  }
  tr__ <- utils::capture.output(utils::write.table(tr__,quote=FALSE,sep=" | ",row.names = FALSE,col.names=FALSE))
  tr__ <- paste(tr__,collapse = paste0("\n",trid__,"   "))
  tr__ <- paste(paste0(trid__,"  "),tr__,collapse="")
  if (!is.null(xfooter)) {
    if (report) {
      tf__ <- paste(tfid__,"  ",xfooter, collapse= "")
    } else {
      tf__ <- paste0(tfid__,xfooter, collapse= "")
    }
    tf__ <- format(tf__, width = nchar(th__))
  } else {
    tf__ <- ""
  }
  if (!is.null(xtitle)) {
    tt__ <- paste(ttid__," ",xtitle, collapse="")
    tt__ <- format(tt__, width = nchar(th__))
  } else {
    tt__ <- ""
  }
  tablength__ <- max(nchar(th__),nchar(tt__))  
  nminus <- 7
  if (!report) nminus <- 3
  xxx <- "       "
  if (!report) xxx <- "   "
  thsep__ <- paste0(c(xxx,rep("-",tablength__-nminus),"\n"),collapse="")
  tfsep__ <- paste0(c(xxx,rep("-",tablength__-nminus)),collapse="")
  ttsep__ <- paste0(c(xxx,rep("=",tablength__-nminus),"\n"),collapse="")
  if (is.null(xtitle)) {
    IQRtableHead__ <- paste(th__,thsep__, sep = "\n")
  } else {
    IQRtableHead__ <- paste(tt__,ttsep__,th__,thsep__, sep = "\n")
  }
  if (is.null(xfooter)) {
    IQRtableFoot__ <- NULL
  } else {
    IQRtableFoot__ <- paste(tfsep__,tf__, sep = "\n")
  }
  allTable__ <- paste0(IQRtableHead__,tr__,"\n",IQRtableFoot__)
  allTable__
}
#'@export
IQRoutputFigure <- function(x = NULL,
                            title = NULL,
                            subtitle = NULL,
                            footer = NULL,
                            filename=NULL,
                            FLAGreport = FALSE,
                            opt.pagesize = list(width = 21/2.54, height = 21/2.54*3/4,
                                                scale = 1, scaleWidth=1, scaleHeight=1,
                                                res = 300),
                            opt.layout   = list(nrow = NULL, ncol = NULL, npage = NULL,
                                                legend.option = c("as.is", "remove", "first.on.page"),
                                                legend.object = NULL,
                                                legend.position = "right", legend.relsize = 0.2,
                                                title.relheight = 0.05, subtitle.relheight = 0.05, footer.relheight = 0.05),
                            FLAGdraft = FALSE,
                            ...) {
  on.exit({if (!is.null(filename)) aux_closeDevice(aux_fileparts(filename)[["fileext"]])})
  if (inherits(x, "IQRoutputFigure")) {
    object <- x
    gr <- NULL
  } else {
    object <- NULL
    gr <- x
  }
  names.opt.layout   <- names(formals("opt.layout"))
  names.opt.pagesize <- names(formals("opt.pagesize"))
  if (!all(names(opt.layout) %in% names.opt.layout)) {
    stopIQR(paste0("No valid layout option(s):", paste0(setdiff(names(opt.layout), names.opt.layout), collapse = ", ")))
  }
  if (!all(names(opt.pagesize) %in% names.opt.pagesize)) {
    stopIQR(paste0("No valid page size option(s): ", paste0(setdiff(names(opt.pagesize), names.opt.pagesize), collapse = ", ")))
  }
  inputs__ <- as.list(match.call(expand.dots = TRUE))
  inputoptions__ <- inputs__[!names(inputs__) %in% c("", "x", "title", "subtitle", "footer", "filename", "FLAGreport", "FLAGdraft")]
  if ("opt.layout" %in% names(inputoptions__)) inputoptions__$opt.layout <- opt.layout 
  if ("opt.pagesize"   %in% names(inputoptions__)) inputoptions__$opt.pagesize   <- opt.pagesize 
  for (optk in intersect(names(inputoptions__), names.opt.layout)) {
    if (!"opt.layout" %in% names(inputoptions__)) inputoptions__$opt.layout <- list()
    inputoptions__$opt.layout[[optk]] <- eval(inputoptions__[[optk]])
    inputoptions__[[optk]] <- NULL
  }
  for (optk in intersect(names(inputoptions__), names.opt.pagesize)) {
    if (!"opt.pagesize" %in% names(inputoptions__)) inputoptions__$opt.pagesize <- list()
    inputoptions__$opt.pagesize[[optk]] <- eval(inputoptions__[[optk]])
    inputoptions__[[optk]] <- NULL
  }
  if ("legend.option" %in% names(inputoptions__$opt.layout)) {
    if (length(inputoptions__$opt.layout$legend.option) != 1) warningIQR("Only first element of legend.option is used.")
    if (is.numeric(inputoptions__$opt.layout$legend.option)) {
      if (!inputoptions__$opt.layout$legend.option[1] %in% 1:3) {
        stopIQR("legend.option needs to be numeric input of 1, 2, or 3, or character input of 'as.is', 'remove', or 'common'.")
      } else {
        inputoptions__$opt.layout$legend.option <- c("as.is", "remove", "common")[inputoptions__$opt.layout$legend.option]
      }
    } else {
      if (!inputoptions__$opt.layout$legend.option[1] %in% c("as.is", "remove", "common")) {
        stopIQR("legend.option needs to be numeric input of 1, 2, or 3, or character input of 'as.is', 'remove', or 'common'.")
      }
    }
  }
  argslist__ <- as.list(formals())
  argslist__ <- argslist__[!names(argslist__) %in% c("x", "title", "subtitle", "footer", "filename", "FLAGreport", "FLAGdraft", "...")]
  argslist__$opt.layout <- eval(argslist__$opt.layout)
  argslist__$opt.pagesize <- eval(argslist__$opt.pagesize)
  defoptions__ <- argslist__[setdiff(names(argslist__), intersect(names(inputoptions__), c(names.opt.layout, names.opt.pagesize)))]
  if ("opt.layout" %in% names(inputoptions__)) defoptions__$opt.layout <- defoptions__$opt.layout[setdiff(names(defoptions__$opt.layout), names(inputoptions__$opt.layout))]
  if ("opt.pagesize"   %in% names(inputoptions__)) defoptions__$opt.pagesize   <- defoptions__$opt.pagesize[setdiff(names(defoptions__$opt.pagesize), names(inputoptions__$opt.pagesize))]
  if (!is.null(object)) {
    defoptions__ <- defoptions__[setdiff(names(defoptions__), c(names(object$opt.layout), names(object$opt.pagesize)))]
    if ("opt.layout" %in% names(object)) defoptions__$opt.layout <- defoptions__$opt.layout[setdiff(names(defoptions__$opt.layout), names(object$opt.layout))]
    if ("opt.pagesize"   %in% names(object)) defoptions__$opt.pagesize   <- defoptions__$opt.pagesize[setdiff(names(defoptions__$opt.pagesize), names(object$opt.pagesize))]
  }
  if (!is.null(object)) {
    output__ <- object
    if ("opt.layout" %in% names(inputoptions__)) {
      new.opt.layout <- inputoptions__$opt.layout
    } else {
      new.opt.layout <- list()
    }
    for (layopt in names(new.opt.layout)) {
      if (is.null(output__$opt.layout)) output__$opt.layout <- list()
      output__$opt.layout[[layopt]] <- new.opt.layout[[layopt]]
    }
    if ("opt.pagesize" %in% names(inputoptions__)){
      new.opt.pagesize <- eval(inputoptions__$opt.pagesize)
    } else {
      new.opt.pagesize <- list()
    }
    for (pagopt in names(new.opt.pagesize)) {
      if (is.null(output__$opt.pagesize)) output__$opt.pagesize <- list()
      output__$opt.pagesize[[pagopt]] <- new.opt.pagesize[[pagopt]]
    }
    if ("title" %in% names(inputs__)) output__$title <- title
    if ("subtitle" %in% names(inputs__)) output__$subtitle <- subtitle
    if ("footer" %in% names(inputs__)) output__$footer <- footer
    if ("filename" %in% names(inputs__)) output__$filename <- filename
    if ("FLAGdraft" %in% names(inputs__)) output__$draft <- FLAGdraft
  }
  if (!is.null(gr)) {
    if (!(is_plot_object(gr) | all(sapply(gr, is_plot_object))))
      stopIQR("Input gr needs to be plot object or list of these")
    if (is_plot_object(gr)) gr <- list(gr)
    output__ <- list(
      content = gr,
      title = title,
      subtitle = subtitle,
      footer = footer,
      filename = filename
    )
    output__ <- purrr::discard(output__, is.null)
    if (!is.null(attr(gr, "plotdata"))) output__$plotdata <- attr(gr, "plotdata")
    if ("opt.layout" %in% names(inputoptions__)){
      opt.layout <- eval(inputoptions__$opt.layout)
    } else {
      opt.layout <- list()
    }
    if (length(opt.layout) > 0) output__$opt.layout <- opt.layout
    if ("opt.pagesize" %in% names(inputoptions__)){
      opt.pagesize <- eval(inputoptions__$opt.pagesize)
    } else {
      opt.pagesize <- list()
    }
    if (length(opt.pagesize) > 0) output__$opt.pagesize <- opt.pagesize
    output__$draft <- FLAGdraft
    class(output__) <- c("IQRoutputFigure", class(output__))
  }
  if (ifelse("legend.option" %in% names(output__$opt.layout), output__$opt.layout$legend.option != "common", TRUE)) {
    if (!is.null(output__$opt.layout$legend.object)) warningIQR("Legend object provided, but legend option not set to 'common'. Object will be ignored when plotting.")
    if (!is.null(output__$opt.layout$legend.position)) warningIQR("Legend position provided, but legend option not set to 'common'. Setting will be ignored when plotting.")
    if (!is.null(output__$opt.layout$legend.relsize)) warningIQR("Relative legend size provided, but legend option not set to 'common'. Setting will be ignored when plotting.")
  }
  if (!is.null(filename)) {
    objectPlot__ <- output__
    if (FLAGreport) {
      anntitle__  <- paste0("Title : ", ifelse("title" %in% names(objectPlot__),objectPlot__$title,""))
      annfooter__ <- paste0("Footer : ", ifelse("footer" %in% names(objectPlot__),objectPlot__$footer,""))
      annotation <- c(anntitle__,annfooter__)
      objectPlot__$title  <- NULL
      objectPlot__$footer <- NULL
    }
    layoutargs__ <- c(list(x=objectPlot__), defoptions__$opt.layout)
    layoutargs__$legend.option <- layoutargs__$legend.option[1]
    pages__     <- do.call(createPages_IQRoutputFigure, layoutargs__)
    devargs__ <- c(objectPlot__$opt.pagesize, defoptions__$opt.pagesize) 
    for (opt__ in names(devargs__)) assign(opt__, devargs__[[opt__]])
    fileType <- dplyr::case_when(
      grepl("[.]pdf$", filename) ~ "PDF",
      grepl("[.]png$", filename) ~ "PNG",
      TRUE ~ NA_character_
    )
    if (is.na(fileType)) stopIQR("File name needs to have .pdf or .png suffix. Only PDF or PNG output is handled.")
    if (fileType == "PDF") {
      argsDot <- list(...)
      namesDotKeep <- setdiff(names(argsDot), names.opt.pagesize) 
      namesDotKeep <- intersect(namesDotKeep, names(formals("pdf"))) 
      argsDot <- argsDot[namesDotKeep]
      do.call(IQRoutputPDF, c(list(gr=pages__, filename=filename,
                                   width = width, height = height,
                                   scale = scale, scaleWidth = scaleWidth, scaleHeight = scaleHeight,
                                   nrow = 1, ncol = 1), argsDot))
      if (FLAGreport) aux_filewrite(annotation, filename = paste0(filename,".ann"))
    }
    if (fileType == "PNG") {
      if (is_plot_object(pages__)) pages__ <- list(pages__)
      if (length(pages__) > 1) {
        nformat <- paste0("%.",1+floor(log10(length(pages__))),"d")
        .fname <- paste0(aux_strrep(filename,".png",""),"_", sprintf(nformat,seq_along(pages__)), ".png")
      } else {
        .fname <- filename
      }
      purrr::map2(.fname, pages__, function(.f,.p) {
        IQRoutputPNG(.p, filename=.f,
                     width = width, height = height, res = res,
                     scale = scale, scaleWidth = scaleWidth, scaleHeight = scaleHeight, ...)
        if (FLAGreport) aux_filewrite(annotation, filename = paste0(.f,".ann"))
      })
    }
    return(invisible(output__))
  } else {
    return(output__)
  }
}
#'@export
print.IQRoutputFigure <- function(
  x,
  nrow = NULL, ncol = NULL, npage = NULL,
  legend.option = "as.is", legend.object = NULL, legend.position = "right",
  legend.relsize = 0.2,
  title.relheight = 0.05, subtitle.relheight = 0.05, footer.relheight = 0.05,
  ...
) {
  inputoptions__ <- as.list(match.call(expand.dots = TRUE))
  inputoptions__ <- inputoptions__[!names(inputoptions__) %in% c("", "x")]
  plot_pages__ <- do.call(createPages_IQRoutputFigure, c(list(x=x), inputoptions__))
  if (is_plot_object(plot_pages__)) {
    print(plot_pages__)
  } else {
    lapply(plot_pages__, print)
    npages__ <- length(plot_pages__)
    if (npages__>1) {
      cat(paste0(length(plot_pages__), " pages were printed to the graphics device."))
    }
  }
  return(invisible(NULL))
}
#'@export
plot.IQRoutputFigure <- function(x, ...) {
  print(x, ...)
}
#'@export
summary.IQRoutputFigure <- function(object, ...) {
  x <- object
  dfmain__ <- tibble::tribble(
    ~a               , ~b,
    "Title"          , ifelse(is.null(object$title),crayon::silver("- none -"),object$title),
    "Number of plots", as.character(length(object$content)),
    "Subtitle"       , ifelse(is.null(object$subtitle),crayon::silver("- none -"),object$subtitle),
    "Footer"         , ifelse(is.null(object$footer),crayon::silver("- none -"),object$footer),
    "Filename"       , ifelse(is.null(object$filename),crayon::silver("- none -"),object$filename)
  )
  dfmain__$a <- format(dfmain__$a)
  dflayout__ <- NULL
  if ("opt.layout" %in% names(object)) {
    if (length(object$opt.layout)>0) {
      if ("legend.object" %in% names(object$opt.layout)) object$opt.layout$legend.object <- "Common legend provided"
      dflayout__ <- data.frame(a=names(object$opt.layout), b=sapply(object$opt.layout,c), stringsAsFactors = FALSE)
      dflayout__$a <- format(dflayout__$a)
    }
  }
  dfdev__ <- NULL
  if ("opt.pagesize" %in% names(object)) {
    if (length(object$opt.pagesize)>0) {
      dfdev__ <- data.frame(a=names(object$opt.pagesize), b=sapply(object$opt.pagesize,c), stringsAsFactors = FALSE)
      dfdev__$a <- format(dfdev__$a)
    }
  }
  cat("=== IQRoutputFigure object ===\n")
  for (k in 1:nrow(dfmain__)) cat(dfmain__$a[k], " : ", dfmain__$b[k],"\n", sep = "")
  if (!is.null(dflayout__)) {
    cat(crayon::blue("--- Defined layout settings:\n"))
    for (k in 1:nrow(dflayout__)) cat(crayon::blue(paste0(dflayout__$a[k], " : ", dflayout__$b[k],"\n")))
  } else {
    cat(crayon::silver("--- No layout settings\n"))
  }
  if (!is.null(dfdev__)) {
    cat(crayon::green("--- Defined page size settings for writing PDF or PNG:\n"))
    for (k in 1:nrow(dfdev__)) cat(crayon::green(paste0(dfdev__$a[k], " : ", dfdev__$b[k],"\n")))
  } else {
    cat(crayon::silver("--- No page size settings defined\n"))
  }
  return(invisible(NULL))
}
#'@export
print.IQRoutputFigureList <- function(x, ...) {
  lapply(x, function(x) summary.IQRoutputFigure(x))
}
#'@export
opt.pagesize <- function(width = NULL, height = NULL,
                         scale = NULL,
                         scaleWidth  = NULL, scaleHeight = NULL,
                         res         = NULL) {
  out <- list()
  out$width       <- width
  out$height      <- height
  out$scale       <- scale
  out$scaleWidth  <- scaleWidth
  out$scaleHeight <- scaleHeight
  out$res         <- res
  return(out)
}
#'@export
opt.layout <- function(nrow               = NULL,
                       ncol               = NULL,
                       npage              = NULL,
                       legend.option      = NULL,
                       legend.object      = NULL,
                       legend.position    = NULL,
                       legend.relsize     = NULL,
                       title.relheight    = NULL,
                       subtitle.relheight = NULL,
                       footer.relheight   = NULL) {
  out <- list()
  out$nrow               = nrow
  out$ncol               = ncol
  out$npage              = npage
  out$legend.option      = legend.option
  out$legend.object      = legend.object
  out$legend.position    = legend.position
  out$legend.relsize     = legend.relsize
  out$title.relheight    = title.relheight
  out$subtitle.relheight = subtitle.relheight
  out$footer.relheight   = footer.relheight
  return(out)
}
#'@export
import_IQRdataOriginal <- function(filename,
                                   description = "No description available",
                                   MD5 = NULL,
                                   target = basename(filename),
                                   interactive = TRUE) {
  path__ <- NULL
  logfile__ <- "datasources.txt"
  stopifnot(length(filename) == 1,
            length(description) == 1,
            length(target) == 1,
            !is.null(logfile__))
  if(!is.character(filename)) {
    stopIQR("The argument filename should be a character string denoting a file.")
  }
  if(!file.exists(filename)) {
    stopIQR("The specified filename ", as.character(filename), " does not exist.")
  }
  if (is.null(MD5)) MD5 <- "not provided"
  if (is.null(path__)) path__ <- "./"
  target.path__ <- dirname(as.character(target[1]))
  target <- basename(as.character(target[1]))
  if (target.path__ != ".") path__ <- target.path__
  logfile__ <- as.character(logfile__[1])
  fromScratch__ <- FALSE
  aux_mkdir(path__)
  if (!file.exists(file.path(path__, logfile__))) {
    file.create(file.path(path__, logfile__))
    message("A logfile called ", logfile__, " was created.")
  }
  con__ <- try(file(file.path(path__, logfile__), "r", blocking = FALSE), silent = TRUE)
  mysources__ <- readLines(con__)
  close(con__)
  files.index__ <- grep("@File:", mysources__)
  if (length(files.index__) == 0) fromScratch__ <- TRUE
  if (!fromScratch__) {
    files.index__ <- c(files.index__, length(mysources__) + 1)
    files__ <- lapply(seq_along(files.index__[-1]), function(i) {
      substream__ <- mysources__[files.index__[i]:(files.index__[i+1]-1)]
      do.call(rbind, lapply(strsplit(substream__, split = ": "), function(v) {
        if (length(v) < 2) return()
        v <- trimws(v)
        data.frame(Tag = v[1], Value = paste(v[-1], collapse = ": "), stringsAsFactors = FALSE)
      }))
    })
    MD5.contained__ <- all(sapply(files__, function(d) any(d[["Tag"]] %in% "@MD5 File")))
    if (!MD5.contained__)
      stopIQR("The specified logfile already existed but is not a valid logfile because MD5 sums are missing.")
  }
  if (file.exists(file.path(path__, target))) {
    MD5.source__ <- tools::md5sum(filename)[[1]]
    MD5.target__ <- tools::md5sum(file.path(path__, target))[[1]]
    if (MD5.target__ != MD5.source__) {
      message__ <- paste0("The target already exists. The MD5 sum between original and new file is different. ",
                          "Would you like to overwrite the file? (Y/n) ")
      if (interactive) {
        answer__ <- readline(message__)
      } else {
        answer__ <- "y"
      }
      if (tolower(substr(answer__, 1, 1)) == "n")
        return(message())
    } else {
      message("The source file was copied to the target location.")
    }
  } else {
    message("The source file was copied to the target location.")
  }
  copied <- file.copy(filename, file.path(path__, target), overwrite = TRUE)
  entry__ <- character()
  entry__["@File"] <- target
  entry__["@Description"] <- description
  entry__["@Source"] <- filename
  entry__["@Date"] <- date()
  entry__["@MD5 Source"] <- tools::md5sum(filename)[[1]]
  entry__["@MD5 File"] <- tools::md5sum(file.path(path__, entry__["@File"]))[[1]]
  entry__["@MD5 Customer"] <- tolower(MD5)
  entry__["@MD5 Check"] <- ifelse(length(setdiff(entry__[c("@MD5 Source", "@MD5 File", "@MD5 Customer")], "not provided")) == 1,
                                  yes = "All available MD5 sums match.",
                                  no = "!!! MD5 MISMATCH !!!")
  newfile__ <- data.frame(Tag = names(entry__), Value = as.character(entry__), stringsAsFactors = FALSE)
  if (grepl("MISMATCH", entry__[["@MD5 Check"]]))
    warningIQR("Encountered MD5 MISMATCH during import. The file was copied and imported. Please check what happend.")
  if (!fromScratch__) {
    file.check__ <- sapply(files__, function(d) {
      md5.ref__ <- d[["Value"]][d[["Tag"]] %in% "@MD5 File"][1]
      file.ref__ <- d[["Value"]][d[["Tag"]] %in% "@File"][1]
      if (md5.ref__ != entry__["@MD5 File"] & file.ref__ == entry__["@File"])
        return("update")
      if (md5.ref__ != entry__["@MD5 File"] & file.ref__ != entry__["@File"])
        return("different")
      if (md5.ref__ == entry__["@MD5 File"] & file.ref__ == entry__["@File"])
        return("identical")
      if (md5.ref__ == entry__["@MD5 File"] & file.ref__ != entry__["@File"])
        return("same")
    })
    if (any(file.check__ == "identical")) {
      message("The identical file already existed in the logfile. ",
              "The date in the logfile was updated.")
      files__[[which(file.check__ == "identical")]] <- newfile__
    } else {
      if (any(file.check__ == "update")) {
        message("The logfile was updated with the new information.")
        files__[[which(file.check__ == "update")]] <- newfile__
      }
      if (any(file.check__ == "same")) {
        message("According to the logfile, a file with the same MD5 sum but different name exists.")
        files__ <- c(files__, list(newfile__))
      }
      if (all(file.check__ == "different")) {
        files__ <- c(files__, list(newfile__))
      }
    }
  } else {
    files__ <- list(newfile__)
  }
  output__ <- paste0(do.call(function(...) paste(..., sep = "\n\n"), lapply(files__, function(f) {
    paste(paste(format(paste0(f[["Tag"]], ":")),
                f[["Value"]]), collapse = "\n")
  })), "\n\n")
  con__ <- file(file.path(path__, logfile__))
  writeLines(output__, con__)
  close(con__)
}
#'@export
IQRoutputXPT <- function(data, filename = NULL, covInfoAdd = NULL, catInfoAdd = NULL, addColLabels = NULL) {
  if (is.null(filename)) {
    stopIQR("filename must be provided")
  }
  if (is_IQRdataGENERAL(data) || ("IQRdataNLME" %in% attr(data, "class"))) {
    stopIQR("Use exportXPT_IQRdataGENERAL for this object.")
  }
  x <- aux_fileparts(filename)
  filename <- x$filename
  pathname <- x$pathname
  if (nchar(filename) > 8) {
    stopIQR("please provide a filename with length of max 8 characters (w/o extension)")
  }
  if (!is.null(covInfoAdd)) {
    attr(data, "covInfo") <- data.frame(covInfoAdd, stringsAsFactors = FALSE)
  }
  if (!is.null(catInfoAdd)) {
    attr(data, "catInfo") <- data.frame(catInfoAdd, stringsAsFactors = FALSE)
  }
  dataX <- data.frame(lapply(names(data), function(x) {
    x <- data[[x]]
    if (!is.null(levels(x))) {
      x <- levels(x)[x]
    }
    if (is.logical(x)) x <- as.double(x)
    if (is.integer(x)) x <- as.double(x)
    x
  }), row.names = NULL, stringsAsFactors = FALSE)
  names(dataX) <- names(data)
  attr(dataX,"methodBLLOQ") <- attr(data,"methodBLLOQ")
  attr(dataX,"doseNAMES")   <- attr(data,"doseNAMES")
  attr(dataX,"obsNAMES")    <- attr(data,"obsNAMES")
  attr(dataX,"aeNAMES")     <- attr(data,"aeNAMES")
  attr(dataX,"covInfo")     <- attr(data,"covInfo")
  attr(dataX,"catInfo")     <- attr(data,"catInfo")
  attr(dataX,"class")       <- c("IQRdataNLME","data.frame")
  attr(dataX,"imputeInfo")  <- attr(data,"imputeInfo")
  data <- dataX
  data <- addLabel_IQRdataGENERAL(data, addColLabels)
  aux_mkdir(pathname)
  haven::write_xpt(
    data = data,
    path = file.path(pathname, paste0(filename, ".xpt")),
    name = filename,
    version = 5
  )
  genComplianceLog(file.path(pathname, paste0(filename, ".xpt")))
}
#'@export
IQRoutputDEFINE <- function(data,
                            datasetName=NULL,
                            datasetLocation=NULL,
                            datasetDescription="Analysis dataset",
                            covInfoAdd=NULL,
                            catInfoAdd=NULL,
                            addColLabels=NULL,
                            filename=NULL) {
  if (is_IQRdataGENERAL(data) || ("IQRdataNLME" %in% attr(data,"class")))
    stopIQR("Use exportDEFINE_IQRdataGeneral function")
  if (is.null(datasetName))
    stopIQR("Please provide a name for the dataset as it should appear in the define file")
  if (is.null(datasetLocation))
    warningIQR("Please provide a location (path) for the dataset as it should appear in the define file")
  attr(data,"covInfo")      <- data.frame(covInfoAdd,stringsAsFactors = FALSE)
  attr(data,"catInfo")      <- data.frame(catInfoAdd,stringsAsFactors = FALSE)
  if (nrow(attr(data,"covInfo"))==0) attr(data,"covInfo") <- NULL
  if (nrow(attr(data,"catInfo"))==0) attr(data,"catInfo") <- NULL
  dataFirst__ <- data.frame(NAME=datasetName,DESCRIPTION=datasetDescription,LOCATION=datasetLocation,stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__ <- data.frame(NAME=names(data),stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__$TYPE <-lapply(data, function (x) { if (is.numeric(x)) { out <- "Numeric" } else { out <- "String" }; out })
  data <- addLabel_IQRdataGENERAL(data,addColLabels)
  dataDefine__$LABEL <-lapply(data, function (x) {
    label__ <- attr(x,"label")
    if (is.null(label__)) label__ <- "UNKNOWN"
    label__
  })
  dataDefine__$VALUES <- getValueTxtDefine(dataDefine__$NAME,data)
  dataDefine__$COMMENTS <- " "
  datasetName__ <- toupper(datasetName)
  RMDTEXT__ <- rmdEMPTY()
  RMDTEXT__ <- RMDTEXT__ + rmdTITLE(title=paste0("Define file for the \"",datasetName__,"\" dataset"),subtitle=NULL,date=NULL)
  RMDTEXT__ <- RMDTEXT__ + rmdNOINTRO()
  RMDTEXT__ <- RMDTEXT__ + rmdSECTION("Dataset name, description, and location",numbered=FALSE)
  RMDTEXT__ <- RMDTEXT__ + rmdTABLEDF(dataFirst__,label="overview",fontsize=8,caption="Dataset information",ignoreCaption=TRUE)
  RMDTEXT__ <- RMDTEXT__ + rmdNEWPAGE()
  RMDTEXT__ <- RMDTEXT__ + rmdSECTION(paste0(datasetName__," specification"),numbered=FALSE)
  RMDTEXT__ <- RMDTEXT__ + "Missing values are coded in the dataset as '.' and referenced in this specification as 'NA'.\n\n"
  RMDTEXT__ <- RMDTEXT__ + rmdTABLEDF(dataDefine__,label="overview",fontsize=8,caption="Definition of dataset contents",ignoreCaption=TRUE)
  RMDTEXT__ <- paste0(RMDTEXT__,"\n")
  if (is.null(filename)) {
    filename__ <- paste0(datasetName__,"_define",".rmd")
  } else {
    x <- aux_fileparts(filename)
    filename__ <- paste0(x$pathname,"/",x$filename,".rmd")
  }
  export_IQRrmd(RMDTEXT__,filename__)
  if (has_IQReport_executable()) {
    IQReport(filename__)
  }
}
#'@export
printGrid <- function (plotList,nrow=4,ncol=4) {
  if (length(plotList)==0) return(invisible(NULL))
  x <- which(!sapply(plotList, is.null))
  plotList <- plotList[x]
  pieces__ <- aux_splitVectorEqualPieces(x = 1:length(plotList), nrow*ncol)
  for (k__ in seq_along(pieces__)) {
    pO__ <- cowplot::plot_grid(plotlist=plotList[pieces__[[k__]]],nrow=nrow,ncol=ncol)
    print(pO__)
  }
}
#'@export
read_IQRoutputTable <- function(file, convertNum = TRUE) {
  con <- file(file)
  text <- readLines(con)
  close.connection(con)
  is_header <- grepl("^<TH>", text)
  is_row <- grepl("^<TR>", text)
  is_title <- grepl("^<TT>", text)
  is_footer <- grepl("^<TF>", text)
  tags <- c("<TH>", "<TR>")
  for (mytag in tags) text <- trimws(sub(mytag, "", text))
  data <- utils::read.table(textConnection(text[is_header|is_row]), sep = "|", header = any(is_header), stringsAsFactors = FALSE, check.names = FALSE)
  if(!convertNum)
    data <- mutate_if(data, is.numeric, function(x) {
      out <- as.character(x)
      out[is.na(out)] <- ""
      out
    })
  if (ncol(data) == 0) {
    warning("Nothing to read.")
    return()
  }
  colnames(data) <- trimws(colnames(data))
  for (i in 1:ncol(data)) {
    if (is.character(data[[i]])) data[[i]] <- trimws(data[[i]])
  }
  title <- footer <- NULL
  if (any(is_title)) {
    text[is_title] <- sub("<TT>", "", text[is_title])
    text[is_title] <- trimws(text[is_title])
    title <- paste(text[is_title], collapse = "\n")
  }
  if (any(is_footer)) {
    text[is_footer] <- sub("<TF>", "", text[is_footer])
    text[is_footer] <- trimws(text[is_footer])
    footer <- paste(text[is_footer], collapse = "\n")
  }
  out <- IQRoutputTable(
    xtable = data,
    xfooter = footer,
    xtitle = title
  )
  out$filename <- file
  return(out)
}
#'@export
IQRcalcTAD <- function (data)
{
  if ("ID" %in% names(data)) {
    data$IDUSESPLIT <- data$ID
  }
  else {
    if ("USUBJID" %in% names(data)) {
      data$IDUSESPLIT <- data$USUBJID
    }
    else {
      stopIQR("ID and/or USUBJID need to be present in data argument")
    }
  }
  if (!all(c("EVID", "TIME") %in% names(data))) {
    stopIQR("EVID and TIME need to be present in data argument")
  }
  data <- do.call(rbind, lapply(split(data, data$IDUSESPLIT),
                                function(x) {
                                  ixDOSE <- rep(NA, nrow(x))
                                  ixEVID <- which(x$EVID == 1)
                                  if (length(ixEVID) == 0) {
                                    xx <- x
                                    xx$TAD <- NA
                                  }
                                  else {
                                    ixDOSE[ixEVID] <- ixEVID
                                    ixDOSE <- aux_na_locf(ixDOSE)
                                    ixDOSE[is.na(ixDOSE)] <- 0
                                    x$ixDOSE <- ixDOSE
                                    xx <- do.call(rbind, lapply(split(x, x$ixDOSE),
                                                                function(y) {
                                                                  if (y$ixDOSE[1] > 0) {
                                                                    y$TAD <- y$TIME - y$TIME[1]
                                                                  }
                                                                  else {
                                                                    y$TAD <- y$TIME
                                                                  }
                                                                  y
                                                                }))
                                    xx$ixDOSE <- NULL
                                  }
                                  xx
                                }))
  data$IDUSESPLIT <- NULL
  data
}
#'@export
IQRexpandADDLII <- function(data) {
  data__ <- data
  data__$II[is.na(data__$II)] <- 0
  data__$ADDL[is.na(data__$ADDL)] <- 0
  dataIIADDL__ <- data__[data__$II>0 & data__$ADDL>0,]
  if (nrow(dataIIADDL__)==0) {
    return(data__)
  }
  expandedDoseRecords__ <- do.call(rbind,lapply(1:nrow(dataIIADDL__), function (krow) {
    II <- dataIIADDL__[krow,]$II
    NRDOSES__ <- dataIIADDL__[krow,]$ADDL + 1
    doseRecords__ <- dataIIADDL__[krow*rep(1,NRDOSES__),]
    doseRecords__$TIME <- dataIIADDL__[krow,]$TIME + seq(0,by=II,length.out=nrow(doseRecords__))
    if (!is.null(doseRecords__$TIMEPOS)) doseRecords__$TIMEPOS <- dataIIADDL__[krow,]$TIMEPOS + seq(0,by=II,length.out=nrow(doseRecords__))
    if (!is.null(doseRecords__$NT)) doseRecords__$NT <- dataIIADDL__[krow,]$NT + seq(0,by=II,length.out=nrow(doseRecords__))
    doseRecords__$ADDL <- 0
    doseRecords__$II <- 0
    doseRecords__
  }))
  data__ <- data__[!(data__$II>0 & data__$ADDL>0),]
  data__ <- rbind(data__,expandedDoseRecords__)
  data__ <- dplyr::arrange(data__,USUBJID,TIME,NAME)
  return(data__)
}
#'@export
is_IQRaedataER <- function(input) {
  methods::is(input,"IQRaedataER")
}
#'@export
info_IQRaedataER <- function (data) {
  if (!is_IQRaedataER(data)) stopIQR("data is not an IQRaedataER object")
  x <- unique(data[,c("USUBJID","PLACEBO")])
  NPlacebo = sum(x$PLACEBO==1)
  NActive = sum(x$PLACEBO==0)
  x <- data[,c("USUBJID","NAME")]
  ONE_AE_NAME_PER_INDIVIDUAL <- TRUE
  if (nrow(x) != nrow(unique(x))) ONE_AE_NAME_PER_INDIVIDUAL <- FALSE
  list(
    AEnames = sort(unique(data$NAME)),
    NPlacebo = NPlacebo,
    NActive = NActive,
    ONE_AE_NAME_PER_INDIVIDUAL = ONE_AE_NAME_PER_INDIVIDUAL,
    covInfo = attributes(data)$covInfo,
    catInfo = attributes(data)$catInfo,
    data_dose = attributes(data)$data_dose,
    modelinfo = attributes(data)$modelinfo,
    INTERVAL_EXPOSURE = attributes(data)$INTERVAL_EXPOSURE,
    DELTA_EXPOSURE = attributes(data)$DELTA_EXPOSURE,
    FLAGduplicateDoses = attributes(data)$FLAGduplicateDoses
  )
}
#'@export
print.IQRaedataER<- function(x, ...) {
  print.data.frame(x, ...)
  cat("\n")
  cat("\nIQRaedataER object\n")
  cat("\n")
  info <- info_IQRaedataER(x)
  cat("The following AEs are present in the data:\n")
  cat(paste0(info$AEnames,collapse = ", "))
  cat("\n\nNumber of placebo treated individuals: ",info$NPlacebo)
  cat("\nNumber of active treated individuals: ",info$NActive)
  if (!info$ONE_AE_NAME_PER_INDIVIDUAL) {
    cat("\n\nMore than one of same AE NAME per individual possible!")
  } else {
    cat("\n\nOnly one of same AE NAME per individual")
  }
  if (is.null(info$modelinfo)) {
    cat("\n\nNO exposure information present. Use function addExposure_IQRdataER() to add it!")
  } else {
    cat("\n\nExposure information:\n")
    cat("  * Models used to calculate exposure:\n")
    dummy <- sapply(seq_along(info$modelinfo), function (k) {
      m <- info$modelinfo[[k]]$NLMEPROJECT
      o <- info$modelinfo[[k]]$OUTPUT
      w <- info$modelinfo[[k]]$WEIGHT
      cat(paste0("    * MODEL ",k, ": ",m," (Output: ",o,", Weight: ",w,")\n"))
    })
    cat("  * Sum of weighted outputs was used to determine exposure metrics.\n")
    if (info$FLAGduplicateDoses) {
      cat("  * Original doses in dataset were duplicated to allow for parallel absorption.\n")
    }
    cat(paste0("  * Exposure metrics were calculated over ",info$INTERVAL_EXPOSURE," ",data$TIMEUNIT[1],".\n"))
    cat(paste0("  * Time step of ",info$DELTA_EXPOSURE," ",data$TIMEUNIT[1]," was used to derive Cavg, Cmin, Cmax, and Cavgacc.\n"))
  }
  cat("\n")
}
#'@export
summary.IQRaedataER <- function(object, ...) {
  print.IQRaedataER(object, ...)
}
#'@export
createAEdata_IQRdataGENERAL <- function (data,minGrade=1,FLAGkeepHighestGradeEarliestTime=FALSE,addCols=NULL) {
  if (!is_IQRdataGENERAL(data)) stopIQR("data is no an IQRdataGENERAL object")
  if (length(attr(data,"aeNAMES"))==0) stopIQR("data does not contain any adverse event information")
  if (!is.null(addCols)) {
    if (!all(addCols %in% names(data))) stopIQR("Contents of addCols are not all in the data")
  }
  data <- data[!(data$AEGRADE < minGrade & !is.na(data$AEGRADE) & data$AE==1),]
  data2 <- IQRexpandADDLII(data = data)
  attr <- attributes(data)
  aeNAMES <- attr$aeNAMES
  nonaeOBSnames <- setdiff(attr$obsNAMES,aeNAMES)
  covNAMES <- attr$covInfo$COLNAME
  catNAMES <- attr$catInfo$COLNAME
  lastEventTimeIndiv <- do.call(rbind,lapply(split(data2,data2$USUBJID), function (x) {
    maxTime <- max(x$TIME,na.rm = TRUE)
    data.frame(
      USUBJID = x$USUBJID[1],
      LASTTIME = maxTime,
      LASTTAD = x$TAD[x$TIME==maxTime][1],
      stringsAsFactors = FALSE
    )
  }))
  data3 <- dplyr::left_join(data2,lastEventTimeIndiv,by="USUBJID")
  data4 <- data3[!data3$NAME %in% nonaeOBSnames,]
  idsplaceboadding <- setdiff(data3$USUBJID,data4$USUBJID)
  if (length(idsplaceboadding) > 0) {
    dataadd <- data3[data3$USUBJID %in% idsplaceboadding,]
    dataadd <- dataadd[!duplicated(dataadd$USUBJID),]
    dataadd$TIME <- 0
    dataadd$TAD <- 0
    dataadd$TRTNAME <- "Placebo"
    dataadd$DURATION <- 0
    dataadd$NAME <- "Placebo Dose"
    dataadd$VALUE <- 0
    dataadd$AE <- 0
    dataadd$AEGRADE <- 0
    dataadd$AMT <- 0
    dataadd$EVID <- 1
    data4X <- dplyr::arrange(rbind(data4,dataadd),USUBJID,TIME)
  } else {
    data4X <- data4
  }
  keepColumns <- c("IXGDF","USUBJID","STUDY","STUDYN","TRTNAME","TRT",
                   "TIMEUNIT","TIME","TAD","DURATION","NAME","VALUE","UNIT",
                   "AE","AEGRADE","AESER","AEDRGREL","AMT","EVID","TINF","ADM",
                   "DOSE","LASTTIME","LASTTAD")
  addCovCols <- setdiff(c(covNAMES,catNAMES,addCols),keepColumns)
  checknames <- setdiff(keepColumns,names(data4X))
  if (length(checknames)!=0) {
    stopIQR(paste0("Missing columns: ",paste(checknames,collapse = ", ")))
  }
  data5 <- data4X[,c(keepColumns,addCovCols)]
  data5$COMMENT <- NA
  if ("PLACEBO" %in% names(data5))
    warningIQR("PLACEBO column in original data is overwritten (1: non-zero dose amounts present in individual)")
  data6 <- do.call(rbind,lapply(split(data5,data5$USUBJID), function (d) {
    PLACEBO <- 1
    if (sum(d$AMT>0,na.rm = TRUE)) PLACEBO <- 0
    PLACEBO
    d$PLACEBO <- PLACEBO
    d
  }))
  dS <- split(data6,data6$USUBJID)
  data7 <- do.call(rbind,lapply(seq_along(dS), function (k) {
    d <- dS[[k]]
    addAEnames <- setdiff(aeNAMES,d$NAME)
    if (length(addAEnames) > 0) {
      r <- d[rep(1,length(addAEnames)),]
      r$IXGDF <- NA 
      r$TIME <- r$LASTTIME
      r$TAD <- r$LASTTAD
      r$DURATION <- NA 
      r$NAME <- addAEnames
      r$VALUE <- 0
      r$UNIT <- NA
      r$EVID <- 0
      r$AMT <- 0
      r$TINF <- 0
      r$ADM <- 0
      r$AE <- 0 
      r$AEGRADE <- 0 
      r$AESER <- 0 
      r$AEDRGREL <- 0 
      r$AECENS <- 1
      d$AECENS <- 0
      r$COMMENT <- "Added (no)AE with AE=AEGRADE=AEDRGREL=AESER=0 and ACENS=1 (censored event)"
    } else {
      r <- NULL
      d$AECENS <- 0 
    }
    rbind(d,r)
  }))
  if (!all(data7$EVID[data7$AE %in% 1] %in% 0)) stopIQR("EVID should be set on 0 for all AE=1 events!")
  data_dose <- data7[data7$EVID==1,]
  data_ae <- data7[data7$EVID==0,]
  rownames(data_ae) <- NULL
  rownames(data_dose) <- NULL
  data_ae$VALUE <- NULL
  data_ae$UNIT <- NULL
  data_ae$EVID <- NULL
  data_ae$TINF <- NULL
  data_ae$ADM <- NULL
  data_ae$AMT <- NULL
  data_ae$LASTTAD <- NULL
  if (FLAGkeepHighestGradeEarliestTime) {
    dS <- split(data_ae,data_ae$USUBJID)
    data_ae <- do.call(rbind,lapply(seq_along(dS), function (k) {
      d <- dS[[k]]
      dS2 <- split(d,d$NAME)
      do.call(rbind,lapply(seq_along(dS2), function (k2) {
        d2 <- dS2[[k2]]
        if (nrow(d2) == 1) return(d2)
        d2 <- dplyr::arrange(d2,desc(AEGRADE),TIME) 
        d2[1,]
      }))
    }))
  }
  class(data_ae) <- c("IQRaedataER","IQRdataER","data.frame")
  attributes(data_ae)$covInfo <- attr$covInfo
  attributes(data_ae)$catInfo <- attr$catInfo
  attributes(data_ae)$data_dose <- data_dose
  data_ae
}
#'@export
exportXPT_IQRaedataER <- function(data,filename=NULL,addColLabels=NULL) {
  if (is.null(filename))
    stopIQR("filename must be provided")
  if (!is_IQRaedataER(data) & !("IQRaedataER" %in% attr(data,"class")))
    stopIQR("data is not an IQRaedataER object")
  x <- aux_fileparts(filename)
  filename <- x$filename
  pathname <- x$pathname
  if (nchar(filename) > 8)
    stopIQR("Please provide a filename with length of max 8 characters (w/o extension)")
  dataX <- data.frame(lapply(names(data), function (x) {
    x <- data[[x]]
    if (!is.null(levels(x))) {
      x <- levels(x)[x]
    }
    if (is.logical(x)) x <- as.double(x)
    if (is.integer(x)) x <- as.double(x)
    x
  }),row.names=NULL,stringsAsFactors=FALSE)
  names(dataX) <- names(data)
  attr(dataX,"covInfo") <- attr(data,"covInfo")
  attr(dataX,"catInfo") <- attr(data,"catInfo")
  attr(dataX,"data_dose") <- attr(data,"data_dose")
  attr(dataX,"modelinfo") <- attr(data,"modelinfo")
  attr(dataX,"INTERVAL_EXPOSURE") <- attr(data,"INTERVAL_EXPOSURE")
  attr(dataX,"DELTA_EXPOSURE")     <- attr(data,"DELTA_EXPOSURE")
  attr(dataX,"FLAGduplicateDoses") <- attr(data,"FLAGduplicateDoses")
  attr(dataX,"class") <- c("IQRaedataER","IQRdataER","data.frame")
  data <- dataX
  data <- addLabel_IQRaedataER(data,addColLabels)
  aux_mkdir(pathname)
  haven::write_xpt(data = data,path = paste0(pathname,'/',filename,'.xpt'),name = filename,version = 5)
  genComplianceLog(paste0(pathname,'/',filename,'.xpt'))
}
#'@export
addLabel_IQRaedataER <- function(data,addColLabels=NULL) {
  addLabel__ <- function (data,colName__,label) {
    if (colName__ %in% names(data)) {
      attr(data[[colName__]],"label") <- label
      attr(data[[colName__]],"class") <- unique(c("labelled",class(data[[colName__]])))
    }
    return(data)
  }
  data <- addLabel__(data,"IXGDF","Index of record in master dataset")
  data <- addLabel__(data,"IGNORE","Exclusion reason")
  data <- addLabel__(data,"USUBJID","Unique subject identifier")
  data <- addLabel__(data,"ID","Numeric subject ID for modeling software")
  data <- addLabel__(data,"STUDY","Short study name/number")
  data <- addLabel__(data,"STUDYN","Numeric study flag")
  data <- addLabel__(data,"TRTNAME","Name actual treatment given to subject")
  data <- addLabel__(data,"TRT","Numeric treatment flag")
  data <- addLabel__(data,"TIME","Actual time relative to first dose")
  data <- addLabel__(data,"TIMEPOS","Time since first record in subject")
  data <- addLabel__(data,"NT","Nominal event time")
  data <- addLabel__(data,"TAD","Time after last dose")
  data <- addLabel__(data,"TIMEUNIT","Unit of all numeric time definitions")
  data <- addLabel__(data,"YTYPE","Observation output number")
  data <- addLabel__(data,"NAME","Short name of event")
  data <- addLabel__(data,"UNIT","Unit of the value")
  data <- addLabel__(data,"DOSE","DOSE of last dose (carry-forward)")
  data <- addLabel__(data,"INDNAME","Indication name")
  data <- addLabel__(data,"IND","Numeric indication flag")
  data <- addLabel__(data,"DURATION","Duration of event")
  data <- addLabel__(data,"TYPENAME","Type of event")
  data <- addLabel__(data,"VALUE","Value of event defined by NAME")
  data <- addLabel__(data,"AE","Adverse event record flag")
  data <- addLabel__(data,"AEGRADE","Adverse event grade")
  data <- addLabel__(data,"AESER","Adverse event seriousness")
  data <- addLabel__(data,"AEDRGREL","Adverse event drug related")
  data <- addLabel__(data,"AECENS","Adverse event censoring flag")
  data <- addLabel__(data,"LASTTIME","Censoring time (last info)")
  data <- addLabel__(data,"COMMENT","Comment")
  data <- addLabel__(data,"PLACEBO","Placebo flag (no dose >0 identified)")
  data <- addLabel__(data,"Cmax","Cmax over defined interval at AE")
  data <- addLabel__(data,"Cavg","Cavg over defined interval at AE")
  data <- addLabel__(data,"Cmin","Cmin over defined interval at AE")
  data <- addLabel__(data,"Cavgacc","Accumulated Cavg from 0 to AE")
  data <- addLabel__(data,"Cae","Concentration at time of AE (NA if no AE)")
  covInfo <- attr(data,"covInfo")
  if (!is.null(covInfo)) {
    if (nrow(covInfo)>0) {
      for (k__ in 1:nrow(covInfo)) {
        if (covInfo$TIME.VARYING[k__]) {
          text <- paste0(covInfo$NAME[k__]," (",covInfo$UNIT[k__],")")
        } else {
          text <- paste0("Baseline ",covInfo$NAME[k__]," (",covInfo$UNIT[k__],")")
        }
        data <- addLabel__(data,covInfo$COLNAME[k__],text)
      }
    }
  }
  catInfo <- attr(data,"catInfo")
  if (!is.null(catInfo)) {
    if (nrow(catInfo)>0) {
      for (k__ in 1:nrow(catInfo)) {
        if (catInfo$TIME.VARYING[k__]) {
          text <- paste0(catInfo$NAME[k__]," (time varying)")
        } else {
          text <- catInfo$NAME[k__]
        }
        data <- addLabel__(data,catInfo$COLNAME[k__],text)
      }
    }
  }
  for (k__ in seq_along(addColLabels)) {
    colname <- names(addColLabels)[k__]
    data <- addLabel__(data,colname,addColLabels[[k__]])
  }
  return(data)
}
#'@export
exportDEFINE_IQRaedataER <- function(data,
                                     datasetName=NULL,
                                     datasetLocation=NULL,
                                     datasetDescription="AE Analysis dataset",
                                     addColLabels=NULL,
                                     filename=NULL) {
  exportDEFINEdocx_IQRaedataER(data,
                               datasetName,
                               datasetLocation,
                               datasetDescription,
                               addColLabels,
                               filename)
}
exportDEFINEdocx_IQRaedataER <- function(data,
                                         datasetName=NULL,
                                         datasetLocation=NULL,
                                         datasetDescription="AE Analysis dataset",
                                         addColLabels=NULL,
                                         filename=NULL) {
  if (!is_IQRaedataER(data) & !("IQRaedataER" %in% attr(data,"class")))
    stopIQR("Data is not an IQRaedataER object")
  if (is.null(datasetName))
    stopIQR("Please provide a name for the dataset as it should appear in the define file")
  if (is.null(datasetLocation))
    warningIQR("Please provide a location (path) for the dataset as it should appear in the define file")
  dataFirst__ <- data.frame(NAME=datasetName,DESCRIPTION=datasetDescription,LOCATION=datasetLocation,stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__ <- data.frame(NAME=names(data),stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__$TYPE <-lapply(data, function (x) { if (is.numeric(x)) { out <- "Numeric" } else { out <- "String" }; out })
  data <- addLabel_IQRaedataER(data,addColLabels)
  dataDefine__$LABEL <-lapply(data, function (x) {
    label__ <- attr(x,"label")
    if (is.null(label__)) label__ <- "UNKNOWN"
    label__
  })
  dataDefine__$VALUES <- getValueTxtDefineAEER(dataDefine__$NAME,unlabel_dataframe(data))
  dataDefine__$COMMENTS <- " "
  datasetName__ <- toupper(datasetName)
  RMDTEXT__ <- rmdEMPTY()
  RMDTEXT__ <- RMDTEXT__ + rmdTITLE(title=paste0("Define file for the \"",datasetName__,"\" dataset"),subtitle=NULL,date=NULL)
  RMDTEXT__ <- RMDTEXT__ + rmdNOINTRO()
  RMDTEXT__ <- RMDTEXT__ + "!BLOCKSTART[keepNext](block_kwnext)\n"
  RMDTEXT__ <- RMDTEXT__ + rmdSECTION("Dataset name, description, and location",numbered=FALSE)
  RMDTEXT__ <- RMDTEXT__ + rmdTABLEDF(dataFirst__,label="overview",fontsize=8,caption="Dataset information",ignoreCaption=TRUE)
  RMDTEXT__ <- RMDTEXT__ + rmdNEWPAGE()
  RMDTEXT__ <- RMDTEXT__ + rmdSECTION(paste0(datasetName__," specification"),numbered=FALSE)
  RMDTEXT__ <- RMDTEXT__ + "Missing values are coded in the dataset as '.' and referenced in this specification as 'NA'.\n\n"
  RMDTEXT__ <- RMDTEXT__ + rmdTABLEDF(dataDefine__,label="overview",fontsize=8,caption="Definition of dataset contents",ignoreCaption=TRUE)
  RMDTEXT__ <- RMDTEXT__ + "\n"
  RMDTEXT__ <- RMDTEXT__ + "!BLOCKEND(block_kwnext)\n"
  if (is.null(filename)) {
    filename__ <- paste0(datasetName__,"_define",".rmd")
  } else {
    x <- aux_fileparts(filename)
    filename__ <- paste0(x$pathname,"/",x$filename,".rmd")
  }
  export_IQRrmd(RMDTEXT__,filename__)
  if (has_IQReport_executable()) {
    IQReport(filename__)
  }
}
getValueTxtDefineAEER <- function(NAMES,data) {
  data <- unlabel_dataframe(data)
  imputationInformation <- attr(data,"imputeInfo")
  covInfo <- attr(data,"covInfo")
  catInfo <- attr(data,"catInfo")
  doseNAMES <- attr(data,"doseNAMES")
  obsNAMES <- attr(data,"obsNAMES")
  aeNAMES  <- attr(data,"aeNAMES")
  methodBLLOQ <- attr(data,"methodBLLOQ")
  if (length(which(grepl(":::",unique(data$NAME)))) > 0) {
    doseNAMES <- gsub(" ",":::",doseNAMES)
    obsNAMES <- gsub(" ",":::",obsNAMES)
  }
  output__ <- rep(" ",length(NAMES))
  for (k__ in seq_along(NAMES)) {
    NAME <- NAMES[k__]
    if (NAME=="IXGDF")
      output__[k__] <- "1...N"
    if (NAME=="USUBJID")
      output__[k__] <- "Unique subject ID or a derivative of it, allowing to identify the subject"
    if (NAME=="ID")
      output__[k__] <- "Numeric subject ID for modeling software"
    if (NAME=="STUDY")
      output__[k__] <- paste0(sort(unique(data$STUDY)),collapse=", \n")
    if (NAME=="STUDYN") {
      if ("STUDY" %in% names(data)) {
        x <- dplyr::arrange(unique(data[,c("STUDYN","STUDY")]),STUDYN)
        output__[k__] <- paste0(paste0(x$STUDYN," (STUDY=",x$STUDY,")"),collapse=", \n")
      } else {
        output__[k__] <- paste0(unique(data$STUDYN),collapse=", ")
      }
    }
    if (NAME=="STUDYDES")
      output__[k__] <- "See 'LABEL' column"
    if (NAME=="TRTNAME")
      output__[k__] <- paste0(sort(unique(data$TRTNAME)),collapse=", \n")
    if (NAME=="TRT") {
      if ("TRTNAME" %in% names(data)) {
        x <- dplyr::arrange(unique(data[,c("TRT","TRTNAME")]),TRT)
        output__[k__] <- paste0(paste0(x$TRT," (TRTNAME=",x$TRTNAME,")"),collapse=", \n")
      } else {
        output__[k__] <- paste0(unique(data$TRT),collapse=", ")
      }
    }
    if (NAME=="INDNAME")
      output__[k__] <- paste0(sort(unique(data$INDNAME)),collapse=", \n")
    if (NAME=="IND") {
      if ("INDNAME" %in% names(data)) {
        x <- dplyr::arrange(unique(data[,c("IND","INDNAME")]),IND)
        output__[k__] <- paste0(paste0(x$IND," (INDNAME=",x$INDNAME,")"),collapse=", \n")
      } else {
        output__[k__] <- paste0(unique(data$IND),collapse=", ")
      }
    }
    if (NAME=="COMPOUND")
      output__[k__] <- paste0(sort(unique(data$COMPOUND)),collapse=", \n")
    if (NAME=="TIME")
      output__[k__] <- "'TIME' values in the unit defined in the 'TIMEUNIT' column"
    if (NAME=="TAD")
      output__[k__] <- "'TAD' values in the unit defined in the 'TIMEUNIT' column"
    if (NAME=="TIMEUNIT")
      output__[k__] <- paste0("'",unique(data$TIMEUNIT),"'")
    if (NAME=="DURATION")
      output__[k__] <- "'DURATION' values in the unit defined in the 'TIMEUNIT' column. -1 if event ongoing post end of observation period"
    if (NAME=="TYPENAME")
      output__[k__] <- paste0(sort(unique(data$TYPENAME)),collapse=", \n")
    if (NAME=="NAME")
      output__[k__] <- paste0(sort(unique(data$NAME)),collapse=", \n")
    if (NAME=="AE")
      output__[k__] <- "1 if record codes an adverse event, 0 if not"
    if (NAME=="AEGRADE")
      output__[k__] <- "Can be 1,2,3,4,5. NA for "
    if (NAME=="AESER")
      output__[k__] <- "1: seriuous adverse event, 0: not serious"
    if (NAME=="AEDRGREL")
      output__[k__] <- "1: drug related AE, 0: not drug related"
    if (NAME=="COMMENT")
      output__[k__] <- "Various content - might be 'NA'"
    if (!is.null(covInfo)) {
      for (kk in 1:nrow(covInfo)) {
        if (NAME==covInfo$COLNAME[kk]) {
          output__[k__] <- "See 'LABEL' column"
        }
      }
    }
    if (!is.null(catInfo)) {
      for (kk__ in 1:nrow(catInfo)) {
        if (NAME==catInfo$COLNAME[kk__]) {
          output__[k__] <- paste0(aux_explodePC(catInfo$VALUES[kk__])," (",aux_explodePC(catInfo$VALUETXT[kk__]),")",collapse=", \n")
        }
      }
    }
  }
  return(output__)
}
#'@export
export_IQRaedataER <- function(data,
                               filename        = NULL,
                               FLAGxpt         = FALSE,
                               FLAGdefine      = TRUE,
                               addColLabels    = NULL) {
  if (!is_IQRaedataER(data))
    stopIQR("input argument is not an IQRaedataER object")
  if (is.null(filename))
    stopIQR("please provide a filename for the NLME CSV file")
  x__ <- aux_fileparts(filename)
  filename__ <- x__$filename
  pathname__ <- x__$pathname
  if (nchar(filename__) > 8 & FLAGxpt)
    stopIQR("please provide a filename with length of max 8 characters (w/o extension)")
  IQRoutputCSV(data,filename=paste0(pathname__,"/",filename__),na=".",quote=FALSE)
  if (FLAGxpt) exportXPT_IQRaedataER(data,paste0(pathname__,"/",filename__),addColLabels=addColLabels)
  if (FLAGdefine) {
    datasetName__     <- filename__
    datasetLocation__ <- paste0(pathname__,"/",datasetName__)
    if (FLAGxpt) {
      datasetLocation__ <- paste0(datasetLocation__,".xpt")
    } else {
      datasetLocation__ <- paste0(datasetLocation__,".csv")
    }
    filenameRMD__        <- paste0(pathname__,"/",datasetName__,"_define.rmd")
    exportDEFINE_IQRaedataER(data               = data,
                             datasetName        = datasetName__,
                             datasetLocation    = datasetLocation__,
                             datasetDescription = "AE analysis dataset",
                             addColLabels       = addColLabels,
                             filename           = filenameRMD__)
  }
}
#'@export
load_IQRaedataER <- function(filename) {
  filenameCSV__ <- paste0(aux_strrep(filename,".csv",""),".csv")
  filenameATR__ <- paste0(aux_strrep(filename,".csv",""),".atr")
  if (!file.exists(filenameCSV__))
    stopIQR(sprintf("File '%s' does not exist",filenameCSV__))
  if (!file.exists(filenameATR__))
    stopIQR(sprintf("Attribute file '%s' does not exist. Please consider importing the datafile with the function 'IQRdataGENERAL'",filenameATR__))
  data__ <- IQRloadCSVdata(filenameCSV__)
  atrcontents <- loadAttributeFile(filenameATR__)
  if (any(grepl("_", names(data__)))) {
    warningIQR("There are underscores in column names which are removed.")
    names(data__) <- gsub("_", "", names(data__))
    atrcontents$catInfo$COLNAME <- gsub("_", "", atrcontents$catInfo$COLNAME)
    atrcontents$covInfo$COLNAME <- gsub("_", "", atrcontents$covInfo$COLNAME)
    atrcontents$imputeInfo$COVNAME <- gsub("_", "", atrcontents$imputeInfo$COVNAME)
  }
  numericColumns__ <- c(
    "IXGDF","IND","STUDYN","TRT","TIME","TAD","DURATION","AE",
    "AEGRADE","AESER","AEDRGREL","DOSE","LASTTIME","PLACEBO","AECENS",
    "Cmax","Cavg","Cmin","Cavgacc","Cae"
  )
  numericColumnsPresent__ <- numericColumns__[numericColumns__ %in% names(data__)]
  for (k__ in seq_along(numericColumnsPresent__)) {
    suppressWarnings(data__[[numericColumnsPresent__[k__]]] <- as.numeric(data__[[numericColumnsPresent__[k__]]]))
  }
  data__$TIMEUNIT <- toupper(data__$TIMEUNIT)
  data__ <- data.frame(lapply(data__, function(x__) {
    if (!is.numeric(x__)) x__ <- gsub(x=x__,pattern=":::",replacement=" ")
    x__
  }),row.names=NULL,stringsAsFactors=FALSE)
  attributes(data__) <- c(attributes(data__),atrcontents)
  class(data__) <- c("IQRaedataER","IQRdataER","data.frame")
  return(data__)
}
#'@export
plotIndivAE_IQRdataGENERAL <- function (data,filename=NULL,AENAMES=NULL,nrow=4,shapesize=3) {
  req_columns <- c("USUBJID","AE","NAME","EVID","TIME","TIMEUNIT")
  for (k in 1:length(req_columns)) {
    if (!req_columns[k] %in% names(data)) stop(paste0("Column ",req_columns[k]," is missing in the data"))
  }
  if (!is.null(AENAMES)) {
    data <- data[data$EVID==1 | data$NAME %in% AENAMES,]
  }
  if (sum(data$AE)==0) stop("No AE=1 records in the dataset (for defined AENAMES - if defined)")
  IDae <- unique(data$USUBJID[data$AE==1])
  dataae <- data[data$USUBJID %in% IDae,]
  dataae <- dataae[dataae$EVID==1 | dataae$AE==1,]
  x <- sort(unique(dataae$NAME))
  dataae$NAMETYPE <- factor(dataae$NAME ,levels=x,labels=x)
  colors <- IQRtoolsColors[2:(length(x)+1)]
  names(colors) <- x
  shapes <- seq(1:length(x))
  names(shapes) <- x
  dS <- split(dataae,dataae$USUBJID)
  p <- lapply(seq_along(dS), function (k) {
    d <- dS[[k]]
    IQRggplot() +
      geom_vline(data=d[d$EVID==1,],aes(xintercept = TIME),color="red",linetype="dashed") +
      ggtitle(NULL,subtitle = paste0(d$USUBJID[1]," (ID=",d$ID[1],")")) +
      geom_point(data=d[d$AE==1,],aes(x=TIME,y=NAME,color=NAME,shape=NAME),size=shapesize) +
      scale_color_manual("",values=colors)+
      scale_shape_manual("",values=shapes) +
      xlab(paste0("Time (",d$TIMEUNIT[1],")")) +
      ylab(NULL) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "bottom")
  })
  if (is.null(filename)) return(p)
  filename <- paste0(gsub(".pdf","",filename,fixed = TRUE),".pdf")
  IQRoutputFigure(x=p,filename = filename,opt.layout = opt.layout(nrow = nrow,ncol = 1),opt.pagesize = opt.pagesize(scaleHeight = 1.5))
  return(invisible(NULL))
}
#'@export
summaryN_IQRaedataER <- function (data,SIGNIF=3,filename=NULL,title="Summary of AE in data") {
  data <- data[data$AE==1 & data$AEDRGREL==1 | data$AE==0,]
  allAENAMEs <- unique(data$NAME)
  tab <- dplyr::arrange(do.call(rbind,lapply(seq_along(allAENAMEs), function (k) {
    AE <- allAENAMEs[k]
    Ntotal <- length(unique(data$USUBJID))
    Nae <-  length(unique(data$USUBJID[data$AE==1 & data$NAME==AE]))
    ci <- stats::binom.test(x = Nae,n = Ntotal,conf.level = 0.95)
    minGrade <- suppressWarnings(min(data$AEGRADE[data$NAME==AE & data$AE==1],na.rm = TRUE))
    maxGrade <- suppressWarnings(max(data$AEGRADE[data$NAME==AE & data$AE==1],na.rm = TRUE))
    data.frame(
      AENAME = AE,
      Ntotal = Ntotal,
      Nae = Nae,
      FREQ = paste0(signif(100*Nae/Ntotal,SIGNIF),"%"),
      FREQ_CI95 = paste0("[",signif(100*ci$conf.int[1],SIGNIF),",",signif(100*ci$conf.int[2],SIGNIF),"] %"),
      GRADE = ifelse(is.infinite(minGrade),"-",paste0(minGrade,"-", maxGrade)),
      stringsAsFactors = FALSE
    )
  })),desc(Nae))
  names(tab) <- c("Adverse Event (AE)","Number of all subjects","Number of subjects with AE","AE Frequency [%]", "AE Frequency 95% CI [%]*","Range of AE grades")
  IQRoutputTable(
    xtable = tab,
    xtitle = title,
    xfooter = "Occurrence of same AE in subject only counted once\nOnly drug related AEs considered (AEDRGREL=1)\n\\* Based on Clopper-Pearson test",
    filename = filename
  )
}
#'@export
summary_IQRaedataER <- function (
  data = data,
  stratifyTable = NULL,
  stratifyPlot = NULL,
  plotX = NULL,
  xlabel = NULL,
  stratName = NULL,
  breaksX = NULL,
  title = "Summary of drug related adverse events",
  addTitleFigure = "",
  filename = NULL,
  xtickangle = 0,
  SIGNIF = 3
) {
  if (!is.null(stratifyTable)) {
    if (!stratifyTable %in% names(data)) stopIQR(paste0(stratifyTable," is not a column of data"))
  }
  allAENAMEs <- unique(data$NAME[data$AE==1]) 
  allSTRAT <- sort(unique(data[[stratifyTable]]))
  order <- names(sort(table(data[data$AE==1,"NAME"]),decreasing = TRUE))
  res <- do.call(rbind, lapply(seq_along(allSTRAT), function (ks) {
    ds <- data[data[[stratifyTable]]==allSTRAT[ks],]
    PLOTX <- NA
    PLOTSTRAT <- NA
    if (!is.null(plotX)) PLOTX <- ds[[plotX]][1]
    if (!is.null(stratifyPlot)) PLOTSTRAT <- ds[[stratifyPlot]][1]
    do.call(rbind,lapply(seq_along(allAENAMEs), function (k) {
      AE <- allAENAMEs[k]
      d <- ds[ds$NAME==AE & ds$AE==1 & ds$AEDRGREL==1,]
      Nsubjects <- length(unique(ds$USUBJID))
      Naesubjects <- length(unique(d$USUBJID))
      minGrade <- suppressWarnings(min(d$AEGRADE))
      maxGrade <- suppressWarnings(max(d$AEGRADE))
      ci <- stats::binom.test(x = Naesubjects,n = Nsubjects,conf.level = 0.95)
      data.frame(
        AENAME = AE,
        STRAT = allSTRAT[ks],
        Nsubjects = Nsubjects,
        Naesubjects = Naesubjects,
        FREQ = paste0(signif(Naesubjects/Nsubjects*100,SIGNIF)," [",signif(100*ci$conf.int[1],SIGNIF),",",signif(100*ci$conf.int[2],SIGNIF),"] %"),
        GRADE = paste0(minGrade,"-", maxGrade),
        ORDERTAB = match(AE,order),
        PLOTX = PLOTX,
        PLOTSTRAT = PLOTSTRAT,
        FREQPLOT = Naesubjects/Nsubjects*100,
        FREQPLOTCI025 = 100*ci$conf.int[1],
        FREQPLOTCI975 = 100*ci$conf.int[2],
        stringsAsFactors = FALSE
      )
    }))
  }))
  tab <- res[,c("AENAME","STRAT","Nsubjects","Naesubjects","FREQ","GRADE","ORDERTAB")]
  tab$GRADE <- gsub("Inf--Inf","-",tab$GRADE)
  tab <- dplyr::arrange(tab,ORDERTAB)
  tab$AENAME[duplicated(tab$AENAME)] <- ""
  tab$ORDERTAB <- NULL
  names(tab) <- c("Adverse Event (AE)", paste0("Stratification (",stratifyTable,")"), "Number overall subjects", "Number subjects with AE", "AE Frequency [95% CI]*", "Range of AE grades")
  tabout <- IQRoutputTable(xtable = tab,xtitle = title,
                           xfooter = "Occurrence of same AE in subject only counted once\nOnly drug related AEs considered (AEDRGREL=1)\n\\* Based on Clopper-Pearson test",
                           filename = filename)
  if (is.null(xlabel)) xlabel <- plotX
  if (is.null(stratName)) stratName <- stratifyPlot
  if (!is.null(plotX)) {
    plotout <- lapply(split(res,res$AENAME), function (dp) {
      if (is.null(stratifyPlot)) {
        dp$PLOTSTRAT <- as.factor(1)
      } else {
        dp$PLOTSTRAT <- factor(dp$PLOTSTRAT,levels=sort(unique(dp$PLOTSTRAT)))
      }
      subtitle = "x indicate the 95% confidence interval (Clopper-Pearson)\nNumbers in labels indicate number of occurred adverse events"
      p <- IQRggplot(dp)
      if (length(unique(dp$PLOTSTRAT))>1) {
        p <- p +
          geom_point(aes(x=PLOTX,y=FREQPLOT,color=PLOTSTRAT,shape=PLOTSTRAT),size=3) +
          geom_label(aes(x=PLOTX,y=FREQPLOT,label=Naesubjects,color=PLOTSTRAT))
        p <- p +  scale_color_IQRtools(stratName) +
          scale_shape_discrete(stratName) +
          scale_linetype_discrete(stratName)
        p <- p +
          geom_point(aes(x=PLOTX,y=FREQPLOTCI025,color=PLOTSTRAT),shape="x") +
          geom_point(aes(x=PLOTX,y=FREQPLOTCI975,color=PLOTSTRAT),shape="x",size=2)
      } else {
        p <- p +
          geom_point(aes(x=PLOTX,y=FREQPLOT),size=3) +
          geom_label(aes(x=PLOTX,y=FREQPLOT,label=Naesubjects))
        p <- p +
          geom_point(aes(x=PLOTX,y=FREQPLOTCI025),shape="x") +
          geom_point(aes(x=PLOTX,y=FREQPLOTCI975),shape="x",size=2)
      }
      p <- p +
        ggtitle(paste0(dp$AENAME[1],addTitleFigure),subtitle = subtitle) +
        xlab(xlabel) +
        ylab("Frequency of AE (%)") +
        theme(axis.text.x = element_text(angle = xtickangle, hjust=1))
      if (!is.null(breaksX)) {
        p <- p + scale_x_continuous(breaks=breaksX)
      }
      p
    })
  } else {
    plotout <- NULL
  }
  list(tab=tabout,plot=plotout)
}
#'@export
summaryCat_IQRdataER <- function(data,
                                 catInfoAdd = NULL,
                                 catNames = NULL,
                                 stratifyColumns = "STUDY",
                                 stratifyOrder = NULL,
                                 FLAGtotal = NULL,
                                 SIGNIF = 3,
                                 tableTitle = NULL,
                                 footerAddText = NULL,
                                 filename = NULL,
                                 FLAGpatients = FALSE) {
  if(is_IQRdataGENERAL(data)){
    stopIQR("Use summaryCat_IQRdataGENERAL instead")
  }
  catInfo__ <- attributes(data)$catInfo
  if (!is.null(levels(catInfo__$COLNAME))) {
    stopIQR("Covariate information in attributes contains levels. This is not allowed.")
  }
  catInfo__ <- rbind(catInfo__,data.frame(catInfoAdd,stringsAsFactors = FALSE))
  catNamesData__ <- catInfo__$COLNAME
  if (is.null(catNames)) {
    catNames <- catNamesData__
  }
  if (is.null(catNames)) {
    message("No categorical covariates defined or present")
    return(invisible(NULL))
  }
  if (!all(catNames %in% catNamesData__)) {
    stopIQR(paste0("The following catNames are not available in the provided data:\n  ",paste0(catNames[!(catNames %in% catNamesData__)],collapse=", ")))
  }
  if (is.null(FLAGtotal)) {
    FLAGtotal <- rep(FALSE,length(stratifyColumns))
  }
  if (length(FLAGtotal)==1) {
    FLAGtotal <- rep(FLAGtotal,length(stratifyColumns))
  }
  if (length(FLAGtotal) != length(stratifyColumns)) {
    stopIQR("Missmatch between length of stratifyColumns and FLAGtotal input arguments")
  }
  if (!is.list(stratifyOrder)) {
    stratifyOrder <- list(stratifyOrder)
  }
  for (k__ in seq_along(stratifyColumns)) {
    entries__ <- unique(data[[stratifyColumns[k__]]])
    order__ <- unlist(stratifyOrder[k__])
    if (!is.null(order)) {
      if (!all(order__ %in% entries__)) {
        stopIQR(paste0("Not all entries in stratifyColumns are present for stratification column ",stratifyColumns[k__]))
      }
    }
  }
  if (is.null(tableTitle)) {
    tableTitle <- "Summary of demographic and baseline characteristics for categorical information"
  }
  datafirst__ <- as.data.frame(data[!duplicated(data$USUBJID),])
  catInfo__ <- catInfo__[catInfo__$COLNAME %in% catNames,]
  catInfo0__ <- catInfo__
  FLAGmissingVal <- FALSE
  for (icat__ in catNames) {
    if (any(is.na(datafirst__[[icat__]]))) {
      tmp__ <- catInfo__[catInfo__$COLNAME == icat__,]
      tmpValImpute__ <- max(as.numeric(aux_explode(tmp__$VALUES)))+1
      tmp__$VALUES <- paste0(tmp__$VALUES, ",", tmpValImpute__)
      tmp__$VALUETXT <- paste0(tmp__$VALUETXT, ",n.a.**")
      catInfo__[catInfo__$COLNAME == icat__,] <- tmp__
      datafirst__[[icat__]][is.na(datafirst__[[icat__]])] <- as.character(tmpValImpute__)
      FLAGmissingVal <- TRUE
    }
  }
  baseTable__ <- do.call(rbind,lapply(1:nrow(catInfo__), function (kRowCatInfo__) {
    row__ <- catInfo__[kRowCatInfo__,]
    characteristic__ <- row__$NAME
    colname__ <- row__$COLNAME
    categories__ <- aux_explodePC(as.character(row__$VALUETXT))
    values__ <- as.numeric(aux_explodePC(as.character(row__$VALUES)))
    subtable__ <- data.frame(
      Characteristic = characteristic__,
      Category = categories__,
      COLNAME = colname__,
      VALUE = values__,
      stringsAsFactors=FALSE
    )
  }))
  table__ <- baseTable__
  table__$COLNAME <- NULL
  table__$VALUE <- NULL
  FLAGmissingCat <- FALSE
  for (kstratcolumn__ in seq_along(stratifyColumns)) {
    stratifyColumn__ <- stratifyColumns[kstratcolumn__]
    FLAGtotal__ <- FLAGtotal[kstratcolumn__]
    stratifyOrder__ <- unlist(stratifyOrder[kstratcolumn__])
    datafirst__$stratifyColumn__ <- datafirst__[[stratifyColumn__]]
    if (stratifyColumn__ %in% catInfo0__$COLNAME) {
      tmpVal__ <- aux_explode(catInfo0__$VALUES[catInfo0__$COLNAME == stratifyColumn__])
      tmpTxt__ <- aux_explode(catInfo0__$VALUETXT[catInfo0__$COLNAME == stratifyColumn__])
      if (!is.null(stratifyOrder__)) stratifyOrder__ <- as.character(factor(stratifyOrder__, levels = tmpVal__, labels = tmpTxt__))
      datafirst__$stratifyColumn__ <- as.character(factor(datafirst__$stratifyColumn__, levels = tmpVal__, labels = tmpTxt__))
    }
    if (is.null(stratifyOrder__))
      stratifyOrder__ <- unique(datafirst__$stratifyColumn__[!is.na(datafirst__$stratifyColumn__)])
    if (any(is.na(datafirst__$stratifyColumn__)))
      stratifyOrder__ <- c(stratifyOrder__, NA)
    stratifyOrder__[is.na(stratifyOrder__)] <- "missing*"
    datafirst__$stratifyColumn__[is.na(datafirst__$stratifyColumn__)] <- "missing*"
    if ("missing*" %in% stratifyOrder__) FLAGmissingCat <- TRUE
    dummy__ <- sapply(stratifyOrder__, function (strata__) {
      stratData__ <- datafirst__[datafirst__$stratifyColumn__ == strata__,]
      NTOTALstrat__ <- nrow(stratData__)
      STRATCOLname__ <- paste0(stratData__$stratifyColumn__[1]," (N=",NTOTALstrat__,")")
      stratValueCol__ <- sapply(1:nrow(baseTable__), function (kRowTable__) {
        row__ <- baseTable__[kRowTable__,]
        COLNAME__ <- row__$COLNAME
        VALUE__ <- row__$VALUE
        NCATstraty__ <- sum(stratData__[COLNAME__] == VALUE__,na.rm=TRUE)
        PERCCATstraty__ <- signif(NCATstraty__/NTOTALstrat__*100,SIGNIF)
        paste0(NCATstraty__, " (",PERCCATstraty__,"%)")
      })
      table__ <<- cbind(table__, stratValueCol__)
      names(table__)[ncol(table__)] <<- STRATCOLname__
    })
    if (FLAGtotal__) {
      dataTOTAL__ <- datafirst__
      NTOTAL__ <- nrow(dataTOTAL__)
      totalValueCol__ <- sapply(1:nrow(baseTable__), function (kRowTable__) {
        row__ <- baseTable__[kRowTable__,]
        COLNAME__ <- row__$COLNAME
        VALUE__ <- row__$VALUE
        NCAT__ <- sum(dataTOTAL__[COLNAME__] == VALUE__,na.rm=TRUE)
        PERCCAT__ <- signif(NCAT__/NTOTAL__*100,SIGNIF)
        paste0(NCAT__, " (",PERCCAT__,"%)")
      })
      table__ <- cbind(table__, totalValueCol__)
      names(table__)[ncol(table__)] <- paste0("TOTAL (N=",NTOTAL__,")")
    }
  } 
  table__$Characteristic <- as.character(table__$Characteristic)
  table__$Characteristic[duplicated(table__$Characteristic)] = " "
  if (!is.null(filename)) {
    report__ <- TRUE
  } else {
    report__ <- FALSE
  }
  if (FLAGpatients) {
    footer__ <- "N: Number of patients<br>Number of patients in each category and percentage within this category"
  } else {
    footer__ <- "N: Number of subjects<br>Number of subjects in each category and percentage within this category"
  }
  if (FLAGmissingCat)
    footer__ <- paste0(footer__, "<br>","* stratification value missing in dataset")
  if (FLAGmissingVal)
    footer__ <- paste0(footer__, "<br>","** missing values in dataset")
  if (!is.null(footerAddText)) {
    footer__ <- paste0(footer__,"<br>",footerAddText)
  }
  if (!is.null(filename)) {
    filename <- paste0(aux_strrep(filename,".txt",""),".txt")
    IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=tableTitle,report=report__,filename=filename)
    return(invisible(NULL))
  }
  output__ <- IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=tableTitle,report=report__)
  output__
}
#'@export
summaryCov_IQRdataER <- function(data,
                                 covInfoAdd = NULL,
                                 covNames = NULL,
                                 stratifyColumns = "STUDY",
                                 stratifyOrder = NULL,
                                 FLAGtotal = NULL,
                                 SIGNIF = 3,
                                 tableTitle = NULL,
                                 footerAddText = NULL,
                                 filename = NULL,
                                 FLAGpatients = FALSE) {
  if(is_IQRdataGENERAL(data)){
    stopIQR("Use summaryCov_IQRdataGENERAL instead")
  }
  covInfo__ <- attributes(data)$covInfo
  if (!is.null(levels(covInfo__$COLNAME))) {
    stopIQR("Covariate information in attributes contains levels. This is not allowed.")
  }
  covInfo__ <- rbind(covInfo__,data.frame(covInfoAdd,stringsAsFactors = FALSE))
  covNamesData__ <- covInfo__$COLNAME
  if (is.null(covNames)) {
    covNames <- covNamesData__
  }
  if (is.null(covNames)) {
    message("No continuous covariates defined or present")
    return(invisible(NULL))
  }
  if (!all(covNames %in% covNamesData__)) {
    stopIQR(paste0("The following covNames are not available in the provided data:\n  ",paste0(covNames[!(covNames %in% covNamesData__)],collapse=", ")))
  }
  if (is.null(FLAGtotal)) {
    FLAGtotal <- rep(FALSE,length(stratifyColumns))
  }
  if (length(FLAGtotal)==1) {
    FLAGtotal <- rep(FLAGtotal,length(stratifyColumns))
  }
  if (length(FLAGtotal) != length(stratifyColumns)) {
    stopIQR("Missmatch between length of stratifyColumns and FLAGtotal input arguments")
  }
  if (!is.list(stratifyOrder)) {
    stratifyOrder <- list(stratifyOrder)
  }
  for (k__ in seq_along(stratifyColumns)) {
    entries__ <- unique(data[[stratifyColumns[k__]]])
    order__ <- unlist(stratifyOrder[k__])
    if (!is.null(order)) {
      if (!all(order__ %in% entries__)) {
        stopIQR(paste0("Not all entries in stratifyColumns are present for stratification column ",stratifyColumns[k__]))
      }
    }
  }
  if (is.null(tableTitle)) {
    tableTitle <- "Summary of demographic and baseline characteristics for continuous information"
  }
  datafirst__ <- as.data.frame(data[!duplicated(data$USUBJID),])
  covInfo__ <- covInfo__[covInfo__$COLNAME %in% covNames,]
  covInfo0__ <- covInfo__
  catInfo__ <- attr(data, "catInfo")
  baseTable__ <- do.call(rbind,lapply(1:nrow(covInfo__), function (kRowcovInfo__) {
    row__ <- covInfo__[kRowcovInfo__,]
    characteristic__ <- paste0(row__$NAME," (",row__$UNIT,")")
    colname__ <- row__$COLNAME
    subtable__ <- data.frame(
      Characteristic = characteristic__,
      COLNAME = colname__,
      stringsAsFactors=FALSE
    )
  }))
  table__ <- baseTable__
  table__$COLNAME <- NULL
  FLAGmissingVal <- FALSE
  FLAGmissingCat <- FALSE
  for (kstratcolumn__ in seq_along(stratifyColumns)) {
    stratifyColumn__ <- stratifyColumns[kstratcolumn__]
    FLAGtotal__ <- FLAGtotal[kstratcolumn__]
    stratifyOrder__ <- unlist(stratifyOrder[kstratcolumn__])
    if (is.null(stratifyOrder__))
      stratifyOrder__ <- unique(datafirst__[[stratifyColumn__]][!is.na(datafirst__[[stratifyColumn__]])])
    if (any(is.na(datafirst__[[stratifyColumn__]])))
      stratifyOrder__ <- c(stratifyOrder__, NA)
    if (stratifyColumn__ %in% catInfo__$COLNAME) {
      tmpVal__ <- aux_explode(catInfo__$VALUES[catInfo__$COLNAME == stratifyColumn__])
      tmpTxt__ <- aux_explode(catInfo__$VALUETXT[catInfo__$COLNAME == stratifyColumn__])
      stratifyOrder__ <- as.character(factor(stratifyOrder__, levels = tmpVal__, labels = tmpTxt__))
      datafirst__[[stratifyColumn__]] <- as.character(factor(datafirst__[[stratifyColumn__]], levels = tmpVal__, labels = tmpTxt__))
    }
    stratifyOrder__[is.na(stratifyOrder__)] <- "missing*"
    datafirst__[[stratifyColumn__]][is.na(datafirst__[[stratifyColumn__]])] <- "missing*"
    if ("missing*" %in% stratifyOrder__) FLAGmissingCat <- TRUE
    dummy__ <- sapply(stratifyOrder__, function (strata__) {
      stratData__ <- datafirst__[datafirst__[stratifyColumn__] == strata__,]
      stratData__ <- stratData__[!is.na(stratData__[stratifyColumn__]),]
      NTOTALstrat__ <- nrow(stratData__)
      STRATCOLname__ <- paste0(stratData__[[stratifyColumn__]][1]," (N=",NTOTALstrat__,")")
      stratValueCol__ <- sapply(1:nrow(baseTable__), function (kRowTable__) {
        row__ <- baseTable__[kRowTable__,]
        COLNAME__ <- row__$COLNAME
        if (!all(is.na(stratData__[[COLNAME__]]))) {
          MEAN <- signif(mean(stratData__[[COLNAME__]],na.rm=TRUE),SIGNIF)
          STD <- signif(stats::sd(stratData__[[COLNAME__]],na.rm=TRUE),SIGNIF)
          MAX <- signif(max(stratData__[[COLNAME__]],na.rm=TRUE),SIGNIF)
          MIN <- signif(min(stratData__[[COLNAME__]],na.rm=TRUE),SIGNIF)
          nNA <- sum(is.na(stratData__[[COLNAME__]]))
          out__ <- paste0(MEAN, " (",STD,") [",MIN,"-",MAX,"]")
          if (nNA > 0) {out__ <- paste0(out__, " (",nNA," n.a.**)"); FLAGmissingVal <<- TRUE}
        } else {
          out__ <- "-"
        }
        out__
      })
      table__ <<- cbind(table__, stratValueCol__)
      names(table__)[ncol(table__)] <<- STRATCOLname__
    })
    if (FLAGtotal__) {
      dataTOTAL__ <- datafirst__[!is.na(datafirst__[stratifyColumn__]),]
      NTOTAL__ <- nrow(dataTOTAL__)
      totalValueCol__ <- sapply(1:nrow(baseTable__), function (kRowTable__) {
        row__ <- baseTable__[kRowTable__,]
        COLNAME__ <- row__$COLNAME
        MEAN <- signif(mean(dataTOTAL__[[COLNAME__]],na.rm=TRUE),SIGNIF)
        STD <- signif(stats::sd(dataTOTAL__[[COLNAME__]],na.rm=TRUE),SIGNIF)
        MAX <- signif(max(dataTOTAL__[[COLNAME__]],na.rm=TRUE),SIGNIF)
        MIN <- signif(min(dataTOTAL__[[COLNAME__]],na.rm=TRUE),SIGNIF)
        nNA <- sum(is.na(dataTOTAL__[[COLNAME__]]))
        out__ <- paste0(MEAN, " (",STD,") [",MIN,"-",MAX,"]")
        if (nNA > 0) out__ <- paste0(out__, " (",nNA," n.a.**)")
        out__
      })
      table__ <- cbind(table__, totalValueCol__)
      names(table__)[ncol(table__)] <- paste0("TOTAL (N=",NTOTAL__,")")
    }
  } 
  if (!is.null(filename)) {
    report__ <- TRUE
  } else {
    report__ <- FALSE
  }
  if (FLAGpatients) {
    footer__ <- "N: Number of patients<br>Entries represent: Mean (Standard deviation) [Minimum-Maximum]"
  } else {
    footer__ <- "N: Number of subjects<br>Entries represent: Mean (Standard deviation) [Minimum-Maximum]"
  }
  if (FLAGmissingCat)
    footer__ <- paste0(footer__, "<br>","* stratification value missing in dataset")
  if (FLAGmissingVal)
    footer__ <- paste0(footer__, "<br>","** missing values in dataset")
  if (!is.null(footerAddText)) {
    footer__ <- paste0(footer__,"<br>",footerAddText)
  }
  if (!is.null(filename)) {
    filename <- paste0(aux_strrep(filename,".txt",""),".txt")
    IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=tableTitle,report=report__,filename=filename)
    return(invisible(NULL))
  }
  output__ <- IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=tableTitle,report=report__)
  output__
}
#'@export
mapContinuousCovariate_csvData <- function(csvfile,covName) {
  ATRinfo__ <- loadATRinfo_csvData(csvfile)
  if (is.null(ATRinfo__)) return(NULL)
  name__ <- ATRinfo__$covInfo[ATRinfo__$covInfo$COLNAME==covName]$NAME
  unit__ <- ATRinfo__$covInfo[ATRinfo__$covInfo$COLNAME==covName]$UNIT
  tv__ <- ATRinfo__$covInfo[ATRinfo__$covInfo$COLNAME==covName]$TIME.VARYING
  out__ <- list(
    NAME = name__,
    UNIT = unit__,
    TIME.VARYING = tv__
  )
  return(out__)
}
#'@export
mapCategoricalCovariate_csvData <- function(csvfile,catName,catValue) {
  ATRinfo__ <- loadATRinfo_csvData(csvfile)
  if (is.null(ATRinfo__)) return(NULL)
  name__ <- ATRinfo__$catInfo[ATRinfo__$catInfo$COLNAME==catName]$NAME
  values__ <- aux_explode(ATRinfo__$catInfo[ATRinfo__$catInfo$COLNAME==catName]$VALUES)
  valuetext__ <- aux_explode(ATRinfo__$catInfo[ATRinfo__$catInfo$COLNAME==catName]$VALUETXT)
  valuetext__ <- valuetext__[which(catValue==values__)]
  tv__ <- ATRinfo__$covInfo[ATRinfo__$catInfo$COLNAME==catName]$TIME.VARYING
  out__ <- list(
    NAME = name__,
    VALUETXT = valuetext__,
    TIME.VARYING = tv__
  )
  return(out__)
}
#'@export
mapContinuousCovariate_IQRnlmeProject <- function(projectPath,covName) {
  header__ <- parseNLMEprojectHeader(projectPath)
  oldpath__ <- getwd()
  setwd(projectPath)
  ATRinfo__ <- loadATRinfo_csvData(header__$DATA)
  setwd(oldpath__)
  if (is.null(ATRinfo__)) return(NULL)
  name__ <- ATRinfo__$covInfo$NAME[ATRinfo__$covInfo$COLNAME==covName]
  unit__ <- ATRinfo__$covInfo$UNIT[ATRinfo__$covInfo$COLNAME==covName]
  tv__ <- ATRinfo__$covInfo$TIME.VARYING[ATRinfo__$covInfo$COLNAME==covName]
  out__ <- list(
    NAME = name__,
    UNIT = unit__,
    TIME.VARYING = tv__
  )
  return(out__)
}
#'@export
mapCategoricalCovariate_IQRnlmeProject <- function(projectPath,catName,catValue=NULL) {
  header__ <- parseNLMEprojectHeader(projectPath)
  oldpath__ <- getwd()
  setwd(projectPath)
  ATRinfo__ <- loadATRinfo_csvData(header__$DATA)
  setwd(oldpath__)
  if (is.null(ATRinfo__)) return(NULL)
  name__ <- ATRinfo__$catInfo[ATRinfo__$catInfo$COLNAME==catName,]$NAME
  if (!is.null(catValue)) {
    if (!is.null(name__)) {
      values__ <- aux_explode(ATRinfo__$catInfo[ATRinfo__$catInfo$COLNAME==catName,]$VALUES)
      valuetext__ <- aux_explode(ATRinfo__$catInfo[ATRinfo__$catInfo$COLNAME==catName,]$VALUETXT)
      valuetext__ <- valuetext__[which(catValue==values__)]
      tv__ <- ATRinfo__$covInfo[ATRinfo__$catInfo$COLNAME==catName,]$TIME.VARYING
    } else {
      valuetext__ <- NULL
      tv__ <- NULL
    }
  } else {
    valuetext__ <- NULL
    tv__ <- NULL
  }
  out__ <- list(
    NAME = name__,
    VALUETXT = valuetext__,
    TIME.VARYING = tv__
  )
  return(out__)
}
#'@export
loadATRinfo_csvData <- function(filename) {
  filenameATR__ <- paste0(aux_strrep(filename,".csv",""),".atr")
  atrcontents <- loadAttributeFile(filenameATR__)
  out__ <- atrcontents
  return(out__)
}
loadAttributeFile <- function(filenameATR) {
  if (!file.exists(filenameATR)) return(NULL)
  atrContent__ <- aux_fileread(filenameATR)
  var0 <- ls() 
  eval(parse(text=atrContent__))
  var1 <- ls()
  if (!exists("atrcontents")) {
    atrcontents <- list()
    attrnames__ <- setdiff(var1, c(var0, "var0"))
    atrcontents <- plyr::alply(attrnames__, 1, function(x__) get(x__))
    names(atrcontents) <- attrnames__
    attr(atrcontents, "split_type") <- NULL
    attr(atrcontents, "split_labels") <- NULL
  }
  out__ <- atrcontents
  return(out__)
}
mapCovValues_Categories_ATR <- function(projectPath,data) {
  tryCatch(
    {
      proj <- as_IQRnlmeProject(projectPath)
    },
    error=function(e) {
      return(data)
    }
  )
  dataPath__ <- attr(proj,"absDataPath")
  atrPath__  <- aux_strrep(dataPath__,".csv",".atr")
  if (!file.exists(atrPath__)) return(data)
  atrContent__ <- aux_fileread(atrPath__)
  eval(parse(text=atrContent__))
  if (!is.null(atrcontents$catInfo)) {
    for (k in 1:nrow(atrcontents$catInfo)) {
      covCol__ <- atrcontents$catInfo$COLNAME[k]
      values__ <- aux_explodePC(atrcontents$catInfo$VALUES[k])
      valuetext__ <- aux_explodePC(atrcontents$catInfo$VALUETXT[k])
      if (covCol__ %in% names(data)) {
        sapply(1:length(values__), function (x) {
          data[[covCol__]][data[[covCol__]]==values__[x]] <<- paste0(values__[x]," (",valuetext__[x],")")
        })
      }
    }
  }
  return(data)
}
#'@export
#'@import dplyr
transformObs_IQRdataGENERAL<- function(data, transformation = "log(x)", x, y = sapply(x, function(i) sub("x", i, transformation)), unit, adjust = NULL) {
  contains.x <- x %in% data[["NAME"]]
  if (!all(contains.x)) {
    warningIQR(paste0("Did not find ", x[!contains.x], " in the data. Element is ignored."))
    x <- x[contains.x]
    if (length(x) == 0) return(data)
  }
  contains.adjust <- adjust %in% colnames(data)
  if (!all(contains.adjust)) {
    warningIQR(paste0("Did not find column(s) ", adjust[!contains.adjust], " in the data. Element is ignored."))
    adjust <- adjust[contains.adjust]
  }
  expr <- parse(text = transformation)
  name <- structure(y, names = x)
  if (length(unit) == 1) unit <- rep(unit, length(x))
  if (is.null(names(unit))) unit <- structure(rep_len(unit, length(x)), names = y)
  ytypeInfo <- unique(as.data.frame(data)[data$YTYPE != 0,c("NAME", "YTYPE")])
  ynew   <- setdiff(y, ytypeInfo$NAME)
  yexist <- setdiff(y, ynew)
  YTYPE <- c(
    structure(seq_along(ynew) + ifelse("YTYPE" %in% names(data), max(data[["YTYPE"]]), 0), names = ynew),
    structure(ytypeInfo$YTYPE[ytypeInfo$NAME %in% yexist], names=ytypeInfo$NAME[ytypeInfo$NAME %in% yexist])
  )
  subdata <-  dplyr::group_by(dplyr::filter(data, NAME %in% get('x')), NAME)
  for (n in intersect(adjust, names(subdata))) {
    subdata[[n]] <- with(list(x = subdata[[n]]), eval(expr))
  }
  subdata[["VALUE"]] <- with(list(x = subdata[["VALUE"]]), eval(expr))
  subdata[["NAME"]] <- name[subdata[["NAME"]]]
  subdata[["UNIT"]] <- unit[subdata[["NAME"]]]
  if ("DV" %in% names(subdata)) subdata[["DV"]] <- subdata[["VALUE"]]
  if ("VALUETXT" %in% names(subdata)) subdata[["VALUETXT"]] <- ifelse(is.na(subdata[["VALUE"]]), NA, format(subdata[["VALUE"]]))
  if ("YTYPE" %in% names(subdata)) subdata[["YTYPE"]] <- YTYPE[subdata[["NAME"]]]
  data <- rbind.data.frame(data, subdata)
  data <- data[order(data$USUBJID, data$TIME),]
  attr(data, "obsNAMES") <- union(attr(data, "obsNAMES"), y)
  mysummary <- data.frame(obsNAME = y,
                          transformation = sapply(x, function(i) sub("x", i, transformation)),
                          UNIT = unit,
                          YTYPE = YTYPE,
                          adjust = paste(adjust, collapse = ", "))
  cat ("The following observables have been added to the data:\n\n")
  print(IQRoutputTable(mysummary))
  return(data)
}
#'@import dplyr
#'@export
mutateCov_IQRdataGENERAL <- function(data, ..., unit, name) {
  equations <- unlist(list(...))
  unit <- rep_len(unit, length(equations))
  if (is.null(names(unit)))
    names(unit) <- names(equations)
  if (length(name) != length(equations))
    stopIQR("Each covariate must have a NAME. Check argument 'name'!")
  abbr <- names(equations)
  catInfo <- catInfo(data)
  catInfoNew <- NULL
  covInfo <- covInfo(data)
  covInfoNew <- NULL
  additionalCov <- lapply(seq_along(covInfo[["NAME"]]), function(i) data[[covInfo$COLNAME[i]]])
  additionalCat <- lapply(seq_along(catInfo[["NAME"]]), function(i) {
    values <- strsplit(catInfo[i, "VALUES"], split = ",")[[1]]
    valuetxt <- strsplit(catInfo[i, "VALUETXT"], split = ",")[[1]]
    names(valuetxt) <- values
    valuetxt[as.character(data[[catInfo$COLNAME[i]]])]
  })
  additional <- c(additionalCov, additionalCat)
  names(additional) <- gsub(" ", ".", c(covInfo$NAME, catInfo$NAME))
  for (i in 1:length(equations)) {
    symbols <- getSymbols(equations[i])
    missing <- !symbols %in% names(data) & !symbols %in% gsub(" ", ".", covInfo$NAME) & !symbols %in% gsub(" ", ".", catInfo$NAME)
    if (any(missing))
      stopIQR("Required column(s) for ", paste(symbols[missing], collapse = ", "), " are missing. Check dots argument!")
    data[[abbr[i]]] <- with(c(additional, data), {
      eval(parse(text = equations[i]))
    })
    is.categorical <- any(sapply(symbols, function(s) inherits(data[[s]], "character"))) | is.character(data[[abbr[i]]])
    nvalue <- sapply(split(data, data$USUBJID), function(d__) {
      abbr__ <- d__[[abbr[i]]]
      length(unique(abbr__[!is.na(abbr__)]))
    })
    if (max(nvalue, na.rm = TRUE) <= 1) time.varying <- FALSE else time.varying <- TRUE
    if (is.categorical) {
      valuetxt <- unique(data[[abbr[i]]])
      values <- 1:length(valuetxt) - 1
      names(values) <- valuetxt
      data[[abbr[i]]] <- values[data[[abbr[i]]]]
      catInfoNew <- rbind(catInfoNew,
                          data.frame(COLNAME = abbr[i], NAME = name[i], UNIT = unit[i],
                                     VALUETXT = paste(valuetxt, collapse = ","), VALUES = paste(values, collapse = ","),
                                     TIME.VARYING = time.varying,
                                     row.names = ifelse(is.null(catInfo), 0, nrow(catInfo)) + 1, stringsAsFactors = FALSE))
    } else {
      covInfoNew <- rbind(covInfoNew,
                          data.frame(COLNAME = abbr[i], NAME = name[i], UNIT = unit[i],
                                     TIME.VARYING = time.varying,
                                     row.names = ifelse(is.null(covInfo), 0, nrow(covInfo)) + 1, stringsAsFactors = FALSE))
    }
  }
  catInfo <- rbind(catInfo, catInfoNew)
  covInfo <- rbind(covInfo, covInfoNew)
  catInfo <- catInfo[rev(!duplicated(rev(catInfo[[1]]))),]
  covInfo <- covInfo[rev(!duplicated(rev(covInfo[[1]]))),]
  attr(data, "catInfo") <- catInfo
  attr(data, "covInfo") <- covInfo
  if (!is.null(covInfoNew)) {
    cat("The following continuous covariates have been added to the data:\n\n")
    print(IQRoutputTable(covInfoNew))
    cat("\n")
  }
  if (!is.null(catInfoNew)) {
    cat("The following categorical covariates have been added to the data:\n\n")
    print(IQRoutputTable(catInfoNew))
    cat("\n")
  }
  if (is.null(covInfoNew) & is.null(catInfoNew))
    cat("No covariates could be added.\n")
  return(data)
}
#'@export
convertCat2Text <- function(x, catValues, textValues) {
  if (length(catValues) != length(textValues)) stopIQR("Vectors with numerical ('catValues') and text ('textValues') values do not have same length.")
  if (!is.numeric(catValues)) {
    catValues <- tryCatch(as.numeric(catValues),
                          warning = function(w) w)
    if ("warning" %in% class(catValues)) {
      stopIQR("'catValue' needs to be numeric or convertable to numeric without issues.")
    }
  }
  if (!is.character(textValues)) stopIQR("'textValue' needs to be character.")
  if (!is.numeric(x)) stopIQR("'x' needs to be numeric.")
  if (!all(x %in% catValues)) {
    missing__ <- unique(setdiff(x, catValues))
    stopIQR(paste0("The following numerical values are missing in 'catValues': ", paste0(missing__, collapse = ", ")))
  }
  out__ <- factor(
    x,
    levels = catValues,
    labels = textValues
  )
  out__ <- as.character(out__)
  return(out__)
}
#'@export
getCatValues_catInfo <- function(catName, catInfo) {
  if (!catName %in% catInfo$COLNAME) stopIQR(catName, "is no categorical covariate in given categorical information table.")
  as.numeric(aux_explode(catInfo$VALUES[catInfo$COLNAME == catName]))
}
#'@export
getTextValues_catInfo <- function(catName, catInfo) {
  if (!catName %in% catInfo$COLNAME) stopIQR(catName, "is no categorical covariate in given categorical information table.")
  aux_explode(catInfo$VALUETXT[catInfo$COLNAME == catName])
}
#'@export
addCatColumnAsText_IQRdataGENERAL <- function(x, catColumn, newName = paste0(catColumn,"C")) {
  if (!is_IQRdataGENERAL(x)) {
    stopIQR("Input x needs to be IQRdataGENERAL.")
  }
  if (!catColumn %in% names(x)) stopIQR(paste0(catColumn, " does not exist in given IQRdataGENERAL object."))
  if (!is.character(newName)) stopIQR("newName input needs to be a character.")
  catInfo__ <- catInfo(x)
  if (!catColumn %in% catInfo__$COLNAME) stopIQR(paste0(catColumn, " is not a covariate column for the given IQRdataGENERAL object."))
  levels__  = aux_explode(catInfo__$VALUES[catInfo__$COLNAME == catColumn])
  labels__  = aux_explode(catInfo__$VALUETXT[catInfo__$COLNAME == catColumn])
  if(length(levels__) != length(labels__)) {
    stopIQR("The number of comma-separated elements in VALUES is different from those in VALUETXT.")
  }
  x[[newName]] <- convertCat2Text(x[[catColumn]],
                                  catValues = levels__,
                                  textValues = labels__)
  return(x)
}
obfuscate_numeric <- function(x, abs = 0, rel = 0.1) {
  stopifnot(is.numeric(x))
  amplitude__ <- max(abs(x[is.finite(x)]))
  matching__ <- match(x, unique(x))
  x__ <- unique(x)/amplitude__
  x__[is.finite(x__)] <- stats::rnorm(length(x__[is.finite(x__)]), mean = x__[is.finite(x__)], sd = abs + rel*abs(x__[is.finite(x__)]))
  x__ <- x__[matching__]
  x__
}
obfuscate_categoric <- function(x, basechar = "L") {
  stopifnot(is.character(x) | is.factor(x) | is.numeric(x))
  myclass__ <- case_when(
    is.factor(x) ~ "factor",
    is.numeric(x) ~ "numeric",
    TRUE ~ "character"
  )
  if (length(x) > 0 & any(!is.na(x))) {
    x <- as.character(x)
    x__ <- as.character(x[!is.na(x)])
    levels__ <- sample(unique(x__))
    if (myclass__ == "numeric") basechar <- ""
    substitute__ <- structure(
      .Data = paste0(basechar, formatC(1:length(levels__), flag = "0", width = ceiling(log10(length(levels__))))),
      names = levels__
    )
    x__ <- as.character(substitute__[x__])
    x[!is.na(x)] <- x__
    if (myclass__ == "numeric") x <- as.numeric(x)
    if (myclass__ == "factor") x <- factor(x, levels = substitute__)
  }
  x
}
obfuscate_NAME_VALUE <- function(data, names, basechar, method = "numeric") {
  if (length(names) > 0) {
    substitute__ <- structure(paste0(basechar, seq_along(names)), names = names)
    data$NAME[data$NAME %in% names] <- substitute__[data$NAME[data$NAME %in% names]]
    for (s__ in substitute__) {
      observations__ <- data$NAME %in% s__
      if (method == "numeric")
        data$VALUE[observations__] <- obfuscate_numeric(data$VALUE[observations__])
      if (method == "categoric")
        data$VALUE[observations__] <- obfuscate_categoric(data$VALUE[observations__])
    }
    attr(data, "ObfuscatedNames") <- as.character(substitute__)
  }
  data
}
obfuscate_IQRdataGENERAL <- function(data, additional = NULL, nSubjects = 10, abs = 0, rel = 0.1) {
  required__ <- c("USUBJID","TIME", "TIMEUNIT", "NAME", "VALUE", "UNIT", "ROUTE")
  stopifnot(all(required__ %in% colnames(data)),
            is.null(additional) | all(additional %in% colnames(data)))
  obsNAMES__ <- obsNAMES(data)
  doseNAMES__ <- doseNAMES(data)
  covNAMES__ <- covInfo(data)[["NAME"]]
  covCOLS__ <- covInfo(data)[["COLNAME"]]
  covTIME__ <- covInfo(data)[["TIME.VARYING"]]
  catNAMES__ <- catInfo(data)[["NAME"]]
  catCOLS__ <- catInfo(data)[["COLNAME"]]
  catTIME__ <- catInfo(data)[["TIME.VARYING"]]
  data__ <- as.data.frame(data)
  data__ <- data__[, unique(c(required__, additional, covCOLS__, catCOLS__))]
  data__ <- split(data__, data__$USUBJID, drop = TRUE)
  data__ <- data__[sample(seq_along(data__), nSubjects, replace = ifelse(length(data__) > nSubjects, FALSE, TRUE))]
  data__ <- lapply(seq_along(data__), function(i) {
    d__ <- data__[[i]]
    d__$USUBJID <- paste0("UID", formatC(i, flag = "0", width = ceiling(log10(nSubjects))))
    d__
  })
  data__ <- do.call(rbind, data__)
  data__ <- data__[data__$NAME %in% c(obsNAMES__, doseNAMES__),]
  data__ <- obfuscate_NAME_VALUE(data__, names = obsNAMES__, basechar = "Obs", method = "numeric")
  obsNAMES__ <- attr(data__, "ObfuscatedNames")
  data__ <- obfuscate_NAME_VALUE(data__, names = doseNAMES__, basechar = "Drug", method = "numeric")
  doseNAMES__ <- attr(data__, "ObfuscatedNames")
  for (s__ in covCOLS__) {
    data__[[s__]] <- obfuscate_numeric(data__[[s__]], abs = abs, rel = rel)
  }
  covNAMES__ <- obfuscate_categoric(covNAMES__, basechar = "Cov")
  names(data__)[match(covCOLS__, names(data__))] <- covNAMES__
  for (s__ in catCOLS__) {
    data__[[s__]] <- obfuscate_categoric(data__[[s__]])
  }
  catNAMES__ <- obfuscate_categoric(catNAMES__, basechar = "Cat")
  names(data__)[match(catCOLS__, names(data__))] <- catNAMES__
  for (s__ in additional) {
    if (is.numeric(data__[[s__]]))
      data__[[s__]] <- obfuscate_numeric(data__[[s__]], abs = abs, rel = rel)
    else
      data__[[s__]] <- obfuscate_categoric(data__[[s__]])
  }
  data__$TIME <- obfuscate_numeric(data__$TIME, abs = abs, rel = rel)
  data__$TIMEUNIT <- "Hours"
  data__$UNIT <- obfuscate_categoric(data__$UNIT, basechar = "Unit")
  data__$ROUTE[!is.na(data__$ROUTE)] <- "ORAL"
  covInfo__ <- catInfo__ <- NULL
  if (length(covNAMES__) > 0) {
    covInfo__ <- list(
      COLNAME = covNAMES__,
      NAME = covNAMES__,
      UNIT = rep("--", length(covNAMES__)),
      TIME.VARYING = covTIME__
    )
  }
  if (length(catNAMES__) > 0) {
    catInfo__ <- list(
      COLNAME = catNAMES__,
      NAME = catNAMES__,
      UNIT = rep("--", length(catNAMES__)),
      VALUETXT = sapply(catNAMES__, function(x) paste(unique(data__[[x]][!is.na(data__[[x]])]), collapse = ", ")),
      VALUES = sapply(catNAMES__, function(x) paste(unique(data__[[x]][!is.na(data__[[x]])]), collapse = ", ")),
      TIME.VARYING = catTIME__
    )
  }
  IQRdataGENERAL(data__, doseNAMES__, obsNAMES__,
                 covInfoAdd = covInfo__, catInfoAdd = catInfo__)
}
#'@export
checkForDuplicatedTimeSeries_IQRdataGENERAL <- function(dataGeneral, name, verbose = FALSE) {
  dtGeneral <- as.data.table(dataGeneral)
  setkey(dtGeneral, USUBJID, NAME, TIME)
  USUBJIDs <- dtGeneral[, unique(USUBJID)]
  duplicatedUSUBJIDs <- list()
  for(i in seq(1, length(USUBJIDs)-1)) {
    Ui <- USUBJIDs[i]
    tsi <- dtGeneral[list(USUBJID = Ui, NAME = name), list(USUBJID, NAME, TIME, VALUE, DV), nomatch = 0]
    for(j in seq(i+1, length(USUBJIDs))) {
      Uj <- USUBJIDs[j]
      tsj <- dtGeneral[list(USUBJID = Uj, NAME = name), list(USUBJID, NAME, TIME, VALUE, DV), nomatch = 0]
      if(nrow(tsi) > 0 && nrow(tsj) > 0 &&
         (identical(tsi[, list(TIME, VALUE)], tsj[, list(TIME, VALUE)]) ||
          identical(tsi[, list(TIME, DV)], tsj[, list(TIME, DV)])) ) {
        if(verbose) {
          cat("======================================================\n")
          cat("tsi:\n")
          print(tsi)
          cat("tsj:\n")
          print(tsj)
          cat("======================================================\n")
        }
        duplicatedUSUBJIDs[[Ui]] <- c(duplicatedUSUBJIDs[[Ui]], Uj)
      }
    }
  }
  duplicatedUSUBJIDs
}
#'@export
plot.IQRdataGENERAL <- function(
  x,
  pathname = "DataExploration",
  stratify = NULL,
  scaleOBS = "log",
  scaleCOV = "linear",
  covNames  = NULL,
  catNames   = NULL,
  ...)
{
  data__ <- x
  if (!is_IQRdataGENERAL(data__))
    stopIQR("Input data is not an IQRdataGENERAL object.")
  aux_mkdir(pathname)
  cat("Plotting individual lines per treatment group ...\n")
  tryCatch({
    plotSpaghetti_IQRdataGENERAL(data__,
                                 scale = scaleOBS,
                                 stratify = NULL,
                                 filename = file.path(pathname,"01_OBSvsTIME_Spaghetti"))
  }, error = function(err) {
    warningIQR(paste0("Spaghetti plots could not be generated: ", err))
  })
  cat("Plotting individual data per panel (may take a while) ...\n")
  tryCatch({
    suppressWarnings(plotIndiv_IQRdataGENERAL(data__,
                                              scale = scaleOBS,
                                              filename = file.path(pathname, "02_OBSvsTIME_Individual")))
  }, error = function(err) {
    warningIQR(paste0("Individual plots could not be generated: ", err))
  })
  cat("Plotting summary timecourses per treatment group ...\n")
  tryCatch({
    plotRange_IQRdataGENERAL(data__,
                             scale = scaleOBS,
                             stratify = NULL,
                             filename = file.path(pathname,"03_OBSvsTIME_Range"))
  }, error = function(err) {
    warningIQR(paste0("Range plots could not be generated: ", err))
  })
  cat("Plotting continuous covariate distributions ...\n")
  tryCatch({
    plotCovDistribution_IQRdataGENERAL(data__,
                                       scale = scaleCOV,
                                       covNames = c(covNames, catNames),
                                       filename = file.path(pathname,"04_ContCovDistributions"))
  }, error = function(err) {
    warningIQR(paste0("\nContinuous covariate distributions could not be plotted: ", err))
  })
  cat("Plotting continuous covariate correlations ...\n")
  tryCatch({
    plotCorCov_IQRdataGENERAL(data__,
                              covNames = covNames,
                              scale = scaleCOV,
                              filename = file.path(pathname, "05_ContCovCorrelation"))
  }, error = function(err) {
    warningIQR(paste0("\nContinuous covariate correlations could not be plotted: ", err))
  })
  cat("Plotting categorical covariate correlations ...\n")
  tryCatch({
    plotCorCat_IQRdataGENERAL(data__,
                              catNames = catNames,
                              filename = file.path(pathname, "06_CatCovCorrelation"))
  }, error = function(err) {
    warningIQR(paste0("\nCategorical covariate correlations could not be plotted: ", err))
  })
  cat("Plotting continuous/categorical covariate correlations ...\n")
  tryCatch({
    plotCorCovCat_IQRdataGENERAL(data__,
                                 covNames = covNames,
                                 catNames = catNames,
                                 filename = file.path(pathname, "07_ContCatCovCorrelation"))
  }, error = function(err) {
    warningIQR(paste0("\nCategorical/continuous covariate correlations could not be plotted: ", err))
  })
}
#'@export
plotSpaghetti_IQRdataGENERAL <- function(data, scale = "log", stratify = NULL, linetype=NULL,
                                         filename = NULL,
                                         FLAGreturnObject = FALSE)
{
  data__ <- data
  if (!is_IQRdataGENERAL(data__))
    stopIQR("Input argument is not an IQRdataGENERAL object.")
  if (!"TRTNAME" %in% names(data__)) {
    data__$TRTNAME <- ""
    data__$TRT <- 0
  }
  if (!class(stratify) %in% c("NULL","character"))
    stopIQR("Invalid input argument for 'stratify'. Must be either NULL or character vector.")
  if (!(is.null(filename) | (is.character(filename)) & length(filename) == 1))
    stopIQR("Filename for PDF needs to be character string (no vector).")
  if (!is.null(linetype)) {
    if (!linetype %in% names(data)) stopIQR("linetype argument is not a column in the dataset")
    if (length(unique(data[[linetype]]))>5) warningIQR("More than 5 distinct elements in linetype column")
  }
  if (!is.null(attr(data__,"covInfo")))
    covInfo0__ <- attr(data__,"covInfo")[!attr(data__,"covInfo")$TIME.VARYING,]
  if (!is.null(attr(data__,"catInfo")))
    catInfo0__ <- attr(data__,"catInfo")[!attr(data__,"catInfo")$TIME.VARYING,]
  obsNAMES__ <- attr(data__, "obsNAMES")
  timelabel__ <- get_timelabelIQR(data__)
  data__ <- handle_MDVBLQplot(data__)
  obsSCALES__ <- handle_obsScalesIQR(scale, obsNAMES__)
  if (is.factor(data__$TRTNAME)) {
    data__$TRTNAMElabel <- factor(data__$TRTNAME, labels = paste0(levels(data__$TRTNAME)))
  } else {
    data__$TRTNAMElabel <- data__$TRTNAME
  }
  trtlabels <- unique(data__$TRTNAMElabel)
  ntrt <- length(trtlabels)
  ltrt <- max(nchar(as.character(trtlabels)), na.rm=TRUE)
  basefs <- max(14 - floor(ltrt/9),6)
  data_out <- list()
  env.data_out <- environment()
  gr__ <- plyr::dlply(
    data__[data__$NAME %in% obsNAMES__,],    
    ~ NAME,                                  
    function(x) {
      name.x <- x$NAME[1]
      title__ <- paste0("Individual ", unique(x$NAME), " over time")
      subtitle__ <- "Stratified by treatment group"
      if (trtlabels[1]=="") subtitle__ <- NULL
      ssInfo <- plyr::ddply(unique(x[,c("USUBJID","TRTNAME","TRTNAMElabel")]), ~TRTNAME+TRTNAMElabel,
                            function(xx__) data.frame(N = sum(!is.na(xx__$TRTNAME))))
      ssInfo$valueMax__ <- max(x$VALUE, na.rm=TRUE)
      ssInfo$timeMin__ <- min(x$TIME, na.rm=TRUE)
      env.data_out$data_out[["unstratified"]][[name.x]] <- x
      if (!is.null(linetype)) {
        x[[linetype]] <- as.factor(x[[linetype]])
      }
      p__ <- IQRggplot(data=x, aes_string("TIME", "VALUE"))
      if (is.null(linetype)) {
        p__ <- p__ + geom_line(aes_string(group="USUBJID"),alpha=0.3)
      } else {
        p__ <- p__ + geom_line(aes_string(group="USUBJID",linetype=linetype),alpha=1)
      }
      p__ +
        geom_point(aes_string(
          shape="BLOQ",
          color="BLOQ"
        ), alpha=0.6) +
        geom_text(data = ssInfo, aes_string(x="timeMin__", y="valueMax__", label="paste0('N=',N)"), hjust = 0, vjust = 1) +
        scale_color_IQRblloq +
        scale_shape_IQRblloq +
        labs(
          x = timelabel__,
          y = get_obslabelIQR(x[1,]),
          title = title__,
          subtitle = subtitle__,
          caption = "Missing values (MDV=1) have been removed, records below LLOQ plotted at value in dataset."
        ) +
        facet_wrap(~TRTNAMElabel) +
        scale_y_apply(obsSCALES__$scale[obsSCALES__$NAME == unique(x$NAME)]) +
        theme(legend.position="bottom",
              strip.text = element_text(size = max(5,basefs-ceiling(sqrt(ntrt)))),
              plot.caption = element_text(size = 7, hjust=0))
    }
  )
  tmp__ <- handle_stratificationIQR(data__, stratify)
  data__ <- tmp__[[1]]
  strat__ <- tmp__[[2]]
  catInfo0__ <- tmp__[[3]]
  grstrat__ <- list()
  attr(grstrat__, "strat_info") <- data.frame()
  for (strk__ in strat__) {
    grstratk__ <- plyr::dlply(
      data__[data__$NAME %in% obsNAMES__,],    
      c("NAME", strk__),                       
      function(x) {
        condition.x <- paste(x$NAME[1], x[[strk__]][1], sep = "_")
        strval__  <- unique(x[[strk__]])
        strname__ <- catInfo0__$NAME[catInfo0__$COLNAME == strk__]
        posval__  <- aux_explode(catInfo0__$VALUES[catInfo0__$COLNAME == strk__])
        postxt__  <- aux_explode(catInfo0__$VALUETXT[catInfo0__$COLNAME == strk__])
        strtxt__  <- postxt__[posval__ == strval__]
        title__   <- paste0("Individual ", unique(x$NAME), " over time")
        subtitle__<- paste0("Stratified by treatment group and ", strname__,": ",strtxt__)
        ssInfo <- plyr::ddply(unique(x[,c("USUBJID","TRTNAME","TRTNAMElabel")]), ~TRTNAME+TRTNAMElabel,
                              function(xx__) data.frame(N = sum(!is.na(xx__$TRTNAME))))
        ssInfo$valueMax__ <- max(x$VALUE, na.rm=TRUE)
        ssInfo$timeMin__ <- min(x$TIME, na.rm=TRUE)
        env.data_out$data_out[["stratified"]][[condition.x]] <- x
        IQRggplot(data=x, aes_string("TIME", "VALUE")) +
          geom_line(aes_string(group = "USUBJID"), alpha=0.3) +
          geom_point(aes_string(
            shape="BLOQ",
            color="BLOQ"
          ), alpha=0.6) +
          geom_text(data = ssInfo, aes_string(x="timeMin__",y="valueMax__", label='paste0("N=",N)'), hjust = 0, vjust = 1) +
          scale_color_IQRblloq +
          scale_shape_IQRblloq +
          labs(
            x = timelabel__,
            y = get_obslabelIQR(x[1,]),
            title = title__,
            subtitle = subtitle__,
            caption = "Missing values (MDV=1) have been removed, records below LLOQ plotted at value in dataset."
          ) +
          facet_wrap(~TRTNAMElabel) +
          scale_y_apply(obsSCALES__$scale[obsSCALES__$NAME == unique(x$NAME)]) +
          theme(legend.position="bottom",
                strip.text = element_text(size = max(5,basefs-ceiling(sqrt(ntrt)))),
                plot.caption = element_text(size = 7, hjust=0))
      }
    )
    si__ <- attr(grstratk__,"split_labels")
    si__$STRAT <- names(si__)[2]
    names(si__)[2] <- "VALUE"
    names(grstratk__) <- with(si__, {paste0(NAME,".",STRAT,"::",VALUE)} )
    grstrat__ <- c(grstrat__,grstratk__)
    attr(grstrat__, "strat_info") <- rbind(attr(grstrat__, "strat_info"), si__)
  }
  if (!is.null(filename)) {
    if (exists("grstrat__")) {
      plotList <- c(gr__,grstrat__)
    }
    IQRoutputPDF(plotList,filename=filename)
  } else {
    plyr::l_ply(gr__, print)
    if (exists("grstrat__")) plyr::l_ply(grstrat__, print)
  }
  if (FLAGreturnObject) {
    out <- list(unstratified=gr__,stratified=grstrat__)
    attr(out, "data") <- data_out
    return(out)
  }
}
#'@export
plotIndiv_IQRdataGENERAL <- function(
  data,
  obsNames = NULL,
  scale = "log",
  filename = "OBSvsTIME_IndivPanels",
  FLAGplotDosesSeparate = TRUE,
  FLAGreturnObject = FALSE)
{
  if (!is_IQRdataGENERAL(data))
    stopIQR("Input argument is not an IQRdataGENERAL object.")
  if (!class(obsNames) %in% c("NULL","character"))
    stopIQR("Invalid input argument for 'obsNames'. Must be either NULL or character vector.")
  if (!(is.null(filename) | (is.character(filename)) & length(filename) == 1))
    stopIQR("Filename for PDF needs to be character string (no vector).")
  data__ <- data
  if (!"TRTNAME" %in% names(data__)) {
    data__$TRTNAME <- ""
    data__$TRT <- 0
  }
  if (is.null(obsNames)) {
    obsNAMES__ <- attr(data__, "obsNAMES")
  } else {
    missingobs__ <- setdiff(obsNames, attr(data__, "obsNAMES"))
    if (length(missingobs__) != 0){
      warningIQR('Following observation(s) are not contained in the dataset: ',paste0(missingobs__, collapse = ", "))
    }
    obsNAMES__ = intersect(obsNames,attr(data__, "obsNAMES"))
    if (length(obsNAMES__) == 0){
      stopIQR('No existing observations chosen for plotting.')
    }
  }
  doseNAMES__ <- attr(data__, "doseNAMES")
  obsSCALES__ <- handle_obsScalesIQR(scale, obsNAMES__)
  timelabel__ <- get_timelabelIQR(data__)
  data__ <- handle_MDVBLQplot(data__, FLAGremoveMDV = FALSE)
  data_out <- list()
  env.data_out <- environment()
  dP__ <- data__[data__$NAME %in% c(obsNAMES__,doseNAMES__),]
  dPs__ <- split(dP__,dP__$ID)
  gr__ <- lapply(seq_along(dPs__), function (kP__) {
    xx <- dPs__[[kP__]]
    usubjid.xx <- xx$USUBJID[1]
    tmin <- min(xx$TIME, na.rm = TRUE)
    tmax <- max(xx$TIME, na.rm = TRUE)
    xx1__ <- xx[xx$NAME %in% obsNAMES__,]
    xx2__ <- xx[xx$NAME %in% doseNAMES__,]
    if (dim(xx1__)[1] == 0) {
      if (FLAGplotDosesSeparate) {
        pl__ <- add_LayerDosingSingle(IQRggplot(xx2__),xx2__, 0, max(xx2__$VALUE, na.rm=TRUE), tmin, tmax, "linear")
      } else {
        if (length(unique(xx2__$NAME)) > 1) {
          warningIQR("Summarized plotting of dosing for different dosing of more than one compound not implemented yet.\n Plot separately.")
          pl__ <- add_LayerDosingSingle(IQRggplot(xx2__),xx2__, 0, max(xx2__$VALUE, na.rm=TRUE), tmin, tmax, "linear")
        } else {
          pl__ <- add_LayerDosingMulti(IQRggplot(xx2__),xx2__, 0, max(xx2__$VALUE, na.rm=TRUE), tmin, tmax, "linear")
        }
      }
      plotList__ <- list(
        pl__ +
          coord_cartesian(ylim = c(-0.5, max(xx2__$VALUE, na.rm = TRUE)+0.5)) +
          labs(
            x=timelabel__,
            y= "Dose amount",
            title = "Dosing (no observations present)"
          )
      )
    } else {
      nOBS__ <- length(unique(xx1__$NAME))
      fs.axistitle__ <- 14-nOBS__*1.5
      plotList__ <- plyr::dlply(
        xx1__, ~NAME, doseInfo = xx2__, tmin = tmin, tmax = tmax,
        function(xxx, doseInfo, tmin, tmax) {
          name.xxx <- xxx$NAME[1]
          sc__ <- obsSCALES__$scale[obsSCALES__$NAME == name.xxx]
          if (!any(xxx$VALUE > 0)) sc__ <- "linear"
          ymin <- switch(sc__,
                         linear = min(xxx$VALUE, na.rm = TRUE),
                         log    = 10^(floor(log10(min(xxx$VALUE[xxx$VALUE>0], na.rm=TRUE))))
          )
          ymax <- max(xxx$VALUE, na.rm = TRUE)
          caption <- "Non-NA values set as missing (MDV=1) marked with orange diamond, records below LLOQ plotted at value in dataset."
          pl__ <- IQRggplot(data=xxx, aes_string("TIME", "VALUE"))
          if (dim(doseInfo)[1] > 0) {
            if (FLAGplotDosesSeparate) {
              pl__ <- add_LayerDosingSingle(pl__, doseInfo, ymin, ymax, tmin, tmax, sc__)
            } else {
              if (length(unique(doseInfo$NAME)) > 1) {
                warningIQR("Summarized plotting of dosing for different dosing of more than one compound not implemented yet.\n Plot separately.")
                pl__ <- add_LayerDosingSingle(pl__, doseInfo, ymin, ymax, tmin, tmax, sc__)
              } else {
                pl__ <- add_LayerDosingMulti(pl__, doseInfo, ymin, ymax, tmin, tmax, sc__)
                caption <- paste0(caption,"\nMerged single dose records to multiple records indicated by dotted box.")
              }
            }
          }
          pl__ <- pl__ +
            geom_line(alpha=0.3) +
            geom_point(aes_string(
              shape="BLOQ"
            ), alpha=0.6) +
            geom_point(data=xxx[xxx$MDV == 1,], color = "darkorange", shape = 5) +
            geom_text(
              aes_string(label='paste0(IXGDF,"@(",format(TAD, digits = 2),",",format(VALUE, digits=2),")")'),
              color = '#008C48', hjust=-0.1,vjust=0.1, size = 2) +
            scale_shape_IQRblloq +
            labs(
              x=timelabel__,
              y= paste0(name.xxx, " (", xxx$UNIT[1], ")"),
              title = name.xxx,
              caption = caption
            ) +
            scale_y_apply(sc__) +
            coord_cartesian(
              xlim=c(tmin-0.05*(tmax-tmin),tmax+0.2*(tmax-tmin))
            ) +
            theme(legend.position="right", legend.text = element_text(size = 8), legend.title = element_text(size = 9),
                  plot.caption = element_text(size = 7, hjust=0), axis.title = element_text(size = fs.axistitle__))
          return(pl__)
        }
      )
    }
    if (any(xx$TRTNAME=="")) {
      text__ <- paste0(xx$USUBJID[1], " (ID=",xx$ID[1],")")
    } else {
      text__ <- paste0(xx$USUBJID[1], " (ID=",xx$ID[1],") in TRTNAME group ", xx$TRTNAME[1])
    }
    suppressWarnings(
      plot__ <- gridExtra::marrangeGrob(
        plotList__, ncol = 1, nrow = length(plotList__),
        top = grid::textGrob(text__,
                             x = unit(0.08, "npc"), just = "left",
                             gp=grid::gpar(fontsize = 12, font =2) ))
    )
    return(plot__)
  })
  if (!is.null(filename)) {
    IQRoutputPDF(gr__,filename=filename)
  } else {
    plyr::l_ply(gr__, print)
  }
  if (FLAGreturnObject) {
    attr(gr__, "data") <- data_out
    return(gr__)
  }
}
#'@export
plotRange_IQRdataGENERAL <- function(data,
                                     scale = "log",
                                     stratify = NULL,
                                     filename = NULL,
                                     FLAGreturnObject = FALSE)
{
  if (!is_IQRdataGENERAL(data))
    stopIQR("Input argument is not an IQRdataGENERAL object.")
  if (!class(stratify) %in% c("NULL","character"))
    stopIQR("Invalid input argument for 'stratify'. Must be either NULL or character vector.")
  if (!(is.null(filename) | (is.character(filename)) & length(filename) == 1))
    stopIQR("Filename for PDF needs to be character string (no vector).")
  if (all(is.na(data$NT))) {
    message("No nominal time available - Range plots not done!")
    return(invisible(NULL))
  }
  data__ <- data
  if (!"TRTNAME" %in% names(data__)) {
    data__$TRTNAME <- ""
    data__$TRT <- 0
  }
  if (!is.null(attr(data__,"covInfo")))
    covInfo0__ <- attr(data__,"covInfo")[!attr(data__,"covInfo")$TIME.VARYING,]
  if (!is.null(attr(data__,"catInfo")))
    catInfo0__ <- attr(data__,"catInfo")[!attr(data__,"catInfo")$TIME.VARYING,]
  obsNAMES__ <- attr(data__, "obsNAMES")
  timelabel__ <- get_timelabelIQR(data__)
  obsSCALES__ <- handle_obsScalesIQR(scale, obsNAMES__)
  data__ <- handle_MDVBLQplot(data__)
  data__ <- data__[!is.na(data__$NT),]
  if (is.factor(data__$TRTNAME)) {
    data__$TRTNAMElabel <- factor(data__$TRTNAME, labels = paste0(levels(data__$TRTNAME)))
  } else {
    data__$TRTNAMElabel <- data__$TRTNAME
  }
  trtlabels <- unique(data__$TRTNAMElabel)
  ntrt <- length(trtlabels)
  ltrt <- max(nchar(as.character(trtlabels)), na.rm=TRUE)
  basefs <- max(14 - floor(ltrt/9),6)
  data_out <- list()
  env.data_out <- environment()
  gr__ <- plyr::dlply(
    data__[data__$NAME %in% obsNAMES__,],    
    ~ NAME,                                  
    function(x) {
      name.x <- x$NAME[[1]]
      title__ <- paste0("Individual ", unique(x$NAME), " over time")
      if (x$TRTNAME[1]!="") {
        subtitle__ <- "Stratified by treatment group"
      } else {
        subtitle__ <- NULL
      }
      ssInfo <- plyr::ddply(unique(x[,c("USUBJID","TRTNAME","TRTNAMElabel")]), ~TRTNAME+TRTNAMElabel,
                            function(xx__) data.frame(N = sum(!is.na(xx__$TRTNAME))))
      ssInfo$valueMax__ <- max(x$VALUE, na.rm=TRUE)
      ssInfo$timeMin__ <- min(x$NT, na.rm=TRUE)
      env.data_out$data_out[["unstratified"]][[name.x]] <- x
      IQRggplot(data=x, aes_string("NT", "VALUE")) +
        stat_summary(fun.data=median90range, geom="line") +
        stat_summary(fun.data=median90range, geom="ribbon", color = NA, fill = '#185AA9', alpha = 0.3) +
        geom_text(data = ssInfo, aes_string(x="timeMin__",y="valueMax__", label='paste0("N=",N)'), hjust = 0, vjust = 1) +
        scale_color_IQRblloq +
        labs(
          x = timelabel__,
          y = get_obslabelIQR(x[1,]),
          title = title__,
          subtitle = subtitle__,
          caption = "Missing values (MDV=1) and records with missing nominal time have been removed, records below LLOQ plotted at value in dataset."
        ) +
        facet_wrap(~TRTNAMElabel) +
        scale_y_apply(obsSCALES__$scale[obsSCALES__$NAME == unique(x$NAME)]) +
        theme(legend.position="bottom",
              strip.text = element_text(size = max(5,basefs-ceiling(sqrt(ntrt)))),
              plot.caption = element_text(size = 7,hjust=0))
    }
  )
  tmp__ <- handle_stratificationIQR(data__, stratify)
  data__ <- tmp__[[1]]
  strat__ <- tmp__[[2]]
  catInfo0__ <- tmp__[[3]]
  grstrat__ <- list()
  attr(grstrat__, "strat_info") <- data.frame()
  for (strk__ in strat__) {
    grstratk__ <- plyr::dlply(
      data__[data__$NAME %in% obsNAMES__,],    
      c("NAME", strk__),                       
      function(x) {
        condition.x <- paste(x$NAME[1], x[[strk__]][1], sep = "_")
        strval__  <- unique(x[[strk__]])
        strname__ <- catInfo0__$NAME[catInfo0__$COLNAME == strk__]
        posval__  <- aux_explode(catInfo0__$VALUES[catInfo0__$COLNAME == strk__])
        postxt__  <- aux_explode(catInfo0__$VALUETXT[catInfo0__$COLNAME == strk__])
        strtxt__  <- postxt__[posval__ == strval__]
        title__   <- paste0("Individual ", unique(x$NAME), " over time")
        subtitle__<- paste0("Stratified by treatment group and ", strname__,": ",strtxt__)
        ssInfo <- plyr::ddply(unique(x[,c("USUBJID","TRTNAME","TRTNAMElabel")]), ~TRTNAME+TRTNAMElabel,
                              function(xx__) data.frame(N = sum(!is.na(xx__$TRTNAME))))
        ssInfo$valueMax__ <- max(x$VALUE, na.rm=TRUE)
        ssInfo$timeMin__ <- min(x$TIME, na.rm=TRUE)
        env.data_out$data_out[["stratified"]][[condition.x]] <- x
        IQRggplot(data=x, aes_string("NT", "VALUE")) +
          stat_summary(fun.data=median90range, geom="line") +
          stat_summary(fun.data=median90range, geom="ribbon", color = NA, fill = '#185AA9', alpha = 0.3) +
          geom_text(data = ssInfo, aes_string(x="timeMin__",y="valueMax__", label='paste0("N=",N)'), hjust = 0, vjust = 1) +
          labs(
            x = timelabel__,
            y = get_obslabelIQR(x[1,]),
            title = title__,
            subtitle = subtitle__,
            caption = "Missing values (MDV=1) have been removed, records below LLOQ plotted at value in dataset."
          ) +
          facet_wrap(~TRTNAMElabel) +
          scale_y_apply(obsSCALES__$scale[obsSCALES__$NAME == unique(x$NAME)]) +
          theme(legend.position="bottom",
                strip.text = element_text(size = max(5,basefs-ceiling(sqrt(ntrt)))),
                plot.caption = element_text(size = 7, hjust=0))
      }
    )
    si__ <- attr(grstratk__,"split_labels")
    si__$STRAT <- names(si__)[2]
    names(si__)[2] <- "VALUE"
    names(grstratk__) <- with(si__, {paste0(NAME,".",STRAT,"::",VALUE)} )
    grstrat__ <- c(grstrat__,grstratk__)
    attr(grstrat__, "strat_info") <- rbind(attr(grstrat__, "strat_info"), si__)
  }
  if (!is.null(filename)) {
    if (exists("grstrat__")) {
      plotList <- c(gr__,grstrat__)
    }
    IQRoutputPDF(plotList,filename=filename)
  } else {
    plyr::l_ply(gr__, print)
    if (exists("grstrat__")) plyr::l_ply(grstrat__, print)
  }
  if (FLAGreturnObject) {
    out <- list(unstratified=gr__,stratified=grstrat__)
    attr(out, "data") <- data_out
    return(out)
  }
}
#'@export
plotCovDistribution_IQRdataGENERAL <- function(data,
                                               covNames = NULL,
                                               scale = "linear",
                                               filename = NULL,
                                               FLAGreturnObject=FALSE)
{
  data__ <- data
  contInfo0__ <- attr(data__, "covInfo")
  catInfo0__ <- attr(data__, "catInfo")
  if (!is_IQRdataGENERAL(data__)) 
    stopIQR("Input argument is not an IQRdataGENERAL object.")
  if (!class(covNames) %in% c("NULL", "character")) 
    stopIQR("Invalid input argument for 'covNames'. Must be either NULL or character vector of covariate names.")
  if (!(is.null(filename) | (is.character(filename)) & length(filename) == 
        1)) 
    stopIQR("Filename for PDF needs to be character string (no vector).")
  if (is.null(covNames)) {
    covNames__ <- c(contInfo0__$COLNAME, catInfo0__$COLNAME)
  }
  else {
    covNames__ <- covNames
    missingCovNames__ <- setdiff(covNames, c(contInfo0__$COLNAME, 
                                             catInfo0__$COLNAME))
    if (length(missingCovNames__) != 0) {
      warningIQR("Following covariate(s) are no covariates in the dataset: ", 
                 paste0(missingCovNames__, collapse = ", "))
      covNames__ <- intersect(covNames__, c(contInfo0__$COLNAME, 
                                            catInfo0__$COLNAME))
    }
  }
  data_out <- list()
  env.data_out <- environment()
  contNames__ <- intersect(covNames__, contInfo0__$COLNAME)
  if (length(contNames__) > 0) {
    obsSCALES__ <- handle_obsScalesIQR(scale, contNames__, 
                                       default = "linear")
    logCOV__ <- obsSCALES__[obsSCALES__$scale == "log", 
                            "NAME"]
    for (k in seq_along(logCOV__)) {
      if (any(data__[[logCOV__[k]]] <= 0, na.rm = TRUE)) {
        warningIQR("Trying to transform covariate that contains negative values. Left on linear scale.")
      }
      else {
        data__[[logCOV__[k]]] <- log(data__[[logCOV__[k]]])
        contInfo0__$UNIT[contInfo0__$COLNAME == logCOV__[k]] <- paste0("log(", 
                                                                       contInfo0__$UNIT[contInfo0__$COLNAME == logCOV__[k]], 
                                                                       ")")
      }
    }
    idCols__ <- c("USUBJID")
    data__ <- data__[order(data__$USUBJID, data__$TIME), 
    ]
    contdata__ <- as.data.frame(data__[!duplicated(data__$USUBJID), 
                                       c(idCols__, contNames__)])
    contdata.long__ <- tidyr::gather(contdata__, COLNAME, 
                                     VALUE, -USUBJID)
    contdata.long__ <- merge(contdata.long__, contInfo0__, 
                             all.x = TRUE, all.y = FALSE)
    contdata.long__$COVlabel <- with(contdata.long__, paste0(COLNAME, 
                                                             "\n", NAME, " (", UNIT, ")"))
    contdata.long__ <- contdata.long__[!is.na(contdata.long__$VALUE), 
    ]
    nBins__ <- ceiling(min(nrow(contdata__)/5, 20))
    data_out[["cont"]] <- contdata.long__
    grCont__ <- IQRggplot(data = contdata.long__) + geom_histogram(aes_string(x = "VALUE"), 
                                                                   bins = nBins__) + facet_wrap(~COVlabel, scales = "free") + 
      ylab("") + xlab("") + ggtitle(label = "Histograms of continuous covariates")
  }
  else {
    grCont__ <- NULL
  }
  catNames__ <- intersect(covNames__, catInfo0__$COLNAME)
  if (length(catNames__) > 0) {
    idCols__ <- c("USUBJID")
    data__ <- data__[order(data__$USUBJID, data__$TIME), 
    ]
    # edited from source here
    catdata__ <- as.data.frame(data__[!duplicated(data__[c("USUBJID",catNames__)]), 
                                      c(idCols__, catNames__)])
    catInfo0__ <- handle_duplicatedLevels(catInfo0__)
    for (covk__ in catNames__) {
      catdata__[[covk__]] <- as.character(factor(catdata__[[covk__]], 
                                                 levels = aux_explode(catInfo0__$VALUES[catInfo0__$COLNAME == 
                                                                                          covk__]), labels = aux_explode(catInfo0__$VALUETXT[catInfo0__$COLNAME == 
                                                                                                                                               covk__])))
    }
    catdata.long__ <- tidyr::gather(catdata__, COLNAME, 
                                    VALUE, -USUBJID)
    catdata.long__ <- merge(catdata.long__, catInfo0__[, 
                                                       c("COLNAME", "NAME", "UNIT")], all.x = TRUE, all.y = FALSE)
    catdata.long__$COVlabel <- with(catdata.long__, paste0(COLNAME, 
                                                           "\n", NAME, " (", UNIT, ")"))
    # eliminate duplicated rows
    catdata.long__ <- catdata.long__ %>% group_by(COLNAME, USUBJID) %>% unique
    data_out[["cat"]] <- catdata.long__
    grCat__ <- IQRggplot(data = catdata.long__) +
      geom_bar(aes_string(x = "VALUE"), stat = "count", orientation = "x", width = 0.8) + 
      facet_wrap(~COVlabel, scales = "free", ncol = 1) +
      ylab("") + 
      xlab("") +
      ggtitle(label = "Level counts of categorical covariates") + 
      coord_flip()
  }
  else {
    grCat__ <- NULL
  }
  if (!is.null(filename)) {
    plotList <- list(grCont__, grCat__)
    IQRoutputPDF(plotList, filename = filename)
  }
  else {
    if (!is.null("grCont__")) 
      print(grCont__)
    if (!is.null("grCat__")) 
      print(grCat__)
  }
  if (FLAGreturnObject) {
    out <- list(continuous = grCont__, categorical = grCat__)
    attr(out, "data") <- data_out
    return(out)
  }
}
#'@export
plotCorCov_IQRdataGENERAL <- function(data,
                                      covNames = NULL,
                                      scale = "linear",
                                      filename = NULL,
                                      CORR_THRESHOLD=0.3,
                                      fontSize = NULL,
                                      FLAGreturnObject=FALSE)
{
  data__ <- data
  contInfo0__ <- attr(data__,"covInfo")
  if (!is_IQRdataGENERAL(data__))
    stopIQR("Input argument is not an IQRdataGENERAL object.")
  if (!class(covNames) %in% c("NULL","character"))
    stopIQR("Invalid input argument for 'covNames'. Must be either NULL or character vector of covariate names.")
  if (!(is.null(filename) | (is.character(filename)) & length(filename) == 1))
    stopIQR("Filename for PDF needs to be character string (no vector).")
  if (is.null(covNames)) {
    covNames__ <- contInfo0__$COLNAME
  } else {
    covNames__ <- covNames
    missingCovNames__ <- setdiff(covNames, contInfo0__$COLNAME)
    if (length(missingCovNames__) != 0){
      warningIQR('Following covariate(s) are no continuous covariates in the dataset: ',paste0(missingCovNames__, collapse = ", "))
      covNames__ <- intersect(covNames__,contInfo0__$COLNAME)
    }
  }
  if (length(covNames__) < 2) {
    warningIQR("Less than 2 covariates available or selected for plotting.");
    return(NULL)
  }
  obsSCALES__ <- handle_obsScalesIQR(scale, covNames__, default = "linear")
  logCOV__    <- obsSCALES__[obsSCALES__$scale == "log", "NAME"]
  for (k in seq_along(logCOV__)) {
    if (any(data__[[logCOV__[k]]]<=0, na.rm = TRUE)){
      warningIQR("Trying to transform covariate that contains negative values. Left on linear scale.")
    } else {
      data__[[logCOV__[k]]] <- log(data__[[logCOV__[k]]])
      contInfo0__$UNIT[contInfo0__$COLNAME == logCOV__[k]] <- paste0("log(",contInfo0__$UNIT[contInfo0__$COLNAME == logCOV__[k]],")")
    }
  }
  idCols__ <- c("USUBJID")
  data__ <- data__[order(data__$USUBJID,data__$TIME),]
  covdata__ <- as.data.frame(data__[!duplicated(data__$USUBJID),c(idCols__,covNames__)])
  y__ <- tidyr::gather(covdata__,CNAME,CVALUE,-USUBJID)
  yy__ <- dplyr::full_join(y__,y__,by="USUBJID")
  yy__ <- within(yy__, {
    CNo.x <- as.numeric(factor(CNAME.x))
    CNo.y <- as.numeric(factor(CNAME.y))
    upper <- CNo.x > CNo.y
    lower <- CNo.x < CNo.y
    diag  <- CNo.x == CNo.y
  })
  cc__ <- plyr::ddply(yy__[yy__$diag|yy__$lower,], ~CNAME.x+CNAME.y, function(xxx) {
    out <- with(xxx, {
      res <- stats::cor.test(CVALUE.x,CVALUE.y,method="pearson")
      pos.x <- mean(range(CVALUE.x, na.rm=TRUE))
      pos.y <- mean(range(CVALUE.y, na.rm=TRUE))
      data.frame(corr=res$estimate, p.value=res$p.value, pos.x=pos.x, pos.y=pos.y)
    })
    return(out)
  })
  cc__$diag <- cc__$CNAME.x == cc__$CNAME.y
  cc__ <- dplyr::left_join(cc__, contInfo0__, by = c("CNAME.x"="COLNAME"))
  cc__ <- dplyr::left_join(cc__, contInfo0__, by = c("CNAME.y"="COLNAME"), suffix = c(".x",".y"))
  cc__$CORR_THRESHOLD <- CORR_THRESHOLD
  if (is.null(fontSize)) {
    if (length(covNames__) <=3) {
      fontSize__ <- 4
    } else {
      if (length(covNames__) <= 5) {
        fontSize__ <- 3
      } else {
        fontSize__ <- 2
      }
    }
  } else {
    fontSize__ <- fontSize
  }
  yy__ <- yy__[!is.na(yy__$CVALUE.x) & !is.na(yy__$CVALUE.y),]
  data_out <- yy__[yy__$upper,]
  gr__ <- IQRggplot(data=yy__[yy__$upper,], aes_string("CVALUE.x","CVALUE.y")) +
    geom_point(alpha=0.3) +
    geom_smooth(method = "lm", color = "forestgreen") +
    geom_label(data=cc__[!cc__$diag,],
               aes_string("pos.x", "pos.y",
                          label = "round(corr,2)",
                          color = 'factor(as.character(p.value < 0.05), levels = c("FALSE", "TRUE"))',
                          fill = 'factor(as.character(abs(corr) > CORR_THRESHOLD), levels = c("FALSE", "TRUE"))'),
               size=fontSize__+1) +
    scale_color_manual("p-value<0.05",values = c("FALSE" = 'grey', "TRUE" = 'firebrick'), drop = FALSE) +
    scale_fill_manual(paste0("|corr|>",CORR_THRESHOLD),values = c("FALSE" = "white", "TRUE" = "#ffaaaa"), drop = FALSE) +
    facet_grid(CNAME.y~CNAME.x, scales = "free") +
    theme(strip.text = element_blank(), axis.title = element_blank()) +
    ggtitle("Correlation of continuous covariates",subtitle="Numeric values correspond to Pearson's product moment correlation coefficient") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(legend.position="bottom")
  if (length(covNames__) <= 9) {
    gr__ <- gr__ + geom_label(data=cc__[cc__$diag,],
                              aes_string("pos.x","pos.y",label= 'paste0(CNAME.x,"\n",aux_strrep(NAME.x," ","\n"), "\n(",UNIT.x,")")'),
                              size = fontSize__, vjust = 0.5, hjust=0.5, label.size = NA)
  } else {
    gr__ <- gr__ + geom_label(data=cc__[cc__$diag,], aes_string("pos.x","pos.y", label= "CNAME.x"), size = fontSize__+0.5, vjust = 0.5)
  }
  if (!is.null(filename)) {
    IQRoutputPDF(gr__,filename=filename,scale = 1.2)
  } else {
    print(gr__)
  }
  if (FLAGreturnObject) {
    attr(gr__, "data") <- data_out
    return(gr__)
  }
}
#'@export
plotCorCat_IQRdataGENERAL <- function(data, catNames = NULL, filename = NULL, fontSize=NULL, FLAGreturnObject=FALSE)
{
  data__ <- data
  catInfo0__ <- attr(data__,"catInfo")
  if (is.null(catInfo0__)) return()
  if (all(c("TRT", "TRTNAME") %in% names(data__)) & !("TRT" %in% catInfo0__$COLNAME)) {
    trtInfo__ <- unique(data__[, c("TRT", "TRTNAME")])
    trtInfo__ <- trtInfo__[order(trtInfo__$TRT),]
    trtInfo__ <- trtInfo__[!is.na(trtInfo__$TRT),]
    addcatinfo__ <- data.frame(COLNAME = "TRT", NAME = "Treatment group", UNIT = "-",
                               VALUETXT = paste0(trtInfo__$TRTNAME, collapse = ","),
                               VALUES  = paste0(trtInfo__$TRT, collapse = ","),
                               TIME.VARYING = FALSE)
    catInfo0__ <- rbind(catInfo0__, addcatinfo__)
  }
  if (all(c("STUDYN", "STUDY") %in% names(data__)) & !("STUDYN" %in% catInfo0__$COLNAME)) {
    stdInfo__ <- unique(data__[, c("STUDYN", "STUDY")])
    stdInfo__ <- stdInfo__[order(stdInfo__$STUDYN),]
    stdInfo__ <- stdInfo__[!is.na(stdInfo__$STUDYN),]
    addcatinfo__ <- data.frame(COLNAME = "STUDYN", NAME = "Study", UNIT = "-",
                               VALUETXT = paste0(stdInfo__$STUDY, collapse = ","),
                               VALUES   = paste0(stdInfo__$STUDYN, collapse = ","),
                               TIME.VARYING = FALSE)
    catInfo0__ <- rbind(catInfo0__, addcatinfo__)
  }
  if (!is_IQRdataGENERAL(data__))
    stopIQR("Input argument is not an IQRdataGENERAL object.")
  if (!class(catNames) %in% c("NULL","character"))
    stopIQR("Invalid input argument for 'catNames'. Must be either NULL or character vector of covariate names.")
  if (!(is.null(filename) | (is.character(filename)) & length(filename) == 1))
    stopIQR("Filename for PDF needs to be character string (no vector).")
  if (is.null(catNames)) {
    catNames__ <- catInfo0__$COLNAME
  } else {
    catNames__ <- catNames
    missingcatNames__ <- setdiff(catNames, catInfo0__$COLNAME)
    if (length(missingcatNames__) != 0){
      warningIQR('Following covariate(s) are no categorical covariates in the dataset: ',paste0(missingcatNames__, collapse = ", "))
      catNames__ = intersect(catNames__,catInfo0__$COLNAME)
    }
  }
  if (length(catNames__) < 2) {
    warningIQR("Less than 2 covariates available or selected for plotting.");
    return(NULL)
  }
  data__ <- data__[order(data__$USUBJID,data__$TIME),]
  covdata__ <- as.data.frame(data__[!duplicated(data__$USUBJID),c("USUBJID",catNames__)])
  catInfo0__ <- handle_duplicatedLevels(catInfo0__)
  for (covk__ in catNames__) {
    x__ <- as.character(factor(
      covdata__[[covk__]],
      levels = aux_explode(catInfo0__$VALUES[catInfo0__$COLNAME == covk__]),
      labels = paste0(aux_explode(catInfo0__$VALUETXT[catInfo0__$COLNAME == covk__]))
    ))
    x__[!is.na(x__)] <- paste0(covk__,": ",x__[!is.na(x__)])
    covdata__[[covk__]] <- x__
  }
  y__ <- tidyr::gather(covdata__,CNAME,CVALUE,-USUBJID)
  y__ <- y__[!is.na(y__$CVALUE),]
  yy__ <- dplyr::full_join(y__,y__,by="USUBJID")
  alllev__ <- unique(yy__[,c("CNAME.x","CVALUE.x")])
  yy__$CNo.x <- as.numeric(factor(yy__$CNAME.x))
  yy__$CNo.y <- as.numeric(factor(yy__$CNAME.y))
  yy__$upper <- yy__$CNo.x > yy__$CNo.y
  yy__$lower <- yy__$CNo.x < yy__$CNo.y
  yy__$diag  <- yy__$CNo.x == yy__$CNo.y
  yy__$LNo.x <- as.numeric(factor(yy__$CVALUE.x, levels = alllev__$CVALUE.x))
  yy__$LNo.y <- as.numeric(factor(yy__$CVALUE.y, levels = alllev__$CVALUE.x))
  cc__ <- plyr::ddply(yy__[yy__$diag|yy__$lower,], ~CNAME.x+CNAME.y, function(xxx) {
    out <- with(xxx, {
      tbl <- table(CVALUE.x,CVALUE.y)
      if (nrow(tbl) >= 2) {
        res <- suppressWarnings(stats::chisq.test(tbl))
      } else {
        res <- list(p.value = 1)
      }
      pos.x <- mean(range(LNo.x, na.rm=TRUE))
      pos.y <- mean(range(LNo.y, na.rm=TRUE))
      data.frame(p.value=res$p.value, pos.x=pos.x, pos.y=pos.y)
    })
    return(out)
  })
  cc__$diag <- cc__$CNAME.x == cc__$CNAME.y
  cc__$upper <- cc__$CNAME.x > cc__$CNAME.y
  cc__$lower <- cc__$CNAME.x < cc__$CNAME.y
  cc__ <- dplyr::left_join(cc__, catInfo0__, by = c("CNAME.x"="COLNAME"))
  cc__ <- dplyr::left_join(cc__, catInfo0__, by = c("CNAME.y"="COLNAME"), suffix = c(".x",".y"))
  ixPsmall__ <- cc__$p.value<0.05
  cc__$p.value[!ixPsmall__] <- paste0("p=",round(cc__$p.value[!ixPsmall__],2))
  cc__$p.value[ixPsmall__] <- "p<0.05"
  cc__$p.value.small <- ixPsmall__
  FirLa__ <- levels(factor(yy__$CNAME.x))
  FirLa__ <- FirLa__[c(1,length(FirLa__))]
  cc__[cc__$CNAME.x == FirLa__[1] ,"pos.x"] <- -1
  cc__[cc__$CNAME.y == FirLa__[2],"pos.y"] <- -1
  yyy__ <- plyr::ddply(
    yy__[yy__$upper,],
    ~CNAME.x+CVALUE.x+CNAME.y+CVALUE.y+diag+lower+upper+CNo.y+CNo.x+LNo.y+LNo.x,
    function(x) c(N = nrow(x))
  )
  pVal__ <- unique(cc__[,c("CNAME.x","CNAME.y", "p.value.small")])
  names(pVal__) <- c("CNAME.y","CNAME.x", "p.value.small")
  yyy__ <- merge(yyy__, pVal__)
  if (is.null(fontSize)) {
    if (length(catNames__) <=3) {
      fontSize__ <- 4
    } else {
      if (length(catNames__) <= 5) {
        fontSize__ <- 3
      } else {
        fontSize__ <- 2
      }
    }
  } else {
    fontSize__ <- fontSize
  }
  adjax__ <- plyr::ddply(yyy__, ~CNAME.y+CNAME.x, function(x) {
    data.frame(
      LNo.x = c(min(x$LNo.x)-0.5, max(x$LNo.x)+0.5),
      LNo.y = c(min(x$LNo.y)-0.5, max(x$LNo.y)+0.5)
    )
  })
  adjax__ <- adjax__[adjax__$CNAME.x != FirLa__[1] | adjax__$CNAME.y != FirLa__[2],]
  data_out <- yyy__
  gr__ <- IQRggplot(data=yyy__, aes_string("LNo.x","LNo.y")) +
    geom_point(data=yyy__[yyy__$upper,],aes_string(size="N"),alpha=0.3) +
    geom_text(data=yyy__, aes_string(label = "N"), color = "darkred", hjust=0.5, size=fontSize__) +
    geom_text(data=yyy__, aes_string(label = "N"), hjust=0.5, size=fontSize__) +
    geom_label(data=cc__[cc__$lower,], aes_string("pos.x", "pos.y", label="p.value", color = "p.value.small"), size = fontSize__, vjust = 0.5) +
    geom_point(data=adjax__, color = "transparent" ) +
    scale_color_manual(values = c('grey', 'darkred')) +
    scale_size_area(max_size=6) +
    scale_x_continuous(breaks = seq_along(alllev__$CVALUE.x), labels = alllev__$CVALUE.x) +
    scale_y_continuous(breaks = seq_along(alllev__$CVALUE.x), labels = alllev__$CVALUE.x) +
    facet_grid(CNAME.y~CNAME.x, scales = "free") +
    theme(strip.text = element_blank(), axis.title = element_blank(),
          axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5, size=fontSize__+4),
          axis.text.y = element_text(size=fontSize__+4)) +
    theme(legend.position="none") +
    ggtitle("Correlation of categorical covariates",
            subtitle="Numeric values correspond to Pearson's Chi-squared Test for Count Data\nNon-available data (NA) is ignored")
  if (length(catNames__) <= 9) {
    gr__ <- gr__ + geom_label(data=cc__[cc__$diag,],
                              aes_string("pos.x","pos.y", label= 'paste0(CNAME.x,"\n",aux_strrep(NAME.x," ","\n"), ifelse((is.na(UNIT.x)),paste0(" (",UNIT.x,")"),""))'),
                              size = fontSize__, vjust = 0.5, hjust=0.5, label.size = NA)
  } else {
    gr__ <- gr__ + geom_label(data=cc__[cc__$diag,], aes_string("pos.x","pos.y", label= "CNAME.x"), size = fontSize__, vjust = 0.5)
  }
  if (!is.null(filename)) {
    IQRoutputPDF(gr__,filename=filename,scale = 1.2)
  } else {
    print(gr__)
  }
  if (FLAGreturnObject) {
    attr(gr__, "data") <- data_out
    return(gr__)
  }
}
#'@export
plotCorCovCat_IQRdataGENERAL <- function(data,
                                         covNames = NULL,
                                         catNames = NULL,
                                         scale = "linear",
                                         filename = NULL,
                                         fontSize = NULL,
                                         FLAGreturnObject = FALSE) {
  data__ <- data
  contInfo0__ <- attr(data__,"covInfo")
  catInfo0__ <- attr(data__,"catInfo")
  if (is.null(contInfo0__)) return()
  if (is.null(catInfo0__)) return()
  if (all(c("TRT", "TRTNAME") %in% names(data__)) & !("TRT" %in% catInfo0__$COLNAME)) {
    trtInfo__ <- unique(data__[, c("TRT", "TRTNAME")])
    trtInfo__ <- trtInfo__[order(trtInfo__$TRT),]
    trtInfo__ <- trtInfo__[!is.na(trtInfo__$TRT),]
    addcatinfo__ <- data.frame(COLNAME = "TRT", NAME = "Treatment group", UNIT = "-",
                               VALUETXT = paste0(trtInfo__$TRTNAME, collapse = ","),
                               VALUES  = paste0(trtInfo__$TRT, collapse = ","),
                               TIME.VARYING = FALSE)
    catInfo0__ <- rbind(catInfo0__, addcatinfo__)
  }
  if (all(c("STUDYN", "STUDY") %in% names(data__)) & !("STUDYN" %in% catInfo0__$COLNAME)) {
    stdInfo__ <- unique(data__[, c("STUDYN", "STUDY")])
    stdInfo__ <- stdInfo__[order(stdInfo__$STUDYN),]
    stdInfo__ <- stdInfo__[!is.na(stdInfo__$STUDYN),]
    addcatinfo__ <- data.frame(COLNAME = "STUDYN", NAME = "Study", UNIT = "-",
                               VALUETXT = paste0(stdInfo__$STUDY, collapse = ","),
                               VALUES   = paste0(stdInfo__$STUDYN, collapse = ","),
                               TIME.VARYING = FALSE)
    catInfo0__ <- rbind(catInfo0__, addcatinfo__)
  }
  if (!is_IQRdataGENERAL(data__))
    stopIQR("Input argument is not an IQRdataGENERAL object.")
  if (!class(covNames) %in% c("NULL","character"))
    stopIQR("Invalid input argument for 'covNames'. Must be either NULL or character vector of covariate names.")
  if (!class(catNames) %in% c("NULL","character"))
    stopIQR("Invalid input argument for 'catNames'. Must be either NULL or character vector of covariate names.")
  if (!(is.null(filename) | (is.character(filename)) & length(filename) == 1))
    stopIQR("Filename for PDF needs to be character string (no vector).")
  if (is.null(covNames)) {
    covNames__ <- contInfo0__$COLNAME
  } else {
    covNames__ <- covNames
    missingcovNames__ <- setdiff(covNames, contInfo0__$COLNAME)
    if (length(missingcovNames__) != 0){
      warningIQR('Following covariate(s) are no continuous covariates in the dataset: ',paste0(missingcovNames__, collapse = ", "))
      covNames__ = intersect(covNames__,contInfo0__$COLNAME)
    }
  }
  if (length(covNames__) < 1) {
    stopIQR("No continuous covariates available or selected for plotting.");
  }
  if (is.null(catNames)) {
    catNames__ <- catInfo0__$COLNAME
  } else {
    catNames__ <- catNames
    missingCatNames__ <- setdiff(catNames, catInfo0__$COLNAME)
    if (length(missingCatNames__) != 0){
      warningIQR('Following covariate(s) are no categorical covariates in the dataset: ',paste0(missingCatNames__, collapse = ", "))
      catNames__ = intersect(catNames__,catInfo0__$COLNAME)
    }
  }
  if (length(catNames__) < 1) {
    stopIQR("No categorical covariates available or selected for plotting.");
  }
  obsSCALES__ <- handle_obsScalesIQR(scale, covNames__, default = "linear")
  logCOV__    <- obsSCALES__[obsSCALES__$scale == "log","NAME"]
  for (k in seq_along(logCOV__)) {
    if (any(data__[[logCOV__[k]]]<=0, na.rm = TRUE)){
      warningIQR("Trying to transform covariate that contains negative values. Left on linear scale.")
    } else {
      data__[[logCOV__[k]]] <- log(data__[[logCOV__[k]]])
      contInfo0__$UNIT[contInfo0__$COLNAME == logCOV__[k]] <- paste0("log(",contInfo0__$UNIT[contInfo0__$COLNAME == logCOV__[k]],")")
    }
  }
  if (is.null(fontSize)) {
    if (length(catNames__) <=3) {
      fontSize__ <- 4
    } else {
      if (length(catNames__) <= 5) {
        fontSize__ <- 3
      } else {
        fontSize__ <- 2
      }
    }
  } else {
    fontSize__ <- fontSize
  }
  idCols__ <- c("USUBJID")
  data__ <- data__[order(data__$USUBJID,data__$TIME),]
  contdata__ <- as.data.frame(data__[!duplicated(data__$USUBJID),c(idCols__,covNames__)])
  catdata__ <- as.data.frame(data__[!duplicated(data__$USUBJID),c(idCols__,catNames__)])
  catInfo0__ <- handle_duplicatedLevels(catInfo0__)
  for (covk__ in catNames__) {
    catdata__[[covk__]] <- as.character(factor(
      catdata__[[covk__]],
      levels = aux_explode(catInfo0__$VALUES[catInfo0__$COLNAME == covk__]),
      labels = aux_explode(catInfo0__$VALUETXT[catInfo0__$COLNAME == covk__])
    ))
  }
  y1__ <- tidyr::gather(contdata__,CNAME,CVALUE,-USUBJID)
  y1__ <- y1__[!is.na(y1__$CVALUE),]
  y1__ <- dplyr::left_join(y1__,contInfo0__[,c("COLNAME","NAME","UNIT")], by=c("CNAME"="COLNAME"))
  y1__$CNAME <- with(y1__, paste0(CNAME, "\n", NAME, " (", UNIT,")"))
  y1__$NAME <- y1__$UNIT <- NULL
  y2__ <- tidyr::gather(catdata__,CNAME,CVALUE,-USUBJID)
  y2__ <- dplyr::left_join(y2__,catInfo0__[,c("COLNAME","NAME","UNIT")], by=c("CNAME"="COLNAME"))
  y2__$NAME <- y2__$UNIT <- NULL
  catN__ <- plyr::ddply(y2__, ~CNAME+CVALUE, function(xxx) c(N=length(xxx$CVALUE)))
  catN__$N <- paste0("(N = ",(catN__$N),")")
  y2__ <- dplyr::left_join(y2__, catN__,by = c("CNAME", "CVALUE"))
  y2__$CVALUE <- with(y2__, paste(CVALUE, N))
  y2__$N <- NULL
  yy__ <- dplyr::full_join(y1__,y2__,by="USUBJID")
  yy__$CNAME.x <- gsub(" ","\n",yy__$CNAME.x)
  data_out <- yy__
  gr__ <- IQRggplot(yy__, aes_string("CVALUE.y","CVALUE.x")) +
    geom_boxplot(size=0.5) +
    facet_grid(CNAME.y~CNAME.x, scales = "free") +
    coord_flip() +
    theme(strip.text.y=element_text(angle = 0,size=fontSize__+4),
          strip.text.x=element_text(angle = 0,size=fontSize__+4),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 90,hjust=1, size=fontSize__+4),
          axis.text.y = element_text(size=fontSize__+4)) +
    ggtitle("Correlation of continuous and categorical covariates")
  if (!is.null(filename)) {
    IQRoutputPDF(gr__,filename=filename,scale = 1.2)
  } else {
    print(gr__)
  }
  if (FLAGreturnObject) {
    attr(gr__, "data") <- data_out
    return(gr__)
  }
}
#'@export
plotDoseSchedule_IQRdataGENERAL <- function(data,
                                            doseNames = NULL,
                                            filename  = NULL,
                                            NperPage  = 10,
                                            FLAGreturnObject = FALSE) {
  data__ <- data
  if (!is_IQRdataGENERAL(data__))
    stopIQR("Input argument is not an IQRdataGENERAL object.")
  if (!(is.null(filename) | (is.character(filename)) & length(filename) == 1))
    stopIQR("Filename for PDF needs to be character string (no vector).")
  if (is.null(doseNames)) {
    doseNAMES__ <- attr(data__, "doseNAMES")
  } else {
    missingdos__ <- setdiff(doseNames, attr(data__, "doseNAMES"))
    if (length(missingdos__) != 0){
      warningIQR('Following dosing(s) are not contained in the dataset: ',paste0(missingdos__, collapse = ", "))
    }
    doseNAMES__ = intersect(doseNames,attr(data__, "doseNAMES"))
    if (length(doseNAMES__) == 0){
      stopIQR('No existing observations chosen for plotting.')
    }
  }
  data_out <- list()
  env.data_out <- environment()
  dos__ <- data__[data__$NAME %in% doseNAMES__,]
  dos__$ADDL <- ifelse(is.na(dos__$ADDL), 0, dos__$ADDL)
  dos__$II <- ifelse(is.na(dos__$II), 0, dos__$II)
  subj__   <- unique(dos__$USUBJID)
  nSubj__  <- length(subj__)
  pageNo__ <- ceiling(1:nSubj__ / NperPage)
  pages__  <- data.frame(USUBJID = subj__, Page = pageNo__)
  dos__    <- plyr::join(dos__, pages__, by = "USUBJID")
  gr__ <- plyr::dlply(dos__, ~Page, function(xx__) {
    env.data_out$data_out[[xx__$Page[1]]] <- xx__
    IQRggplot(xx__, aes_string(x="TIME", xend="TIME+(ADDL+1)*II+1", y = "USUBJID", yend = "USUBJID")) +
      geom_segment(data=dplyr::filter(xx__, ADDL > 0), aes_string(color = "NAME"), alpha = 0.7)+
      geom_point(data=dplyr::filter(xx__, ADDL == 0), aes_string(color = "NAME"), alpha = 1, shape = "|")+
      scale_color_manual(values = IQRtoolsColors[2:10]) +
      labs(
        x=paste0("Time ", xx__$TIMEUNIT[1]),
        y = "",
        title = paste0("Dosing schedule (page",xx__$Page[1]," of ",max(pageNo__)," )"),
        caption = "Missing ADDL and II set to 0"
      ) +
      theme(legend.position = "bottom")
  })
  if (!is.null(filename)) {
    IQRoutputPDF(gr__,filename=filename)
  } else {
    plyr::l_ply(gr__, print)
  }
  if (FLAGreturnObject) {
    attr(gr__, "data") <- data_out
    return(gr__)
  }
}
#'@export
plotSampleSchedule_IQRdataGENERAL <- function(data,
                                              obsNames = NULL,
                                              filename  = NULL,
                                              NperPage  = 10,
                                              FLAGreturnObject = FALSE) {
  data__ <- data
  if (!is_IQRdataGENERAL(data__))
    stopIQR("Input argument is not an IQRdataGENERAL object.")
  if (!(is.null(filename) | (is.character(filename)) & length(filename) == 1))
    stopIQR("Filename for PDF needs to be character string (no vector).")
  if (!"TRTNAME" %in% names(data__)) {
    data__$TRTNAME <- ""
    data__$TRT <- 0
  }
  doseNAMES__ <- attr(data__, "doseNAMES")
  if (is.null(obsNames)) {
    obsNAMES__ <- attr(data__, "obsNAMES")
  } else {
    missingobs__ <- setdiff(obsNames, attr(data__, "obsNAMES"))
    if (length(missingobs__) != 0){
      warningIQR('Following observation(s) are not contained in the dataset: ',paste0(missingobs__, collapse = ", "))
    }
    obsNAMES__ = intersect(obsNames,attr(data__, "obsNAMES"))
    if (length(obsNAMES__) == 0){
      stopIQR('No existing observations chosen for plotting.')
    }
  }
  data__ <- data__[data__$NAME %in% c(doseNAMES__, obsNAMES__),]
  data_out <- list()
  env.data_out <- environment()
  data__$TRTNAME <- ifelse(is.na(data__$TRTNAME), "UNKNOWN", data__$TRTNAME)
  subj__   <- unique(as.data.frame(data__)[,c("TRTNAME","USUBJID")])
  if (any(duplicated(subj__$USUBJID)))  {
    warningIQR("Different treatments annotated for subject(s).\n  Please check!!! Only first annotated in plots.")
    subj__ <- subj__[!duplicated(subj__$USUBJID),]
  }
  subj__ <- subj__[order(subj__$TRTNAME),]
  nSubj__  <- dim(subj__)[1]
  pageNo__ <- ceiling(1:nSubj__ / NperPage)
  subj__$Page <- pageNo__
  data__    <- plyr::join(data__, subj__, by = c("TRTNAME","USUBJID"))
  data__$USUBJID <- factor(data__$USUBJID, levels = subj__$USUBJID)
  gr__ <- plyr::dlply(data__, ~Page, function(xx__) {
    obs__ <- xx__[xx__$NAME %in% obsNAMES__,]
    dos__ <- xx__[xx__$NAME %in% doseNAMES__,]
    dos__$ADDL <- ifelse(is.na(dos__$ADDL), 0, dos__$ADDL)
    dos__$II  <- ifelse(is.na(dos__$II), 0, dos__$II)
    if (dim(obs__)[1] > 0) {
      gr__ <- IQRggplot(obs__, aes_string(y="TIME", x = 1)) +
        geom_point(aes_string(color = "NAME"), position = position_dodge(width=0.8), shape = 124)
      if (dim(dos__)[1] > 0) {
        gr__ <- gr__ +
          geom_segment(data = dos__, aes_string(y="TIME", yend="TIME+(ADDL+1)*II+1", x = 0, xend = 0,color = "NAME"), show.legend = FALSE, size = 2)
      }
    } else {
      gr__ <- IQRggplot(data = dos__, aes_string(y="TIME", x = 0)) +
        geom_segment(aes_string(yend="TIME+(ADDL+1)*II+1", xend = 0,color = "NAME"), size = 2)
    }
    gr__ <- gr__ +
      geom_text(data=unique(rbind(obs__[,c("USUBJID", "TRTNAME")], dos__[,c("USUBJID", "TRTNAME")])),
                aes_string(y=0, x= 1.5, label = "TRTNAME"),
                size = 2.5, hjust=0, vjust=0) +
      scale_color_manual("Observation/Dose",values = IQRtoolsColors[2:20]) +
      scale_x_continuous(breaks = c(0,1), labels = c("Dosing","Observation")) +
      labs(
        y=paste0("Time (", xx__$TIMEUNIT[1],")"),
        x = "",
        title = paste0("Sampling schedule (page ",xx__$Page[1]," of ",max(pageNo__)," )"),
        caption = "Missing ADDL and II set to 0"
      ) +
      facet_grid(USUBJID~.) + coord_flip(xlim = c(-0.5, 2)) +
      theme(legend.position = "bottom", legend.direction = "vertical",
            strip.text.y = element_text(angle=0),
            axis.text.y = element_text(angle = 0))
  })
  if (!is.null(filename)) {
    suppressWarnings(IQRoutputPDF(gr__,filename=filename,scale = 1.2))
  } else {
    plyr::l_ply(gr__, print)
  }
  if (FLAGreturnObject) {
    attr(gr__, "data") <- data_out
    return(gr__)
  }
}
#'@export
summaryObservations_IQRdataGENERAL <- function(data,
                                               obsNames = NULL,
                                               stratifyColumn = "STUDY",
                                               SIGNIF = 3,
                                               tableTitle = NULL,
                                               footerAddText = NULL,
                                               filename = NULL,
                                               FLAGpk = TRUE,
                                               FLAGpatients = FALSE,
                                               FLAGsimple = FALSE) {
  if(!is_IQRdataGENERAL(data)) {
    stopIQR("Input argument 'data' is not an IQRdataGENERAL object")
  }
  if (!"STUDY" %in% names(data)) {
    data$STUDY <- "Undefined Study"
    data$STUDYN <- 0
  }
  obsNamesData__ <- attributes(data)$obsNAMES
  if (is.null(obsNames)) {
    obsNames <- obsNamesData__
  }
  if (!all(obsNames %in% obsNamesData__)) {
    stopIQR(paste0("The following obsNames are not available in the provided data:\n  ",paste0(obsNames[!(obsNames %in% obsNamesData__)],collapse=", ")))
  }
  if (is.null(tableTitle)) {
    tableTitle <- "Summary of available observations"
  }
  data__ <- data[!is.na(data[[stratifyColumn]]),]
  data__ <- data__[data__$NAME %in% obsNames | data__$NAME %in% attributes(data__)$doseNAMES,]
  dataSplit__ <- split(data__,data__[[stratifyColumn]])
  dataSplit__[["TOTAL"]] <- data__
  table__ <- do.call(rbind,lapply(seq_along(dataSplit__), function (k__) {
    datak__ <- dataSplit__[[k__]]
    TIMEfirstDose__ <- datak__$TIME[datak__$EVID==1][1]
    datak__ <- datak__[datak__$NAME %in% obsNames,]
    Name__ <- names(dataSplit__)[k__]
    Nindiv__ <- length(unique(datak__$USUBJID))
    Nsamples__ <- nrow(datak__)
    NsamplesBLOQ__ <- nrow(datak__[(datak__$DV<datak__$LLOQ | datak__$CENS==1) & !is.na(datak__$DV) & !is.na(datak__$LLOQ) & !is.na(datak__$TIME),])
    NsamplesBLOQpostFirstDose__ <- nrow(datak__[(datak__$DV<datak__$LLOQ | datak__$CENS==1) & !is.na(datak__$DV) & !is.na(datak__$LLOQ) & !is.na(datak__$TIME) & datak__$TIME > TIMEfirstDose__,])
    NmissingDV__ <- nrow(datak__[is.na(datak__$DV),])
    NmissingTIME__ <- nrow(datak__[is.na(datak__$TIME),])
    NNONZEROPFD__ <- nrow(datak__[datak__$VALUE>0 & !is.na(datak__$DV) & !is.na(datak__$TIME) & datak__$TIME < TIMEfirstDose__,])
    NMDV1outliers__ <- nrow(datak__[datak__$MDV==1 & datak__$EVID==0,])
    NsamplesMDV0__ <- nrow(datak__[datak__$MDV==0,])
    data.frame(
      DATA = Name__,
      SUBJECT_N = Nindiv__,
      NSAMPLES = Nsamples__,
      NSAMPLESBLOQ = paste0(NsamplesBLOQ__, " (", signif(100*NsamplesBLOQ__/Nsamples__,SIGNIF), "%)"),
      NSAMPLESBLOQPFD = paste0(NsamplesBLOQpostFirstDose__, " (", signif(100*NsamplesBLOQpostFirstDose__/Nsamples__,SIGNIF), "%)"),
      NSAMPLESMISSINGDV = paste0(NmissingDV__, " (", signif(100*NmissingDV__/Nsamples__,SIGNIF), "%)"),
      NSAMPLESMISSINGTIME = paste0(NmissingTIME__, " (", signif(100*NmissingTIME__/Nsamples__,SIGNIF), "%)"),
      NSAMPLESNONZEROPFD = paste0(NNONZEROPFD__, " (", signif(100*NNONZEROPFD__/Nsamples__,SIGNIF), "%)"),
      NSAMPLESOUTLIERS = paste0(NMDV1outliers__, " (", signif(100*NMDV1outliers__/Nsamples__,SIGNIF), "%)"),
      NSAMPLESANALYSIS = paste0(NsamplesMDV0__, " (", signif(100*NsamplesMDV0__/Nsamples__,SIGNIF), "%)"),
      stringsAsFactors=FALSE
    )
  }))
  if (FLAGpatients) {
    pattext__ <- "patients"
  } else {
    pattext__ <- "subjects"
  }
  names(table__) <- c(
    "Data",
    paste0("N ",pattext__),
    "N samples",
    "N BLOQ samples*",
    "N BLOQ samples post first dose*",
    "N missing observations",
    "N missing time information",
    "N non-zero pre-first dose samples",
    "N total ignored observations",
    "N samples included in analysis"
  )
  table__[["N non-zero pre-first dose samples"]] <- NULL
  if (!FLAGpk) {
    table__[["N BLOQ samples post first dose*"]] <- NULL
  }
  catInfo__ <- attributes(data)$catInfo
  if (stratifyColumn %in% catInfo__$COLNAME) {
    numValues__  <- aux_explode(catInfo__$VALUES[catInfo__$COLNAME == stratifyColumn])
    textValues__ <- aux_explode(catInfo__$VALUETXT[catInfo__$COLNAME == stratifyColumn])
    table__$Data <- as.character(factor(table__$Data,
                                        levels = c(numValues__, "TOTAL"),
                                        labels = c(textValues__, "TOTAL")))
  }
  footer__ <- "N: Number of<br>"
  if (attributes(data)$methodBLLOQ=="M1") footer__ <- paste0(footer__,"* These records are excluded from the analysis (M1 method).<br>")
  if (attributes(data)$methodBLLOQ=="M3") footer__ <- paste0(footer__,"* These records are not excluded from the analysis but censored (M3 method).<br>")
  if (attributes(data)$methodBLLOQ=="M4") footer__ <- paste0(footer__,"* These records are not excluded from the analysis but censored (M4 method).<br>")
  if (attributes(data)$methodBLLOQ=="M5") footer__ <- paste0(footer__,"* These records are not excluded from the analysis but set to LLOQ/2 (M5 method).<br>")
  if (attributes(data)$methodBLLOQ=="M6") footer__ <- paste0(footer__,"* Only the first BLOQ value in a sequence is kept in the analysis with LLOQ/2 (M6 method).<br>")
  if (attributes(data)$methodBLLOQ=="M7") footer__ <- paste0(footer__,"* These records are not excluded from the analysis but set to 0 (M7 method).<br>")
  footer__ <- paste0(footer__,footerAddText)
  if (!is.null(filename)) {
    report__ <- TRUE
  } else {
    report__ <- FALSE
  }
  if (FLAGsimple) {
    footer__ <- NULL
    table__$`N BLOQ samples*` <- NULL
    table__$`N BLOQ samples post first dose*` <- NULL
    table__$`N missing observations` <- NULL
    table__$`N missing time information` <- NULL
    table__$`N non-zero pre-first dose samples` <- NULL
    table__$`N total ignored observations` <- NULL
    table__$`N samples` <- NULL
  }
  if (!is.null(filename)) {
    filename <- paste0(aux_strrep(filename,".txt",""),".txt")
    IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=tableTitle,report=report__,filename=filename)
    return(invisible(NULL))
  }
  output__ <- IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=tableTitle,report=report__)
  output__
}
#'@export
summaryCat_IQRdataGENERAL <- function (data, catNames = NULL, stratifyColumns = "STUDY", 
                                       stratifyOrder = NULL, FLAGtotal = NULL, SIGNIF = 3, tableTitle = NULL, 
                                       footerAddText = NULL, filename = NULL, FLAGpatients = FALSE) 
{
  if (!is_IQRdataGENERAL(data)) {
    stopIQR("Input argument 'data' is not an IQRdataGENERAL object")
  }
  if (!"STUDY" %in% names(data)) {
    data$STUDY <- "Undefined Study"
    data$STUDYN <- 0
  }
  catNamesData__ <- attributes(data)$catInfo$COLNAME
  if (is.null(catNames)) {
    catNames <- catNamesData__
  }
  if (is.null(catNames)) {
    message("No categorical covariates defined or present")
    return(invisible(NULL))
  }
  if (!all(catNames %in% catNamesData__)) {
    stopIQR(paste0("The following catNames are not available in the provided data:\n  ", 
                   paste0(catNames[!(catNames %in% catNamesData__)], 
                          collapse = ", ")))
  }
  if (is.null(FLAGtotal)) {
    FLAGtotal <- rep(FALSE, length(stratifyColumns))
  }
  if (length(FLAGtotal) == 1) {
    FLAGtotal <- rep(FLAGtotal, length(stratifyColumns))
  }
  if (length(FLAGtotal) != length(stratifyColumns)) {
    stopIQR("Missmatch between length of stratifyColumns and FLAGtotal input arguments")
  }
  if (!is.list(stratifyOrder)) {
    stratifyOrder <- list(stratifyOrder)
  }
  for (k__ in seq_along(stratifyColumns)) {
    entries__ <- unique(data[[stratifyColumns[k__]]])
    order__ <- unlist(stratifyOrder[k__])
    if (!is.null(order)) {
      if (!all(order__ %in% entries__)) {
        stopIQR(paste0("Not all entries in stratifyColumns are present for stratification column ", 
                       stratifyColumns[k__]))
      }
    }
  }
  if (is.null(tableTitle)) {
    tableTitle <- "Summary of demographic and baseline characteristics for categorical information"
  }
  datafirst__ <- unique(data[,c("USUBJID",catNames, stratifyColumns)])
  catInfo0__ <- attributes(data)$catInfo
  catInfo__ <- catInfo0__
  catInfo__ <- catInfo__[catInfo__$COLNAME %in% catNames, 
  ]
  FLAGmissingVal <- FALSE
  for (icat__ in catNames) {
    if (any(is.na(datafirst__[[icat__]]))) {
      tmp__ <- catInfo__[catInfo__$COLNAME == icat__, 
      ]
      tmpValImpute__ <- max(as.numeric(aux_explode(tmp__$VALUES))) + 
        1
      tmp__$VALUES <- paste0(tmp__$VALUES, ",", tmpValImpute__)
      tmp__$VALUETXT <- paste0(tmp__$VALUETXT, ",n.a.**")
      catInfo__[catInfo__$COLNAME == icat__, ] <- tmp__
      datafirst__[[icat__]][is.na(datafirst__[[icat__]])] <- as.character(tmpValImpute__)
      FLAGmissingVal <- TRUE
    }
  }
  baseTable__ <- do.call(rbind, lapply(1:nrow(catInfo__), 
                                       function(kRowCatInfo__) {
                                         row__ <- catInfo__[kRowCatInfo__, ]
                                         characteristic__ <- row__$NAME
                                         colname__ <- row__$COLNAME
                                         categories__ <- aux_explodePC(row__$VALUETXT)
                                         values__ <- aux_explodePC(row__$VALUES)
                                         subtable__ <- data.frame(Characteristic = characteristic__, 
                                                                  Category = categories__, COLNAME = colname__, 
                                                                  VALUE = values__, stringsAsFactors = FALSE)
                                       }))
  table__ <- baseTable__
  table__$COLNAME <- NULL
  table__$VALUE <- NULL
  FLAGmissingCat <- FALSE
  for (kstratcolumn__ in seq_along(stratifyColumns)) {
    stratifyColumn__ <- stratifyColumns[kstratcolumn__]
    FLAGtotal__ <- FLAGtotal[kstratcolumn__]
    stratifyOrder__ <- unlist(stratifyOrder[kstratcolumn__])
    datafirst__$stratifyColumn__ <- datafirst__[[stratifyColumn__]]
    if (stratifyColumn__ %in% catInfo0__$COLNAME) {
      tmpVal__ <- aux_explode(catInfo0__$VALUES[catInfo0__$COLNAME == 
                                                  stratifyColumn__])
      tmpTxt__ <- aux_explode(catInfo0__$VALUETXT[catInfo0__$COLNAME == 
                                                    stratifyColumn__])
      if (!is.null(stratifyOrder__)) 
        stratifyOrder__ <- as.character(factor(stratifyOrder__, 
                                               levels = tmpVal__, labels = tmpTxt__))
      datafirst__$stratifyColumn__ <- as.character(factor(datafirst__$stratifyColumn__, 
                                                          levels = tmpVal__, labels = tmpTxt__))
    }
    if (is.null(stratifyOrder__)) 
      stratifyOrder__ <- unique(datafirst__$stratifyColumn__[!is.na(datafirst__$stratifyColumn__)])
    if (any(is.na(datafirst__$stratifyColumn__))) 
      stratifyOrder__ <- c(stratifyOrder__, NA)
    stratifyOrder__[is.na(stratifyOrder__)] <- "missing*"
    datafirst__$stratifyColumn__[is.na(datafirst__$stratifyColumn__)] <- "missing*"
    if ("missing*" %in% stratifyOrder__) 
      FLAGmissingCat <- TRUE
    dummy__ <- sapply(stratifyOrder__, function(strata__) {
      stratData__ <- datafirst__[datafirst__$stratifyColumn__ == 
                                   strata__, ]
      NTOTALstrat__ <- length(unique(stratData__[,"USUBJID"]))
      STRATCOLname__ <- paste0(stratData__$stratifyColumn__[1], 
                               " [N=", NTOTALstrat__, "]")
      stratValueCol__ <- sapply(1:nrow(baseTable__), function(kRowTable__) {
        row__ <- baseTable__[kRowTable__, ]
        COLNAME__ <- row__$COLNAME
        VALUE__ <- row__$VALUE
        NCATstraty__ <- length(stratData__[stratData__[[COLNAME__]]==VALUE__ & !duplicated.array(stratData__[,c(COLNAME__,"USUBJID")]),"USUBJID"])
        PERCCATstraty__ <- signif(NCATstraty__/NTOTALstrat__ * 
                                    100, SIGNIF)
        paste0(NCATstraty__, " (", PERCCATstraty__, 
               "%)")
      })
      table__ <<- cbind(table__, stratValueCol__)
      names(table__)[ncol(table__)] <<- STRATCOLname__
    })
    if (FLAGtotal__) {
      dataTOTAL__ <- datafirst__
      NTOTAL__ <- nrow(dataTOTAL__)
      totalValueCol__ <- sapply(1:nrow(baseTable__), function(kRowTable__) {
        row__ <- baseTable__[kRowTable__, ]
        COLNAME__ <- row__$COLNAME
        VALUE__ <- row__$VALUE
        NCAT__ <- sum(dataTOTAL__[COLNAME__] == VALUE__, 
                      na.rm = TRUE)
        PERCCAT__ <- signif(NCAT__/NTOTAL__ * 100, SIGNIF)
        paste0(NCAT__, " (", PERCCAT__, "%)")
      })
      table__ <- cbind(table__, totalValueCol__)
      names(table__)[ncol(table__)] <- paste0("TOTAL [N=", 
                                              NTOTAL__, "]")
    }
  }
  table__$Characteristic[duplicated(table__$Characteristic)] = " "
  if (!is.null(filename)) {
    report__ <- TRUE
  }
  else {
    report__ <- FALSE
  }
  if (FLAGpatients) {
    footer__ <- "N: Number of patients<br>Number of patients in each category and percentage within this category"
  }
  else {
    footer__ <- "N: Number of subjects<br>Number of subjects in each category and percentage within this category"
  }
  if (FLAGmissingCat) 
    footer__ <- paste0(footer__, "<br>", "* stratification value missing in dataset")
  if (FLAGmissingVal) 
    footer__ <- paste0(footer__, "<br>", "** missing values in dataset")
  if (!is.null(footerAddText)) {
    footer__ <- paste0(footer__, "<br>", footerAddText)
  }
  if (!is.null(filename)) {
    filename <- paste0(aux_strrep(filename, ".txt", ""), 
                       ".txt")
    IQRoutputTable(xtable = table__, xfooter = footer__, 
                   xtitle = tableTitle, report = report__, filename = filename)
    return(invisible(NULL))
  }
  output__ <- IQRoutputTable(xtable = table__, xfooter = footer__, 
                             xtitle = tableTitle, report = report__)
  output__
}
#'@export
summaryCov_IQRdataGENERAL <- function(data,
                                      covNames = NULL,
                                      stratifyColumns = "STUDY",
                                      stratifyOrder = NULL,
                                      FLAGtotal = NULL,
                                      SIGNIF = 3,
                                      tableTitle = NULL,
                                      footerAddText = NULL,
                                      filename = NULL,
                                      FLAGpatients = FALSE) {
  if(!is_IQRdataGENERAL(data)){
    stopIQR("Input argument 'data' is not an IQRdataGENERAL object")
  }
  if (!"STUDY" %in% names(data)) {
    data$STUDY <- "Undefined Study"
    data$STUDYN <- 0
  }
  covNamesData__ <- attributes(data)$covInfo$COLNAME
  if (is.null(covNames)) {
    covNames <- covNamesData__
  }
  if (is.null(covNames)) {
    message("No continuous covariates defined or present")
    return(invisible(NULL))
  }
  if (!all(covNames %in% covNamesData__)) {
    stopIQR(paste0("The following covNames are not available in the provided data:\n  ",paste0(covNames[!(covNames %in% covNamesData__)],collapse=", ")))
  }
  if (is.null(FLAGtotal)) {
    FLAGtotal <- rep(FALSE,length(stratifyColumns))
  }
  if (length(FLAGtotal)==1) {
    FLAGtotal <- rep(FLAGtotal,length(stratifyColumns))
  }
  if (length(FLAGtotal) != length(stratifyColumns)) {
    stopIQR("Missmatch between length of stratifyColumns and FLAGtotal input arguments")
  }
  if (!is.list(stratifyOrder)) {
    stratifyOrder <- list(stratifyOrder)
  }
  for (k__ in seq_along(stratifyColumns)) {
    entries__ <- unique(data[[stratifyColumns[k__]]])
    order__ <- unlist(stratifyOrder[k__])
    if (!is.null(order)) {
      if (!all(order__ %in% entries__)) {
        stopIQR(paste0("Not all entries in stratifyColumns are present for stratification column ",stratifyColumns[k__]))
      }
    }
  }
  if (is.null(tableTitle)) {
    tableTitle <- "Summary of demographic and baseline characteristics for continuous information"
  }
  datafirst__ <- as.data.frame(data[!duplicated(data$USUBJID),])
  covInfo__ <- attributes(data)$covInfo
  catInfo__ <- attributes(data)$catInfo
  covInfo__ <- covInfo__[covInfo__$COLNAME %in% covNames,]
  baseTable__ <- do.call(rbind,lapply(1:nrow(covInfo__), function (kRowcovInfo__) {
    row__ <- covInfo__[kRowcovInfo__,]
    characteristic__ <- paste0(row__$NAME," (",row__$UNIT,")")
    colname__ <- row__$COLNAME
    subtable__ <- data.frame(
      Characteristic = characteristic__,
      COLNAME = colname__,
      stringsAsFactors=FALSE
    )
  }))
  table__ <- baseTable__
  table__$COLNAME <- NULL
  FLAGmissingVal <- FALSE
  FLAGmissingCat <- FALSE
  for (kstratcolumn__ in seq_along(stratifyColumns)) {
    stratifyColumn__ <- stratifyColumns[kstratcolumn__]
    FLAGtotal__ <- FLAGtotal[kstratcolumn__]
    stratifyOrder__ <- unlist(stratifyOrder[kstratcolumn__])
    if (is.null(stratifyOrder__))
      stratifyOrder__ <- unique(datafirst__[[stratifyColumn__]][!is.na(datafirst__[[stratifyColumn__]])])
    if (any(is.na(datafirst__[[stratifyColumn__]])))
      stratifyOrder__ <- c(stratifyOrder__, NA)
    if (stratifyColumn__ %in% catInfo__$COLNAME) {
      tmpVal__ <- aux_explode(catInfo__$VALUES[catInfo__$COLNAME == stratifyColumn__])
      tmpTxt__ <- aux_explode(catInfo__$VALUETXT[catInfo__$COLNAME == stratifyColumn__])
      stratifyOrder__ <- as.character(factor(stratifyOrder__, levels = tmpVal__, labels = tmpTxt__))
      datafirst__[[stratifyColumn__]] <- as.character(factor(datafirst__[[stratifyColumn__]], levels = tmpVal__, labels = tmpTxt__))
    }
    stratifyOrder__[is.na(stratifyOrder__)] <- "missing*"
    datafirst__[[stratifyColumn__]][is.na(datafirst__[[stratifyColumn__]])] <- "missing*"
    if ("missing*" %in% stratifyOrder__) FLAGmissingCat <- TRUE
    dummy__ <- sapply(stratifyOrder__, function (strata__) {
      stratData__ <- datafirst__[datafirst__[stratifyColumn__] == strata__,]
      stratData__ <- stratData__[!is.na(stratData__[stratifyColumn__]),]
      NTOTALstrat__ <- nrow(stratData__)
      STRATCOLname__ <- paste0(stratData__[[stratifyColumn__]][1]," [N=",NTOTALstrat__,"]")
      stratValueCol__ <- sapply(1:nrow(baseTable__), function (kRowTable__) {
        row__ <- baseTable__[kRowTable__,]
        COLNAME__ <- row__$COLNAME
        if (!all(is.na(stratData__[[COLNAME__]]))) {
          MEAN <- signif(mean(stratData__[[COLNAME__]],na.rm=TRUE),SIGNIF)
          STD <- signif(stats::sd(stratData__[[COLNAME__]],na.rm=TRUE),SIGNIF)
          MAX <- signif(max(stratData__[[COLNAME__]],na.rm=TRUE),SIGNIF)
          MIN <- signif(min(stratData__[[COLNAME__]],na.rm=TRUE),SIGNIF)
          nNA <- sum(is.na(stratData__[[COLNAME__]]))
          out__ <- paste0(MEAN, " (",STD,") [",MIN,"-",MAX,"]")
          if (nNA > 0) {out__ <- paste0(out__, " (",nNA," n.a.**)"); FLAGmissingVal <<- TRUE}
        } else {
          out__ <- "-"
        }
        out__
      })
      table__ <<- cbind(table__, stratValueCol__)
      names(table__)[ncol(table__)] <<- STRATCOLname__
    })
    if (FLAGtotal__) {
      dataTOTAL__ <- datafirst__[!is.na(datafirst__[stratifyColumn__]),]
      NTOTAL__ <- nrow(dataTOTAL__)
      totalValueCol__ <- sapply(1:nrow(baseTable__), function (kRowTable__) {
        row__ <- baseTable__[kRowTable__,]
        COLNAME__ <- row__$COLNAME
        MEAN <- signif(mean(dataTOTAL__[[COLNAME__]],na.rm=TRUE),SIGNIF)
        STD <- signif(stats::sd(dataTOTAL__[[COLNAME__]],na.rm=TRUE),SIGNIF)
        MAX <- signif(max(dataTOTAL__[[COLNAME__]],na.rm=TRUE),SIGNIF)
        MIN <- signif(min(dataTOTAL__[[COLNAME__]],na.rm=TRUE),SIGNIF)
        nNA <- sum(is.na(dataTOTAL__[[COLNAME__]]))
        out__ <- paste0(MEAN, " (",STD,") [",MIN,"-",MAX,"]")
        if (nNA > 0) out__ <- paste0(out__, " (",nNA," n.a.**)")
        out__
      })
      table__ <- cbind(table__, totalValueCol__)
      names(table__)[ncol(table__)] <- paste0("TOTAL [N=",NTOTAL__,"]")
    }
  } 
  if (!is.null(filename)) {
    report__ <- TRUE
  } else {
    report__ <- FALSE
  }
  if (FLAGpatients) {
    footer__ <- "N: Number of patients<br>Entries represent: Mean (Standard deviation) [Minimum-Maximum]"
  } else {
    footer__ <- "N: Number of subjects<br>Entries represent: Mean (Standard deviation) [Minimum-Maximum]"
  }
  if (FLAGmissingCat)
    footer__ <- paste0(footer__, "<br>","* stratification value missing in dataset")
  if (FLAGmissingVal)
    footer__ <- paste0(footer__, "<br>","** missing values in dataset")
  if (!is.null(footerAddText)) {
    footer__ <- paste0(footer__,"<br>",footerAddText)
  }
  if (!is.null(filename)) {
    filename <- paste0(aux_strrep(filename,".txt",""),".txt")
    IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=tableTitle,report=report__,filename=filename)
    return(invisible(NULL))
  }
  output__ <- IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=tableTitle,report=report__)
  output__
}
#'@export
is_IQRdataGENERAL <- function(input) {
  methods::is(input,"IQRdataGENERAL")
}
#'@export
print.IQRdataGENERAL <- function(x, ...) {
  print.data.frame(x, ...)
  cat("\n")
  cat("\nIQRdataGENERAL object\n")
  cat("\n")
  checkResults__ <- check_IQRdataGENERAL(x,FLAGreturnText=TRUE)
  if (nchar(checkResults__$checkInfoText_ERROR) > 0) {
    message(
      "\nERRORS in the dataset that need to be addressed\n===============================================\n",
      checkResults__$checkInfoText_ERROR
    )
  }
  if (nchar(checkResults__$checkInfoText_WARNING) > 0) {
    message(
      "\nWARNINGS in the dataset that should be addressed\n================================================\n",
      checkResults__$checkInfoText_WARNING
    )
  }
  if (nchar(checkResults__$checkInfoText_MINOR) > 0) {
    message(
      "\nMINOR issues in the dataset that might be addressed\n===================================================\n",
      checkResults__$checkInfoText_MINOR
    )
  }
  if (nchar(checkResults__$checkInfoText_WARNING) > 0)
    warningIQR("Issues in the dataset detected that should be addressed (see above)")
  if (nchar(checkResults__$checkInfoText_ERROR) > 0)
    warningIQR("Issues in the dataset detected that need to be addressed (see above)")
}
#'@export
summary.IQRdataGENERAL <- function(object, extended = FALSE, ...) {
  if(!"AE" %in% names(object)) object$AE <- 0
  x__ <- object
  xrow__ <- NULL
  dI__ <- doseInfo_IQRdataGENERAL(x__)
  if (nrow(dI__) > 0) {
    VALUE <- paste0("Ntotal: ",dI__$TOTAL.DOSES, ",  Nindiv (min/median/max): ",dI__$MIN.INDIV.DOSES,"/",dI__$MEDIAN.INDIV.DOSES,"/",dI__$MAX.INDIV.DOSES,")")
    xtable__ <- data.frame(INFO="Dose events",
                           NAME=dI__$DOSE.NAME,
                           VALUE=VALUE)
    xtable__ <- rbind(xtable__,xrow__)
  }
  oI__ <- obsInfo_IQRdataGENERAL(x__[x__$AE==0,])
  if (nrow(oI__) > 0) {
    VALUE <- paste0("Ntotal: ",oI__$TOTAL.OBSERVATIONS, ",  Nindiv (min/median/max): ",oI__$MIN.INDIV.OBSERVATIONS,"/",oI__$MEDIAN.INDIV.OBSERVATIONS,"/",oI__$MAX.INDIV.OBSERVATIONS,")")
    xtableadd__ <- data.frame(INFO="Observation events (all)",
                              NAME=oI__$OBSERVATION.NAME,
                              VALUE=VALUE)
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  oI__ <- obsInfo_IQRdataGENERAL(x__[x__$MDV==0 & x__$AE==0,])
  if (nrow(oI__) > 0) {
    VALUE <- paste0("Ntotal: ",oI__$TOTAL.OBSERVATIONS, ",  Nindiv (min/median/max): ",oI__$MIN.INDIV.OBSERVATIONS,"/",oI__$MEDIAN.INDIV.OBSERVATIONS,"/",oI__$MAX.INDIV.OBSERVATIONS,")")
    xtableadd__ <- data.frame(INFO="Observation events (MDV=0)",
                              NAME=oI__$OBSERVATION.NAME,
                              VALUE=VALUE)
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  oI__ <- dplyr::arrange(obsInfo_IQRdataGENERAL(x__[x__$AE==1,]),desc(as.numeric(TOTAL.OBSERVATIONS)),OBSERVATION.NAME)
  if (nrow(oI__) > 0) {
    VALUE <- paste0("Ntotal: ",oI__$TOTAL.OBSERVATIONS, ",  Nindiv (min/median/max): ",oI__$MIN.INDIV.OBSERVATIONS,"/",oI__$MEDIAN.INDIV.OBSERVATIONS,"/",oI__$MAX.INDIV.OBSERVATIONS,")")
    xtableadd__ <- data.frame(INFO="Adverse events (AE=1)",
                              NAME=oI__$OBSERVATION.NAME,
                              VALUE=VALUE)
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  zI__ <- zeroDoses_IQRdataGENERAL(x__)
  if (sum(zI__$N.ZERO.DOSES)>0) {
    xtableadd__ <- data.frame(INFO="Doses AMT=0 present",
                              NAME="ALL dose events",
                              VALUE=paste0(as.character(sum(zI__$N.ZERO.DOSES)>0), " (N=",sum(zI__$N.ZERO.DOSES),")"))
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  pI__ <- placeboIndiv_IQRdataGENERAL(x__)
  if (length(pI__) > 0) {
    xtableadd__ <- data.frame(INFO="Placebo subjects present (AMT=0 or no doses)",
                              NAME="ALL dose events",
                              VALUE=paste0(as.character(length(pI__)>0), " (N=",length(pI__),")"))
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  N_IGNORED_OBS <- sum(x__$EVID==0 & x__$YTYPE>0 & x__$MDV==1)
  if (N_IGNORED_OBS > 0) {
    xtableadd__ <- data.frame(INFO="IGNORED (MDV=1) observation records present",
                              NAME="ALL observation events",
                              VALUE=paste0(as.character(N_IGNORED_OBS>0), " (N=",N_IGNORED_OBS,")"))
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  pI__ <- noObsIndiv_IQRdataGENERAL(x__)
  if (length(pI__) > 0) {
    xtableadd__ <- data.frame(INFO="Subjects without observations (MDV=0) present",
                              NAME="ALL observation events",
                              VALUE=paste0(as.character(length(pI__)>0), " (N=",length(pI__),")"))
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  bI__ <- blloqInfo_IQRdataGENERAL(x__[x__$AE==0,])
  xtableadd__ <- data.frame(INFO="Total BLLOQ information",
                            NAME=bI__$blloqInfoTotal$OBSERVATION,
                            VALUE=paste0("N=",bI__$blloqInfoTotal$BLLOQ.OBSERVATIONS," / ",bI__$blloqInfoTotal$PERCENT.BLLOQ,"%"))
  xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  VALUE <- unname(unlist(lapply(bI__$blloqInfoIndividual, function (x__) max(x__$PERCENT.BLLOQ))))
  xtableadd__ <- data.frame(INFO="Max % BLLOQ values in a subject",
                            NAME=bI__$blloqInfoTotal$OBSERVATION,
                            VALUE=paste0(VALUE, "%"))
  xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  VALUE <- attr(x__,"methodBLLOQ")
  if (is.null(VALUE)) VALUE <- "UNDEFINED"
  xtableadd__ <- data.frame(INFO="BLLOQ handling method",
                            NAME="All observation events",
                            VALUE=VALUE)
  xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  allCovInfo     <- c(attr(x__,"cov0"),attr(x__,"covT"),attr(x__,"cat0"),attr(x__,"catT"))
  covColumnNAMES <- names(allCovInfo)
  covColumnValues <- data.frame(do.call(rbind,lapply(split(x__,x__$USUBJID), function (y) {
    y[1,covColumnNAMES]
  })),stringsAsFactors=FALSE)
  missingPercent <- signif(100*apply(is.na(covColumnValues),2,sum) / nrow(covColumnValues),3)
  missingPercent <- missingPercent[missingPercent>0]
  if (length(missingPercent) > 0) {
    xtableadd__ <- data.frame(INFO="Missing covariates",
                              NAME=names(missingPercent),
                              VALUE=paste0(missingPercent,"%"))
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  colContainingNA__ <- getNAcolNLME_IQRdataGENERAL(x__)
  if (length(colContainingNA__) > 0) {
    xtableadd__ <- data.frame(INFO="NLME columns containing NA",
                              NAME="All events",
                              VALUE=paste0(colContainingNA__,collapse=", "))
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  if (as.character(sum(x__$AE) > 0)) {
    xtableadd__ <- data.frame(INFO="Adverse event information present",
                              NAME="All events",
                              VALUE=as.character(sum(x__$AE) > 0))
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
    if (sum(x__$AE) > 0) {
      xtableadd__ <- data.frame(INFO="Drug unrelated adverse events present",
                                NAME="All events",
                                VALUE=as.character(sum(x__$AEDRGREL[x__$AE==1]==0) > 0))
      xtable__ <- rbind(xtable__,xtableadd__,xrow__)
    }
  }
  dosObsNames <- unique(x__$NAME[x__$EVID==1 | x__$EVID==0 & x__$YTYPE>0])
  if (as.character(length(setdiff(unique(x__$NAME),dosObsNames)) > 0)) {
    xtableadd__ <- data.frame(INFO="Non dose/observation events present",
                              NAME="All events",
                              VALUE=as.character(length(setdiff(unique(x__$NAME),dosObsNames)) > 0))
    xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  }
  checkResults <- check_IQRdataGENERAL(x__,FLAGreturnText=TRUE)
  VALUE <- sapply(checkResults,function (x__) {
    out <- "NONE"
    if (nchar(x__)>0) out <- "YES (see text below the table for more information)"
    out
  })
  xtableadd__ <- data.frame(INFO="Issues present in the data",
                            NAME=c("Minor","Warnings","Errors"),
                            VALUE=VALUE)
  xtable__ <- rbind(xtable__,xtableadd__,xrow__)
  print(IQRoutputTable(xtable__,report=FALSE))
  if (extended) {
    covInfo__ <- IQRoutputTable(attr(x__, "covInfo"))
    catInfo__ <- IQRoutputTable(attr(x__, "catInfo"))
    regressorNames__ <- attr(x__, "regressorNames")
    cat("\nAdditional Information:\n")
    cat("\nContinuous covariates:\n")
    cat(covInfo__)
    cat("\nCategorical covariates:\n")
    cat(catInfo__)
    cat("\nRegressor names:\n")
    cat(regressorNames__)
    cat("\n\n")
  }
  if (nchar(checkResults$checkInfoText_ERROR) > 0) {
    message(
      "\nERRORS in the dataset that need to be addressed\n===============================================\n",
      checkResults$checkInfoText_ERROR
    )
  }
  if (nchar(checkResults$checkInfoText_WARNING) > 0) {
    message(
      "\nWARNINGS in the dataset that should be addressed\n================================================\n",
      checkResults$checkInfoText_WARNING
    )
  }
  if (nchar(checkResults$checkInfoText_MINOR) > 0) {
    message(
      "\nMINOR issues in the dataset that might be addressed\n===================================================\n",
      checkResults$checkInfoText_MINOR
    )
  }
  if (nchar(checkResults$checkInfoText_WARNING) > 0)
    warningIQR("Issues in the dataset detected that should be addressed (see above)")
  if (nchar(checkResults$checkInfoText_ERROR) > 0)
    warningIQR("Issues in the dataset detected that need to be addressed (see above)")
}
#'@export
exportNLME_IQRdataGENERAL <- function(data,
                                      regressorNames = NULL,
                                      doseNAMES      = NULL,
                                      obsNAMES       = NULL,
                                      filename       = NULL,
                                      FLAGxpt        = FALSE,
                                      FLAGdefine     = TRUE,
                                      FLAGzip        = FALSE,
                                      addColLabels   = NULL) {
  FLAGrmUnusedOBSdose <- TRUE
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  if (is.null(filename))
    stopIQR("please provide a filename for the NLME CSV file")
  if (!is.null(obsNAMES)) {
    if (!all(obsNAMES %in% attr(data,"obsNAMES")))
      stopIQR("not all obsNAMES available in the dataset")
  }
  x <- aux_fileparts(filename)
  filename <- x$filename
  pathname <- x$pathname
  if (nchar(filename) > 8 & FLAGxpt)
    stopIQR("please provide a filename with length of max 8 characters (w/o extension)")
  if (length(setdiff(regressorNames,names(data))) > 0)
    stopIQR("not all regressorNames available in the dataset")
  newColOrder__ <- c("IXGDF",  "IGNORE", "USUBJID","ID",     "STUDY",    "STUDYN",  "TRTNAME", "TRT",
                     "TIME",   "TIMEPOS","NT",     "TAD",    "TIMEUNIT", "YTYPE",   "NAME",    "VALUE",   "DV",
                     "UNIT",   "LLOQ",   "CENS",   "MDV",    "EVID",     "AMT",     "ADM",     "II",      "ADDL",
                     "ROUTE",  "TINF",   "RATE",   "DOSE",   "PROFNR",   "PROFTIME")
  newColOrderPresent__ <- newColOrder__[newColOrder__ %in% names(data)]
  if ("CONDITION" %in% names(data))
    newColOrder__ <- c(newColOrderPresent__[1:4], "CONDITION", newColOrderPresent__[5:length(newColOrderPresent__)])
  newColOrderPresent__ <- setdiff(newColOrderPresent__,regressorNames)
  newColOrderPresent__ <- c(newColOrderPresent__, names(data)[!(names(data) %in% newColOrderPresent__ | names(data) %in% regressorNames)])
  newColOrderPresent__ <- c(newColOrderPresent__, regressorNames)
  newColOrderPresent__ <- setdiff(newColOrderPresent__,c("AE","AEGRADE","AESER","AEDRGREL","VALUETXT","BASE","SCREEN"))
  dataNLME__ <- data[,newColOrderPresent__]
  if (!is.null(doseNAMES)) {
    if (length(setdiff(doseNAMES,attr(data,"doseNAMES"))) > 0) {
      stopIQR("Povided input argument doseNAMES contains element(s) that are not defined as doses")
    }
    dataNLME__$ADM[dataNLME__$EVID==1] <- 999
    dataNLME__$YTYPE[dataNLME__$EVID==1] <- 0 
    for (k in seq_along(doseNAMES)) {
      dataNLME__$ADM[dataNLME__$NAME == doseNAMES[k]] <- k
    }
  } else {
    doseNAMES <- attr(data,"doseNAMES")
  }
  if (FLAGrmUnusedOBSdose) {
    dataNLME__ <- dataNLME__[is.na(dataNLME__$ADM) | dataNLME__$ADM != 999,]
  }
  if (!is.null(obsNAMES)) {
    dataNLME__$YTYPE[dataNLME__$EVID==0] <- 999
    dataNLME__$ADM[dataNLME__$EVID==0] <- 0
    for (k in seq_along(obsNAMES)) {
      dataNLME__$YTYPE[dataNLME__$NAME == obsNAMES[k]] <- k
    }
  } else {
    obsNAMES <- attr(data,"obsNAMES")
  }
  if (FLAGrmUnusedOBSdose) {
    dataNLME__ <- dataNLME__[is.na(dataNLME__$YTYPE) | dataNLME__$YTYPE != 999,]
  }
  dataNLME__ <- data.frame(lapply(dataNLME__, function(x) {
    if (!is.numeric(x)) x <- gsub(x=x,pattern=" ",replacement=":::")
    x
  }),stringsAsFactors = FALSE)
  dataNLME__$ADM[dataNLME__$EVID==0] <- NA
  dataNLME__$YTYPE[dataNLME__$EVID==1] <- NA
  attr(dataNLME__,"methodBLLOQ") <- attr(data,"methodBLLOQ")
  attr(dataNLME__,"doseNAMES") <- doseNAMES
  attr(dataNLME__,"obsNAMES") <- obsNAMES
  attr(dataNLME__,"aeNAMES")     <- attr(data,"aeNAMES")
  attr(dataNLME__,"covInfo") <- attr(data,"covInfo")
  attr(dataNLME__,"catInfo") <- attr(data,"catInfo")
  attr(dataNLME__,"imputeInfo") <- attr(data,"imputeInfo")
  attr(dataNLME__,"regressorNames") <- regressorNames
  attr(dataNLME__,"class") <- c("IQRdataGENERAL","data.frame")
  if (is.null(addColLabels)) addColLabels <- list()
  for (k__ in seq_along(regressorNames)) {
    addColLabels[[regressorNames[k__]]] <- paste0("Regression parameter ",regressorNames[k__])
  }
  export_IQRdataGENERAL(dataNLME__,filename=paste0(pathname,"/",filename),
                        FLAGxpt=FLAGxpt,
                        FLAGdefine=FLAGdefine,
                        addColLabels=addColLabels,
                        FLAGzip=FLAGzip)
}
#'@export
#'@importFrom data.table fread
load_IQRdataGENERAL <- function(filename) {
  if (substr(filename,nchar(filename)-7,nchar(filename)) == ".dat.zip") {
    if (!file.exists(filename))
      stopIQR(sprintf("file '%s' does not exist",filename))
    oldpath__ <- getwd()
    pfe__ <- aux_fileparts(filename);
    setwd(pfe__$pathname)
    datzippath__ <- paste0(getwd(),"/",pfe__$filename,pfe__$fileext)
    tempdir <- paste0(tempdirIQR(),"_data")
    aux_mkdir(tempdir)
    setwd(tempdir)
    unlink("*",recursive=TRUE)
    file.copy(from=datzippath__,to=".")
    utils::unzip(paste0(pfe__$filename,pfe__$fileext))
    filename <- paste0(getwd(),"/",aux_strrep(pfe__$filename,".dat",""),".csv")
    FLAG_zip_loaded__ <- TRUE
  } else {
    FLAG_zip_loaded__ <- FALSE
  }
  filenameCSV__ <- paste0(aux_strrep(filename,".csv",""),".csv")
  filenameATR__ <- paste0(aux_strrep(filename,".csv",""),".atr")
  if (!file.exists(filenameCSV__))
    stopIQR(sprintf("File '%s' does not exist",filenameCSV__))
  if (!file.exists(filenameATR__))
    stopIQR(sprintf("Attribute file '%s' does not exist. Please consider importing the datafile with the function 'IQRdataGENERAL'",filenameATR__))
  data__ <- as.data.frame(data.table::fread(input = filenameCSV__, header=TRUE,sep=",",stringsAsFactors=FALSE,na.strings="."))
  atrcontents <- loadAttributeFile(filenameATR__)
  if (any(grepl("_", names(data__)))) {
    warningIQR("There are underscores in column names which are removed.")
    names(data__) <- gsub("_", "", names(data__))
    atrcontents$catInfo$COLNAME <- gsub("_", "", atrcontents$catInfo$COLNAME)
    atrcontents$covInfo$COLNAME <- gsub("_", "", atrcontents$covInfo$COLNAME)
    atrcontents$imputeInfo$COVNAME <- gsub("_", "", atrcontents$imputeInfo$COVNAME)
  }
  data__ <- generalDataHandleMissingColums(data__)
  numericColumns__ <- getColumnsInfo_IQRdataGENERAL()$numericColumns
  numericColumnsPresent__ <- numericColumns__[numericColumns__ %in% names(data__)]
  for (k__ in seq_along(numericColumnsPresent__)) {
    suppressWarnings(data__[[numericColumnsPresent__[k__]]] <- as.numeric(data__[[numericColumnsPresent__[k__]]]))
  }
  data__$TIMEUNIT <- toupper(data__$TIMEUNIT)
  data__$ROUTE <- toupper(data__$ROUTE)
  data__ <- generalData_IDcolumn(data__,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_TIMEPOScolumn(data__,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_EVIDcolumn(data__,doseNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_MDVcolumn(data__,doseNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_CENScolumn(data__,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_AMTcolumn(data__,doseNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_DVcolumn(data__,doseNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_TINFcolumn(data__,doseNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_RATEcolumn(data__,doseNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_ADMcolumn(data__,doseNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_TADcolumns(data__,doseNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_DOSEcolumns(data__,doseNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  data__ <- generalData_YTYPEcolumn(data__,obsNAMES,FLAGforceOverwriteNLMEcols=FALSE)
  namesOrder__ <- getColumnsInfo_IQRdataGENERAL()$namesOrder
  numericOrderPresent__ <- namesOrder__[namesOrder__ %in% names(data__)]
  dataOrdered__ <- data__[,numericOrderPresent__]
  data__ <- cbind(dataOrdered__, data__[,setdiff(names(data__),numericOrderPresent__)])
  data__ <- data.frame(lapply(data__, function(x__) {
    if (!is.numeric(x__)) x__ <- gsub(x=x__,pattern=":::",replacement=" ")
    x__
  }),row.names=NULL,stringsAsFactors=FALSE)
  attributes(data__) <- c(attributes(data__),atrcontents)
  attr(data__,"class") <- c("IQRdataGENERAL", attr(data__,"class"))
  if (FLAG_zip_loaded__) {
    unlink("*",recursive=TRUE)
    setwd(oldpath__)
    unlink(tempdir)
  }
  data__$ADM[is.na(data__$ADM)] <- 0
  data__$YTYPE[is.na(data__$YTYPE)] <- 0
  return(data__)
}
#'@export
#'@importFrom data.table fwrite
export_IQRdataGENERAL <- function(data,
                                  filename        = NULL,
                                  FLAGxpt         = FALSE,
                                  FLAGdefine      = TRUE,
                                  addColLabels    = NULL,
                                  FLAGzip         = FALSE) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  if (is.null(filename))
    stopIQR("please provide a filename for the NLME CSV file")
  x__ <- aux_fileparts(filename)
  filename__ <- x__$filename
  pathname__ <- x__$pathname
  if (nchar(filename__) > 8 & FLAGxpt)
    stopIQR("please provide a filename with length of max 8 characters (w/o extension)")
  IQRoutputCSV(data,filename=paste0(pathname__,"/",filename__),na=".",quote=FALSE)
  if (FLAGxpt) exportXPT_IQRdataGENERAL(data,paste0(pathname__,"/",filename__),addColLabels=addColLabels)
  if (FLAGdefine) {
    datasetName__     <- filename__
    datasetLocation__ <- paste0(pathname__,"/",datasetName__)
    if (FLAGxpt) {
      datasetLocation__ <- paste0(datasetLocation__,".xpt")
    } else {
      datasetLocation__ <- paste0(datasetLocation__,".csv")
    }
    filenameRMD__        <- paste0(pathname__,"/",datasetName__,"_define.rmd")
    exportDEFINE_IQRdataGENERAL(data               = data,
                                datasetName        = datasetName__,
                                datasetLocation    = datasetLocation__,
                                datasetDescription = "General analysis dataset",
                                addColLabels       = addColLabels,
                                filename           = filenameRMD__)
  }
  methodBLLOQ__     <- paste0(deparse(attr(data,"methodBLLOQ")),collapse="\n")
  doseNAMES__       <- paste0(deparse(attr(data,"doseNAMES")),collapse="\n")
  obsNAMES__        <- paste0(deparse(attr(data,"obsNAMES")),collapse="\n")
  aeNAMES__         <- paste0(deparse(attr(data,"aeNAMES")),collapse="\n")
  covInfo__         <- paste0(deparse(attr(data,"covInfo")),collapse="\n")
  catInfo__         <- paste0(deparse(attr(data,"catInfo")),collapse="\n")
  imputeInfo__      <- paste0(deparse(attr(data,"imputeInfo")),collapse="\n")
  regressionNames__ <- paste0(deparse(attr(data,"regressorNames")),collapse="\n")
  ATTRTEXT__ <- paste0("# Attributes file for dataset ",filename__,"\n\n")
  ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents <- list()\n\n")
  ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$methodBLLOQ <- ",methodBLLOQ__,"\n\n")
  ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$doseNAMES <- ",doseNAMES__,"\n\n")
  ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$obsNAMES <- ",obsNAMES__,"\n\n")
  ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$aeNAMES <- ",aeNAMES__,"\n\n")
  ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$covInfo <- ",covInfo__,"\n\n")
  ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$catInfo <- ",catInfo__,"\n\n")
  ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$imputeInfo <- ",imputeInfo__,"\n\n")
  ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$regressorNames <- ",regressionNames__,"\n\n")
  IQRoutputFile(ATTRTEXT__,paste0(pathname__,"/",filename__,".atr"))
  if (FLAGzip) {
    oldpath__ <- getwd()
    setwd(pathname__)
    files__ <- dir(pattern=paste0(filename__,".csv*"))
    files__ <- c(files__,dir(pattern=paste0(filename__,".atr*")))
    if (FLAGdefine)
      files__ <- c(files__,dir(pattern=paste0(filename__,"_define.*")))
    if (FLAGxpt)
      files__ <- c(files__,dir(pattern=paste0(filename__,".xpt*")))
    utils::zip(paste0(filename__,".dat.zip"),files__)
    unlink(files__)
    setwd(oldpath__)
  }
}
#'@export
exportDEFINE_IQRdataGENERAL <- function(data,
                                        datasetName=NULL,
                                        datasetLocation=NULL,
                                        datasetDescription="Analysis dataset",
                                        addColLabels=NULL,
                                        filename=NULL) {
  exportDEFINEdocx_IQRdataGENERAL(data,
                                  datasetName,
                                  datasetLocation,
                                  datasetDescription,
                                  addColLabels,
                                  filename)
}
exportDEFINEdocx_IQRdataGENERAL <- function(data,
                                            datasetName=NULL,
                                            datasetLocation=NULL,
                                            datasetDescription="Analysis dataset",
                                            addColLabels=NULL,
                                            filename=NULL) {
  if (!is_IQRdataGENERAL(data) & !("IQRdataNLME" %in% attr(data,"class")))
    stopIQR("input data is not an IQRdataGENERAL object")
  if (is.null(datasetName))
    stopIQR("Please provide a name for the dataset as it should appear in the define file")
  if (is.null(datasetLocation))
    warningIQR("Please provide a location (path) for the dataset as it should appear in the define file")
  dataFirst__ <- data.frame(NAME=datasetName,DESCRIPTION=datasetDescription,LOCATION=datasetLocation,stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__ <- data.frame(NAME=names(data),stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__$TYPE <-lapply(data, function (x) { if (is.numeric(x)) { out <- "Numeric" } else { out <- "String" }; out })
  data <- addLabel_IQRdataGENERAL(data,addColLabels)
  dataDefine__$LABEL <-lapply(data, function (x) {
    label__ <- attr(x,"label")
    if (is.null(label__)) label__ <- "UNKNOWN"
    label__
  })
  dataDefine__$VALUES <- getValueTxtDefine(dataDefine__$NAME,unlabel_dataframe(data))
  dataDefine__$COMMENTS <- " "
  datasetName__ <- toupper(datasetName)
  RMDTEXT__ <- rmdEMPTY()
  RMDTEXT__ <- RMDTEXT__ + rmdTITLE(title=paste0("Define file for the \"",datasetName__,"\" dataset"),subtitle=NULL,date=NULL)
  RMDTEXT__ <- RMDTEXT__ + rmdNOINTRO()
  RMDTEXT__ <- RMDTEXT__ + "!BLOCKSTART[keepNext](block_kwnext)\n"
  RMDTEXT__ <- RMDTEXT__ + rmdSECTION("Dataset name, description, and location",numbered=FALSE)
  RMDTEXT__ <- RMDTEXT__ + rmdTABLEDF(dataFirst__,label="overview",fontsize=8,caption="Dataset information",ignoreCaption=TRUE)
  RMDTEXT__ <- RMDTEXT__ + rmdNEWPAGE()
  RMDTEXT__ <- RMDTEXT__ + rmdSECTION(paste0(datasetName__," specification"),numbered=FALSE)
  RMDTEXT__ <- RMDTEXT__ + "Missing values are coded in the dataset as '.' and referenced in this specification as 'NA'.\n\n"
  RMDTEXT__ <- RMDTEXT__ + rmdTABLEDF(dataDefine__,label="overview",fontsize=8,caption="Definition of dataset contents",ignoreCaption=TRUE)
  RMDTEXT__ <- RMDTEXT__ + "\n"
  RMDTEXT__ <- RMDTEXT__ + "!BLOCKEND(block_kwnext)\n"
  if (is.null(filename)) {
    filename__ <- paste0(datasetName__,"_define",".rmd")
  } else {
    x <- aux_fileparts(filename)
    filename__ <- paste0(x$pathname,"/",x$filename,".rmd")
  }
  export_IQRrmd(RMDTEXT__,filename__)
  if (has_IQReport_executable()) {
    IQReport(filename__)
  }
}
#'@export
exportDEFINEpdf_IQRdataGENERAL <- function(data,
                                           datasetName=NULL,
                                           datasetLocation=NULL,
                                           datasetDescription="Analysis dataset",
                                           addColLabels=NULL,
                                           filename=NULL) {
  if (!is_IQRdataGENERAL(data) & !("IQRdataNLME" %in% attr(data,"class")))
    stopIQR("input data is not an IQRdataGENERAL object")
  if (is.null(datasetName))
    stopIQR("Please provide a name for the dataset as it should appear in the define file")
  if (is.null(datasetLocation))
    warningIQR("Please provide a location (path) for the dataset as it should appear in the define file")
  splitRows <- function(data, skip = 5) {
    data <- split(data, seq_along(data[[1]]))
    data <- do.call(rbind, lapply(data, function(d) {
      nr <- max(unlist(lapply(d, function(x) length(strsplit(as.character(x), "\n")[[1]]))))
      if (nr > skip) {
        d <- as.data.frame(lapply(d, function(x) {
          x <- strsplit(as.character(x), "\n")[[1]][1:nr]
          x[is.na(x)] <- ""
          x <- c(paste(x[1:skip], collapse = "\n"), x[-(1:skip)])
          return(x)
        }), stringsAsFactors = FALSE)
      }
      return(d)
    }))
    rownames(data) <- NULL
    return(data)
  }
  defineFile_makeTable_Description <- function(NAME, DESCRIPTION, LOCATION, width = 16) {
    table <- kableExtra::kable(data.frame(Name = NAME, Description = DESCRIPTION, Location = LOCATION), format = "latex",
                               longtable = TRUE, booktabs = TRUE)
    table <- kableExtra::kable_styling(table, latex_options = c("repeat_header"), font_size = 8)
    table <- kableExtra::column_spec(table, 1, width = paste0(width*3/17, "cm"))
    table <- kableExtra::column_spec(table, 2, width = paste0(width*4/17, "cm"))
    table <- kableExtra::column_spec(table, 3, width = paste0(width*9/17, "cm"))
    table <- kableExtra::row_spec(table, 0, bold = TRUE)
    return(table)
  }
  defineFile_makeTable_Dataspec <- function(NAME, TYPE, LABEL, VALUES, COMMENTS, width = 16) {
    mydata <- data.frame(Name = unlist(NAME), Type = unlist(TYPE), Label = unlist(LABEL),
                         Values = gsub(":::", " ", unlist(VALUES)),
                         Comments = unlist(COMMENTS),
                         row.names = NULL)
    mydata <- splitRows(mydata)
    table <- kableExtra::kable(mydata, format = "latex",
                               longtable = TRUE, booktabs = TRUE)
    table <- kableExtra::kable_styling(table, latex_options = c("repeat_header"), font_size = 8)
    table <- kableExtra::column_spec(table, 1, width = paste0(width*2/18, "cm"))
    table <- kableExtra::column_spec(table, 2, width = paste0(width*2/18, "cm"))
    table <- kableExtra::column_spec(table, 3, width = paste0(width*2/18, "cm"))
    table <- kableExtra::column_spec(table, 4, width = paste0(width*8/18, "cm"))
    table <- kableExtra::column_spec(table, 5, width = paste0(width*2/18, "cm"))
    table <- kableExtra::row_spec(table, 0, bold = TRUE)
    return(table)
  }
  dataFirst__ <- data.frame(NAME=datasetName,DESCRIPTION=datasetDescription,LOCATION=datasetLocation,stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__ <- data.frame(NAME=names(data),stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__$TYPE <-lapply(data, function (x) { if (is.numeric(x)) { out <- "Numeric" } else { out <- "String" }; out })
  data <- addLabel_IQRdataGENERAL(data,addColLabels)
  dataDefine__$LABEL <-lapply(data, function (x) {
    label__ <- attr(x,"label")
    if (is.null(label__)) label__ <- "UNKNOWN"
    label__
  })
  dataDefine__$VALUES <- getValueTxtDefine(dataDefine__$NAME,unlabel_dataframe(data))
  dataDefine__$COMMENTS <- " "
  table1__ <- defineFile_makeTable_Description(datasetName, datasetDescription, datasetLocation)
  table2__ <- with(dataDefine__, defineFile_makeTable_Dataspec(NAME, TYPE, LABEL, VALUES, COMMENTS))
  datasetName__ <- toupper(datasetName)
  templatefile__ <- template <- system.file(package="IQRtools", file.path("templates", "definefile.tex"))
  con__ <- file(templatefile__)
  txt__ <- readLines(con__)
  close.connection(con__)
  txt__ <- lapply(txt__, function(mytxt__) {
    mytxt__ <- sub("<dataname>", datasetName, mytxt__)   
    if (mytxt__ == "<table1>") return(table1__)          
    if (mytxt__ == "<table2>") return(table2__)          
    return(mytxt__)
  })
  if (is.null(filename)) {
    filename.tex__ <- paste0(datasetName__,"_define",".tex")
  } else {
    x <- aux_fileparts(filename)
    filename.tex__ <- paste0(x$pathname,"/", x$filename,".tex")
  }
  con__ <- file(filename.tex__)
  writeLines(text = unlist(txt__), con = con__)
  close.connection(con__)
  msg__ <- try(tinytex::pdflatex(filename.tex__), silent = TRUE)
  return(invisible(NULL))
}
exportXPT_IQRdataGENERAL <- function(data,filename=NULL,addColLabels=NULL) {
  if (is.null(filename))
    stopIQR("filename must be provided")
  if (!is_IQRdataGENERAL(data) & !("IQRdataNLME" %in% attr(data,"class")))
    stopIQR("input data is not an IQRdataGENERAL object")
  x <- aux_fileparts(filename)
  filename <- x$filename
  pathname <- x$pathname
  if (nchar(filename) > 8)
    stopIQR("please provide a filename with length of max 8 characters (w/o extension)")
  dataX <- data.frame(lapply(names(data), function (x) {
    x <- data[[x]]
    if (!is.null(levels(x))) {
      x <- levels(x)[x]
    }
    if (is.logical(x)) x <- as.double(x)
    if (is.integer(x)) x <- as.double(x)
    x
  }),row.names=NULL,stringsAsFactors=FALSE)
  names(dataX) <- names(data)
  attr(dataX,"methodBLLOQ") <- attr(data,"methodBLLOQ")
  attr(dataX,"doseNAMES") <- attr(data,"doseNAMES")
  attr(dataX,"obsNAMES") <- attr(data,"obsNAMES")
  attr(dataX,"aeNAMES")     <- attr(data,"aeNAMES")
  attr(dataX,"covInfo") <- attr(data,"covInfo")
  attr(dataX,"catInfo") <- attr(data,"catInfo")
  attr(dataX,"class") <- c("IQRdataNLME","data.frame")
  attr(dataX,"imputeInfo") <- attr(data,"imputeInfo")
  data <- dataX
  data <- addLabel_IQRdataGENERAL(data,addColLabels)
  aux_mkdir(pathname)
  haven::write_xpt(data = data,path = paste0(pathname,'/',filename,'.xpt'),name = filename,version = 5)
  genComplianceLog(paste0(pathname,'/',filename,'.xpt'))
}
#'@export
clean_IQRdataGENERAL <- function(data,
                                 methodBLLOQ="M1",
                                 records=NULL,
                                 subjects=NULL,
                                 FLAGrmPlacebo=FALSE,
                                 FLAGrmIGNOREDrecords=FALSE,
                                 FLAGrmDosePostLastObs=FALSE,
                                 continuousCovs=NULL,
                                 categoricalCovs=NULL,
                                 pathname=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  filename <- NULL
  dataNew <- handleSameTimeObs_IQRdataGENERAL(data)
  if (!is.null(methodBLLOQ)) dataNew <- blloq_IQRdataGENERAL(dataNew,methodBLLOQ=methodBLLOQ)
  if (!is.null(pathname)) filename <- paste0(pathname,"/01_Manually_Selected_Records.txt")
  dataNew <- setIGNORErecords_IQRdataGENERAL(dataNew,records=records,filename=filename)
  if (!is.null(pathname)) filename <- paste0(pathname,"/02_Missing_TIME_Observation_Records.txt")
  dataNew <- rmMissingTIMEobsRecords_IQRdataGENERAL(dataNew,filename=filename)
  if (!is.null(pathname)) filename <- paste0(pathname,"/03_Missing_DV_Observation_Records.txt")
  dataNew <- setMissingDVobsRecordsIGNORE_IQRdataGENERAL(dataNew,filename=filename)
  if (!is.null(pathname)) filename <- paste0(pathname,"/04_Manually_Selected_Subjects.txt")
  dataNew <- rmSubjects_IQRdataGENERAL(dataNew,subjects=subjects,filename=filename)
  if (!is.null(pathname)) filename <- paste0(pathname,"/05_Non_Dose_Observation_Records.txt")
  dataNew <- rmNonTask_IQRdataGENERAL(dataNew,filename=filename)
  if (FLAGrmPlacebo) {
    if (!is.null(pathname)) filename <- paste0(pathname,"/06_Placebo_Subjects.txt")
    dataNew <- rmPLACEBO_IQRdataGENERAL(dataNew,filename=filename)
  }
  if (!is.null(pathname)) filename <- paste0(pathname,"/07_No_Observations_Subjects.txt")
  dataNew <- rmNOobsSUB_IQRdataGENERAL(dataNew,filename=filename)
  if (!is.null(pathname)) filename <- paste0(pathname,"/08_Zero_Amount_Dose_Records.txt")
  dataNew <- rmAMT0_IQRdataGENERAL(dataNew,filename=filename)
  if (FLAGrmIGNOREDrecords) {
    if (!is.null(pathname)) filename <- paste0(pathname,"/09_IGNORED_records.txt")
    dataNew <- rmIGNOREd_IQRdataGENERAL(dataNew,filename=filename)
  }
  if (!is.null(pathname)) filename <- paste0(pathname,"/10_Covariate_Imputations.txt")
  dataNew <- covImpute_IQRdataGENERAL(dataNew,continuousCovs=continuousCovs,categoricalCovs=categoricalCovs,filename=filename)
  if (FLAGrmDosePostLastObs) {
    if (!is.null(pathname)) filename <- paste0(pathname,"/11_Doses_Post_Last_Observation.txt")
    dataNew <- rmDosePostLastObs_IQRdataGENERAL(dataNew,filename=filename)
  }
  return(dataNew)
}
#'@export
covImpute_IQRdataGENERAL <- function(data,continuousCovs=NULL,categoricalCovs=NULL, filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  covColNames <- c()
  covColImputationValues <- c()
  if (length(continuousCovs)>0) {
    if (!is.null(names(continuousCovs))) {
      covColNames <- c(covColNames, names(continuousCovs))
      covColImputationValues <- c(covColImputationValues, unname(continuousCovs))
    } else {
      covColNames <- c(covColNames, continuousCovs)
      covColImputationValues <- c(covColImputationValues, rep("median",length(continuousCovs)))
    }
  }
  if (length(categoricalCovs)>0) {
    if (is.null(names(categoricalCovs))) {
      stopIQR("categoricalCovs needs to be a named vector")
    } else {
      covColNames <- c(covColNames, names(categoricalCovs))
      covColImputationValues <- c(covColImputationValues, unname(categoricalCovs))
    }
  }
  imputeInfo <- data.frame(COVNAME=covColNames, IMPUTATION=covColImputationValues,stringsAsFactors=FALSE)
  if (length(unique(imputeInfo$COVNAME)) != length(imputeInfo$COVNAME))
    stopIQR("Imputation rule defined more than once for same covariate")
  notInData <- imputeInfo$COVNAME[which(!(imputeInfo$COVNAME %in% names(data)))]
  if (length(notInData)>0)
    stopIQR(paste0("The following covariate column is not in the data:\n    ",paste0(notInData,collapse=", ")))
  nonNumericDefCols <- imputeInfo$COVNAME[is.na(suppressWarnings(as.numeric(imputeInfo$IMPUTATION)))]
  indivValues <- data.frame(do.call(rbind,lapply(split(data,data$USUBJID), function (x) {
    sapply(nonNumericDefCols, function (y) {
      x[[y]][1]
    })
  })),stringsAsFactors=FALSE,row.names=NULL)
  imputationValuesNN <- sapply(nonNumericDefCols, function (x) {
    fct   <- imputeInfo$IMPUTATION[imputeInfo$COVNAME==x]
    expr  <- paste0(fct,"(indivValues$",x,",na.rm=TRUE)")
    eval(parse(text=expr))
  })
  imputeInfo$VALUES <- suppressWarnings(as.numeric(imputeInfo$IMPUTATION))
  for (k__ in seq_along(imputationValuesNN)) {
    imputeInfo$VALUES[imputeInfo$COVNAME==names(imputationValuesNN)[k__]] <- signif(imputationValuesNN[k__],5)
  }
  xtable <- data.frame()
  for (k__ in seq_along(imputeInfo$COVNAME)) {
    ixNA <- which(is.na(data[[imputeInfo$COVNAME[k__]]]))
    data[[imputeInfo$COVNAME[k__]]][ixNA] <- imputeInfo$VALUES[k__]
    if (length(ixNA)>0) {
      N          <- length(unique(data$USUBJID[ixNA]))
      COVNAME    <- c(imputeInfo$COVNAME[k__], rep(" ",N))
      IMPUTATION <- c(imputeInfo$IMPUTATION[k__], rep(" ",N))
      VALUES     <- c(imputeInfo$VALUES[k__], rep(" ",N))
      N.IMPUTED  <- c(N, rep(" ",N))
      PERCENT.IMPUTED <- c(paste0(signif(100*N/length(unique(data$USUBJID)),4),"%"), rep(" ",N))
      USUBJID    <- c(unique(data$USUBJID[ixNA]), " ")
      xtableadd__  <- data.frame(COVNAME=COVNAME,
                                 IMPUTATION=IMPUTATION,
                                 VALUES=VALUES,
                                 N.IMPUTED=N.IMPUTED,
                                 PERCENT.IMPUTED=PERCENT.IMPUTED,
                                 USUBJID=USUBJID)
      xtable     <- rbind(xtable, xtableadd__)
    }
  }
  xtitle <- sprintf("Protocol of covariate imputations (N_Total_Subjects=%d)",length(unique(data$USUBJID)))
  if (nrow(xtable)==0) xtable <- data.frame(COVNAME="-",IMPUTATION="-",VALUES="-",N.IMPUTED=0,PERCENT.IMPUTED=0,USUBJID="-")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable,xtitle=xtitle,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable,xtitle=xtitle,report=FALSE))
  }
  attr(data,"imputeInfo") <- imputeInfo
  return(data)
}
#'@export
getNAcolNLME_IQRdataGENERAL <- function(data) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  globalColNames <- getColumnsInfo_IQRdataGENERAL()$namesOrder
  nlmeColNames <- setdiff(names(data),globalColNames)
  colContainingNA__ <- unlist(lapply(nlmeColNames, function (x) {
    col <- data[,x]
    if (length(which(is.na(col))) > 0) {
      outx__ <- x
    } else {
      outx__ <- NULL
    }
    outx__
  }))
  colContainingNA__ <- colContainingNA__[!grepl("^TADD",colContainingNA__)]
  return(colContainingNA__)
}
#'@export
rmSubjects_IQRdataGENERAL <- function(data,subjects,filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  removeTable <- do.call(rbind,lapply(seq_along(subjects), function (k) {
    data.frame(USUBJID=subjects[[k]],REMOVE.REASON=names(subjects)[k],stringsAsFactors=FALSE,row.names=NULL)
  }))
  if (!is.null(removeTable)) removeTable <- dplyr::arrange(removeTable,USUBJID)
  ixnotpresent__ <- removeTable$USUBJID[!(removeTable$USUBJID %in% data$USUBJID)]
  if (length(ixnotpresent__) > 0)
    stopIQR(paste0("The following USUBJIDs are not present in the dataset:\n    ",paste0(ixnotpresent__,collapse=" ")))
  output <- data[!(data$USUBJID %in% removeTable$USUBJID),]
  if (!is.null(removeTable)) {
    xtable <- dplyr::arrange(removeTable,USUBJID)
  } else {
    xtable <- data.frame(USUBJID="-",REMOVE.REASON="No subjects removed")
  }
  Nremoved__ <- nrow(removeTable)
  if (is.null(Nremoved__)) Nremoved__ = 0
  xtitle <- paste0("N=",Nremoved__," subjects removed from the data")
  xfooter <- paste0("Rule for removal: user defined input")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,report=FALSE))
  }
  return(output)
}
#'@export
setIGNORErecords_IQRdataGENERAL <- function(data,records,filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  ignoreTable__ <- do.call(rbind,lapply(seq_along(records), function (k) {
    if (length(records[[k]])==0) {
      stopIQR(paste0("No entries in list entry ",names(records)[k]," of the ignored records list"))
    }
    data.frame(IXGDF=records[[k]],IGNORE.REASON=names(records)[k],stringsAsFactors=FALSE,row.names=NULL)
  }))
  ix__ <- ignoreTable__$IXGDF[duplicated(ignoreTable__$IXGDF)]
  if (length(ix__) > 0) {
    message("Concatenating multiple reasons for record(s): ", paste0(ix__, collapse = ","))
    ignoreTable__ <- plyr::ddply(ignoreTable__, ~IXGDF, function(y__) {
      out__ <- y__[1,]
      out__$IGNORE.REASON <- paste0(y__$IGNORE.REASON, collapse = ";")
      out__
    })
  }
  if (!is.null(ignoreTable__)) ignoreTable__ <- dplyr::arrange(ignoreTable__,IXGDF)
  ixnotpresent__ <- ignoreTable__$IXGDF[!(ignoreTable__$IXGDF %in% data$IXGDF)]
  if (length(ixnotpresent__) > 0)
    stopIQR(paste0("The following IXGDFs are not present in the dataset:\n    ",paste0(ixnotpresent__,collapse=" ")))
  ixdose__ <- unlist(sapply(ignoreTable__$IXGDF, function (x__) {
    if (data$EVID[data$IXGDF==x__] == 1) return(x__)
    return(NULL)
  }))
  if (length(ixdose__) > 0)
    stopIQR(paste0("The following defined IXGDF values contain dose records:\n    ",paste0(ixdose__,collapse=" ")))
  ixNonObs__ <- ignoreTable__$IXGDF[data$YTYPE[data$IXGDF %in% ignoreTable__$IXGDF] == 0]
  if (length(which(data$YTYPE[data$IXGDF %in% ignoreTable__$IXGDF] == 0)) > 0)
    warningIQR(paste0("setIGNORErecords_IQRdataGENERAL: The following defined IXGDF values contain non-observation records (EVID=0 and YTYPE=0):\n    ",paste0(ixNonObs__,collapse=" ")))
  data$MDV[data$IXGDF %in% ignoreTable__$IXGDF] <- 1
  dummy__ <- sapply(ignoreTable__$IXGDF, function(x__) {
    data$IGNORE[data$IXGDF==x__] <<- ignoreTable__$IGNORE.REASON[ignoreTable__$IXGDF==x__]
  })
  xtable <- data.frame(data[data$IXGDF %in% ignoreTable__$IXGDF,c("IXGDF","USUBJID","NAME","TIME","TAD","EVID","DV","MDV","IGNORE")],stringsAsFactors=FALSE)
  if (nrow(xtable)==0) xtable <- data.frame(IXGDF="-",USUBJID="-",NAME="-",TIME="-",TAD="-",EVID="-",DV="-",MDV="-",IGNORE="-")
  xtable <- dplyr::arrange(xtable,IXGDF)
  Nremoved__ <- nrow(ignoreTable__)
  if (is.null(Nremoved__)) Nremoved__ = 0
  xtitle <- paste0("N=",Nremoved__," observation records have been set to MDV=1 and an IGNORE reason was set")
  xfooter <- paste0("Rule for IGNORE: user defined input")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,report=FALSE))
  }
  return(data)
}
#'@export
setMissingDVobsRecordsIGNORE_IQRdataGENERAL <- function(data,filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  output__ <- data
  ix_ignore__ <- output__$EVID==0 & is.na(output__$DV)
  output__$MDV[ix_ignore__] <- 1
  output__$IGNORE[ix_ignore__ & is.na(output__$IGNORE)] <- "Missing value"
  dataIGNORE__ <- as.data.frame(output__[ix_ignore__,])
  xtable__ <- dataIGNORE__[,c("IXGDF","USUBJID","NAME","TIME","DV","MDV","IGNORE")]
  if (nrow(xtable__)==0) xtable__ <- data.frame(IXGDF="-",USUBJID="-",NAME="-",TIME="-",DV="-",MDV="-",IGNORE="-")
  xtable__ <- dplyr::arrange(xtable__,IXGDF)
  xtitle__ <- paste0("N=",nrow(dataIGNORE__),' observation records with missing DV have been set to ignore (MDV=1, IGNORE="Missing value")')
  xfooter__ <- paste0("Rule for ignoring: EVID==0 & is.na(DV)")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable__,xfooter=xfooter__,xtitle=xtitle__,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable__,xfooter=xfooter__,xtitle=xtitle__,report=FALSE))
  }
  return(output__)
}
#'@export
rmMissingTIMEobsRecords_IQRdataGENERAL <- function(data,filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  output__ <- data
  ix_ignore__ <- output__$EVID==0 & is.na(output__$TIME)
  dataIGNORE__ <- as.data.frame(output__[ix_ignore__,])
  output__ <- output__[!ix_ignore__,]
  xtable__ <- dataIGNORE__[,c("IXGDF","USUBJID","NAME","TIME","DV")]
  if (nrow(xtable__)==0) xtable__ <- data.frame(IXGDF="-",USUBJID="-",NAME="-",TIME="-",DV="-")
  xtable__ <- dplyr::arrange(xtable__,IXGDF)
  xtitle__ <- paste0("N=",nrow(dataIGNORE__),' observation records with missing TIME have been removed from the dataset.')
  xfooter__ <- paste0("Rule for removal: EVID==0 & is.na(TIME)")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable__,xfooter=xfooter__,xtitle=xtitle__,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable__,xfooter=xfooter__,xtitle=xtitle__,report=FALSE))
  }
  return(output__)
}
#'@export
handleSameTimeObs_IQRdataGENERAL <- function(data, timestep = 0.0001) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  out <- do.call(rbind,lapply(split(data,data$USUBJID), function (x) {
    outx__ <- do.call(rbind,lapply(split(x,x$NAME), function (y) {
      if (y$EVID[1]==0) {
        yTIMEna__ <- y[is.na(y$TIME),]
        yTIME__ <- y[!is.na(y$TIME),]
        if (length(unique(yTIME__$TIME)) != length(yTIME__$TIME)) {
          outy__ <- do.call(rbind,lapply(split(yTIME__,yTIME__$TIME), function (z) {
            if (length(z$TIME)>1) {
              delta     <- seq(0,by=timestep,length.out=length(z$TIME))
              z$TIME    <- z$TIME+delta
              z$TIMEPOS <- z$TIMEPOS+delta
              z$TAD     <- z$TAD+delta
            }
            z
          }))
          outy__ <- rbind(outy__,yTIMEna__)
        } else {
          outy__ <- y
        }
      } else {
        outy__ <- y
      }
      outy__
    }))
    outx__
  }))
  if ("STUDY" %in% names(out)) {
    if ("TYPENAME" %in% names(out)) {
      output <- dplyr::arrange(out,STUDY,USUBJID,TIME,TYPENAME,NAME)
    } else {
      output <- dplyr::arrange(out,STUDY,USUBJID,TIME,NAME)
    }
  } else {
    if ("TYPENAME" %in% names(out)) {
      output <- dplyr::arrange(out,USUBJID,TIME,TYPENAME,NAME)
    } else {
      output <- dplyr::arrange(out,USUBJID,TIME,NAME)
    }
  }
  attributes(output) <- attributes(data)
  return(output)
}
#'@export
rmAMT0_IQRdataGENERAL <- function(data, filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  dataREMOVE__ <- as.data.frame(data[data$EVID==1 & data$AMT==0,])
  xtable <- dataREMOVE__[,c("IXGDF","USUBJID","NAME","TIME","EVID","AMT")]
  if (nrow(xtable)==0) xtable <- data.frame(IXGDF="-",USUBJID="-",NAME="-",TIME="-",EVID="-",AMT="-")
  xtable <- dplyr::arrange(xtable,IXGDF)
  xtitle <- paste0("N=",nrow(dataREMOVE__)," dose records with AMT=0 have been removed")
  xfooter <- paste0("Rule for removal: EVID==1 & AMT=0")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,report=FALSE))
  }
  output <- data[!(data$EVID==1 & data$AMT==0),]
  return(output)
}
#'@export
rmNOobsSUB_IQRdataGENERAL <- function(data, filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  noObsSubject__ <- noObsIndiv_IQRdataGENERAL(data)
  if (length(noObsSubject__)>0) {
    xtable <- data.frame(USUBJID=noObsSubject__,REMOVAL.REASON="No or no MDV=0 observations")
  } else {
    xtable <- data.frame(USUBJID="-",REMOVAL.REASON="-")
  }
  xtable <- dplyr::arrange(xtable,USUBJID)
  xtitle <- paste0("N=",length(noObsSubject__)," subjects without MDV=0 observations have been removed")
  xfooter <- paste0("Rule for removal: No observations in subject. Observations defined by EVID=0 & MDV=0 & YTYYPE>0")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,report=FALSE))
  }
  output <- data[!(data$USUBJID %in% noObsSubject__),]
  return(output)
}
#'@export
rmPLACEBO_IQRdataGENERAL <- function(data, filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  placeboSubject__ <- placeboIndiv_IQRdataGENERAL(data)
  if (length(placeboSubject__)>0) {
    xtable <- data.frame(USUBJID=placeboSubject__,REMOVAL.REASON="Placebo Subject")
  } else {
    xtable <- data.frame(USUBJID="-",REMOVAL.REASON="-")
  }
  xtable <- dplyr::arrange(xtable,USUBJID)
  xtitle <- paste0("N=",length(placeboSubject__)," placebo subjects have been removed")
  xfooter <- paste0("Rule for removal: subject has no dose events or all dose events have AMT=0")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,report=FALSE))
  }
  output <- data[!(data$USUBJID %in% placeboSubject__),]
  return(output)
}
#'@export
rmNonTask_IQRdataGENERAL <- function(data, filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("Input argument is not an IQRdataGENERAL object")
  validRecordNAMEs__ <- unique(data$NAME[data$EVID==1 | data$EVID==0 & data$YTYPE>0])
  dataREMOVE__ <- as.data.frame(data[!(data$NAME %in% validRecordNAMEs__),])
  xtable <- dataREMOVE__[,c("IXGDF","USUBJID","NAME","TIME","VALUE","EVID","YTYPE")]
  if (nrow(xtable)==0) xtable <- data.frame(IXGDF="-",USUBJID="-",NAME="-",TIME="-",VALUE="-",EVID="-",YTYPE="-")
  xtable <- dplyr::arrange(xtable,IXGDF)
  xtitle <- paste0("N=",nrow(dataREMOVE__)," non-dose and non-observation records have been removed")
  xfooter <- paste0("Rule for removal: !(EVID==1 | EVID==0 & YTYPE>0)")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,report=FALSE))
  }
  output <- data[data$NAME %in% validRecordNAMEs__,]
  return(output)
}
#'@export
rmIGNOREd_IQRdataGENERAL <- function(data, filename=NULL) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("Input argument is not an IQRdataGENERAL object")
  if (length(which(data$MDV[!is.na(data$IGNORE)]==0)) > 0)
    stopIQR("Not all records with a non empty IGNORE entry are set to MDV=1")
  dataIGNORE__ <- as.data.frame(data[!is.na(data$IGNORE),])
  xtable <- dataIGNORE__[,c("IXGDF","USUBJID","NAME","TIME","VALUE","IGNORE")]
  if (nrow(xtable)==0) xtable <- data.frame(IXGDF="-",USUBJID="-",NAME="-",TIME="-",IGNORE="-")
  xtable <- dplyr::arrange(xtable,IXGDF)
  xtitle <- paste0("N=",nrow(dataIGNORE__)," IGNOREd records have been removed")
  xfooter <- paste0("Rule for removal: entry in IGNORE column is NOT NA")
  if (!is.null(filename)) {
    IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,filename=filename,report=TRUE)
  } else {
    print(IQRoutputTable(xtable=xtable,xfooter=xfooter,xtitle=xtitle,report=FALSE))
  }
  output <- data[is.na(data$IGNORE),]
  return(output)
}
getColumnsInfo_IQRdataGENERAL <- function () {
  list(
    numericColumns  = c("IXGDF","IND","STUDYN","TRT","TRTR","NT","TIME","PROFTIME","DURATION","VALUE","ULOQ","LLOQ","II","ADDL","AESER","AEDRGREL","OCC"),
    requiredColumns = c("USUBJID", "TIMEUNIT", "TIME", "NAME", "VALUE", "UNIT", "ROUTE"),
    namesOrder      = c("IXGDF","IGNORE","USUBJID","CENTER","SUBJECT","INDNAME","IND","COMPOUND",
                        "STUDY","STUDYN","STUDYDES","PART","EXTENS","TRTNAME","TRT","TRTNAMER","TRTR",
                        "VISIT","VISNAME","BASE","SCREEN","STDTC","ENDTC","TIMEUNIT","DATEDAY","DATETIME","NT","TIME","PROFNR","PROFTIME","DURATION",
                        "TYPENAME","NAME","VALUE","VALUETXT","UNIT","ULOQ","LLOQ","ROUTE","II","ADDL","OCC",
                        "AE","AEGRADE","AESER","AEDRGREL","COMMENT"),
    requiredColumnsNLME = c("ID","TIMEPOS","EVID","MDV","CENS","AMT","DV",
                            "TINF","RATE","ADM","YTYPE","TAD","DOSE")
  )
}
#'@export
IQRdataGENERAL <- function(input,
                           doseNAMES=NULL,
                           obsNAMES=NULL,
                           cov0=NULL,
                           covT=NULL,
                           cat0=NULL,
                           catT=NULL,
                           covInfoAdd=NULL,
                           catInfoAdd=NULL,
                           methodBLLOQ="M1",
                           FLAGforceOverwriteNLMEcols=TRUE,
                           FLAGtaskEventsOnly=TRUE,
                           FLAGnoNAlocf=FALSE) {
  if (!is.null(covInfoAdd)) {
    required__ <- c("COLNAME","NAME","UNIT","TIME.VARYING")
    if (length(setdiff(required__,names(covInfoAdd))) > 0) {
      stopIQR("Names in covInfoAdd list do not match the requird ones: COLNAME,NAME,UNIT,TIME.VARYING")
    }
  }
  if (!is.null(catInfoAdd)) {
    required__ <- c("COLNAME","NAME","UNIT","VALUETXT","VALUES","TIME.VARYING")
    if (length(setdiff(required__,names(catInfoAdd))) > 0) {
      stopIQR("Names in catInfoAdd list do not match the requird ones: COLNAME,NAME,UNIT,VALUETXT,VALUES,TIME.VARYING")
    }
    for (icol__ in catInfoAdd$COLNAME) {
      tmpVal__ <- aux_explode(catInfoAdd$VALUES[catInfoAdd$COLNAME == icol__])
      tmpValTxt__ <- aux_explode(catInfoAdd$VALUETXT[catInfoAdd$COLNAME == icol__])
      if (length(tmpVal__) != length(tmpValTxt__))
        stopIQR(paste0("Number of numerical values and text values given in catInfoAdd does not match for ", icol__))
    }
  }
  if (length(cov0)==0) cov0 <- NULL
  if (length(covT)==0) covT <- NULL
  if (length(cat0)==0) cat0 <- NULL
  if (length(catT)==0) catT <- NULL
  covNAMEScheck <- names(c(cov0,covT,cat0,catT))
  if (length(unique(covNAMEScheck)) != length(covNAMEScheck))
    stopIQR("Defined covariate column names contain same name more than once")
  if (is.character(input)) {
    if(!file.exists(input))
      stopIQR("Trouble loading the data file. Please check if provided datafile argument is correct")
    data <- data.table::fread(file = input,na.strings=c(".","","NA","NaN"), strip.white = TRUE)
  } else {
    if (is.data.frame(input)) {
      data <- input
    } else {
      stopIQR("datafile argument needs to be a path to a csv file or a data.frame object")
    }
  }
  data <- data.frame(
    lapply(data, function (col) {
      if (is.character(col) || is.factor(col)){
        if (any(grepl(",",col, fixed = TRUE))) {
          warningIQR("Some entries in the dataset contain commata ',', which are replaced by ' '.")
          col <- gsub(","," ",col)
        }
      }
      col
    }),stringsAsFactors = FALSE
  )
  requiredColumns__ <- getColumnsInfo_IQRdataGENERAL()$requiredColumns
  missingColumns__ <- setdiff(requiredColumns__,names(data))
  if (length(missingColumns__) > 0)
    stopIQR(paste0("Missing required columns in the dataset:\n    ",paste0(missingColumns__,collapse="\n    ")))
  if ("DURATION" %in% names(data)) {
    if (any(is.na(data$DURATION))) stopIQR("DURATION column not allowed to contain NA entries")
  }
  if ("TINF" %in% names(data)) {
    if (any(is.na(data$TINF))) stopIQR("TINF column not allowed to contain NA entries")
  }
  if ("YTYPE" %in% names(data)) {
    if (any(is.na(data$YTYPE))) stopIQR("YTYPE column not allowed to contain NA entries. Dosing records have YTYPE = 0.")
  }
  if (is.null(doseNAMES)) {
    doseNAMES <- unique(data$NAME[!is.na(data$ROUTE)])
  }
  if (is.null(obsNAMES)) {
    obsNAMES <- unique(data$NAME[is.na(data$ROUTE)])
  }
  for (k__ in seq_along(doseNAMES)) {
    if (!(doseNAMES[k__] %in% data$NAME))
      stopIQR("Not all provided doseNAMES are present in the NAME column of the dataset")
  }
  for (k__ in seq_along(obsNAMES)) {
    if (!(obsNAMES[k__] %in% data$NAME))
      stopIQR("Not all provided obsNAMES are present in the NAME column of the dataset")
  }
  is.factor <- sapply(data, function(x) inherits(x, "factor"))
  if (any(is.factor))
    stopIQR("Data contains factor variables. Please use IQRloadCSVdata() to read data from file.")
  if (!is.null(catInfoAdd)) {
    if (any(sapply(data[,catInfoAdd$COLNAME, drop = FALSE], is.character))) {
      charcol__ <- sapply(data[,catInfoAdd$COLNAME, drop = FALSE], is.character)
      charcol__ <- names(charcol__)[charcol__]
      for (icol__ in charcol__) {
        tmpVal__ <- aux_explode(catInfoAdd$VALUES[catInfoAdd$COLNAME == icol__])
        tmpValTxt__ <- aux_explode(catInfoAdd$VALUETXT[catInfoAdd$COLNAME == icol__])
        if (!all(unique(stats::na.omit(data[[icol__]])) %in% tmpValTxt__))
          stopIQR("Given categorical covariate column contains text values for which no numerical value is defined.")
        data[[icol__]] <- as.numeric(as.character(factor(data[[icol__]], levels = tmpValTxt__, labels = tmpVal__)))
      }
    }
  }
  data <- generalDataHandleMissingColums(data)
  numericColumns__ <- getColumnsInfo_IQRdataGENERAL()$numericColumns
  for (k__ in seq_along(numericColumns__)) {
    if (numericColumns__[k__] %in% names(data))
      suppressWarnings(data[[numericColumns__[k__]]] <- as.numeric(data[[numericColumns__[k__]]]))
  }
  data$TIMEUNIT <- toupper(data$TIMEUNIT)
  data$ROUTE <- toupper(data$ROUTE)
  if ("STUDY" %in% names(data)) {
    if ("TYPENAME" %in% names(data)) {
      data <- dplyr::arrange(data,STUDY,USUBJID,TIME,TYPENAME,NAME)
    } else {
      data <- dplyr::arrange(data,STUDY,USUBJID,TIME,NAME)
    }
  } else {
    if ("TYPENAME" %in% names(data)) {
      data <- dplyr::arrange(data,USUBJID,TIME,TYPENAME,NAME)
    } else {
      data <- dplyr::arrange(data,USUBJID,TIME,NAME)
    }
  }
  data <- generalDataMapVALUETXT2VALUE(data)
  data <- generalData_IDcolumn(data,FLAGforceOverwriteNLMEcols)
  data <- generalData_TIMEPOScolumn(data,FLAGforceOverwriteNLMEcols)
  data <- generalData_EVIDcolumn(data,doseNAMES,FLAGforceOverwriteNLMEcols)
  data <- generalData_MDVcolumn(data,doseNAMES,FLAGforceOverwriteNLMEcols)
  data <- generalData_CENScolumn(data,FLAGforceOverwriteNLMEcols)
  data <- generalData_AMTcolumn(data,doseNAMES,FLAGforceOverwriteNLMEcols)
  data <- generalData_DVcolumn(data,doseNAMES,FLAGforceOverwriteNLMEcols)
  data <- generalData_TINFcolumn(data,doseNAMES,FLAGforceOverwriteNLMEcols)
  data <- generalData_RATEcolumn(data,doseNAMES,FLAGforceOverwriteNLMEcols)
  data <- generalData_ADMcolumn(data,doseNAMES,FLAGforceOverwriteNLMEcols)
  data <- generalData_TADcolumns(data,doseNAMES,FLAGforceOverwriteNLMEcols)
  data <- generalData_DOSEcolumns(data,doseNAMES,FLAGforceOverwriteNLMEcols)
  data <- generalData_YTYPEcolumn(data,obsNAMES,FLAGforceOverwriteNLMEcols)
  ixmissingDV__ <- which(data$EVID==0 & data$MDV==0 & is.na(data$DV))
  data$MDV[ixmissingDV__] <- 1
  data$IGNORE[ixmissingDV__] <- "Missing value"
  ixmissingTIME__ <- which(data$EVID==0 & data$MDV==0 & is.na(data$TIME))
  data$MDV[ixmissingTIME__] <- 1
  data$IGNORE[ixmissingTIME__] <- "Missing time"
  data <- generalDataTimeIndependentCovariates(data,cov0,cat0)
  data <- generalDataTimeDependentCovariates(data,covT,catT,FLAGnoNAlocf)
  namesOrderGeneral__ <- getColumnsInfo_IQRdataGENERAL()$namesOrder
  namesOrderGeneralPresent__ <- namesOrderGeneral__[namesOrderGeneral__ %in% names(data)]
  namesOrder__ <- c(namesOrderGeneralPresent__,getColumnsInfo_IQRdataGENERAL()$requiredColumnsNLME)
  dataOrdered__ <- data[,namesOrder__]
  if (!is.null(c(cov0,cat0))) dataOrdered__ <- cbind(dataOrdered__, data[,names(c(cov0,cat0)),drop=FALSE])
  if (!is.null(c(covT,catT))) dataOrdered__ <- cbind(dataOrdered__, data[,names(c(covT,catT)),drop=FALSE])
  addNames__ <- setdiff(names(data),names(dataOrdered__))
  dataOrdered__ <- cbind(dataOrdered__, data[,addNames__,drop=FALSE])
  output <- dataOrdered__
  rownames(output) <- NULL
  attr(output,"class")                 <- c("IQRdataGENERAL", attr(output,"class"))
  output <- blloq_IQRdataGENERAL(output,methodBLLOQ)
  covInfo <- data.frame(COLNAME=names(c(cov0,covT)),NAME=sapply(c(cov0,covT),function(y) y),stringsAsFactors=FALSE,row.names=NULL)
  catInfo <- data.frame(COLNAME=names(c(cat0,catT)),NAME=sapply(c(cat0,catT),function(y) y),stringsAsFactors=FALSE,row.names=NULL)
  if (nrow(covInfo) > 0) covInfo <- dplyr::left_join(covInfo,unique(output[,c("NAME","UNIT")]),by="NAME")
  if (nrow(catInfo) > 0) catInfo <- dplyr::left_join(catInfo,unique(output[,c("NAME","UNIT")]),by="NAME")
  value_text_Mapping <- mappingVALUETXT_IQRdataGENERAL(output)
  if (nrow(covInfo)==0) covInfo <- NULL
  if (nrow(catInfo)>0) {
    catMapInfo <- do.call(rbind,lapply(1:nrow(catInfo), function (k__) {
      name <- catInfo$NAME[k__]
      mapname <- value_text_Mapping[value_text_Mapping$NAME==name,c("VALUETXT","VALUE")]
      if (!is.null(mapname)) {
        if (nrow(mapname)>0) {
          valtext <- paste0(mapname$VALUETXT,collapse=",")
          values  <- paste0(mapname$VALUE,collapse=",")
        } else {
          values  <- paste0(sort(unique(data$VALUE[data$NAME==name])),collapse=",")
          valtext <- paste0(rep("UNKNOWN",length(unique(data$VALUE[data$NAME==name]))),collapse=",")
        }
      } else {
        values  <- paste0(sort(unique(data$VALUE[data$NAME==name])),collapse=",")
        valtext <- paste0(rep("UNKNOWN",length(unique(data$VALUE[data$NAME==name]))),collapse=",")
      }
      data.frame(NAME=name,VALUETXT=valtext,VALUES=values,row.names=NULL,stringsAsFactors=FALSE)
    }))
  } else {
    catMapInfo <- NULL
    catInfo <- NULL
  }
  if (!is.null(catInfo)) catInfo <- dplyr::left_join(catInfo,catMapInfo,by="NAME")
  if (!is.null(covInfo)) {
    covInfo$TIME.VARYING <- FALSE
    covInfo$TIME.VARYING[covInfo$COLNAME %in% names(covT)] <- TRUE
  }
  if (!is.null(catInfo)) {
    catInfo$TIME.VARYING <- FALSE
    catInfo$TIME.VARYING[catInfo$COLNAME %in% names(catT)] <- TRUE
  }
  if (is.null(catInfo)) catInfo <- list()
  if (all(c("STUDYN","STUDY") %in% names(output))) {
    if (length(unique(output$STUDY)) > 1) {
      if (!"STUDYN" %in% catInfoAdd$COLNAME) {
        catInfoAdd$COLNAME <- c(catInfoAdd$COLNAME, "STUDYN")
        catInfoAdd$NAME <- c(catInfoAdd$NAME, "Study")
        catInfoAdd$UNIT <- c(catInfoAdd$UNIT, "-")
        catInfoAdd$VALUETXT <- c(catInfoAdd$VALUETXT, paste0(unique(output$STUDY),collapse = ","))
        catInfoAdd$VALUES <- c(catInfoAdd$VALUES, paste0(unique(output$STUDYN),collapse = ","))
        catInfoAdd$TIME.VARYING <- c(catInfoAdd$TIME.VARYING, FALSE)
      }
    }
  }
  if (all(c("TRT","TRTNAME") %in% names(output))) {
    if (length(unique(output$TRTNAME)) > 1) {
      if (!"TRT" %in% catInfoAdd$COLNAME) {
        catInfoAdd$COLNAME <- c(catInfoAdd$COLNAME, "TRT")
        catInfoAdd$NAME <- c(catInfoAdd$NAME, "TRTNAME")
        catInfoAdd$UNIT <- c(catInfoAdd$UNIT, "-")
        catInfoAdd$VALUETXT <- c(catInfoAdd$VALUETXT, paste0(unique(output$TRTNAME),collapse = ","))
        catInfoAdd$VALUES <- c(catInfoAdd$VALUES, paste0(unique(output$TRT),collapse = ","))
        catInfoAdd$TIME.VARYING <- c(catInfoAdd$TIME.VARYING, FALSE)
      }
    }
  }
  covInfo <- rbind(covInfo, data.frame(covInfoAdd,stringsAsFactors=FALSE,row.names=NULL))
  catInfo <- rbind(catInfo, data.frame(catInfoAdd,stringsAsFactors=FALSE,row.names=NULL))
  if (nrow(covInfo)==0) covInfo <- NULL
  if (nrow(catInfo)==0) catInfo <- NULL
  if (length(unique(covInfo$COLNAME)) != length(covInfo$COLNAME))
    stopIQR("Same covariates defined within cov0, covT, or covInfoAdd. Other possible reason: a covariate is listed with different units.")
  if (length(unique(catInfo$COLNAME)) != length(catInfo$COLNAME))
    stopIQR("Same covariates defined within cat0, catT, or catInfoAdd. Other possible reason: a covariate is listed with different units.")
  if ("STUDY" %in% names(data)) {
    if ("TYPENAME" %in% names(data)) {
      data <- dplyr::arrange(data,STUDY,USUBJID,TIME,TYPENAME,NAME)
      message("Sorting by STUDY, USUBJID, TIME, TYPENAME, NAME")
    } else {
      data <- dplyr::arrange(data,STUDY,USUBJID,TIME,NAME)
      message("Sorting by STUDY, USUBJID, TIME, NAME")
    }
  } else {
    if ("TYPENAME" %in% names(data)) {
      data <- dplyr::arrange(data,USUBJID,TIME,TYPENAME,NAME)
      message("Sorting by USUBJID, TIME, TYPENAME, NAME")
    } else {
      data <- dplyr::arrange(data,USUBJID,TIME,NAME)
      message("Sorting by USUBJID, TIME, NAME")
    }
  }
  AEnames__ <- unique(data$NAME[data$AE==1])
  attr(output,"doseNAMES")    <- doseNAMES
  attr(output,"obsNAMES")     <- obsNAMES
  attr(output,"aeNAMES")      <- AEnames__ 
  attr(output,"covInfo")      <- covInfo
  attr(output,"catInfo")      <- catInfo
  if (FLAGtaskEventsOnly) {
    output <- output[output$NAME %in% c(doseNAMES,obsNAMES),]
  }
  check_IQRdataGENERAL(output)
  data.table::setDF(output)
  attr(output,"class") <- c("IQRdataGENERAL", attr(output,"class"))
  return(output)
}
#'@export
check_IQRdataGENERAL <- function(data,FLAGreturnText=FALSE) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("Provided input argument is not an IQRdataGENERAL object")
  checkInfoText_ERROR   <- ""
  checkInfoText_WARNING <- ""
  checkInfoText_MINOR   <- ""
  AEGRADE <- data$AEGRADE[data$AE==1]
  if (!all(AEGRADE %in% c(1,2,3,4,5)))
    checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: AEs present. Not all entries in AEGRADE (for AE=1) lie between 1 and 5\n'))
  if (any(!is.na(data$PROFNR[is.na(data$PROFTIME)])))
    checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: There are PROFNR entries that have PROFTIME set to NA\n'))
  if (any(!is.na(data$PROFNR[is.na(data$PROFTIME)])))
    checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: There are PROFNR entries that have PROFTIME set to NA\n'))
  timeunit <- unique(data$TIMEUNIT)
  if (length(timeunit) > 1)
    checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: Different time units are present in the TIMEUNIT column\n'))
  ix <- which(!(timeunit %in% c("HOURS", "MINUTES", "DAYS", "SECONDS", "WEEKS", "MONTHS", "YEARS")))
  if (length(ix)>0)
    checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: TIMEUNIT column contains not acceptable entries\n              (allowed are: "HOURS", "MINUTES", "DAYS", "SECONDS", "WEEKS", "MONTHS", "YEARS")\n'))
  if (length(setdiff(sort(unique(data$MDV)),c(0,1)))>0)
    checkInfoText_ERROR <- paste0(checkInfoText_WARNING,sprintf('GLOBAL LEVEL: MDV column contains wrong contents (only 0 and 1 allowed)\n'))
  if (length(setdiff(sort(unique(data$EVID)),c(0,1)))>0)
    checkInfoText_ERROR <- paste0(checkInfoText_WARNING,sprintf('GLOBAL LEVEL: EVID column contains wrong contents (only 0 and 1 allowed)\n'))
  if (length(which(is.na(unique(data$IGNORE[data$MDV==1 & data$EVID==0])))) > 0)
    checkInfoText_ERROR <- paste0(checkInfoText_WARNING,sprintf('GLOBAL LEVEL: MDV==1 & EVID==0 entries do not have an entry in IGNORE\n'))
  if (length(which(!is.na(unique(data$IGNORE[data$MDV==0 & data$EVID==0])))) > 0)
    checkInfoText_ERROR <- paste0(checkInfoText_WARNING,sprintf('GLOBAL LEVEL: IGNORE value for MDV=0 and EVID=0 entries must be undefined (NA)\n'))
  if (sum(is.na(data$TIME)) > 0)
    checkInfoText_WARNING <- paste0(checkInfoText_WARNING,sprintf('GLOBAL LEVEL: TIME column contains undefined (NA) entries\n'))
  if (sum(is.na(data$NT[data$AE==0])) > 0)
    checkInfoText_MINOR <- paste0(checkInfoText_MINOR,sprintf('GLOBAL LEVEL: NT column contains undefined (NA) entries\n'))
  if (sum(is.na(data$EXTENS)) > 0)
    checkInfoText_MINOR <- paste0(checkInfoText_MINOR,sprintf('GLOBAL LEVEL: EXTENS column contains undefined (NA) entries\n'))
  if (sum(is.na(data$CENTER)) > 0)
    checkInfoText_MINOR <- paste0(checkInfoText_MINOR,sprintf('GLOBAL LEVEL: CENTER column contains undefined (NA) entries\n'))
  if (sum(is.na(data$VISIT[data$AE==0])) > 0)
    checkInfoText_MINOR <- paste0(checkInfoText_MINOR,sprintf('GLOBAL LEVEL: VISIT column contains undefined (NA) entries\n'))
  if (sum(is.na(data$BASE)) > 0)
    checkInfoText_WARNING <- paste0(checkInfoText_WARNING,sprintf('GLOBAL LEVEL: BASE column contains undefined (NA) entries\n'))
  if (sum(is.na(data$SCREEN)) > 0)
    checkInfoText_WARNING <- paste0(checkInfoText_WARNING,sprintf('GLOBAL LEVEL: SCREEN column contains undefined (NA) entries\n'))
  route <- unique(data$ROUTE[!is.na(data$ROUTE)])
  test <- setdiff(route,c("IV","SUBCUT","ORAL","INHALED","INTRAMUSCULAR","INTRAARTICULAR","RECTAL","TOPICAL","GENERAL_IV","GENERAL_ABS1","GENERAL_ABS0"))
  if (length(test)>0)
    checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: ROUTE column contains not acceptable entries\n              (allowed are: "IV","SUBCUT","ORAL","INHALED","INTRAMUSCULAR","INTRAARTICULAR","RECTAL",\n              "TOPICAL","GENERAL_IV","GENERAL_ABS1","GENERAL_ABS0")\n'))
  if (length(which(is.na(data$VALUE[data$AE==0]) & is.na(data$VALUETXT[data$AE==0]))) > 0)
    checkInfoText_WARNING <- paste0(checkInfoText_WARNING,sprintf('GLOBAL LEVEL: Records are present that have neither VALUE nor VALUETXT defined\n'))
  colname.is.alphanumeric <- grepl("^[[:alnum:]]+$", names(data))
  if (any(!colname.is.alphanumeric))
    checkInfoText_WARNING <- paste0(checkInfoText_WARNING,sprintf('GLOBAL LEVEL: Some column names contain non-alphanumeric values.\n'))
  if ("OCC" %in% names(data)) {
    if (!is.numeric(data$OCC)) {
      checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: OCC column is not numeric\n'))
    } else {
      if (!all(round(data$OCC)-data$OCC==0)) checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: OCC column does not contain only integer values\n'))
      if (length(unique(data$OCC))<2) checkInfoText_WARNING <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: OCC column contains only one occasion\n'))
      if (NA %in% data$OCC) checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('GLOBAL LEVEL: OCC column contains NA values\n'))
    }
  }
  if ("INDNAME" %in% names(data)) {
    dummy <- lapply(split(data,data$INDNAME), function (x) {
      if (length(unique(x$IND)) > 1)
        checkInfoText_ERROR <<- paste0(checkInfoText_ERROR,sprintf('INDICATION LEVEL (%s): IND column entries are not unique\n',x$INDNAME[1]))
    })
  }
  if (all(c("STUDY","STUDYN") %in% names(data))) {
    dummy <- lapply(split(data,data$STUDY), function (x) {
      if (length(unique(x$STUDYN)) > 1)
        checkInfoText_ERROR <<- paste0(checkInfoText_ERROR,sprintf('STUDY LEVEL (%s): STUDYN column entries are not unique\n',x$STUDY[1]))
      if (length(unique(x$STUDYDES)) > 1)
        checkInfoText_ERROR <<- paste0(checkInfoText_ERROR,sprintf('STUDY LEVEL (%s): STUDYDES column entries are not unique\n',x$STUDY[1]))
    })
  }
  if (all(c("TRTNAME","TRT") %in% names(data))) {
    dummy <- lapply(split(data,data$TRTNAME), function (x) {
      if (length(unique(x$TRT)) > 1)
        checkInfoText_ERROR <<- paste0(checkInfoText_ERROR,sprintf('TREATMENT LEVEL (%s): TRT column entries are not unique for TRTNAME\n',x$TRTNAME[1]))
    })
  }
  if (all(c("TRTNAMER","TRTR") %in% names(data))) {
    dummy <- lapply(split(data,data$TRTNAMER), function (x) {
      if (length(unique(x$TRTR)) > 1)
        checkInfoText_ERROR <<- paste0(checkInfoText_ERROR,sprintf('TREATMENT LEVEL (%s): TRTR column entries are not unique for TRTNAMER\n',x$TRTNAME[1]))
    })
  }
  dummy <- lapply(split(data,data$NAME), function (x) {
    if (length(unique(x$UNIT)) > 1)
      checkInfoText_ERROR <<- paste0(checkInfoText_ERROR,sprintf('NAME LEVEL (%s): UNIT column entries are not unique\n',x$NAME[1]))
  })
  dummy <- lapply(split(data,data$NAME), function (x) {
    ixNotNAvalue <- which(!is.na(x$VALUE))
    ixNotNAvaluetxt <- which(!is.na(x$VALUETXT))
    if (length(ixNotNAvalue)>0 & length(ixNotNAvaluetxt)>0) {
      if (length(c(setdiff(ixNotNAvalue,ixNotNAvaluetxt), setdiff(ixNotNAvaluetxt,ixNotNAvalue)))>0)
        checkInfoText_ERROR <<- paste0(checkInfoText_ERROR,sprintf('NAME LEVEL (%s): Sometimes VALUE and sometimes VALUETXT defined. Define either one for each NAME or both\n',x$NAME[1]))
    }
  })
  
  data.table::setDT(data)
  dataTemp <- data[!is.na(data$VALUE) & !is.na(data$VALUETXT),]
  dummy <- lapply(split(dataTemp,dataTemp$NAME), function (x) {
    values_txt <- unique(x$VALUETXT)
    lapply(values_txt, function (y) {
      datak <- x[x$VALUETXT==y,]
      if (length(unique(datak$VALUE))>1)
        checkInfoText_ERROR <<- paste0(checkInfoText_ERROR,sprintf('NAME LEVEL (%s): Used values for VALUE and VALUETXT do not match\n',x$NAME[1]))
    })
  })
  
  dummy <- data[is.na(ADM),ADM := 0,
                by = .(USUBJID,ADM)][,
                                     .(Test_ADM = ADM > 0 & length(TIME) != length(unique(TIME))), 
                                     by = .(USUBJID,ADM)]
  
  if (dummy[,any(Test_ADM)]){
    checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('SUBJECT LEVEL (%s): Subject has DOSE record of ADM "%d" at same TIME points\n',dummy[(Test_ADM),USUBJID],dummy[(Test_ADM),ADM]))
  }
  
  dummy <- data[,.(Test_Time = length(TIME) != length(unique(TIME)),
                   Test_LLOQ = length(unique(LLOQ)) > 1,
                   Test_ULOQ = length(unique(ULOQ)) > 1
  ),
  by = .(USUBJID,NAME)]
  
  if (dummy[,any(Test_Time)]){
    checkInfoText_MINOR <- paste0(checkInfoText_MINOR,sprintf('SUBJECT LEVEL (%s): Subject has records of NAME "%s" at same TIME points\n',dummy[(Test_Time),USUBJID],dummy[(Test_Time),NAME]))
  }
  if (dummy[,any(Test_LLOQ)]){
    checkInfoText_MINOR <- paste0(checkInfoText_MINOR,sprintf('SUBJECT LEVEL (%s): Subject has different LLOQ for NAME "%s"\n',dummy[(Test_LLOQ),USUBJID],dummy[(Test_LLOQ),NAME]))
  }
  if (dummy[,any(Test_ULOQ)]){
    checkInfoText_MINOR <- paste0(checkInfoText_MINOR,sprintf('SUBJECT LEVEL (%s): Subject has different ULOQ for NAME "%s"\n',dummy[(Test_ULOQ),USUBJID],dummy[(Test_ULOQ),NAME]))
  }
  
  dummy <- data[,.(
    Test_Time2 = sum(diff(TIME) < 0, na.rm=TRUE)
  ), by = USUBJID]
  
  if (dummy[,any(Test_Time2)]){
    checkInfoText_ERROR <- paste0(checkInfoText_ERROR,sprintf('SUBJECT LEVEL (%s): TIME not monotonously non-decreasing\n',dummy[(Test_Time2),USUBJID]))
  }
  
  # check these columns
  col_list <- list("CENTER","SUBJECT","INDNAME","COMPOUND",
                   "STUDY","STUDYN","STUDYDES", 
                   "PART","EXTENS",
                   "TRTNAME","TRTNAMER","TRT","TRTR")
  # but only those that actually exist in the data
  col_list <- intersect(col_list, names(data))
  names(col_list) <- col_list 
  
  # for each column in the list, check if it has more than one unique value (per USUBJID)
  data[,lapply(.SD,function(x){ dplyr::if_else(length(unique(x)) > 1, T, F) }),
       .SDcols = col_list, 
       by = USUBJID][
         # filter so that we have any subjects that meet the condition for any column
         (eval(parse(text = paste(col_list,collapse = "|"))))] %>%
    # pivot the table to name-value pairs per USUBJID
    data.table::melt(id.vars = "USUBJID") %>%
    # for any column that has more than one unique value per subject, add to the error message
    .[(value),
      .(checkInfoText_ERROR <- paste0(checkInfoText_ERROR, sprintf('SUBJECT LEVEL (%s): Multiple different entries for %s present\n',USUBJID,variable)))]
  # combine all the errors and minor messages from vectors to one string each
  checkInfoText_ERROR <- paste0(checkInfoText_ERROR, collapse = "")
  checkInfoText_MINOR <- paste0(checkInfoText_MINOR, collapse = "")
  data.table::setDF(data)
  if (FLAGreturnText) {
    output <- list(
      checkInfoText_MINOR = checkInfoText_MINOR,
      checkInfoText_WARNING = checkInfoText_WARNING,
      checkInfoText_ERROR = checkInfoText_ERROR
    )
    return(output)
  }
  if (nchar(checkInfoText_ERROR) > 0) {
    message(
      "\ncheck_IQRdataGENERAL: ERROR messages that need to be addressed\n==============================================================\n",
      checkInfoText_ERROR
    )
  }
  if (nchar(checkInfoText_WARNING) > 0) {
    message(
      "\ncheck_IQRdataGENERAL: WARNING messages that should be addressed\n===============================================================\n",
      checkInfoText_WARNING
    )
  }
  if (nchar(checkInfoText_MINOR) > 0) {
    message(
      "\ncheck_IQRdataGENERAL: MINOR messages that might be addressed\n============================================================\n",
      checkInfoText_MINOR
    )
  }
  if (nchar(checkInfoText_WARNING)) warningIQR("check_IQRdataGENERAL: Warnings present in general dataset - please check messages above")
  if (nchar(checkInfoText_ERROR)) warningIQR("check_IQRdataGENERAL: Errors present in general dataset - please check messages above")
  cat("\n\n")
}

#'@export 
generalDataHandleMissingColums <- function(data) {
  if (!("IXGDF" %in% names(data))) {
    data$IXGDF <- as.numeric(1:nrow(data))
    message("Adding default IXGDF column")
  }
  if (!("IGNORE" %in% names(data))) {
    data$IGNORE <- NA
    message("Adding default IGNORE column")
  }
  if ("INDNAME" %in% names(data)) {
    if (!("IND" %in% names(data))) {
      data$IND <- as.numeric(factor(data$INDNAME,levels=sort(unique(data$INDNAME))))
      message("Adding default IND column")
    }
  }
  if ("STUDY" %in% names(data)) {
    if (!("STUDYN" %in% names(data))) {
      data$STUDYN <- as.numeric(factor(data$STUDY,levels=sort(unique(data$STUDY))))
      message("Adding default STUDYN column")
    }
  }
  if ("TRTNAME" %in% names(data)) {
    if (!("TRT" %in% names(data))) {
      data$TRT <- as.numeric(factor(data$TRTNAME,levels=sort(unique(data$TRTNAME))))
      message("Adding default TRT column")
    }
  }
  if ("TRTNAMER" %in% names(data)) {
    if (!("TRTR" %in% names(data))) {
      data$TRTR <- as.numeric(factor(data$TRTNAMER,levels=sort(unique(data$TRTNAMER))))
      message("Adding default TRTR column")
    }
  }
  if (!("BASE" %in% names(data))) {
    data$BASE <- as.numeric(0)
    message("Adding default BASE column")
  }
  if (!("SCREEN" %in% names(data))) {
    data$SCREEN <- as.numeric(0)
    message("Adding default SCREEN column")
  }
  if (!("DURATION" %in% names(data))) {
    data$DURATION <- as.numeric(0)
    message("Adding default DURATION column")
  }
  if (!("VALUETXT" %in% names(data))) {
    data$VALUETXT <- NA
    message("Adding default VALUETXT column")
  }
  if (!("LLOQ" %in% names(data))) {
    data$LLOQ <- as.numeric(NA)
    message("Adding default LLOQ column")
  }
  if (!("II" %in% names(data))) {
    data$II <- as.numeric(0)
    message("Adding default II column")
  }
  if (!("ADDL" %in% names(data))) {
    data$ADDL <- as.numeric(0)
    message("Adding default ADDL column")
  }
  if ("AE" %in% names(data)) {
    if (!("AEGRADE" %in% names(data))) {
      data$AEGRADE <- as.numeric(NA)
      message("Adding default AEGRADE column")
    }
    if (!("AESER" %in% names(data))) {
      data$AESER <- as.numeric(NA)
      message("Adding default AESER column")
    }
    if (!("AEDRGREL" %in% names(data))) {
      data$AEDRGREL <- as.numeric(NA)
      message("Adding default AEDRGREL column")
    }
  }
  namesOrder__ <- c(getColumnsInfo_IQRdataGENERAL()$namesOrder)
  namesOrderPresent__ <- names(data)[names(data) %in% namesOrder__]
  dataOrdered__ <- data[,namesOrderPresent__]
  addcols <- setdiff(names(data),names(dataOrdered__))
  data <- cbind(dataOrdered__, data[,addcols,drop=FALSE])
  rownames(data) <- NULL
  return(data)
}
generalDataMapVALUETXT2VALUE <- function(data) {
  ix_VALUE_undefined      <- which(is.na(data$VALUE))
  ix_VALUE_TEXT_defined   <- which(!is.na(data$VALUETXT))
  ix_handle               <- intersect(ix_VALUE_undefined,ix_VALUE_TEXT_defined)
  dataHandle              <- unique(data[ix_handle,c('NAME','VALUETXT')])
  if (length(ix_handle) > 0) {
    dummy__ <- lapply(split(dataHandle,dataHandle$NAME), function (x) {
      x <- dplyr::arrange(x,VALUETXT)
      x$VALUE <- 1:nrow(x)
      lapply(x$VALUETXT, function (y) {
        data$VALUE[data$VALUETXT==y & data$NAME == x$NAME[1]] <<- x$VALUE[x$VALUETXT==y]
      })
      x
    })
  }
  dataX <- data[!is.na(data$VALUETXT),c("NAME","VALUETXT","VALUE")]
  mappingVALUETXTinfo <- lapply(split(dataX,dataX$NAME), function (x) {
    x <- dplyr::arrange(x,VALUE)
    x <- unique(x,drop=FALSE)
    lapply(split(x,x$VALUETXT), function (y) {
      if (length(unique(y$VALUETXT)) != length(y$VALUETXT)) {
        stopIQR(paste0(y$NAME[1],": Wrong definition of VALUE and VALUETXT columns.\nEnsure assignment of unique VALUE for each VALUETXT"))
      }
    })
    names(x) <- c("NAME",x$NAME[1],"VALUE")
    x[,2:ncol(x)]
    x
  })
  if (length(mappingVALUETXTinfo)==0) mappingVALUETXTinfo <- NULL
  rownames(data) <- NULL
  return(data)
}
generalData_IDcolumn <- function(data,FLAGforceOverwriteNLMEcols) {
  if (!("ID" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    count <- 1
    data <- do.call(rbind,lapply(split(data,data$USUBJID), function (x) { x$ID <- count; count<<-count+1; x}))
    rownames(data) <- NULL
    message("Adding default ID column")
  }
  return(data)
}
generalData_TIMEPOScolumn <- function(data,FLAGforceOverwriteNLMEcols) {
  if (!("TIMEPOS" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data <- do.call(rbind,lapply(split(data,data$USUBJID), function (x) { x$TIMEPOS <- x$TIME-x$TIME[1]; x}))
    rownames(data) <- NULL
    message("Adding default TIMEPOS column")
  }
  return(data)
}
generalData_EVIDcolumn <- function(data,doseNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("EVID" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data$EVID <- 0
    for (k__ in seq_along(doseNAMES)) {
      data$EVID[data$NAME==doseNAMES[k__]] <- 1
    }
    rownames(data) <- NULL
    message("Adding default EVID column")
  }
  return(data)
}
generalData_MDVcolumn <- function(data,doseNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("MDV" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data$MDV <- 0
    for (k__ in seq_along(doseNAMES)) {
      data$MDV[data$NAME==doseNAMES[k__]] <- 1
    }
    data$MDV[!is.na(data$IGNORE)] <- 1
    rownames(data) <- NULL
    message("Adding default MDV column")
  }
  return(data)
}
generalData_DVcolumn <- function(data,doseNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("DV" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data$DV <- 0
    if ("AE" %in% names(data)) {
      data$DV[!(data$NAME %in% doseNAMES) & data$AE==0] <- data$VALUE[!(data$NAME %in% doseNAMES) & data$AE==0]
    } else {
      data$DV[!(data$NAME %in% doseNAMES)] <- data$VALUE[!(data$NAME %in% doseNAMES)]
    }
    rownames(data) <- NULL
    message("Adding default DV column")
  }
  return(data)
}
generalData_AMTcolumn <- function(data,doseNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("AMT" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data$AMT <- 0
    data$AMT[data$NAME %in% doseNAMES] <- data$VALUE[data$NAME %in% doseNAMES]
    rownames(data) <- NULL
    message("Adding default AMT column")
  }
  return(data)
}
generalData_CENScolumn <- function(data,FLAGforceOverwriteNLMEcols) {
  if (!("CENS" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data$CENS <- 0
    rownames(data) <- NULL
    message("Adding default CENS column")
  }
  return(data)
}
generalData_TINFcolumn <- function(data,doseNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("TINF" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data$TINF <- 0
    data$TINF[data$NAME %in% doseNAMES] <- data$DURATION[data$NAME %in% doseNAMES]
    rownames(data) <- NULL
    message("Adding default TINF column")
  }
  return(data)
}
generalData_RATEcolumn <- function(data,doseNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("RATE" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data$RATE <- 0
    data$RATE[data$TINF>0] <- data$AMT[data$TINF>0] / data$TINF[data$TINF>0]
    rownames(data) <- NULL
    message("Adding default RATE column")
  }
  return(data)
}
generalData_ADMcolumn <- function(data,doseNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("ADM" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data$ADM <- 0
    if (length(doseNAMES) > 1) {
      for (k__ in seq_along(doseNAMES)) {
        data$ADM[data$NAME==doseNAMES[k__]] <- k__
      }
    } else {
      ROUTES__ <- unique(data$ROUTE)
      ROUTES__ <- ROUTES__[!is.na(ROUTES__)]
      if (length(ROUTES__) > 1) {
        data$ADM[data$ROUTE %in% c("SUBCUT", "ORAL", "INTRAMUSCULAR", "INTRAARTICULAR", "RECTAL", "INHALED", "GENERAL_ABS1")] <- 1
        data$ADM[data$ROUTE %in% c("IV", "GENERAL_IV")] <- 2
        data$ADM[data$ROUTE %in% c("TOPICAL", "GENERAL_ABS0")] <- 3
      } else {
        data$ADM[data$ROUTE %in% c("SUBCUT", "ORAL", "INTRAMUSCULAR", "INTRAARTICULAR", "RECTAL", "INHALED", "GENERAL_ABS1", "IV", "GENERAL_IV","TOPICAL", "GENERAL_ABS0")] <- 1
      }
    }
    rownames(data) <- NULL
    message("Adding default ADM column")
  }
  return(data)
}
generalData_TADcolumns <- function(data,doseNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("TAD" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data <- do.call(rbind,lapply(split(data,data$USUBJID), function (x) {
      ixDOSE__                    <- rep(NA,nrow(x))
      ixEVID__                    <- which(x$EVID==1)
      if (length(ixEVID__)==0) {
        xx__ <- x
        xx__$TAD <- NA
      } else {
        ixDOSE__[ixEVID__]          <- ixEVID__
        ixDOSE__                  <- aux_na_locf(ixDOSE__)
        ixDOSE__[is.na(ixDOSE__)]     <- 0
        x$ixDOSE__                  <- ixDOSE__
        xx__ <- do.call(rbind,lapply(split(x,x$ixDOSE__), function (y) {
          if (y$ixDOSE__[1]>0) {
            y$TAD <- y$TIME-y$TIME[1]
          } else {
            y$TAD <- y$TIME
          }
          y
        }))
        xx__$ixDOSE__ <- NULL
      }
      xx__
    }))
    message("Adding default TAD column")
  }
  if (length(doseNAMES) > 1) {
    FLAG_add_TAD_cols__ <- c()
    for (k__ in seq_along(doseNAMES)) {
      TAD_name__             <- paste0("TADD",k__)
      if (!(TAD_name__ %in% names(data)) | FLAGforceOverwriteNLMEcols) {
        data[[TAD_name__]] <- NA
        FLAG_add_TAD_cols__[k__] <- TRUE
      } else {
        FLAG_add_TAD_cols__[k__] <- FALSE
      }
    }
    for (k__ in seq_along(doseNAMES)) {
      if (FLAG_add_TAD_cols__[k__]) {
        data <- do.call(rbind,lapply(split(data,data$USUBJID), function (x) {
          TAD_name__                  <- paste0("TADD",k__)
          ixDOSE__                    <- rep(NA,nrow(x))
          ixDOSE_name               <- which(x$EVID==1 & x$NAME==doseNAMES[k__])
          if (length(ixDOSE_name)==0) {
            xx__ <- x
            xx__[[TAD_name__]] <- NA
          } else {
            ixDOSE__[ixDOSE_name]       <- ixDOSE_name
            ixDOSE__                    <- aux_na_locf(ixDOSE__)
            ixDOSE__[is.na(ixDOSE__)]     <- 0
            x$ixDOSE__                  <- ixDOSE__
            x[[TAD_name__]]             <- x$TIMEPOS-x$TIMEPOS[min(ixDOSE_name)]
            xx__ <- do.call(rbind,lapply(split(x,x$ixDOSE__), function (y) {
              if (y$ixDOSE__[1]>0) {
                y[[TAD_name__]] <- y$TIMEPOS-y$TIMEPOS[1]
              }
              y
            }))
            xx__$ixDOSE__ <- NULL
          }
          xx__
        }))
      }
    }
  }
  rownames(data) <- NULL
  return(data)
}
generalData_DOSEcolumns <- function(data,doseNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("DOSE" %in% names(data)) | FLAGforceOverwriteNLMEcols) {
    data <- do.call(rbind,lapply(split(data,data$USUBJID), function (x) {
      x$DOSE <- NA
      x$DOSE[x$EVID==1] <- x$AMT[x$EVID==1]
      x$DOSE <- aux_na_locf(x$DOSE,x$TIME)
      x$DOSE[is.na(x$DOSE)] <- 0
      x
    }))
    message("Adding default DOSE column")
  }
  if (length(doseNAMES) > 1) {
    FLAG_add_DOSE_cols__ <- c()
    for (k__ in seq_along(doseNAMES)) {
      DOSE_name__             <- paste0("DOSED",k__)
      if (!(DOSE_name__ %in% names(data)) | FLAGforceOverwriteNLMEcols) {
        data[[DOSE_name__]] <- NA
        FLAG_add_DOSE_cols__[k__] <- TRUE
      } else {
        FLAG_add_DOSE_cols__[k__] <- FALSE
      }
    }
    for (k__ in seq_along(doseNAMES)) {
      if (FLAG_add_DOSE_cols__[k__]) {
        DOSE_name__ <- paste0("DOSED",k__)
        data <- do.call(rbind,lapply(split(data,data$USUBJID), function (x) {
          ixDOSEn <- which(x$NAME==doseNAMES[k__])
          x[[DOSE_name__]][ixDOSEn] <- x$AMT[ixDOSEn]
          x[[DOSE_name__]] <- aux_na_locf(x[[DOSE_name__]],x$TIME)
          x[[DOSE_name__]][is.na(x[[DOSE_name__]])] <- 0
          x
        }))
      }
    }
  }
  rownames(data) <- NULL
  return(data)
}
generalData_YTYPEcolumn <- function(data,obsNAMES,FLAGforceOverwriteNLMEcols) {
  if (!("YTYPE" %in% names(data))) {
    AEobsNames <- sort(obsNAMES[which(obsNAMES %in% data$NAME[data$AE==1])])
    NonAEobsNames <- sort(setdiff(obsNAMES,AEobsNames))
    obsNamesYTYPEorder <- c(NonAEobsNames,AEobsNames)
    data$YTYPE <- 0
    for (k__ in seq_along(obsNAMES)) {
      data$YTYPE[data$NAME==obsNAMES[k__]] <- k__
    }
    rownames(data) <- NULL
    message("Adding default YTYPE column")
  }
  return(data)
}

generalDataTimeIndependentCovariates <- function (data,cov0,cat0) {
  x__ <- setdiff(unlist(cov0),unique(data$NAME))
  if (length(x__) > 0) {
    stopIQR(paste0("Input argument 'cov0' selected covariates that are not present in the datasat (NAME):\n  ",
                   paste0(x__,collapse=",")))
  }
  x__ <- setdiff(unlist(cat0),unique(data$NAME))
  if (length(x__) > 0) {
    stopIQR(paste0("Input argument 'cat0' selected covariates that are not present in the datasat (NAME):\n  ",
                   paste0(x__,collapse=",")))
  }
  x__ <- intersect(cov0, unique(data$NAME[data$AE==1]))
  if (length(x__)>0) {
    stopIQR(paste0("Input argument 'cov0' selected covariates that are adverse events:\n  ",
                   paste0(x__,collapse=",")))
  }
  x__ <- intersect(cat0, unique(data$NAME[data$AE==1]))
  if (length(x__)>0) {
    stopIQR(paste0("Input argument 'cat0' selected covariates that are adverse events:\n  ",
                   paste0(x__,collapse=",")))
  }
  x__ <- intersect(names(cov0),names(data))
  if (length(x__) > 0) {
    stopIQR(paste0("Input argument 'cov0' defines covariate columns that are already present in the dataset:\n  ",
                   paste0(x__,collapse=",")))
  }
  x__ <- intersect(names(cat0),names(data))
  if (length(x__) > 0) {
    stopIQR(paste0("Input argument 'cat0' defines covariate columns that are already present in the dataset:\n  ",
                   paste0(x__,collapse=",")))
  }
  covariateInfo0 <- c(cov0,cat0)
  
  # edited from source to improve performance
  # Define conditions as quoted expressions
  cond1 <- quote(NAME == covNAME__ & BASE != 0)
  cond2 <- quote(NAME == covNAME__ & SCREEN != 0)
  cond3 <- quote(NAME == covNAME__ & TIME <= 0)
  
  # Convert data.frame to data.table
  data.table::setDT(data)
  
  # Iterate on each time-independent covariate (cov0,cat0)
  # Iterate over each time-independent covariate (cov0,cat0)
  for (k__ in seq_along(covariateInfo0)) {
    
    # Get the covariate name and column name
    colName__ <- names(covariateInfo0)[k__]
    covNAME__ <- covariateInfo0[[k__]]
    
    # Create a new column for the current covariate
    data[, (colName__) := NA_real_]
    
    # Apply conditions sequentially to each group of USUBJID
    data[, (colName__) := {
      y <- .SD[NAME == covNAME__ & BASE != 0]
      if (nrow(y) == 0) {
        z <- NA_real_
      } else {
        z <- mean(y$VALUE)
      }
      if (is.na(z)) {
        y <- .SD[NAME == covNAME__ & SCREEN != 0]
        if (nrow(y) == 0) {
          z <- NA_real_
        } else {
          z <- mean(y$VALUE)
        }
        if (is.na(z)) {
          y <- .SD[NAME == covNAME__ & TIME <= 0]
          if (nrow(y) == 0) {
            z <- NA_real_
          } else {
            z <- mean(y$VALUE)
          }
        }
      }
      z
    }, by = USUBJID]
  }
  rownames(data) <- NULL
  data.table::setDF(data)
  attr(data,"class") <- c("IQRdataGENERAL", attr(data,"class"))
  return(data)
}
generalDataTimeDependentCovariates <- function (data,covT,catT,FLAGnoNAlocf) {
  if (length(setdiff(unlist(covT),unique(data$NAME))) > 0)
    stopIQR("Input argument 'covT' selects covariates that are not present in the datasat (NAME)")
  if (length(setdiff(unlist(catT),unique(data$NAME))) > 0)
    stopIQR("Input argument 'catT' selectes covariates that are not present in the datasat (NAME)")
  if (length(intersect(covT, unique(data$NAME[data$AE==1])))>0)
    stopIQR("Input argument 'covT' selectes covariates that are adverse events")
  if (length(intersect(catT, unique(data$NAME[data$AE==1])))>0)
    stopIQR("Input argument 'catT' selectes covariates that are adverse events")
  if (length(intersect(names(covT),names(data))) > 0)
    stopIQR("Input argument 'covT' defines covariate columns that are already present in the dataset")
  if (length(intersect(names(catT),names(data))) > 0)
    stopIQR("Input argument 'catT' defines covariate columns that are already present in the dataset")
  covariateInfoT__ <- c(covT,catT)
  for (k__ in seq_along(covariateInfoT__)) {
    colName__ <- names(covariateInfoT__)[k__]
    covNAME__ <- covariateInfoT__[[k__]]
    data[[colName__]] <- NA
    data[[colName__]][data$NAME==covNAME__] <- data$VALUE[data$NAME==covNAME__]
    if (!FLAGnoNAlocf) {
      data <- do.call(rbind,lapply(split(data,data$USUBJID), function (x) {
        x[[colName__]] <- aux_na_locf(x[[colName__]],x$TIME)
        ix_notNA__ <- which(!is.na(x[[colName__]]))
        if (length(ix_notNA__) > 0) x[[colName__]][is.na(x[[colName__]])] <- x[[colName__]][min(ix_notNA__)]
        x
      }))
    }
  }
  rownames(data) <- NULL
  return(data)
}
mappingVALUETXT_IQRdataGENERAL <- function(data) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  dataX <- data[!is.na(data$VALUETXT),c("NAME","VALUETXT","VALUE")]
  mappingVALUETXT <- data.frame(do.call(rbind,mappingVALUETXT <- lapply(split(dataX,dataX$NAME), function (x) {
    x <- dplyr::arrange(x,VALUE)
    unique(x)
  })),stringsAsFactors=FALSE,row.names=NULL)
  if (length(mappingVALUETXT)==0) mappingVALUETXT <- NULL
  return(mappingVALUETXT)
}
doseInfo_IQRdataGENERAL <- function (data) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  doseNAMES <- unique(data$NAME[data$EVID==1])
  output <- data.frame(do.call(rbind,lapply(doseNAMES, function (x) {
    dN__ <- data[data$NAME==x & data$EVID==1,]
    N_TOTAL__ <- nrow(dN__)
    N_INDIV__ <- sapply(split(dN__,dN__$USUBJID), function (y) {
      nrow(y)
    })
    c(DOSE.NAME=x,
      TOTAL.DOSES=N_TOTAL__,
      MIN.INDIV.DOSES=min(N_INDIV__),
      MEDIAN.INDIV.DOSES=stats::median(N_INDIV__),
      MAX.INDIV.DOSES=max(N_INDIV__))
  })),stringsAsFactors=FALSE)
  if (nrow(output)==0) {
    output <- data.frame(
      DOSE.NAME = "No Dose name present",
      TOTAL.DOSES = 0,
      MIN.INDIV.DOSES = 0,
      MEDIAN.INDIV.DOSES = 0,
      MAX.INDIV.DOSES = 0
    )
  }
  output$TOTAL.DOSES        <- as.numeric(output$TOTAL.DOSES)
  output$MIN.INDIV.DOSES    <- as.numeric(output$MIN.INDIV.DOSES)
  output$MEDIAN.INDIV.DOSES <- as.numeric(output$MEDIAN.INDIV.DOSES)
  output$MAX.INDIV.DOSES    <- as.numeric(output$MAX.INDIV.DOSES)
  rownames(output) <- NULL
  return(output)
}
obsInfo_IQRdataGENERAL <- function (data) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  obsNAMES <- unique(data$NAME[data$EVID==0 & data$YTYPE>0])
  output <- data.frame(do.call(rbind,lapply(obsNAMES, function (x) {
    dN__ <- data[data$NAME==x & data$EVID==0,]
    N_TOTAL__ <- as.character(nrow(dN__))
    N_INDIV__ <- do.call(rbind,lapply(split(dN__,dN__$USUBJID), function (y) {
      c(nrow(y))
    }))
    c(OBSERVATION.NAME=x,
      TOTAL.OBSERVATIONS=N_TOTAL__,
      MIN.INDIV.OBSERVATIONS=as.character(min(N_INDIV__[,1])),
      MEDIAN.INDIV.OBSERVATIONS=as.character(stats::median(N_INDIV__[,1])),
      MAX.INDIV.OBSERVATIONS=as.character(max(N_INDIV__[,1])))
  })),stringsAsFactors=FALSE)
  rownames(output) <- NULL
  return(output)
}
#'@export
blloq_IQRdataGENERAL <- function(data,methodBLLOQ="M1") {
  if (!is_IQRdataGENERAL(data))
    stopIQR("data argument is not an IQRdataGENERAL object")
  if (!(methodBLLOQ %in% c("M1","M3","M4","M5","M6","M7")))
    stopIQR('methodBLLOQ argument is not correctly set (choose one from: "M1","M3","M4","M5","M6","M7")')
  dataBLLOQ <- data
  dataBLLOQ$DV[dataBLLOQ$CENS==1] <- dataBLLOQ$VALUE[dataBLLOQ$CENS==1]
  dataBLLOQ$CENS <- 0
  ix_BLLOQ_handled_MDV <- which(grepl("BLLOQ (M",dataBLLOQ$IGNORE,fixed=TRUE))
  dataBLLOQ$MDV[ix_BLLOQ_handled_MDV] <- 0
  dataBLLOQ$IGNORE[ix_BLLOQ_handled_MDV] <- NA
  if (methodBLLOQ=="M1") {
    ix_handle <- which(!is.na(dataBLLOQ$LLOQ) & dataBLLOQ$VALUE<dataBLLOQ$LLOQ & dataBLLOQ$MDV==0 & dataBLLOQ$EVID==0 & dataBLLOQ$YTYPE>0)
    dataBLLOQ$MDV[ix_handle] <- 1
    dataBLLOQ$IGNORE[ix_handle] <- "BLLOQ (M1)"
  }
  if (methodBLLOQ=="M3" | methodBLLOQ=="M4") {
    ix_handle <- which(!is.na(dataBLLOQ$LLOQ) & dataBLLOQ$VALUE<dataBLLOQ$LLOQ & dataBLLOQ$MDV==0 & dataBLLOQ$EVID==0 & dataBLLOQ$YTYPE>0)
    dataBLLOQ$CENS[ix_handle] <- 1
    dataBLLOQ$DV[ix_handle] <- dataBLLOQ$LLOQ[ix_handle]
  }
  if (methodBLLOQ=="M5") {
    ix_handle <- which(!is.na(dataBLLOQ$LLOQ) & dataBLLOQ$VALUE<dataBLLOQ$LLOQ & dataBLLOQ$MDV==0 & dataBLLOQ$EVID==0 & dataBLLOQ$YTYPE>0)
    dataBLLOQ$DV[ix_handle] <- dataBLLOQ$LLOQ[ix_handle] / 2
  }
  if (methodBLLOQ=="M7") {
    ix_handle <- which(!is.na(dataBLLOQ$LLOQ) & dataBLLOQ$VALUE<dataBLLOQ$LLOQ & dataBLLOQ$MDV==0 & dataBLLOQ$EVID==0 & dataBLLOQ$YTYPE>0)
    dataBLLOQ$DV[ix_handle] <- 0
  }
  if (methodBLLOQ=="M6") {
    ix_handle <- which(!is.na(dataBLLOQ$LLOQ) & dataBLLOQ$VALUE<dataBLLOQ$LLOQ & dataBLLOQ$MDV==0 & dataBLLOQ$EVID==0 & dataBLLOQ$YTYPE>0)
    NAMES_BLLOQ_present <- unique(data$NAME[ix_handle])
    for (k00 in seq_along(NAMES_BLLOQ_present)) {
      handleNAME <- NAMES_BLLOQ_present[k00]
      dataBLLOQ <- do.call(rbind,lapply(split(dataBLLOQ,dataBLLOQ$USUBJID), function (x) {
        x$BLOQ <- 0
        x$BLOQ[which(!is.na(x$LLOQ) & x$DV<x$LLOQ & x$MDV==0 & x$EVID==0 & x$YTYPE>0 & x$NAME==handleNAME)] <- 1
        x$seq_check <- NA
        x$seq_check[x$NAME==handleNAME] <- 1
        x$cumsum <- NA
        x$cumsum[!is.na(x$seq_check)] <- cumsum(x$seq_check[!is.na(x$seq_check)])
        x$cumsumLLOQ <- NA
        x$cumsumLLOQ[x$BLOQ==1] <- x$cumsum[x$BLOQ==1]
        x$delta <- NA
        x$delta[x$BLOQ==1] <- c(Inf, diff(x$cumsumLLOQ[x$BLOQ==1]))
        x$first <- FALSE
        x$first[x$delta>1] <- TRUE
        x$consecutive <- FALSE
        x$consecutive[x$delta==1] <- TRUE
        x$MDV[x$consecutive] <- 1
        x$IGNORE[x$consecutive] <- "BLLOQ (M6)"
        x$DV[x$BLOQ==1] <- x$LLOQ[x$BLOQ==1] / 2
        x$seq_check <- NULL
        x$cumsum <- NULL
        x$cumsumLLOQ <- NULL
        x$delta <- NULL
        x$first <- NULL
        x$consecutive <- NULL
        x$BLOQ <- NULL
        x
      }))
    }
  }
  output <- dataBLLOQ
  attr(output,"methodBLLOQ") <- methodBLLOQ
  rownames(output) <- NULL
  return(output)
}
#'@export
blloqInfo_IQRdataGENERAL <- function (data) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("data argument is not an IQRdataGENERAL object")
  data <- data[data$YTYPE>0,]
  blloqInfoTotal <- data.frame(do.call(rbind,lapply(split(data,data$NAME), function (x) {
    x$BLOQ <- as.numeric(x$VALUE < x$LLOQ)
    NobsTotal <- nrow(x)
    NobsBLLOQ <- sum(x$BLOQ,na.rm=TRUE)
    PercBLLOQ <- signif(100*NobsBLLOQ/NobsTotal,3)
    out <- c(OBSERVATION=unique(x$NAME),TOTAL.OBSERVATIONS=NobsTotal,BLLOQ.OBSERVATIONS=NobsBLLOQ,PERCENT.BLLOQ=PercBLLOQ)
    rownames(out) <- NULL
    out
  })),stringsAsFactors=FALSE)
  blloqInfoIndividual <- lapply(split(data,data$NAME), function (x) {
    x$BLOQ <- as.numeric(x$VALUE < x$LLOQ)
    infoNAME <- data.frame(do.call(rbind,lapply(split(x,x$USUBJID), function (y) {
      NobsTotal <- nrow(y)
      NobsBLLOQ <- sum(y$BLOQ,na.rm=TRUE)
      PercBLLOQ <- signif(100*NobsBLLOQ/NobsTotal,3)
      out <- c(OBSERVATION=unique(y$USUBJID),TOTAL.OBSERVATIONS=NobsTotal,BLLOQ.OBSERVATIONS=NobsBLLOQ,PERCENT.BLLOQ=PercBLLOQ)
      rownames(out) <- NULL
      out
    })),stringsAsFactors=FALSE)
  })
  output <- list(blloqInfoTotal=blloqInfoTotal,blloqInfoIndividual=blloqInfoIndividual)
  return(output)
}
zeroDoses_IQRdataGENERAL <- function (data) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  doseNAMES <- unique(data$NAME[data$EVID==1])
  output__ <- data.frame(do.call(rbind,lapply(doseNAMES, function (x) {
    dN__ <- data[data$NAME==x & data$EVID==1 & data$AMT==0,]
    N_TOTAL__ <- nrow(dN__)
    c(DOSE.NAME=x,
      N.ZERO.DOSES=N_TOTAL__)
  })),stringsAsFactors=FALSE)
  output__$N.ZERO.DOSES        <- as.numeric(output__$N.ZERO.DOSES)
  rownames(output__) <- NULL
  return(output__)
}
placeboIndiv_IQRdataGENERAL <- function (data) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  output__ <- unname(unlist(sapply(split(data,data$USUBJID), function (x) {
    Ndoses <- nrow(x[x$EVID==1,])
    NdosesAMT0 <- nrow(x[x$EVID==1 & x$AMT==0,])
    NdosesAMTnot0 <- Ndoses-NdosesAMT0
    if (NdosesAMTnot0 > 0) {
      out <- NULL
    } else {
      out <- x$USUBJID[1]
    }
    out
  })))
  return(output__)
}
noObsIndiv_IQRdataGENERAL <- function (data) {
  if (!is_IQRdataGENERAL(data))
    stopIQR("input argument is not an IQRdataGENERAL object")
  output__ <- unname(unlist(sapply(split(data,data$USUBJID), function (x) {
    if (!any(x$EVID==0)) {
      out <- x$USUBJID[1]
    } else {
      NobsNotIgnored <- nrow(x[x$EVID==0 & x$MDV==0 & x$YTYPE>0,])
      if (NobsNotIgnored > 0) {
        out <- NULL
      } else {
        out <- x$USUBJID[1]
      }
    }
    out
  })))
  return(output__)
}
#'@export
addLabel_IQRdataGENERAL <- function(data,addColLabels=NULL) {
  addLabel__ <- function (data,colName__,label) {
    if (colName__ %in% names(data)) {
      attr(data[[colName__]],"label") <- label
      attr(data[[colName__]],"class") <- unique(c("labelled",class(data[[colName__]])))
    }
    return(data)
  }
  data <- addLabel__(data,"IXGDF","Index of record in master dataset")
  data <- addLabel__(data,"IGNORE","Exclusion reason")
  data <- addLabel__(data,"USUBJID","Unique subject identifier")
  data <- addLabel__(data,"ID","Numeric subject ID for modeling software")
  data <- addLabel__(data,"STUDY","Short study name/number")
  data <- addLabel__(data,"STUDYN","Numeric study flag")
  data <- addLabel__(data,"TRTNAME","Name actual treatment given to subject")
  data <- addLabel__(data,"TRT","Numeric treatment flag")
  data <- addLabel__(data,"TIME","Actual time relative to first dose")
  data <- addLabel__(data,"TIMEPOS","Time since first record in subject")
  data <- addLabel__(data,"NT","Nominal event time")
  data <- addLabel__(data,"TAD","Time after last dose")
  data <- addLabel__(data,"TIMEUNIT","Unit of all numeric time definitions")
  data <- addLabel__(data,"YTYPE","Observation output number")
  data <- addLabel__(data,"NAME","Short name of event")
  data <- addLabel__(data,"DV","Dependent variable")
  data <- addLabel__(data,"LNDV","Log dependent variable")
  data <- addLabel__(data,"UNIT","Unit of the value")
  data <- addLabel__(data,"CENS","Censoring flag")
  data <- addLabel__(data,"LIMIT","Lower limit censoring (for MONOLIX)")
  data <- addLabel__(data,"MDV","Missing dependent variable flag")
  data <- addLabel__(data,"EVID","Event ID")
  data <- addLabel__(data,"AMT",sprintf("Dose amount (%s)",unique(unclass(data$UNIT)[data$EVID==1])))
  data <- addLabel__(data,"ADM","Administration input number")
  data <- addLabel__(data,"II","Interval of dosing")
  data <- addLabel__(data,"ADDL","Number of addl doses with II interval")
  data <- addLabel__(data,"ROUTE","Route of administration")
  data <- addLabel__(data,"TINF","Infusion time")
  data <- addLabel__(data,"RATE","Rate of infusion")
  data <- addLabel__(data,"DOSE","DOSE of last dose (carry-forward)")
  data <- addLabel__(data,"CENTER","Center number")
  data <- addLabel__(data,"SUBJECT","Subject number")
  data <- addLabel__(data,"INDNAME","Indication name")
  data <- addLabel__(data,"IND","Numeric indication flag")
  data <- addLabel__(data,"COMPOUND","Name of the studied compound")
  data <- addLabel__(data,"STUDYDES","Study title, short description")
  data <- addLabel__(data,"PART","Part of study as defined per protocol")
  data <- addLabel__(data,"EXTENS","Extension of the core study")
  data <- addLabel__(data,"TRTNAMER","Name of randomized treatment")
  data <- addLabel__(data,"TRTR","Numeric randomized treatment flag")
  data <- addLabel__(data,"VISIT","Visit number")
  data <- addLabel__(data,"VISNAME","Visit name")
  data <- addLabel__(data,"BASE","Flag indicating assessments at baseline")
  data <- addLabel__(data,"SCREEN","Flag indicating assessments at screening")
  data <- addLabel__(data,"STDTC","Start date and time of event")
  data <- addLabel__(data,"ENDTC","End date and time of event")
  data <- addLabel__(data,"DATEDAY","Start date of event")
  data <- addLabel__(data,"DATETIME","Start time of event")
  data <- addLabel__(data,"DURATION","Duration of event")
  data <- addLabel__(data,"PROFNR","Profile number")
  data <- addLabel__(data,"PROFTIME","Profile time")
  data <- addLabel__(data,"TYPENAME","Type of event")
  data <- addLabel__(data,"VALUE","Value of event defined by NAME")
  data <- addLabel__(data,"VALUETXT","Text version of value")
  data <- addLabel__(data,"OCC","Index of different occasions for IOV")
  data <- addLabel__(data,"ULOQ","Upper limit of quantification")
  data <- addLabel__(data,"LLOQ","Lower limit of quantification")
  data <- addLabel__(data,"AE","Adverse event record flag")
  data <- addLabel__(data,"AEGRADE","Adverse event grade")
  data <- addLabel__(data,"AESER","Adverse event seriousness")
  data <- addLabel__(data,"AEDRGREL","Adverse event drug related")
  data <- addLabel__(data,"COMMENT","Comment")
  data <- addLabel__(data,"CONDITION","Condition name")
  covInfo <- attr(data,"covInfo")
  if (!is.null(covInfo)) {
    if (nrow(covInfo)>0) {
      for (k__ in 1:nrow(covInfo)) {
        if (covInfo$TIME.VARYING[k__]) {
          text <- paste0(covInfo$NAME[k__]," (",covInfo$UNIT[k__],")")
        } else {
          text <- paste0("Baseline ",covInfo$NAME[k__]," (",covInfo$UNIT[k__],")")
        }
        data <- addLabel__(data,covInfo$COLNAME[k__],text)
      }
    }
  }
  catInfo <- attr(data,"catInfo")
  if (!is.null(catInfo)) {
    if (nrow(catInfo)>0) {
      for (k__ in 1:nrow(catInfo)) {
        if (catInfo$TIME.VARYING[k__]) {
          text <- paste0(catInfo$NAME[k__]," (time varying)")
        } else {
          text <- catInfo$NAME[k__]
        }
        data <- addLabel__(data,catInfo$COLNAME[k__],text)
      }
    }
  }
  TAD_Dcols__ <- names(data)[grepl("TAD_D",names(data))]
  TAD_Dcols_label <- paste0("TAD for ", attr(data,"doseNAMES")[as.numeric(gsub("TAD_D","",TAD_Dcols__))])
  for (k__ in seq_along(TAD_Dcols__)) {
    data <- addLabel__(data,TAD_Dcols__[k__],TAD_Dcols_label[k__])
  }
  DOSE_Dcols__ <- names(data)[grepl("^DOSED",names(data))]
  DOSE_Dcols_label <- paste0("DOSE for ", attr(data,"doseNAMES")[as.numeric(gsub("DOSED","",DOSE_Dcols__))])
  for (k__ in seq_along(DOSE_Dcols__)) {
    data <- addLabel__(data,DOSE_Dcols__[k__],DOSE_Dcols_label[k__])
  }
  for (k__ in seq_along(addColLabels)) {
    colname <- names(addColLabels)[k__]
    data <- addLabel__(data,colname,addColLabels[[k__]])
  }
  return(data)
}
getValueTxtDefine <- function(NAMES,data) {
  data <- unlabel_dataframe(data)
  imputationInformation <- attr(data,"imputeInfo")
  covInfo <- attr(data,"covInfo")
  catInfo <- attr(data,"catInfo")
  doseNAMES <- attr(data,"doseNAMES")
  obsNAMES <- attr(data,"obsNAMES")
  aeNAMES  <- attr(data,"aeNAMES")
  methodBLLOQ <- attr(data,"methodBLLOQ")
  if (length(which(grepl(":::",unique(data$NAME)))) > 0) {
    doseNAMES <- gsub(" ",":::",doseNAMES)
    obsNAMES <- gsub(" ",":::",obsNAMES)
  }
  output__ <- rep(" ",length(NAMES))
  for (k__ in seq_along(NAMES)) {
    NAME <- NAMES[k__]
    if (NAME=="IXGDF")
      output__[k__] <- "1...N"
    if (NAME=="IGNORE")
      output__[k__] <- "Reason for ignoring an observation record in the analysis. If NA then the record is not ignored"
    if (NAME=="USUBJID")
      output__[k__] <- "Unique subject ID or a derivative of it, allowing to identify the subject"
    if (NAME=="ID")
      output__[k__] <- "Numeric subject ID for modeling software"
    if (NAME=="STUDY")
      output__[k__] <- paste0(sort(unique(data$STUDY)),collapse=", \n")
    if (NAME=="STUDYN") {
      if ("STUDY" %in% names(data)) {
        x <- dplyr::arrange(unique(data[,c("STUDYN","STUDY")]),STUDYN)
        output__[k__] <- paste0(paste0(x$STUDYN," (STUDY=",x$STUDY,")"),collapse=", \n")
      } else {
        output__[k__] <- paste0(unique(data$STUDYN),collapse=", ")
      }
    }
    if (NAME=="STUDYDES")
      output__[k__] <- "See 'LABEL' column"
    if (NAME=="TRTNAME")
      output__[k__] <- paste0(sort(unique(data$TRTNAME)),collapse=", \n")
    if (NAME=="TRT") {
      if ("TRTNAME" %in% names(data)) {
        x <- dplyr::arrange(unique(data[,c("TRT","TRTNAME")]),TRT)
        output__[k__] <- paste0(paste0(x$TRT," (TRTNAME=",x$TRTNAME,")"),collapse=", \n")
      } else {
        output__[k__] <- paste0(unique(data$TRT),collapse=", ")
      }
    }
    if (NAME=="TRTNAMER")
      output__[k__] <- paste0(sort(unique(data$TRTNAMER)),collapse=", \n")
    if (NAME=="TRTR") {
      if ("TRTNAMER" %in% names(data)) {
        x <- dplyr::arrange(unique(data[,c("TRTR","TRTNAMER")]),TRTR)
        output__[k__] <- paste0(paste0(x$TRTR," (TRTNAMER=",x$TRTNAMER,")"),collapse=", \n")
      } else {
        output__[k__] <- paste0(unique(data$TRTR),collapse=", ")
      }
    }
    if (NAME=="CENTER")
      output__[k__] <- "See 'LABEL' column"
    if (NAME=="SUBJECT")
      output__[k__] <- "See 'LABEL' column"
    if (NAME=="INDNAME")
      output__[k__] <- paste0(sort(unique(data$INDNAME)),collapse=", \n")
    if (NAME=="IND") {
      if ("INDNAME" %in% names(data)) {
        x <- dplyr::arrange(unique(data[,c("IND","INDNAME")]),IND)
        output__[k__] <- paste0(paste0(x$IND," (INDNAME=",x$INDNAME,")"),collapse=", \n")
      } else {
        output__[k__] <- paste0(unique(data$IND),collapse=", ")
      }
    }
    if (NAME=="COMPOUND")
      output__[k__] <- paste0(sort(unique(data$COMPOUND)),collapse=", \n")
    if (NAME=="PART")
      output__[k__] <- "1=part 1, A=part A, ...\n '0' codes for a study with a single part or an undefined value either because information was not available or not needed for the analysis"
    if (NAME=="EXTENS")
      output__[k__] <- "0=core, 1=extension 1, 2=extension 2, ...\n '0': codes for core and also for an undefined value either because information was not available or not needed for the analysis"
    if (NAME=="VISIT")
      output__[k__] <- "'NA' if undefined or information not available / not needed for analysis"
    if (NAME=="VISNAME")
      output__[k__] <- "'UNKNOWN': if undefined or information not available / not needed for analysis"
    if (NAME=="BASE")
      output__[k__] <- "0 for non-baseline, 1 for first, 2 for second, ..."
    if (NAME=="SCREEN")
      output__[k__] <- "0 for non-screening, 1 for first, 2 for second, ..."
    if (NAME=="TIME")
      output__[k__] <- "'TIME' values in the unit defined in the 'TIMEUNIT' column"
    if (NAME=="TIMEPOS")
      output__[k__] <- "'TIMEPOS' values in the unit defined in the 'TIMEUNIT' column"
    if (NAME=="NT")
      output__[k__] <- "'NT' values in the unit defined in the 'TIMEUNIT' column. Can be 'NA' if value is undefined"
    if (NAME=="TAD")
      output__[k__] <- "'TAD' values in the unit defined in the 'TIMEUNIT' column"
    if (NAME=="TIMEUNIT")
      output__[k__] <- paste0("'",unique(data$TIMEUNIT),"'")
    if (NAME=="DATEDAY")
      output__[k__] <- "'UNKNOWN': if undefined or information not available / not needed for analysis"
    if (NAME=="DATETIME")
      output__[k__] <- "'UNKNOWN': if undefined or information not available / not needed for analysis"
    if (NAME=="DURATION")
      output__[k__] <- "'DURATION' values in the unit defined in the 'TIMEUNIT' column. -1 if event ongoing post end of observation period"
    if (NAME=="TYPENAME")
      output__[k__] <- paste0(sort(unique(data$TYPENAME)),collapse=", \n")
    if (NAME=="NAME")
      output__[k__] <- paste0(sort(unique(data$NAME)),collapse=", \n")
    if (NAME=="VALUE")
      output__[k__] <- "'VALUE' values in the unit defined in the 'UNIT' column for the same record"
    if (NAME=="VALUETXT")
      output__[k__] <- "'NA': if undefined or information not available / not needed for analysis"
    if (NAME=="OCC")
      output__[k__] <- "Integer indices, each unique index coding for a distinct occasion to consider in IOV modeling"
    if (NAME=="ULOQ")
      output__[k__] <- "In units as defined in the 'UNIT' column of the same record. 'NA': if undefined or information not available / not needed for analysis"
    if (NAME=="LLOQ")
      output__[k__] <- "In units as defined in the 'UNIT' column of the same record. 'NA': if undefined or information not available / not needed for analysis"
    if (NAME=="AE")
      output__[k__] <- "1 if record codes an adverse event, 0 if not"
    if (NAME=="AEGRADE")
      output__[k__] <- "Can be 1,2,3,4,5. NA for "
    if (NAME=="AESER")
      output__[k__] <- "1: seriuous adverse event, 0: not serious"
    if (NAME=="AEDRGREL")
      output__[k__] <- "1: drug related AE, 0: not drug related"
    if (NAME=="COMMENT")
      output__[k__] <- "Various content - might be 'NA'"
    if (NAME=="UNIT")
      output__[k__] <- "'NA': if undefined or information not available / not needed for analysis"
    if (NAME=="ROUTE")
      output__[k__] <- paste0(paste0(sort(unique(data$ROUTE)),collapse=", \n"),". 'NA' for non-dosing records")
    if (NAME=="II")
      output__[k__] <- "In units as defined in the 'UNIT' column of the same record. '0': if undefined (non-dosing events) or information not available / not needed for analysis"
    if (NAME=="ADDL")
      output__[k__] <- "'0': if undefined (non-dosing events) or information not available / not needed for analysis"
    if (NAME=="EVID")
      output__[k__] <- "1 for dosing records, 0 for non-dosing records"
    if (NAME=="MDV")
      output__[k__] <- "1 for dosing records and ignored observation records, 0 otherwise"
    if (NAME=="CENS")
      output__[k__] <- "1 for observation records below 'LLOQ' if handling in the model via the M3 or M4 method. 0 otherwise"
    if (NAME=="LIMIT")
      output__[k__] <- "Lower censoring limit - only used for MONOLIX. Set to 0 if M4 used."
    if (NAME=="AMT")
      output__[k__] <- "Dose given in the unit of the 'UNIT' column of this record. 0 for non-dosing records"
    if (NAME=="ADM") {
      if ("ROUTE" %in% data$NAME) {
        x <- dplyr::arrange(unique(data[data$NAME %in% doseNAMES,c("NAME","ADM","ROUTE")]),ADM)
        output__[k__] <- paste0(paste0(paste0(x$ADM," (",x$NAME,", Route: ",x$ROUTE,")"),collapse=", \n"),",\n0 or NA (non-dose records)")
      } else {
        x <- dplyr::arrange(unique(data[data$NAME %in% doseNAMES,c("NAME","ADM")]),ADM)
        output__[k__] <- paste0(paste0(paste0(x$ADM," (",x$NAME,")"),collapse=", \n"),",\n0 or NA (non-dose records)")
      }
    }
    if (NAME=="TINF")
      output__[k__] <- "Duration of dose administration / infusion time for dosing records. 0 indicates a bolus admnistration or a non-dosing record"
    if (NAME=="RATE")
      output__[k__] <- "Rate of dose administration for dosing records. 0 indicates a bolus admnistration or a non-dosing record"
    if (NAME=="LNDV") {
      output__[k__] <- "Log transformed version of 'DV'"
    }
    if (NAME=="DOSE")
      output__[k__] <- "In units as defined in the 'UNIT' column of current dose record. 0 for pre-first dose records in a subject"
    if (NAME=="YTYPE") {
      x <- dplyr::arrange(unique(data[data$NAME %in% obsNAMES,c("NAME","YTYPE")]),YTYPE)
      output__[k__] <- paste0(paste0(paste0(x$YTYPE," (",x$NAME,")"),collapse=", \n"),",\n0 or NA (non-observation records)")
    }
    if (NAME=="DV") {
      output__[k__] <- "0 for dosing events.\nFor observation events the value of the observation in the units defined in the 'UNIT' column of this record.\nMissing observations are coded as 'NA'.\n\n"
      if (!is.null(methodBLLOQ)) {
        if (methodBLLOQ %in% c("M3","M4")) output__[k__] <- paste0(output__[k__],"For observations below the 'LLOQ' the value is set to the 'LLOQ' for handling the M3 or M4 method in the modeling.")
        if (methodBLLOQ %in% c("M5","M6")) output__[k__] <- paste0(output__[k__],"For observations below the 'LLOQ' the value is set to the 'LLOQ'/2 for handling the M5 or M6 method in the modeling.")
        if (methodBLLOQ %in% c("M7")) output__[k__] <- paste0(output__[k__],"For observations below the 'LLOQ' the value is set to the 0 for handling the M7 method in the modeling.")
      }
    }
    if (NAME=="CONDITION")
      output__[k__] <- paste0(sort(unique(data$CONDITION)),collapse=", \n")
    if (!is.null(covInfo)) {
      for (kk in 1:nrow(covInfo)) {
        if (NAME==covInfo$COLNAME[kk]) {
          output__[k__] <- "See 'LABEL' column"
        }
      }
    }
    if (!is.null(catInfo)) {
      for (kk__ in 1:nrow(catInfo)) {
        if (NAME==catInfo$COLNAME[kk__]) {
          output__[k__] <- paste0(aux_explodePC(catInfo$VALUES[kk__])," (",aux_explodePC(catInfo$VALUETXT[kk__]),")",collapse=", \n")
        }
      }
    }
    if (grepl("^TADD",NAME))
      output__[k__] <- "'NA' before first administration of the dose"
    if (grepl("^DOSED",NAME))
      output__[k__] <- "'0' before first administration of the dose"
  }
  return(output__)
}
#'@export
rmDosePostLastObs_IQRdataGENERAL <- function (data,filename=NULL) {
  doseRecPostLastObs__ <- do.call(rbind,lapply(split(data,data$USUBJID), function (datak__) {
    maxTimeObs__ <- max(datak__$TIME[datak__$EVID==0 & datak__$YTYPE>0],na.rm=TRUE)
    doseDataPostObs__ <- datak__[datak__$EVID==1 & datak__$TIME > maxTimeObs__,]
  }))
  if (nrow(doseRecPostLastObs__)==0) {
    xtable__ <- data.frame(IXGDF = "-",
                           USUBJID = "-",
                           TIME = "-",
                           NAME = "-",
                           AMT = "-",
                           II = "-",
                           ADDL = "-",
                           ROUTE = "-",
                           COMMENT = "-",stringsAsFactors = FALSE)
    if (!is.null(filename)) {
      IQRoutputTable(xtable__,"Rule for removal: Dose time larger than latest observation time in an individual",
                     sprintf("N=0 dose records post last individual observation have been removed"),
                     report=TRUE,filename=filename)
    } else {
      print(IQRoutputTable(xtable__,"Rule for removal: Dose time larger than latest observation time in an individual",
                           sprintf("N=0 dose records post last individual observation have been removed"),
                           report=FALSE,filename=filename)
      )
    }
    return(data)
  } else {
    cleanedDataWOdoseRecPostLastObs__ <- do.call(rbind,lapply(split(data,data$USUBJID), function (datak__) {
      maxTimeObs__ <- max(datak__$TIME[datak__$EVID==0 & datak__$YTYPE>0],na.rm=TRUE)
      datakDosesPostRemoved__ <- datak__[!(datak__$EVID==1 & datak__$TIME > maxTimeObs__),]
    }))
    xtable__ <- as.data.frame(doseRecPostLastObs__[,c("IXGDF","USUBJID","TIME","NAME","AMT","II","ADDL","ROUTE")])
    xtable__ <- cbind(xtable__,COMMENT = "Dose record post last observation")
    if (!is.null(filename)) {
      IQRoutputTable(xtable__,"Rule for removal: Dose time larger than latest observation time in an individual",
                     sprintf("N=%d dose records post last individual observation have been removed",nrow(xtable__)),
                     report=TRUE,filename=filename)
    } else {
      print(IQRoutputTable(xtable__,"Rule for removal: Dose time larger than latest observation time in an individual",
                           sprintf("N=%d dose records post last individual observation have been removed",nrow(xtable__)),
                           report=FALSE,filename=filename)
      )
    }
    return(cleanedDataWOdoseRecPostLastObs__)
  }
}
#'@export
combine_IQRdataGENERAL <- function(
  data1, data2,
  FLAGkeepAllCols = FALSE,
  FLAGforceOverwriteNLMEcols = TRUE,
  methodBLOQ = NULL,
  FLAGforceOverwriteNUMcols = TRUE
) {
  x1__ <- as.data.frame(data1)
  x2__ <- as.data.frame(data2)
  if (!is_IQRdataGENERAL(data1) | !(is_IQRdataGENERAL(data2)))
    stopIQR("Both inputs need to be IQRdataGENERAL objects.")
  covInfo1__ <- attr(data1, "covInfo")
  catInfo1__ <- attr(data1, "catInfo")
  covInfo2__ <- attr(data2, "covInfo")
  catInfo2__ <- attr(data2, "catInfo")
  doseNames1__ <- attr(data1, "doseNAMES")
  doseNames2__ <- attr(data2, "doseNAMES")
  obsNames1__ <- attr(data1, "obsNAMES")
  obsNames2__ <- attr(data2, "obsNAMES")
  covInfo__ <- unique(rbind(covInfo1__, covInfo2__))
  catInfo__ <- unique(rbind(catInfo1__, catInfo2__))
  if (any(duplicated(covInfo__$COLNAME))) {
    warningIQR("Continous covariate information not consistent.")
  }
  if (any(duplicated(catInfo__$COLNAME))) {
    warningIQR("Categorical covariate information not consistent.")
  }
  obsNames__ <- union(obsNames1__, obsNames2__)
  doseNames__ <- union(doseNames1__, doseNames2__)
  mBLOQcomm__ <- unique(c(attr(data1, "methodBLLOQ"),attr(data2, "methodBLLOQ")))
  if (FLAGforceOverwriteNLMEcols) {
    if (!is.null(methodBLOQ)) {
      mBLOQcomm__ <- "M1"
    } else {
      if (length(mBLOQcomm__) != 1) {
        warningIQR("BLLOQ methods different between datasets. Will be overwritten by default M1 method.")
        mBLOQcomm__ <- "M1"
      }
    }
  } else {
    if (length(mBLOQcomm__) != 1)
      warningIQR("BLLOQ methods different between datasets. Consider overwriting NLME columns.")
  }
  namesCommon__ <- intersect(names(x1__), names(x2__))
  namesCov1__   <- c(covInfo1__$COLNAME, catInfo1__$COLNAME)
  namesCov2__   <- c(covInfo2__$COLNAME, catInfo2__$COLNAME)
  namesU1__       <- setdiff(names(x1__), c(namesCommon__, namesCov1__))
  namesU2__       <- setdiff(names(x2__), c(namesCommon__, namesCov2__))
  if (FLAGkeepAllCols) {
    names1__ <- unique(c(namesCommon__, namesCov1__, namesU1__))
    names2__ <- unique(c(namesCommon__, namesCov2__, namesU2__))
  } else {
    names1__ <- unique(c(namesCommon__, namesCov1__))
    names2__ <- unique(c(namesCommon__, namesCov2__))
  }
  out__ <- plyr::rbind.fill(x1__[, names1__], x2__[, union(namesCommon__, names2__)])
  out__ <- dplyr::arrange(out__,STUDY,USUBJID,TIME,TYPENAME,NAME)
  attr(out__,"class")        <- c("IQRdataGENERAL", attr(out__,"class"))
  if (FLAGforceOverwriteNLMEcols) {
    out__ <- generalData_IDcolumn(out__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_TIMEPOScolumn(out__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_EVIDcolumn(out__,doseNames__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_MDVcolumn(out__,doseNames__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_CENScolumn(out__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_AMTcolumn(out__,doseNames__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_DVcolumn(out__,doseNames__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_TINFcolumn(out__,doseNames__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_RATEcolumn(out__,doseNames__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_ADMcolumn(out__,doseNames__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_TADcolumns(out__,doseNames__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_DOSEcolumns(out__,doseNames__,FLAGforceOverwriteNLMEcols)
    out__ <- generalData_YTYPEcolumn(out__,obsNames__,FLAGforceOverwriteNLMEcols)
    ixmissingDV__ <- which(out__$EVID==0 & out__$MDV==0 & is.na(out__$DV))
    out__$MDV[ixmissingDV__] <- 1
    out__$IGNORE[ixmissingDV__] <- "Missing value"
    ixmissingTIME__ <- which(out__$EVID==0 & out__$MDV==0 & is.na(out__$TIME))
    out__$MDV[ixmissingTIME__] <- 1
    out__$IGNORE[ixmissingTIME__] <- "Missing time"
    out__ <- blloq_IQRdataGENERAL(out__, mBLOQcomm__)
  }
  out__$IXGDF <- 1:dim(out__)[1]
  if (FLAGforceOverwriteNUMcols) {
    if ("INDNAME" %in% names(out__)) out__$IND <- as.numeric(factor(out__$INDNAME))
    if ("STUDY" %in% names(out__)) out__$STUDYN <- as.numeric(factor(out__$STUDY))
    if ("TRTNAME" %in% names(out__)) out__$TRT <- as.numeric(factor(out__$TRTNAME, levels = unique(out__$TRTNAME)))
    if ("TRTNAMER" %in% names(out__)) out__$TRTR <- as.numeric(factor(out__$TRTNAMER, levels = unique(out__$TRTNAMER)))
  }
  attr(out__,"doseNAMES")    <- doseNames__
  attr(out__,"obsNAMES")     <- obsNames__
  attr(out__,"covInfo")      <- covInfo__
  attr(out__,"catInfo")      <- catInfo__
  attr(out__,"methodBLLOQ")  <- mBLOQcomm__
  out__
}
#'@export
"+.IQRdataGENERAL" <- function (data1,data2) {
  out__ <- combine_IQRdataGENERAL(data1,data2)
  return(out__)
}
#'@export
subset.IQRdataGENERAL <- function(x, subset, select, ...) {
  x.attr <- attributes(x)
  x.attr0 <- attributes0(x)
  r <- if (missing(subset))
    rep_len(TRUE, nrow(x))
  else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r))
      stopIQR("'subset' must be logical")
    r & !is.na(r)
  }
  vars <- if (missing(select))
    TRUE
  else {
    nl <- as.list(seq_along(x))
    names(nl) <- names(x)
    eval(substitute(select), nl, parent.frame())
  }
  subx <- x[r, vars, drop = FALSE]
  info <- getColumnsInfo_IQRdataGENERAL()
  removed <- setdiff(names(x), names(subx))
  if (any(removed %in% info$requiredColumns))
    stopIQR("It is not allowed to remove columns that are required by IQRdataGENERAL")
  if (any(removed %in% info$requiredColumnsNLME)) {
    stopIQR("It is not allowed to remove existing columns required for NLME modeling.")
  }
  attr.expected <- c("doseNAMES", "obsNAMES", "covInfo", "catInfo", "regressorNames")
  for (a in intersect(attr.expected, names(x.attr))) {
    if (a == "doseNAMES")  {
      doseNAMES <- intersect(x.attr[["doseNAMES"]], unique(subx[["NAME"]]))
      Dx <- ifelse(length(x.attr[["doseNAMES"]]) > 1, "D", "")
      index <- match(doseNAMES, x.attr[["doseNAMES"]]) 
      TAD_Dx <- unclass(subx)[grepl(paste0("^TAD", Dx), colnames(subx))] 
      DOSE_Dx <- unclass(subx)[grepl(paste0("^DOSE", Dx), colnames(subx))] 
      subx <- subx[, !grepl("^TADD", colnames(subx)) & !grepl("^DOSED", colnames(subx))] 
      if (length(doseNAMES) == 0) { 
        subx[["TAD"]] <- NA
        subx[["DOSE"]] <- 0
      }
      if (length(doseNAMES) == 1) { 
        subx[["TAD"]] <- TAD_Dx[[index]]
        subx[["DOSE"]] <- DOSE_Dx[[index]]
      }
      if (length(doseNAMES) > 1) { 
        for (i in 1:length(index)) {
          subx[[paste0("TADD", i)]] <- TAD_Dx[[index[i]]]
          subx[[paste0("DOSED", i)]] <- DOSE_Dx[[index[i]]]
        }
        DOSE_matrix <- do.call(cbind, subx[grepl("^DOSED", names(subx))])
        TAD_matrix <- do.call(cbind, subx[grepl("^TADD", names(subx))])
        minIndex <- apply(TAD_matrix, 1, which.min)
        minIndex <- sapply(seq_along(minIndex), function(i) ifelse(length(minIndex[[i]])==0, 1, minIndex[[i]]))
        subx[["TAD"]] <- sapply(seq_along(minIndex), function(i) TAD_matrix[i, minIndex[i]])
        subx[["DOSE"]] <- sapply(seq_along(minIndex), function(i) DOSE_matrix[i, minIndex[i]])
      }
      adms.present <- sort(unique(c(0, subx[["ADM"]])))
      adms.replace <- seq_along(adms.present) - 1
      subx[["ADM"]] <- adms.replace[match(subx[["ADM"]], adms.present)]
      attr(subx, "doseNAMES") <- doseNAMES
    }
    if (a == "obsNAMES")  {
      obsNAMES <- intersect(x.attr[["obsNAMES"]], unique(subx[["NAME"]]))
      attr(subx, "obsNAMES") <- obsNAMES
    }
    if (a == "covInfo")  {
      covInfo <- x.attr[["covInfo"]]
      select <- (covInfo[["COLNAME"]] %in% colnames(subx))
      attr(subx, "covInfo") <- covInfo[select, ]
    }
    if (a == "catInfo") {
      catInfo <- x.attr[["catInfo"]]
      select <- (catInfo[["COLNAME"]] %in% colnames(subx))
      catInfo <- catInfo[select, ]
      if (any(select)) {
        valuetxt <- strsplit(catInfo[["VALUETXT"]], split = ","); names(valuetxt) <- catInfo[["COLNAME"]]
        values <- strsplit(catInfo[["VALUES"]], split = ","); names(values) <- catInfo[["COLNAME"]]
        new_values <- lapply(1:nrow(catInfo), function(i){
          with(catInfo[i, ], sort(unique(subx[[COLNAME]])))
        })
        VALUETXT <- sapply(1:length(new_values), function(i) {
          paste(valuetxt[[i]][match(new_values[[i]], values[[i]])], collapse = ",")
        })
        VALUES <- sapply(1:length(new_values), function(i) {
          paste(new_values[[i]], collapse = ",")
        })
        catInfo[["VALUETXT"]] <- VALUETXT
        catInfo[["VALUES"]] <- VALUES
        catInfo <- catInfo[catInfo[["VALUES"]] != "",]
      }
      attr(subx, "catInfo") <- catInfo
    }
    if (a == "regressorNames") {
      regressorNames <- intersect(x.attr[["regressorNames"]], colnames(subx))
      attr(subx, "regressorNames") <- regressorNames
    }
  }
  n <- setdiff(names(x.attr0), c("doseNAMES", "obsNAMES", "covInfo", "catInfo", "regressorNames"))
  if (length(n) > 0)
    attributes(subx) <- c(attributes(subx), x.attr0[n])
  return(subx)
}
#'@export
attributes0 <- function(x) UseMethod("attributes0", x)
#'@export
attributes0.IQRdataGENERAL <- function(x) {
  allAttributes <- attributes(x)
  exclude <- c("names", "row.names", "class")
  allAttributes[setdiff(names(allAttributes), exclude)]
}
#'@export
obsNAMES <- function(x) UseMethod("obsNAMES", x)
#'@export
obsNAMES.IQRdataGENERAL <- function(x) attr(x, "obsNAMES")
#'@export
doseNAMES <- function(x) UseMethod("doseNAMES", x)
#'@export
doseNAMES.IQRdataGENERAL <- function(x) attr(x, "doseNAMES")
#'@export
covInfo <- function(x) UseMethod("covInfo", x)
#'@export
covInfo.IQRdataGENERAL <- function(x) attr(x, "covInfo")
#'@export
catInfo <- function(x) UseMethod("catInfo", x)
#'@export
catInfo.IQRdataGENERAL <- function(x) attr(x, "catInfo")
#'@export
addIndivRegressors_IQRdataGENERAL <- function(
  projectPath,
  dataPath        = NULL,
  data            = NULL,
  filename        = NULL,
  model           = NULL,
  regressorNames  = NULL,
  doseNAMES       = NULL,
  obsNAMES        = NULL,
  FLAGrmNoInd     = FALSE,
  FLAGnoCovSample = FALSE,
  FLAGxpt         = FALSE,
  FLAGdefine      = TRUE,
  FLAGzip         = FALSE,
  addColLabels    = NULL) {
  if (!(is.null(regressorNames) | "character" %in% class(regressorNames)) ) {
    stopIQR("regressorNames need to be NULL or of class character.")
  }
  if (!(is.null(model) | is_IQRmodel(model)) ) {
    if (!file.exists(model)) {
      stopIQR("modelFile need to be NULL, IQR model or path to model file.")
    }
  }
  if (is.null(dataPath) & is.null(data)) {
    stopIQR("Please provide either dataPath or data input argument")
  }
  if (!is.null(dataPath) & !is.null(data)) {
    stopIQR("Please provide either dataPath or data input argument")
  }
  if (!is.null(dataPath)) {
    data__ <- load_IQRdataGENERAL(dataPath)
  } else {
    data__ <- data
  }
  if (!is_IQRdataGENERAL(data__)) {
    stopIQR("Data is not a saved IQRdataGENERAL object")
  }
  projInfo__ <- parseNLMEprojectHeader(projectPath)
  dataProj__ <- IQRloadCSVdata(file.path(projectPath, projInfo__$DATA))
  subjProj__ <- unique(dataProj__[,c("ID","USUBJID")])
  indivPars__ <- getIndivParameters_IQRnlmeProject(projectPath, FLAGcovariate = FALSE)
  indivPars__ <- merge(indivPars__, subjProj__)
  indivPars__$ID <- NULL
  subjNoIndPar    <- setdiff(unique(data__$USUBJID), indivPars__$USUBJID)
  if (length(subjNoIndPar) > 0) {
    if (FLAGrmNoInd) {
    } else {
      if (all(projInfo__$COVARIATESUSED != "") & !FLAGnoCovSample) {
        if ((!all(projInfo__$COVARIATESUSED %in% names(data__)))) {
          warningIQR("Data set does not contain all covariates in the model.\nParameters for subjects for which estimates are not available will not be adjusted by covariates.")
          popPars__  <- suppressWarnings(getPopParameters_IQRnlmeProject(projectPath,verbose=FALSE))
          popParsDF__  <- as.data.frame(t(replicate(length(subjNoIndPar),popPars__)))
          popParsDF__$USUBJID <- subjNoIndPar
        } else {
          IndCov__   <- unique(data__[data__$USUBJID %in% subjNoIndPar, c("USUBJID", projInfo__$COVARIATESUSED)])
          contCovNames__ <- intersect(projInfo__$COVNAMES, names(IndCov__))
          for (kccn in seq_along(contCovNames__)) {
            idxccn__ <- grep(contCovNames__[kccn], projInfo__$BETACOVNAMES)[1]
            refValue__ <- as.numeric(gsub(")","",gsub("log(cov/", "", projInfo__$BETACOVTRANS[idxccn__], fixed = TRUE), fixed= TRUE))
            idx0__ <- IndCov__[[contCovNames__[kccn]]] == 0 | is.na(IndCov__[[contCovNames__[kccn]]])
            if (any(idx0__)) {
              warningIQR("There are continuous covariate values that are zero or NA. Zero is not feasible since log-transformation is applied.\n   For all subjects with NA or zero reference covariate value will be used.")
              IndCov__[[contCovNames__[kccn]]][idx0__] <- refValue__
            }
          }
          popParsDF__  <- getPopParameters_IQRnlmeProject(projectPath, IndCovariates=IndCov__,verbose=FALSE)
          popParsDF__$USUBJID <- IndCov__$USUBJID
        }
      } else {
        popPars__  <- suppressWarnings(getPopParameters_IQRnlmeProject(projectPath,verbose=FALSE))
        popParsDF__  <- as.data.frame(t(replicate(length(subjNoIndPar),popPars__)))
        popParsDF__$USUBJID <- subjNoIndPar
      }
      indivPars__ <- rbind(indivPars__, popParsDF__)
    }
  }
  if (!is.null(model)) {
    if (!is_IQRmodel(model))
      model <- IQRmodel(model)
    modPars__ <- names(model$parameters)
    indivPars__ <- indivPars__[,c("USUBJID", intersect(names(indivPars__), modPars__) )]
  }
  if (!is.null(regressorNames)) {
    notAvail__ <- setdiff(regressorNames, names(indivPars__))
    if (length(notAvail__) > 0) {
      warningIQR("Some required regressorNames do not exist in NLME model.")
    }
    indivPars__ <- indivPars__[,c("USUBJID", intersect(names(indivPars__), regressorNames) )]
  }
  cat("The following parameters are added to the dataset as regression parameters:\n")
  AddRegrNames__ <- setdiff(names(indivPars__), "USUBJID")
  if (length(AddRegrNames__) == 0) {
    stopIQR("no regression variables to be added.")
  } else {
    print(paste0(AddRegrNames__, collapse = ", "))
  }
  colIndexist <- setdiff(intersect(names(data__), names(indivPars__)), "USUBJID")
  if (length(colIndexist) > 0) {
    cat("The following regression colums exist and will be substituted:\n")
    print(paste0(colIndexist, collapse = ", "))
    data__[,colIndexist] <- NULL
  }
  attrData__                <- attributes(data__)
  data__                    <- merge(data__, indivPars__, by = "USUBJID", all = FALSE, sort = FALSE)
  attrData__$names          <- names(data__)
  attrData__$row.names      <- row.names(data__)
  attrData__$regressorNames <- unique(c(attrData__$regressorNames, AddRegrNames__))
  class(data__)             <- c("IQRdataGENERAL", "data.frame")
  attributes(data__)        <- attrData__
  regressorNames__ <- NULL
  if (!is.null(regressorNames)) {
    regressorNames__ <- intersect(regressorNames, attrData__$regressorNames)
  }
  if (!is.null(model)) {
    regressorNames__ <- intersect(modPars__, attrData__$regressorNames)
  }
  if (!is.null(regressorNames__)) {
    colIndex__ <- seq_along(data__)
    colIndex.regressors__ <- match(regressorNames__, names(data__))
    colIndex__[colIndex__ %in% colIndex.regressors__] <- colIndex.regressors__
    data__ <- data__[colIndex__]
    attrData__$names          <- names(data__)
    attrData__$regressorNames <- regressorNames__
    class(data__)             <- c("IQRdataGENERAL", "data.frame")
    attributes(data__)        <- attrData__
  }
  attrX__ <- attributes(data__)
  data__ <- dplyr::arrange(data__,ID,TIME,YTYPE)
  attributes(data__) <- attrX__
  if (!is.null(filename)) {
    exportNLME_IQRdataGENERAL(data            = data__,
                              regressorNames  = attrData__$regressorNames,
                              doseNAMES       = doseNAMES,
                              obsNAMES        = obsNAMES,
                              filename        = filename,
                              FLAGxpt         = FLAGxpt,
                              FLAGdefine      = FLAGdefine,
                              FLAGzip         = FLAGzip,
                              addColLabels    = addColLabels)
    return(NULL)
  }
  if (!is.null(doseNAMES)) {
    if (length(setdiff(doseNAMES,attr(data__,"doseNAMES"))) > 0) {
      stopIQR("Provided input argument doseNAMES contains element(s) that are not defined as doses")
    }
    data__$ADM[data__$EVID==1] <- 999
    data__$YTYPE[data__$EVID==1] <- 0 
    for (k in seq_along(doseNAMES)) {
      data__$ADM[data__$NAME == doseNAMES[k]] <- k
    }
  } else {
    doseNAMES <- attr(data__,"doseNAMES")
  }
  data__ <- data__[data__$ADM != 999,]
  if (!is.null(obsNAMES)) {
    if (length(setdiff(obsNAMES,attr(data__,"obsNAMES"))) > 0) {
      stopIQR("Provided input argument obsNAMES contains element(s) that are not defined as observations.")
    }
    data__$YTYPE[data__$EVID==0] <- 999 
    data__$ADM[data__$EVID==0] <- 0
    for (k in seq_along(obsNAMES)) {
      data__$YTYPE[data__$NAME == obsNAMES[k]] <- k
    }
  } else {
    obsNAMES <- attr(data__,"obsNAMES")
  }
  data__ <- data__[data__$YTYPE != 999,]
  attr(data__,"doseNAMES") <- doseNAMES
  attr(data__,"obsNAMES") <- obsNAMES
  return(list(regressorNames=AddRegrNames__, data=data__))
}
#'@importFrom utils zip
#'@export
zip_IQRdataGENERAL <- function(filenameinput, filenameoutput, FLAGpurge = FALSE) {
  pathname__ <- aux_fileparts(filenameinput)[["pathname"]]
  basename <- gsub("\\..*", "", basename(filenameinput)) 
  oldpath__ <- getwd()
  setwd(pathname__)
  files__ <- dir(pattern=paste0(basename,"\\."))
  files__ <- grep("zip", files__, invert = TRUE, value = TRUE)
  utils::zip(filenameoutput,files__)
  utils::zip()
  if (FLAGpurge) unlink(files__)
  setwd(oldpath__)
}
#'@export
#'@importFrom data.table fread
IQRloadCSVdata <- function(filename) {
  is_zip__ <- grepl("\\.csv\\.zip$", filename, ignore.case = TRUE)
  if (is_zip__) {
    outdir__ <- file.path(tempdirIQR(), "CSV")
    unlink(outdir__, recursive = TRUE)
    utils::unzip(filename, exdir = outdir__)
    myfiles__ <- list.files(outdir__, full.names = TRUE)
    if (length(myfiles__) == 0) stopIQR("Zip archive did not contain files.")
    if (length(myfiles__) > 1) stopIQR("Automatic CSV reading from zip file with multiple files is not supported.")
    if (!grepl("\\.csv$", myfiles__)) stopIQR("The zip file did not contain a csv file.")
    data__ <- as.data.frame(data.table::fread(myfiles__, na.strings=c(".","", "NA","NaN"), strip.white = TRUE,showProgress=FALSE))
    unlink(outdir__, recursive = TRUE)
    filenameATR__ <- paste0(aux_strrep(filename, ".csv.zip",""),".atr")
  } else {
    data__ <- as.data.frame(data.table::fread(filename,
                                              na.strings=c(".","", "NA","NaN"),
                                              strip.white = TRUE))
    filenameATR__ <- gsub('\\.csv(.gz)?$','.atr', filename)
  }
  atrcontents <- loadAttributeFile(filenameATR__)
  attributes(data__) <- c(attributes(data__),atrcontents)
  return(data__)
}
#'@export
#'@importFrom data.table fwrite
IQRsaveCSVdata <- function(data,filename,na=".",quote=FALSE,row.names=FALSE,FLAGattributes=TRUE,replaceComma=NULL) {
  if (is.null(filename)) return()
  aux_mkdir(aux_fileparts(filename)$pathname)
  containsComma__ <- unlist(lapply(data, function(x__) {
    if (is.character(x__)||is.factor(x__)){
      return(any(grepl(",", x__)))
    } else {
      return(FALSE)
    }
  }))
  if (!is.null(replaceComma) & any(containsComma__)) {
    for (i__ in which(containsComma__)) data[[i__]] <- gsub(",", replaceComma, data[[i__]])
  }
  if (is.null(replaceComma) & any(containsComma__)) {
    stopIQR("Column(s) ", paste(names(containsComma__)[containsComma__], collapse = ", "),
            " contain(s) comma. Cannot export as csv.\n",
            "  Use argument 'replaceComma' to define a replacement character.")
  }
  if (!grepl(".csv",filename)) filename <- paste0(filename,".csv")
  data.table::fwrite(x=data, file=filename, na=na, quote=quote, row.names=row.names)
  if (FLAGattributes) {
    attrStandard__ <- c("names","row.names","class", ".internal.selfref") 
    attr__ <- attributes(data)
    for (k in seq_along(attrStandard__)) {
      attr__[[attrStandard__[[k]]]] <- NULL
    }
    if (length(attr__) == 0) return(invisible(NULL))
    ATTRTEXT__ <- paste0("# Attributes file for dataset ",filename,"\n\n")
    ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents <- list()\n\n")
    for (k in seq_along(attr__)) {
      n__ <- names(attr__[k])
      v__ <- attr__[[k]]
      ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$",n__," <- ",paste0(deparse(v__),collapse=""),"\n\n")
    }
    filenameATR <- gsub('\\.csv(.gz)?$','.atr', filename)
    aux_filewrite(ATTRTEXT__,filenameATR)
  }
}
#'@export
IQRloadSASdata <- function(data_file,as.data.frame=TRUE,noLabels=TRUE,replaceComma=TRUE) {
  if (grepl(".xpt",tolower(data_file),fixed = TRUE)){
    data__ <- haven::read_xpt(data_file)
  } else {
    if (grepl(".sas7bdat",tolower(data_file),fixed = TRUE)){
      data__ <- haven::read_sas(data_file)
    } else {
      stopIQR("Unknown data file extension")
    }
  }
  if (as.data.frame) data__ <- as.data.frame(data__)
  if (noLabels) {
    data__ <- unlabel_dataframe(data__)
  }
  flagmessage <- TRUE
  if (replaceComma) {
    for (k in seq_along(data__)) {
      col <- data__[[k]]
      if (is.character(col)) {
        newcol <- gsub(","," ",col)
        if (any(newcol!=col) & flagmessage) {
          message("Commata in character elements removed from the data during import and replaced by space (' ')")
          flagmessage <- FALSE
        }
      } else {
        newcol <- col
      }
      data__[[k]] <- newcol
    }
  }
  return(data__)
}
#'@export
unlabel_dataframe <- function(data,removeLabelledClass=TRUE,removeLabel=FALSE) {
  ddddd__ <- sapply(names(data), function (name) {
    if (removeLabel) attr(data[[name]],"label") <<- NULL
    if (removeLabelledClass) class(data[[name]]) <<- setdiff(class(data[[name]]),"labelled")
  })
  return(data)
}
#'@export
removeCommata_dataframe <- function(data,replaceComma=" ") {
  ddddd__ <- sapply(names(data), function (name) {
    if (!is.numeric(data[[name]])) data[[name]] <<- gsub(",",replaceComma,data[[name]])
  })
  return(data)
}
#'@export
getLabels_dataframe <- function (data,orderAlphabetically=FALSE,table=TRUE) {
  out <- do.call(rbind,lapply(names(data), function (name) {
    x <- attr(data[[name]],"label")
    if (is.null(x)) x <- ""
    data.frame(
      COLNAME = name,
      COLLABEL = x,
      stringsAsFactors = FALSE
    )
  }))
  if (orderAlphabetically) out <- dplyr::arrange(out,COLNAME)
  if (table) {
    out <- IQRoutputTable(xtable = out,xtitle="Content information of data.frame")
  }
  out
}
#'@export
date2time_IQRdataProgramming <- function (dateString,format="%Y-%m-%dT%H:%M") {
  time_seconds <- as.numeric(as.POSIXct(dateString,format=format))
  time_minutes <- time_seconds/60
  time_hours <- time_minutes/60
  time_days <- time_hours/24
  time_weeks <- time_days/7
  time_years <- time_days/365
  out <- list(
    time_seconds = time_seconds,
    time_minutes = time_minutes,
    time_hours = time_hours,
    time_days = time_days,
    time_weeks = time_weeks,
    time_years = time_years
  )
  out
}
#'@export
date2dateday_IQRdataProgramming <- function (dateString,format="%Y-%m-%dT%H:%M",formatday="%Y-%m-%d") {
  z <- as.POSIXct(dateString,format=format)
  out <- format(z,formatday)
  out
}
#'@export
date2datetime_IQRdataProgramming <- function (dateString,format="%Y-%m-%dT%H:%M",formattime="%H:%M") {
  z <- as.POSIXct(dateString,format=format)
  out <- format(z,formattime)
  out
}
#'@export
import_IQRsysData <- function(data, CONDITION = NULL, ID = "ID", TIME = "TIME", DV = "DV", YTYPE = "YTYPE",
                              covNames = NULL, catNames = NULL, regressorNames = NULL) {
  if (is.character(data))
    data__ <- IQRloadCSVdata(data)
  else if (is.data.frame(data))
    data__ <- data
  else
    stopIQR("Data is neither a filename nor a data frame.")
  if (is.null(CONDITION)) {
    if ("CONDITION" %in% names(data__))
      CONDITION <- "CONDITION"
    else
      CONDITION <- "ID"
  }
  standardCols__ <- c(CONDITION, ID, TIME, DV, YTYPE)
  requestedCols__ <- unique(c(standardCols__, covNames, catNames, regressorNames))
  contained__ <- sapply(requestedCols__, function(n) n %in% names(data__))
  if (!all(contained__))
    stopIQR("The following requested columns are not provided by the data file: ", paste(requestedCols__[!contained__], collapse = ", "))
  out__ <- data.frame(
    CONDITION        = as.character(data__[[CONDITION]]),
    ID               = as.integer(  data__[[ID]]),
    TIME             = as.double(   data__[[TIME]]),
    DV               = as.double(   data__[[DV]]),
    YTYPE            = as.integer(  data__[[YTYPE]]),
    stringsAsFactors = FALSE
  )
  nlmeCols__ <- c("IXGDF", "USUBJID", "ID", "TIME", "TIMEPOS", "TAD", "DV",
                  "MDV", "EVID", "CENS", "AMT", "ADM", "RATE", "TINF", "YTYPE")
  toImport__ <- setdiff(intersect(nlmeCols__, names(data__)), names(out__))
  for (n in toImport__) {
    out__[[n]] <- data__[[n]]
  }
  user__ <- setdiff(names(data__), standardCols__)
  for (n in user__) {
    out__[[n]] <- data__[[n]]
  }
  toImpute__ <- setdiff(c("IXGDF", "USUBJID", "MDV", "EVID", "CENS", "AMT", "ADM", "TINF", "RATE"), names(out__))
  if (length(toImpute__) > 0) {
    IDs__ <- unique(out__[["ID"]])
    for (n in toImpute__) {
      out__[[n]] <- switch(
        n,
        "IXGDF"   = seq_along(out__[[1]]),
        "USUBJID" = paste0("SJ", match(out__[["ID"]], IDs__)),
        "MDV"     = ifelse(out__[["YTYPE"]] == 0 | is.na(out__[["YTYPE"]]), 1, 0),
        "CENS"    = rep(0, length(out__[[1]])),
        "EVID"    = ifelse(out__[["YTYPE"]] == 0 | is.na(out__[["YTYPE"]]), 1, 0),
        "AMT"     = 0,
        "ADM"     = 0,
        "TINF"    = 0,
        "RATE"    = 0
      )
    }
  }
  for (n in covNames) out__[[n]] <- as.numeric(data__[[n]])
  for (n in catNames) {
    levels__ <- unique(sort(utils::type.convert(as.character(data__[[n]]))))
    out__[[n]] <- as.integer(factor(as.character(data__[[n]]), levels = as.character(levels__)))
  }
  for (n in regressorNames) out__[[n]] <- as.numeric(data__[[n]])
  out__ <- out__[order(out__[["ID"]], out__[["TIME"]]),]
  if (!"TAD" %in% names(out__)) {
    out__ <- IQRcalcTAD(out__)
  }
  if (!"TIMEPOS" %in% names(out__)) {
    out__ <- do.call(rbind,lapply(split(out__,out__[[ID]]), function (d__) {
      d__$TIMEPOS <- d__$TIME-d__$TIME[1]
      d__
    }))
  }
  if (!"ADDL" %in% names(out__)) out__$ADDL <- 0
  if (!"II" %in% names(out__)) out__$II <- 0
  out__$ADDL[is.na(out__$ADDL)] <- 0
  out__$II[is.na(out__$II)] <- 0
  if (any(out__$ADDL!=0 & out__$II==0)) stopIQR("ADDL non-zero/NA defined when II undefined")
  if (any(out__$ADDL==0 & out__$II!=0)) stopIQR("II non-zero/NA defined when ADDL undefined")
  add__ <- as.data.frame(out__[out__$ADDL!=0,])
  if (nrow(add__) > 0) {
    adddoses__ <- do.call(rbind,lapply(1:nrow(add__), function (k__) {
      row__ <- add__[k__,]
      addrows__ <- row__[rep(1,row__$ADDL+1),]
      addrows__$NNN__ <- 0:row__$ADDL
      addrows__$TIME <- addrows__$TIME+addrows__$NNN__*addrows__$II
      addrows__$TIMEPOS <- addrows__$TIMEPOS+addrows__$NNN__*addrows__$II
      addrows__$TAD <- 0
      addrows__$NNN__ <- NULL
      addrows__$ADDL <- 0
      addrows__$II <- 0
      addrows__
    }))
    out__ <- out__[out__$ADDL==0,]
    out__ <- rbind(
      out__,
      adddoses__
    )
    out__ <- dplyr::arrange(out__,CONDITION,ID,TIME,YTYPE)
  }
  out__ <- IQRnlmeData(out__, covNames, catNames, regressorNames,FLAGqsp=TRUE)
  class(out__) <- c("IQRsysData", class(out__))
  return(out__)
}
#'@export
IQRsysData <- function(input, covNames = NULL, catNames = NULL, regressorNames = NULL) {
  data__ <- IQRnlmeData(input, covNames, catNames, regressorNames,FLAGqsp=TRUE)
  if (!"CONDITION" %in% names(data__))
    stopIQR("CONDITION column is missing but is required for IQRsysData. Cosider importing with import_IQRsysData().")
  data__[["CONDITION"]] <- as.character(data__[["CONDITION"]])
  if (is.character(input)) {
    attr(data__, "filename") <- input
  } else {
    attr(data__, "filename") <- NA
  }
  class(data__) <- c("IQRsysData", class(data__))
  return(data__)
}
#'@export
exportSYS_IQRdataGENERAL <- function(data,
                                     regressorNames = NULL,
                                     doseNAMES      = NULL,
                                     obsNAMES       = NULL,
                                     filename       = NULL,
                                     FLAGxpt        = FALSE,
                                     FLAGdefine     = TRUE,
                                     FLAGzip        = FALSE,
                                     addColLabels   = NULL) {
  if (!"CONDITION" %in% names(data))
    data[["CONDITION"]] <- as.character(data[["ID"]])
  exportNLME_IQRdataGENERAL(
    data = data,
    regressorNames = regressorNames,
    doseNAMES = doseNAMES,
    obsNAMES = obsNAMES,
    filename = filename,
    FLAGxpt = FLAGxpt,
    FLAGdefine = FLAGdefine,
    FLAGzip = FLAGzip,
    addColLabels = addColLabels
  )
}
export_IQRsysData <- function(data, filename, ...) {
  filename__ <- paste0(aux_strrep(filename, ".csv", ""), ".csv")
  IQRoutputCSV(data = data, filename = filename__, ...)
}
#'@export
IQRsysEst <- function(model, dosing, data,
                      modelSpec = NULL, dMod_dataSpec = NULL,
                      ...) {
  if (!is.null(modelSpec$COVcentering))
    warningIQR(paste0("Custom centering values are not yet implemented in IQRtools SYSFIT functionality.",
                      "Default reference values generated from the data will be used.", collapse = "\n"))
  parameter_fields__ <- setNames(nm = c("POPvalues0", "POPestimate", "IIVdistribution", "IIVvalues0", "IIVestimate"))
  if (!all(parameter_fields__ %in% names(modelSpec))) {
    stopIQR("For the definition of modelSpec argument please use the modelSpec_IQRsysEst() function")
  }
  sysFit_parameters__ <- do.call(dMod_fill_sysEst_parameters, modelSpec[parameter_fields__])
  modelSpec__ <- modelSpec
  modelSpec__[parameter_fields__] <- sysFit_parameters__[parameter_fields__]
  est <- IQRnlmeEst(model, dosing, data, modelSpec__,FLAGqsp=TRUE)
  if (is.null(attr(est$model, "original")))
    attr(est$model, "original") <- est$model
  parsOfInterest__ <- list(...)[["parsOfInterest"]]
  if (is.null(parsOfInterest__))
    parsOfInterest__ <- union(names(modelSpec$POPvalues0),
                              sapply(names(modelSpec$LOCvalues0), function(n__) paste(n__, names(modelSpec$LOCvalues0[[n__]]), sep = "_")))
  args0__ <- list(
    Scenario = list(
      model = "IQRmodel",
      dosing = "NLME",
      data = "NLME",
      spec = "modelSpec"
    ),
    model = model,
    data = data,
    dosing = dosing,
    modelSpec = modelSpec,
    estSpec = modelSpec,
    parsOfInterest = parsOfInterest__
  )
  class(est) <- c("IQRsysEst", class(est))
  attr(est, "args0") <- args0__
  return(est)
}
#'@export
as_IQRsysEst <- function(sysModel, modelSpec = NULL) {
  if (all(sapply(sysModel$data[[1]], function(x__) nrow(x__) == 0)))
    stopIQR("No data was registered with your sysModel. Cannot convert to estimation object without data.")
  attributes__ <- attributes(sysModel)
  sysModelEst__ <- attributes__[["sysModelEst"]]
  model__ <- sysModelEst__[["model"]]
  dosing__ <- sysModelEst__[["dosing"]]
  data__ <- sysModelEst__[["data"]]
  estSpec__ <- sysModelEst__[["estSpec"]]
  parsOfInterest__ <- sysModelEst__[["parsOfInterest"]]
  fields__ <- c("POPvalues0", "POPestimate",
                "IIVdistribution", "IIVvalues0", "IIVestimate",
                "LOCmodel", "LOCvalues0", "LOCestimate")
  estSpec__[fields__] <- sysModel$parameters[[1]][fields__]
  modelSpec0__ <- modelSpec 
  merge__ <- function(x__, y__) c(y__, x__)[names(x__)]
  if (!is.null(modelSpec0__)) {
    estSpec__$POPestimate <- merge__(estSpec__$POPestimate, modelSpec0__[["POPestimate"]])
    estSpec__$IIVdistribution <- merge__(estSpec__$IIVdistribution, modelSpec0__[["IIVdistribution"]])
    estSpec__$IIVestimate <- merge__(estSpec__$IIVestimate, modelSpec0__[["IIVestimate"]])
    for (n__ in names(estSpec__$LOCestimate)) {
      estSpec__$LOCestimate[[n__]] <- merge__(estSpec__$LOCestimate[[n__]], modelSpec0__[["LOCestimate"]][[n__]])
    }
    estSpec__$POPvalues0 <- merge__(estSpec__$POPvalues0, modelSpec0__[["POPvalues0"]])
    estSpec__$IIVvalues0 <- merge__(estSpec__$IIVvalues0, modelSpec0__[["IIVvalues0"]])
    fields__ <- c("errorModel", "covariateModel", "covariateModelValues", "COVestimate")
    for (f__ in fields__) {
      if (!is.null(modelSpec0__[[f__]]))
        estSpec__[[f__]] <- modelSpec0__[[f__]]
    }
  }
  return(IQRsysEst(model__, dosing__, data__, estSpec__, parsOfInterest = parsOfInterest__))
}
#'@export
is_IQRsysEst <- function(input) {
  methods::is(input,"IQRsysEst")
}
#'@export
print.IQRsysEst <- function(x, ...) {
  print.IQRnlmeEst(x,...)
}
#'@export
modelSpec_IQRsysEst <- function (
  POPvalues0 = NULL,
  POPestimate = NULL,
  IIVdistribution = NULL,
  IIVvalues0 = NULL,
  IIVestimate = NULL,
  errorModel = NULL,
  covarianceModel = NULL,
  covariateModel = NULL,
  covariateModelValues = NULL,
  COVestimate = NULL,
  COVcentering = NULL,
  PriorVarPOP = NULL,
  PriorVarCovariateModelValues = NULL,
  PriorDFerrorModel = NULL,
  PriorIIV = NULL,
  PriorDFIIV = NULL,
  LOCmodel = NULL,
  LOCvalues0 = NULL,
  LOCestimate = NULL,
  LOCdistribution = NULL
) {
  modelSpec__ <- list()
  merge__ <- function(x__, y__, whoCalls__ = "") {
    if (is.null(y__)) return(x__)
    additional__ <- setdiff(names(y__), names(x__))
    if (length(additional__) > 0) {
      warningIQR("Found the following elements in ", whoCalls__,
                 " which are disregarded because they are not present in POPvalues0: ",
                 paste(additional__, collapse = ", "))
    }
    c(y__, x__)[names(x__)]
  }
  if (is.null(POPvalues0) & is.null(LOCvalues0))
    stopIQR("At least POPvalues0 or LOCvalues0 has to be supplied.")
  modelSpec__$POPvalues0 <- POPvalues0
  if (length(LOCvalues0) > 0) {
    first_LOCvalues0 <- sapply(LOCvalues0, function(v__) v__[[1]])
    POPvalues0 <- c(POPvalues0, first_LOCvalues0[!names(first_LOCvalues0) %in% names(POPvalues0)])
    modelSpec__$POPvalues0 <- POPvalues0
  }
  if (length(LOCestimate) > 0) {
    first_LOCestimate <- sapply(LOCestimate, function(v__) v__[[1]])
    POPestimate <- c(POPestimate, first_LOCestimate[!names(first_LOCestimate) %in% names(POPestimate)])
    modelSpec__$POPestimate <- POPestimate
  }
  modelSpec__$POPestimate <- merge__(0*POPvalues0 + 1, POPestimate)
  modelSpec__$IIVdistribution <-
    merge__(ifelse(POPvalues0 <= 0, "N", "L"), IIVdistribution)
  modelSpec__$IIVvalues0 <- merge__(0*POPvalues0, IIVvalues0)
  modelSpec__$IIVestimate <- merge__(0*POPvalues0, IIVestimate)
  modelSpec__$errorModel <- errorModel
  modelSpec__$covarianceModel <- covarianceModel
  modelSpec__$covariateModel <- covariateModel
  modelSpec__$covariateModelValues <- covariateModelValues
  modelSpec__$COVestimate <- COVestimate
  modelSpec__$COVcentering <- COVcentering
  modelSpec__$PriorVarPOP <- PriorVarPOP
  modelSpec__$PriorVarCovariateModelValues <- PriorVarCovariateModelValues
  modelSpec__$PriorDFerrorModel <- PriorDFerrorModel
  modelSpec__$PriorIIV <- PriorIIV
  modelSpec__$PriorDFIIV <- PriorDFIIV
  parameters__ <- union(names(LOCmodel), names(LOCvalues0))
  if (!all(parameters__ %in% names(POPvalues0)))
    warningIQR("The following parameters listed in LOCmodel or LOCvalues0 ",
               "were not defined in POPvalues0 and were discarded: ",
               paste(setdiff(parameters__, names(POPvalues0)), collapse = ", "))
  parameters <- intersect(parameters__, names(POPvalues0))
  names(parameters__) <- parameters__
  modelSpec__$LOCmodel <- lapply(parameters__, function(n__) {
    union(LOCmodel[[n__]], names(LOCvalues0[[n__]]))
  })
  modelSpec__$LOCvalues0 <- lapply(parameters__, function(n__) {
    conditions__ <- modelSpec__$LOCmodel[[n__]]
    default.values__ <- structure(rep(modelSpec__$POPvalues0[n__], length(conditions__)), names = conditions__)
    merge__(default.values__, LOCvalues0[[n__]])
  })
  modelSpec__$LOCestimate <- lapply(parameters__, function(n__) {
    conditions__ <- modelSpec__$LOCmodel[[n__]]
    default.values__ <- structure(rep(modelSpec__$POPestimate[n__], length(conditions__)), names = conditions__)
    merge__(default.values__, LOCestimate[[n__]])
  })
  modelSpec__$LOCdistribution <- lapply(parameters__, function(n__) {
    conditions__ <- modelSpec__$LOCmodel[[n__]]
    default.values__ <- structure(rep(modelSpec__$IIVdistribution[n__], length(conditions__)), names = conditions__)
    merge__(default.values__, LOCdistribution[[n__]])
  })
  return(modelSpec__)
}
dMod_build_indiv_grids <- function(myNlmeEst__, my_dMod_data, basic_dMod_model__ = NULL) {
  IDs <- stats::setNames(nm = names(my_dMod_data))
  fixed.grid__ <- covariates(my_dMod_data)
  est.grid__       <- fixed.grid__[!names(fixed.grid__)%in%c(attr(my_dMod_data, "covNames"),
                                                             attr(my_dMod_data, "catNames"),
                                                             attr(my_dMod_data, "regressorNames"))]
  est.vec <- NULL
  for(mycatname__ in myNlmeEst__$data$catNames) {
    fixed.grid__ <- dMod_expand_cat(fixed.grid__,
                                    mycatname__,
                                    myNlmeEst__$data$catValues[[mycatname__]],
                                    myNlmeEst__$data$covariateCATreference[[mycatname__]])
  }
  cov_trafo__ <- dMod_build_covcat_trafo(myNlmeEst__)
  if (!is.null(attr(cov_trafo__, "fixed")))
    fixed.grid__ <- cbind(fixed.grid__, as.data.frame(t(attr(cov_trafo__, "fixed"))))
  betas__ <- dMod_get_default_betas(myNlmeEst__, cov_trafo__)
  if (!is.null(betas__))
    est.grid__ <- cbind(est.grid__, as.data.frame(t(stats::setNames(names(betas__), names(betas__))), stringsAsFactors = FALSE))
  est.vec <- c(est.vec, betas__)
  parameters__ <- myNlmeEst__$modelSpec[c("POPvalues0", "POPestimate", "IIVdistribution", "IIVvalues0", "IIVestimate")]
  parameters_est_IIV__ <- names(parameters__[["IIVvalues0"]][parameters__[["IIVestimate"]] != 0])
  IIVpars__ <- dMod_build_IIVpars(parameters = parameters__, conditions = IDs)
  if (!is.null(IIVpars__$fixed.grid_IIV))
    fixed.grid__ <- merge(fixed.grid__, IIVpars__$fixed.grid_IIV, by = "ID")
  if (!is.null(IIVpars__$est.grid_IIV))
    est.grid__ <- merge(est.grid__, IIVpars__$est.grid_IIV, by = "ID")
  est.vec <- c(est.vec, IIVpars__$pars)
  est_pop__ <- parameters__[["POPvalues0"]][parameters__[["POPestimate"]] == 1]
  fixed_pop__ <- parameters__[["POPvalues0"]][parameters__[["POPestimate"]] == 0]
  if (!is.null(fixed_pop__))
    fixed.grid__ <- cbind(fixed.grid__, as.data.frame(t(fixed_pop__)))
  if (!is.null(est_pop__))
    est.grid__ <- cbind(est.grid__, as.data.frame(t(stats::setNames(names(est_pop__), names(est_pop__))), stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  est.vec <- c(est.vec, est_pop__)
  fixed_by_dosing__ <- attr(my_dMod_data, "dosing_pars")
  fixed_by_dosing__ <- fixed_by_dosing__[!names(fixed_by_dosing__)%in%unique(c(names(est_pop__), parameters_est_IIV__))]
  regressionPars__ <- sapply(myNlmeEst__[["modelInfo"]][["param_reg"]], function(x__) x__[["name"]])
  fixed_by_dosing__ <- fixed_by_dosing__[!names(fixed_by_dosing__)%in%regressionPars__]
  if (!is.null(fixed_by_dosing__))
    fixed.grid__ <-  merge(fixed.grid__[ , c("ID", setdiff(names(fixed.grid__), names(fixed_by_dosing__)))], fixed_by_dosing__, by = "ID")
  if (!is.null(basic_dMod_model__$fixed))
    fixed_unspecified__ <- data.frame(ID = IDs, as.data.frame(t(basic_dMod_model__$fixed)))
  wanted_names__ <- c("ID", setdiff(names(fixed_unspecified__), names(fixed.grid__)))
  if (!is.null(fixed_unspecified__[wanted_names__]))
    fixed.grid__ <- merge(fixed.grid__, fixed_unspecified__[wanted_names__], by = "ID", all = FALSE, all.x = TRUE, all.y = FALSE)
  errorModel <- dMod_convert2dMod_errorModel__(myNlmeEst__$modelSpec$errorModel)
  errorPars__ <- attr(errorModel, "values0")
  if (!is.null(errorPars__))
    est.grid__ <- cbind(est.grid__, as.data.frame(t(stats::setNames(names(errorPars__), names(errorPars__))), stringsAsFactors = FALSE))
  est.vec <- c(est.vec, attr(errorModel, "values0"))
  CONDITION_exists__ <- !is.null(c(fixed.grid__$CONDITION, est.grid__$CONDITION))
  if (CONDITION_exists__){
    IDCOND.grid__ <- est.grid__[c("ID", "CONDITION")]
    LOC_grids__ <- dMod_build_LOC.grids(myNlmeEst__, IDCOND.grid__)
    if (!is.null(LOC_grids__$fixed.grid_LOC))
      fixed.grid__ <- merge(fixed.grid__[setdiff(names(fixed.grid__), names(myNlmeEst__$modelSpec$LOCmodel))], LOC_grids__$fixed.grid_LOC)
    if (!is.null(LOC_grids__$est.grid_LOC))
      est.grid__ <- merge(est.grid__[setdiff(names(est.grid__), names(myNlmeEst__$modelSpec$LOCmodel))], LOC_grids__$est.grid_LOC)
    if (!is.null(LOC_grids__$est.vec_LOC))
      est.vec <- c(est.vec, LOC_grids__$est.vec_LOC)
    est.vec <- est.vec[intersect(names(est.vec), unlist(est.grid__))]
  }
  fixed.grid__$ID <- as.character(fixed.grid__$ID)
  est.grid__$ID <- as.character(est.grid__$ID)
  tG0__ <- names(parameters__[["IIVdistribution"]])[parameters__[["IIVdistribution"]] == "G"]
  tL0__ <- names(parameters__[["IIVdistribution"]])[parameters__[["IIVdistribution"]] == "L"]
  tG__ <- tG0__[!tG0__ %in% names(cov_trafo__)]
  tL__ <- tL0__[!tL0__ %in% names(cov_trafo__)]
  base_pars__ <- structure(names(est.vec), names = names(est.vec))
  if (CONDITION_exists__) {
    loc_est_parnames__ <- setdiff(names(LOC_grids__$est.grid_LOC), c("CONDITION", "ID"))
    loc_base_pars__ <- unlist(lapply(loc_est_parnames__, function(n__) {
      value__ <- structure(rep(n__, nrow(LOC_grids__$est.grid_LOC)), names = LOC_grids__$est.grid_LOC[[n__]])
      value__[!is.na(names(value__))]
    }))
    base_pars__[intersect(base_pars__, names(loc_base_pars__))] <- loc_base_pars__[intersect(base_pars__, names(loc_base_pars__))]
  }
  tG1__ <- names(base_pars__)[base_pars__ %in% tG0__]
  tL1__ <- names(base_pars__)[base_pars__ %in% tL0__]
  est.vec[tG1__] <- logit(est.vec[tG1__])
  est.vec[tL1__] <- log(est.vec[tL1__])
  est.vec[est.vec == Inf] <- 750
  est.vec[est.vec == -Inf] <- -750
  tG2__ <- tG0__[tG0__ %in% names(fixed.grid__)]
  tL2__ <- tL0__[tL0__ %in% names(fixed.grid__)]
  fixed.grid__[tG2__] <- lapply(fixed.grid__[tG2__], logit)
  fixed.grid__[tL2__] <- lapply(fixed.grid__[tL2__], log)
  infinites__ <- vapply(fixed.grid__, function(.x) is.numeric(.x) & !is.na(.x) & !is.finite(.x), FUN.VALUE = rep(FALSE, nrow(fixed.grid__)))
  fixed.grid__[infinites__] <- -750 
  return(list(fixed.grid = fixed.grid__, est.grid = est.grid__, est.vec = est.vec))
}
dMod_build_nlme_trafo <- function(myNlmeEst__, IDs, basic_dMod_model__) {
  cov_trafo__ <- dMod_build_covcat_trafo(myNlmeEst__)
  parameters__ <- myNlmeEst__$modelSpec[c("POPvalues0", "POPestimate", "IIVdistribution", "IIVvalues0", "IIVestimate")]
  parameters_est_IIV__ <- names(parameters__[["IIVvalues0"]][parameters__[["IIVestimate"]] != 0])
  IIVpars__ <- dMod_build_IIVpars(parameters = parameters__, conditions = IDs)
  errorModel <- dMod_convert2dMod_errorModel__(myNlmeEst__$modelSpec$errorModel)
  tG0__ <- names(parameters__[["IIVdistribution"]])[parameters__[["IIVdistribution"]] == "G"]
  tL0__ <- names(parameters__[["IIVdistribution"]])[parameters__[["IIVdistribution"]] == "L"]
  tG__ <- tG0__[!tG0__ %in% names(cov_trafo__)]
  tL__ <- tL0__[!tL0__ %in% names(cov_trafo__)]
  trafo__ <- getEquations(basic_dMod_model__[["p"]])[[1]]
  trafo1__ <- setdiff(names(parameters__[["POPvalues0"]]), names(trafo__))
  names(trafo1__) <- trafo1__
  trafo__ <- c(trafo__, trafo1__)
  trafo__ <- repar("x ~ y", x = names(cov_trafo__), y = cov_trafo__, trafo = trafo__)
  trafo__ <- repar("x ~ exp(x)",            x = tL__,                         trafo = trafo__)
  trafo__ <- repar("x ~ exp(x)/(1+exp(x))", x = tG__,                         trafo = trafo__)
  trafo__ <- repar("x ~ exp(x)",            x = getSymbols(errorModel), trafo = trafo__)
  if (any(parameters__[["IIVestimate"]] > 0)) {
    trafo__ <- repar(expr = "x ~ (x + ETA_x)", trafo = trafo__,
                     x = names(parameters__[["IIVvalues0"]])[parameters__[["IIVestimate"]] > 0])
  }
  return(trafo__)
}
dMod_build_nlme_p <- function(trafo__) {
  mywd__ <- getwd()
  setwd(tempdirIQR())
  output__ <- try({
    mymodelname__ <- paste0("p_individual", paste0(sample(c(0:9,letters), 8), collapse = ""))
    p__ <- P(trafo__, modelname = mymodelname__, compile = FALSE)
    files__ <- list.files(pattern = utils::glob2rx(paste0("p_individual", "*.c*")))
    timestamp__ <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    output__ <- paste0("IQRparTrafo_", timestamp__)
    path__ <- gsub("\\", "/", tempdirIQR(), fixed = TRUE)
    .so__ <- .Platform[["dynlib.ext"]]
    clean_loadedDLLs("IQRparTrafo_")
    system(paste0(R.home(component = "bin"), "/R CMD SHLIB ", paste(files__, collapse = " "), " -o ", file.path(path__, output__), .so__, " >xxx"), intern = TRUE)
    dyn.load(paste0(file.path(path__, output__), .so__))
    modelname(p__) <- output__
    unlink(paste0("*p_individual*.c"))
    unlink(paste0("*p_individual*.o"))
    unlink("xxx")
    p__
  }, silent = TRUE)
  setwd(mywd__)
  if (inherits(output__, "try-error"))
    stopIQR("Problems encountered with the generation of the parameter transformations.")
  return(output__)
}
print_IQRpartable <- function(partable, hardExclude = "ETA", softExclude = c("dosing", "ini"), level = 1, NSIGNIF=4, optimum = 1) {
  tasks__ <- c("Error model" = "error",
               "Dosing" = "dosing",
               "Covariates" = "beta",
               "Random effects" = "omega",
               "ETAs" = "ETA",
               "Fixed effects" = "pop")
  cols_l1__ <- c("Parameter" = "parametername",
                 "Value" = "parametervalue_linear",
                 "Estimate" = "estimated",
                 "Condition" = "CONDITION",
                 "Description" = "parametertask",
                 "Transformation" = "trafo",
                 "Base" = "belongs_to")
  cols_l2__ <- c("SE" = "sigma_linear",
                 "L95" = "lower95_parametervalue_linear",
                 "U95" = "upper95_parametervalue_linear")
  cols__ <- switch(as.character(level),
                   "1" = cols_l1__,
                   "2" = c(cols_l1__, cols_l2__))
  for (n__ in setdiff(cols__, names(partable))) partable[[n__]] <- NA
  partable__ <- partable[,cols__]
  partable__ <- filter(partable__, !parametertask %in% softExclude | estimated  == 1)
  partable__ <- filter(partable__, !parametertask %in% hardExclude)
  names(partable__) <- names(cols__)
  partable__[["Description"]] <- names(tasks__)[match(partable__[["Description"]], tasks__)]
  partable__[["Estimate"]] <- ifelse(partable__[["Estimate"]] == 1, paste0("+", partable__[["Transformation"]]), "-")
  pars_with_IIV__ <- match(partable__[["Base"]][partable__[["Description"]] == "Random effects"], partable__[["Parameter"]])
  fixed_pars_with_IIV__ <- pars_with_IIV__[partable__[["Estimate"]][pars_with_IIV__] == ""]
  partable__[["Estimate"]][fixed_pars_with_IIV__] <- partable__[["Transformation"]][fixed_pars_with_IIV__]
  partable__[["Value"]] <- signif(partable__[["Value"]], NSIGNIF)
  printcolumns__ <- 1:3
  if (level > 1) {
    partable__[["RSE (%)"]] <-
      ifelse(is.na(partable__[["SE"]]),
             yes = "--",
             no  = signif(100*partable__[["SE"]]/partable__[["Value"]], NSIGNIF)
      )
    printcolumns__ <- c(1:3, which(names(partable__) == "RSE (%)"))
    if (any(!is.na(c(partable__[["L95"]], partable__[["U95"]])))) {
      partable__[["CI (95%)"]] <-
        ifelse(is.na(partable__[["L95"]]),
               yes = "--",
               no  = paste0("[", signif(partable__[["L95"]], NSIGNIF), ", ", signif(partable__[["U95"]], NSIGNIF), "]")
        )
      printcolumns__ <- c(1:3, which(names(partable__) %in% c("RSE (%)", "CI (95%)")))
    }
  }
  global__ <- dplyr::filter(partable__, is.na(Condition))
  local__ <- dplyr::filter(partable__, !is.na(Condition))
  global__ <- split(global__[printcolumns__], global__[["Description"]])
  local__ <- split(local__[sort(union(printcolumns__, 4))], local__[["Description"]])
  convert2IQRtable <- function(x__) {
    if (length(x__) == 0) return(NULL)
    header.x__ <- names(x__[[1]])
    x__ <- lapply(names(x__), function(n__) {
      header__ <- paste0("**", n__, "**")
      values__ <- as_IQRtable(x__[[n__]])[-1,]
      compose_IQRtable(H = header__, V = values__, pattern = "H\n \n V")
    })
    x__[["rule"]] <- "x\n \n y"
    x__ <- c(list(matrix(header.x__, nrow = 1)), x__)
    x__ <- do.call(composeByRule_IQRtable, x__)
    x__
  }
  global__ <- convert2IQRtable(global__)
  local__ <- convert2IQRtable(local__)
  message__ <- paste0("--- Selected optimum: ", optimum, " ")
  message__ <- paste0(message__, paste(rep("-", 55 - nchar(message__)), collapse = ""), "\n")
  message(message__)
  if (length(global__) > 0) {
    message("--- Global parameters ---------------------------------\n")
    print(global__)
    cat("\n")
  } else {
    message("--- No global parameters defined ----------------------\n")
  }
  if (length(local__) > 0) {
    message("--- Local parameters ----------------------------------\n")
    print(local__)
    cat("\n")
  } else {
    message("--- No local parameters defined -----------------------\n")
  }
  cat(paste0("Values rounded to ", NSIGNIF," significant digits.\n"))
  cat("Estimated/fixed parameters (+/-)\nEstimation on (N) natural / (L) log / (G) logit scale.\n")
  return(invisible())
}
#'@export
comparePars_IQRsysModel <- function(sysmodel,...,SIMOPT.atol = 1e-6,SIMOPT.rtol=1e-6) {
  args__ <- list(...)
  if (!is_IQRsysModel(sysmodel))
    stopIQR("sysmodel argument is not an IQRsysModel")
  if (length(args__) > 0 && (is.null(names(args__)) || any(names(args__) == "")))
    stopIQR("All arguments passed via ... to setPars_IQRsysModel() must be named.")
  if (!all(sapply(args__, is.numeric)))
    stopIQR("Arguments passed via ... to setPars_IQRsysModel() must be numeric.")
  pred <- do.call(rbind,lapply(seq_along(args__), function (k) {
    param <- args__[[k]]
    cbind(Parameters=names(args__)[k],
          sim_IQRsysModel(sysmodel,
                          parameters = param,
                          SIMOPT.atol = SIMOPT.atol,
                          SIMOPT.rtol=SIMOPT.rtol)$prediction[[1]])
  }))
  pred$name <- as.character(pred$name)
  data <- as.data.frame(sysmodel$data[[1]])
  if (nrow(data)>0) {
    data$name <- as.character(data$name)
    dataOrig__ <- as.data.frame(attr(sysmodel, "sysModelEst")$IQRsysData)
    if (!"UNIT" %in% names(dataOrig__)) dataOrig__$UNIT <- NA
    info <- unique(dataOrig__[!is.na(dataOrig__$YTYPE) & dataOrig__$YTYPE>0,c("NAME","YTYPE","UNIT")])
    info$name <- paste0("OUTPUT",info$YTYPE)
    info$TEXT <- paste0(info$NAME," (",info$UNIT,")")
    info <- info[,c("name","TEXT")]
    data <- dplyr::left_join(data,info,by="name")
    pred <- dplyr::left_join(pred,info,by="name")
    data$name <- data$TEXT
    pred$name <- pred$TEXT
  }
  p <- IQRggplot(pred, aes(x = time, y = value)) +
    geom_line(aes(color=Parameters),size=1)
  if (nrow(data)>0) p <- p + geom_point(data = data,aes(x=time,y=value))
  p <- p + facet_wrap(~CONDITION*name,scales = "free") +
    scale_color_IQRtools()
  p
}
#'@export
is_IQRsysModel <- function(input) {
  methods::is(input,"IQRsysModel")
}
#'@export
model_IQRsysModel <- function(sysmodel) {
  attr(attr(sysmodel, "sysModelEst")[["model"]], "original")
}
#'@export
dataSpec_IQRsysModel <- function(sysmodel) {
  est <- attr(sysmodel, "sysModelEst")
  est$est$original$data
}
#'@export
modelSpec_IQRsysModel <- function(sysmodel) {
  est__ <- attr(sysmodel, "sysModelEst")
  parsOfInterest__ <- unlist(est__[["parsOfInterest"]])
  modelSpec__ <- est__[["modelSpec"]]
  modelSpec__$POPvalues0 <- modelSpec__$POPvalues0[intersect(names(modelSpec__$POPvalues0), parsOfInterest__)]
  modelSpec__$POPestimate <- modelSpec__$POPestimate[intersect(names(modelSpec__$POPestimate), parsOfInterest__)]
  modelSpec__$IIVdistribution <- modelSpec__$IIVdistribution[intersect(names(modelSpec__$IIVdistribution), parsOfInterest__)]
  modelSpec__$IIVestimate <- modelSpec__$IIVestimate[intersect(names(modelSpec__$IIVestimate), parsOfInterest__)]
  modelSpec__$IIVvalues0 <- modelSpec__$IIVvalues0[intersect(names(modelSpec__$IIVvalues0), parsOfInterest__)]
  modelSpec__$LOCmodel <- modelSpec__$LOCmodel[intersect(names(modelSpec__$LOCmodel), parsOfInterest__)]
  modelSpec__$LOCvalues0 <- modelSpec__$LOCvalues0[intersect(names(modelSpec__$LOCvalues0), parsOfInterest__)]
  modelSpec__$LOCestimate <- modelSpec__$LOCestimate[intersect(names(modelSpec__$LOCestimate), parsOfInterest__)]
  modelSpec__$LOCdistribution <- modelSpec__$LOCdistribution[intersect(names(modelSpec__$LOCdistribution), parsOfInterest__)]
  if (length(modelSpec__$LOCmodel) > 0)
    stopIQR("Models with local parameters not yet supported.")
  pars__ <- getPars_IQRsysModel(sysmodel, FLAGverbose = FALSE)
  pars.pop__ <- pars__[parsOfInterest__]
  pars.beta__ <- pars__[grepl("^beta_", names(pars__))]
  pars.error__ <- pars__[grepl("^error_", names(pars__))]
  modelSpec__$POPvalues0[parsOfInterest__] <- pars.pop__[parsOfInterest__]
  for (n__ in names(modelSpec__$covariateModelValues)) {
    currentValues__ <- modelSpec__$covariateModelValues[[n__]]
    for (m__ in names(currentValues__)) {
      newvalue__ <- pars.beta__[grepl(paste0("^beta_", n__, "_", m__), names(pars.beta__))]
      modelSpec__$covariateModelValues[[n__]][m__] <- newvalue__[[1]]
    }
  }
  for (n__ in names(modelSpec__$errorModel)) {
    ADM__ <- sub("OUTPUT", "", n__)
    newvalues__ <- pars.error__[names(pars.error__) %in% paste0("error_", c("ADD", "PROP"), ADM__)]
    modelSpec__$errorModel[[n__]] <- c(modelSpec__$errorModel[[n__]][1], newvalues__)
  }
  return(modelSpec__)
}
#'@export
getPrediction_IQRsysModel <- function(sysmodel) {
  prediction <- sysmodel[["prediction"]][[1]]
  prediction <- prediction[ , -match("condition", names(prediction))]
  return(prediction)
}
#'@export
getPars_IQRsysModel <- function(sysModel, ..., parameters = NULL, FLAGverbose = TRUE) {
  args__ <- list(...)
  if (!is.null(names(args__)))
    warningIQR("Arguments passed via ... to getPars_IQRsysModel() are expected to be unnamed. Names were removed.")
  if (!all(sapply(args__, is.character)))
    warningIQR("Found non-character arguments passed via ... to getPars_IQRsysModel(). Will be ignored.")
  if (!all(sapply(args__, length) == 1))
    warningIQR("Found arguments of length > 1 passed via ... to getPars_IQRsysModel(). Only the first element will be used.")
  names(args__) <- NULL
  args__ <- lapply(args__, function(x__) x__[[1]])
  args__ <- lapply(args__, function(x__) {
    if (!is.character(x__)) return(NULL) else return(x__)
  })
  if (!is.null(parameters)) {
    if (!is.character(parameters))
      stopIQR("Argument parameters must be character.")
    names(parameters) <- NULL
    parameters__ <- as.list(parameters)
    args__ <- c(args__, parameters__)
  }
  args__ <- c(args__, FLAGverbose = FLAGverbose)
  do.call(pars_IQRsysModel, c(list(sysModel), args__))
}
#'@export
setPars_IQRsysModel <- function(sysModel, ..., parameters = NULL, FLAGverbose = FALSE) {
  dosing <- NULL
  args__ <- list(...)
  if (length(args__) > 0 && (is.null(names(args__)) || any(names(args__) == "")))
    stopIQR("All arguments passed via ... to setPars_IQRsysModel() must be named.")
  if (!all(sapply(args__, is.numeric)))
    stopIQR("Arguments passed via ... to setPars_IQRsysModel() must be numeric.")
  if (!is.null(parameters) && !is.numeric(parameters))
    stopIQR("Parameters argument must be numeric or NULL.")
  if (!is.null(dosing) && (!is.list(dosing) & !is_IQRdosing(dosing)))
    stopIQR("Dosing argument must be IQRdosing object or list of IQRdosing objects or NULL.")
  pars_IQRsysModel(sysModel, ..., parameters = parameters, dosing = dosing, FLAGverbose = FLAGverbose)
}
#'@export
getDosing_IQRsysModel <- function(sysmodel) {
  IQmessage <- function(m, width = 55) message(paste0(substr(paste("---", m, paste(rep("_", width), collapse = "")), 1, width), "\n"))
  selectFirst__ <- function(x) x[1]
  partable__ <- sysmodel[["IQRpartable"]][[1]]
  dosingpars__ <- partable__[partable__[["parametertask"]] %in% "dosing",]
  if (nrow(dosingpars__) == 0) {
    IQmessage("No dosing available")
    return()
  }
  conditions__ <- unique(dosingpars__[["CONDITION"]])
  dosings__ <- lapply(conditions__, function(mycond__) {
    d__ <- dosingpars__[dosingpars__[["CONDITION"]] == mycond__, ]
    d__ <- d__[order(d__[["base_par"]]),]
    timepars__ <- d__[["base_par"]][grepl("^TIME", d__[["base_par"]])]
    ADM__  <- sapply(strsplit(gsub("TIME", "", timepars__), split = "_", fixed = TRUE), selectFirst__)
    TIME__ <- d__[["parametervalue_linear"]][grepl("^TIME", d__[["base_par"]])]
    TINF__ <- d__[["parametervalue_linear"]][grepl("^TINF", d__[["base_par"]])]
    AMT__  <- d__[["parametervalue_linear"]][grepl("^AMT" , d__[["base_par"]])]
    is_amountZero__ <- (AMT__ == 0)
    IQRdosing(
      TIME = as.numeric(TIME__[!is_amountZero__]),
      ADM  = as.integer(ADM__[!is_amountZero__]),
      AMT  = as.numeric(AMT__[!is_amountZero__]),
      TINF = as.numeric(TINF__[!is_amountZero__])
    )
  })
  names(dosings__) <- conditions__
  for (i__ in 1:length(dosings__)) {
    IQmessage(conditions__[i__])
    print(as_IQRtable(dosings__[[i__]]))
  }
  return(invisible(dosings__))
}
#'@export
sim_IQRsysModel <- function(sysModel, simtime = NULL, parameters = NULL, FLAGoutputsOnly = TRUE,
                            FLAGverbose  = FALSE,
                            SIMOPT.method = NULL, SIMOPT.atol = NULL, SIMOPT.rtol = NULL,
                            SIMOPT.hmin = NULL, SIMOPT.hmax = NULL, SIMOPT.hini = NULL,
                            SIMOPT.maxsteps = NULL, SIMOPT.nauxtimes = NULL) {
  if (!is_IQRsysModel(sysModel)) stopIQR("sysModel is not an IQRsysModel object")
  dosing <- NULL
  if (!is.null(parameters) | !is.null(dosing))
    sysModel <- pars_IQRsysModel(sysModel, parameters = parameters, dosing = dosing, FLAGverbose = FLAGverbose)
  if (!is.null(simtime))
    sysModel <- mutate_IQRsysModel(sysModel, times = list(simtime))
  prd__ <- sysModel[["prd"]][[1]]
  times__ <- sysModel[["times"]][[1]]
  pars__ <- sysModel[["pars"]][[1]]
  data__ <- sysModel[["data"]][[1]]
  errfn__ <- sysModel[["e"]][[1]]
  controls_g__ <- controls(sysModel[["g"]][[1]], NULL, "attach.input")
  controls_prd__ <- controls(sysModel[["x"]][[1]], NULL, "optionsOde")
  if (FLAGoutputsOnly)
    controls(sysModel[["g"]][[1]], NULL, "attach.input") <- FALSE
  else
    controls(sysModel[["g"]][[1]], NULL, "attach.input") <- TRUE
  SIMOPT__ <- list(method = SIMOPT.method, atol = SIMOPT.atol, rtol = SIMOPT.rtol,
                   hmin = SIMOPT.hmin, hini = SIMOPT.hini, maxsteps = SIMOPT.maxsteps,
                   nauxtimes = SIMOPT.nauxtimes)
  isNULL_SIMOPT__ <- sapply(SIMOPT__, is.null)
  if (!all(isNULL_SIMOPT__)) {
    for (k__ in which(!isNULL_SIMOPT__)) {
      controls(sysModel[["x"]][[1]], NULL, "optionsOde")[[names(SIMOPT__)[k__]]] <- SIMOPT__[[k__]]
    }
  }
  prediction__ <- as.data.frame(prd__(times__, pars__, deriv = FALSE), errfn = errfn__, data = data__)
  if (!is.null(simtime))
    prediction__ <- prediction__[prediction__[["time"]] %in% simtime, ]
  class(prediction__) <- c("IQRsysSimulation", "data.frame")
  controls(sysModel[["g"]][[1]], NULL, "attach.input") <- controls_g__
  controls(sysModel[["x"]][[1]], NULL, "optionsOde") <- controls_prd__
  mutate_IQRsysModel(sysModel,
                     prediction = list(prediction__))
}
#'@export
profile_IQRsysModel <- function(sysModel, parameters = NULL, limits = NULL, fixed = NULL, alpha = 0.05, ncores = 1, ...) {
  obj__ <- sysModel[["obj"]][[1]]
  pars__ <- unclass(sysModel[["pars"]][[1]])
  fixed_on_lower__ <- sysModel[["fixed_on_lower"]][[1]]
  fixed_on_upper__ <- sysModel[["fixed_on_upper"]][[1]]
  selected__ <- as.character(sysModel[["selected"]][[1]])
  dllfolder__ <- sysModel[["dllfolder"]][[1]]
  FLAGderiv__ <- sysModel[["options"]][[1]][["opt.method"]] %in% "trust"
  if (is.null(obj__))
    stopIQR("The sysModel does not contain and objective function. Please run parameter estimation before computing profile likelihood.")
  if (!FLAGderiv__)
    stopIQR("The objective function has been built without support for derivatives. Unable to compute profiles.")
  fixed_by_user__ <- pars__[fixed]
  if (is.null(limits)) limits <- c(lower = -Inf, upper = Inf)
  if (is.null(parameters)) parameters <- names(pars__)[!(grepl("^(omega|ETA|error_ADD|error_PROP)", names(pars__)) |
                                                           names(pars__)%in% names(c(fixed_on_lower__, fixed_on_upper__, fixed_by_user__)))]
  released__ <- intersect(parameters, names(c(fixed_on_lower__, fixed_on_upper__)))
  if (length(released__) > 0) {
    fixed_on_lower__ <- fixed_on_lower__[!names(fixed_on_lower__) %in% released__]
    fixed_on_upper__ <- fixed_on_upper__[!names(fixed_on_upper__) %in% released__]
  }
  mywd__ <- getwd()
  setwd(dllfolder__)
  profiles__ <- try(
    suppressMessages(profile(obj = obj__, pars = pars__, whichPar = parameters,
                             alpha = alpha, limits = limits, cores = ncores,
                             fixed = c(fixed_on_lower__, fixed_on_upper__, fixed_by_user__), ...)), silent = TRUE)
  setwd(mywd__)
  if (inherits(profiles__, "try-error")) {
    warningIQR("Profiles could not be computed. SysModel is returned without any changes.")
    return(sysModel)
  }
  if (!is.null(fixed))
    selected__ <- paste(c(selected__, paste(paste(fixed, collapse = ", "), "fixed")), collapse = "\n")
  if (length(released__) > 0)
    selected__ <- paste(c(selected__, paste(paste(released__, collapse = ", "), "released")), collapse = "\n")
  profiles0__ <- sysModel[["profiles"]][[1]]
  if (!is.null(profiles0__)) {
    if (selected__ %in% names(profiles0__)) {
      subprofiles0__ <- profiles0__[[selected__]]
      subprofiles0__ <- subprofiles0__[!subprofiles0__[["whichPar"]] %in% parameters, ] 
      profiles0__[[selected__]] <- rbind(profiles__, subprofiles0__)
    } else {
      profiles0__[[selected__]] <- profiles__
    }
  } else {
    profiles0__[[selected__]] <- profiles__
  }
  sysModel <- mutate_IQRsysModel(sysModel, profiles = list(profiles0__))
  sysModel <- partable_IQRsysModel(sysModel)
  return(sysModel)
}
#'@export
switchOpt_IQRsysModel <- function(sysModel, optimum) {
  parframe__ <- sysModel[["parframes"]][[1]]
  if (is.null(parframe__))
    stopIQR("The sysModel object does not (yet) contain multi-start fit information.")
  if (optimum > nrow(parframe__))
    stopIQR("Selected optimum number too high. Please check against plotWaterfall_IQRsysModel(.).")
  pars__ <- as.parvec(parframe__, optimum)
  selected__ <- optimum
  sysModel <- mutate_IQRsysModel(sysModel,
                                 pars = list(structure(as.numeric(pars__), names = names(pars__))),
                                 selected = list(selected__))
  sysModel <- vcov_IQRsysModel(sysModel)
  sysModel <- partable_IQRsysModel(sysModel)
  sysModel <- pred_IQRsysModel(sysModel)
  return(sysModel)
}
#'@export
plot.IQRsysModel <- function(x, ...,
                             time = NULL, log = NULL,
                             FLAGpred = TRUE, FLAGdata = TRUE,
                             FLAGerror = TRUE) {
  plot_IQRsysModel(x, time, log, FLAGpred, FLAGdata, FLAGerror)
}
#'@export
plot_IQRsysModel <- function(sysModel, time = NULL, log = NULL,
                             FLAGpred = TRUE, FLAGdata = TRUE, FLAGerror = NULL,
                             FLAGgroupCONDITION = FALSE, FLAGrmPredWithoutData = TRUE,
                             FLAGmultipage = FALSE, multipage.nrow = 2, multipage.ncol = 2,
                             states = NULL, conditions = NULL, IDs = NULL,
                             group = "ID") {
  if (group!="ID") warningIQR("Use of 'group' argument has been deprecated. Use 'FLAGgroupCONDITION=TRUE' instead")
  if (FLAGgroupCONDITION) {
    group = "CONDITION"
  } else {
    group = "ID"
  }
  group <- match.arg(group, c("ID", "CONDITION"))
  data__ <- as.data.frame(sysModel[["data"]][[1]])
  prediction__ <- as.data.frame(sysModel[["prediction"]][[1]])
  if (!is.null(states)) {
    if (!all(states %in% as.character(unique(prediction__$name)))) {
      stopIQR("At least one of the provided state names are not in the simulation output.\n  Check if you set FLAGoutputsOnly=FALSE in the sim_IQRsysModel() function.")
    }
  }
  if (!is.null(time) && !is.numeric(time) && length(time) < 2)
    stopIQR("Unable to compute time frame from argument 'time'. Please check 'time'.")
  if (!is.null(log) && !is.character(log))
    stopIQR("Argument 'log' must be character.")
  if (!is.null(log) && (log != "x" & log != "y" & log != "xy"))
    stopIQR("When argument 'log' is specified, it must be either 'x' or 'y' or 'xy'.")
  if (is.null(FLAGerror)) {
    pars__ <- utils::capture.output(suppressMessages(getPars_IQRsysModel(sysModel)))
    pars__ <- pars__[grepl("^error_", names(pars__))]
    if (inherits(sysModel, "optIQRsysModel")) {
      FLAGerror <- TRUE
    } else if (length(pars__) > 0 && any(pars__ != pi/10)) {
      FLAGerror <- TRUE
    } else {
      FLAGerror <- FALSE
    }
  }
  levels__ <- try(alphnumsort(unique(c(data__[["condition"]], prediction__[["condition"]]))), silent = TRUE)
  if (!inherits(levels__, "try-error")) {
    data__[["condition"]] <- factor(data__[["condition"]], levels = levels__)
    prediction__[["condition"]] <- factor(prediction__[["condition"]], levels = levels__)
  }
  if (!is.null(data__)) {
    if (!is.null(states)) {
      data__ <- filter(data__, name %in% states)
    }
    if (!is.null(conditions))
      data__ <- filter(data__, CONDITION %in% conditions)
    if (!is.null(IDs))
      data__ <- filter(data__, ID %in% IDs)
  }
  if (!is.null(prediction__)) {
    if (!is.null(states))
      prediction__ <- filter(prediction__, name %in% states)
    if (!is.null(conditions))
      prediction__ <- filter(prediction__, CONDITION %in% conditions)
    if (!is.null(IDs))
      prediction__ <- filter(prediction__, ID %in% IDs)
  }
  d__ <- suppressWarnings(
    dplyr::bind_rows(data = data__, prediction = prediction__, .id = ".source")
  )
  dataOrig__ <- as.data.frame(attr(sysModel, "sysModelEst")$IQRsysData)
  outputIX__ <- sort(as.numeric(gsub("OUTPUT","",as.character(unique(d__$name[grepl("OUTPUT[0-9]+",d__$name)])))))
  dataOutputIX__ <- unique(sort(dataOrig__$YTYPE))
  dataOutputIX__ <- dataOutputIX__[!is.na(dataOutputIX__)]
  if (length(outputIX__) > 0 & length(dataOutputIX__) > 0) {
    if (!"UNIT" %in% names(dataOrig__)) dataOrig__$UNIT <- "NA"
    outputInfo__ <- unique(dataOrig__[dataOrig__$YTYPE %in% outputIX__,c("NAME","UNIT","YTYPE")])
    outputInfo__$OUTPUT <- paste0("OUTPUT",outputInfo__$YTYPE)
    outputInfo__$TEXT <- gsub(":::"," ",paste0(outputInfo__$NAME," [",outputInfo__$UNIT,"]\n(",outputInfo__$OUTPUT,")"))
    d__$name <- as.character(d__$name)
    for (k in 1:nrow(outputInfo__)) {
      d__$name[d__$name==outputInfo__$OUTPUT[k]] <- outputInfo__$TEXT[k]
    }
    data__$name <- as.character(data__$name)
    for (k in 1:nrow(outputInfo__)) {
      data__$name[data__$name==outputInfo__$OUTPUT[k]] <- outputInfo__$TEXT[k]
    }
    prediction__$name <- as.character(prediction__$name)
    for (k in 1:nrow(outputInfo__)) {
      prediction__$name[prediction__$name==outputInfo__$OUTPUT[k]] <- outputInfo__$TEXT[k]
    }
    d__$CONDITION <- gsub(":::"," ",d__$CONDITION)
    data__$CONDITION <- gsub(":::"," ",data__$CONDITION)
    prediction__$CONDITION <- gsub(":::"," ",prediction__$CONDITION)
  }
  if (!"TIMEUNIT" %in% names(dataOrig__)) {
    xlab <- "Time"
  } else {
    xlab <- paste0("Time [",dataOrig__$TIMEUNIT[1],"]")
  }
  if (nrow(data__) > 0 & FLAGdata & FLAGrmPredWithoutData) {
    dataSelection__ <- Reduce(paste, unique(data__[, c("name", "ID", "CONDITION")]))
    if (nrow(prediction__) > 0) prediction__ <- prediction__[Reduce(paste, prediction__[, c("name", "ID", "CONDITION")]) %in% dataSelection__, ]
    d__ <- d__[Reduce(paste, d__[, c("name", "ID", "CONDITION")]) %in% dataSelection__, ]
  }
  facetLevels <- 1
  if (FLAGmultipage) facetLevels <- unique(paste(d__$CONDITION, d__$name))
  plots__ <- lapply(facetLevels, function(mylevel) {
    if (FLAGmultipage) {
      myd__ <- d__[paste(d__$CONDITION, d__$name) == mylevel,]
      mydata__ <- data__[paste(data__$CONDITION, data__$name) == mylevel,]
      myprediction__ <- prediction__[paste(prediction__$CONDITION, prediction__$name) == mylevel,]
    } else {
      myd__ <- d__
      mydata__ <- data__
      myprediction__ <- prediction__
    }
    if (group == "ID") {
      p__ <- ggplot(myd__, aes(x = time, y = value, color = condition, fill = condition)) +
        facet_wrap(~CONDITION*name, scales = "free")
    } else if (group == "CONDITION") {
      p__ <- ggplot(myd__, aes(x = time, y = value, group = condition, color = CONDITION, fill = CONDITION)) +
        facet_wrap(~name, scales = "free")
    }
    if (nrow(myprediction__) > 0 & FLAGpred) {
      if (FLAGerror && !is.null(myprediction__$sigma) && any(!is.na(myprediction__$sigma))) {
        p__ <- p__ + geom_ribbon(data = myprediction__,
                                 aes(ymin = value - sigma, ymax = value + sigma),
                                 alpha = .3, lty = 0)
      }
      p__ <- p__ + geom_line(data = myprediction__,size=1)
    }
    if (nrow(mydata__) > 0 & FLAGdata) {
      if (FLAGerror && !is.null(mydata__$sigma) && any(!is.na(mydata__$sigma))) {
        p__ <- p__ + geom_errorbar(data = mydata__,
                                   aes(ymin = value - sigma, ymax = value + sigma))
      }
      if (any(mydata__[["value"]] <= mydata__[["lloq"]]))
        p__ <- p__ +
          geom_point(data = mydata__, aes(pch = ifelse(value <= lloq, "yes", "no"))) +
          scale_shape_manual(name = "BLOQ", values = c(yes = 4, no = 19))
      else
        p__ <- p__ +
          geom_point(data = mydata__)
    }
    nID__ <- length(unique(c(mydata__[["ID"]], myprediction__[["ID"]])))
    mycolors__ <- rep_len(IQRtoolsColors[1:9], nID__)
    p__ <- p__ + scale_color_manual(name = group, values = mycolors__)
    p__ <- p__ + scale_fill_manual(name = group, values = mycolors__)
    if (!is.null(log)) {
      if (grepl("x", log)) p__ <- p__ + scale_x_log10()
      if (grepl("y", log)) p__ <- p__ + scale_y_log10()
    }
    if (!is.null(time)) {
      timerange__ <- range(time)
      p__ <- p__ + coord_cartesian(xlim = timerange__)
    }
    p__ <- p__ + ylab(label = NULL) + xlab(xlab)
    p__ + themeIQRtools()
  })
  if (FLAGmultipage) {
    fig__ <- IQRoutputFigure(plots__,
                             opt.layout = opt.layout(legend.option = "common",
                                                     nrow = multipage.nrow, ncol = multipage.ncol))
  } else {
    fig__ <- IQRoutputFigure(plots__,
                             opt.layout = opt.layout(legend.option = "common"),
                             npage = 1)
  }
  fig__
}
#'@export
print.optIQRsysModel <- function(x, ...) {
  args0__ <- attr(x, "args0")
  sysModelEst__ <- attr(x, "sysModelEst")
  model__ <- switch(args0__[["Scenario"]][["model"]],
                    filename = args0__$model,
                    IQRmodel = "IQRmodel object")
  if (is.null(model__)) model__ <- "No model information available!"
  dosing__ <- switch(args0__[["Scenario"]][["dosing"]],
                     null = "No dosing.",
                     IQRdosing = "IQRdosing object",
                     `List of IQRdosing` = "List of IQRdosing objects",
                     NLME = "Dosing provided with data")
  if (is.null(dosing__)) dosing__ <- "No dosing information available!"
  data__ <- switch(args0__[["Scenario"]][["data"]],
                   null = "No data",
                   IQRsysData = "IQRsysData object.",
                   NLME = sysModelEst__[["data"]][["datafile"]])
  if (is.null(data__)) data__ <- "No data information available!"
  spec__ <- switch(args0__[["Scenario"]][["spec"]],
                   null = "No model specification",
                   modelSpec = "Model was specified.")
  if (is.null(spec__)) spec__ <- "No model specification information available!"
  conditions__ <- unique(x[["fixed.grid"]][[1]][["CONDITION"]])
  if (is.null(conditions__)) conditions__ <- "No conditions available"
  prediction__ <- FALSE
  if (!is.null(x[["prediction"]][[1]]))
    prediction <- TRUE
  multistart__ <- FALSE
  if (!is.null(x[["parframes"]][[1]]))
    multistart__ <- TRUE
  profiles__ <- FALSE
  if (!is.null(x[["profiles"]][[1]]))
    profiles__ <- TRUE
  message("--- Optimized IQRsysModel object - basic information: --")
  cat("\n")
  cat("Model:      ", model__, "\n")
  cat("Dosing:     ", dosing__, "\n")
  cat("Data:       ", data__, "\n")
  cat("Specs:      ", spec__, "\n")
  cat("\n")
  cat("Conditions: ", conditions__[1], "\n")
  if (length(conditions__) > 1)
    for (i__ in 2:length(conditions__)) cat("            ", conditions__[i__], "\n")
  cat("\n")
  message("--- Additional information stored: --------------------")
  cat("\n")
  cat("Multistart estimation results: ", ifelse(multistart__, "yes", "no"), "\n")
  cat("Profile likelihood analysis: ", ifelse(profiles__, "yes", "no"), "\n")
  cat("\n")
  pars_IQRsysModel(x)
}
#'@export
print.IQRsysModel <- function(x, ...) {
  args0__ <- attr(x, "args0")
  sysModelEst__ <- attr(x, "sysModelEst")
  model__ <- switch(args0__[["Scenario"]][["model"]],
                    filename = args0__$model,
                    IQRmodel = "IQRmodel object")
  if (is.null(model__)) model__ <- "No model information available!"
  dosing__ <- switch(args0__[["Scenario"]][["dosing"]],
                     null = "No dosing.",
                     IQRdosing = "IQRdosing object",
                     `List of IQRdosing` = "List of IQRdosing objects",
                     NLME = "Dosing provided with data")
  if (is.null(dosing__)) dosing__ <- "No dosing information available!"
  data__ <- switch(args0__[["Scenario"]][["data"]],
                   null = "No data",
                   IQRsysData = "IQRsysData object.",
                   NLME = sysModelEst__[["data"]][["datafile"]])
  if (is.null(data__)) data__ <- "No data information available!"
  spec__ <- switch(args0__[["Scenario"]][["spec"]],
                   null = "No model specification",
                   modelSpec = "Model was specified.")
  if (is.null(spec__)) spec__ <- "No model specification information available!"
  conditions__ <- unique(x[["fixed.grid"]][[1]][["CONDITION"]])
  if (is.null(conditions__)) conditions__ <- "No conditions available"
  prediction__ <- FALSE
  if (!is.null(x[["prediction"]][[1]]))
    prediction <- TRUE
  multistart__ <- FALSE
  if (!is.null(x[["parframes"]][[1]]))
    multistart__ <- TRUE
  profiles__ <- FALSE
  if (!is.null(x[["profiles"]][[1]]))
    profiles__ <- TRUE
  message("--- IQRsysModel object - basic information: -----------")
  cat("\n")
  cat("Model:      ", model__, "\n")
  cat("Dosing:     ", dosing__, "\n")
  cat("Data:       ", data__, "\n")
  cat("Specs:      ", spec__, "\n")
  cat("\n")
  cat("Conditions: ", conditions__[1], "\n")
  if (length(conditions__) > 1)
    for (i__ in 2:length(conditions__)) cat("            ", conditions__[i__], "\n")
  cat("\n")
  message("--- Additional information stored: --------------------")
  cat("\n")
  cat("Multistart estimation results: ", ifelse(multistart__, "yes", "no"), "\n")
  cat("Profile likelihood analysis: ", ifelse(profiles__, "yes", "no"), "\n")
  cat("\n")
}
#'@export
plotProfile_IQRsysModel <- function(res) {
  partable__ <- res[["IQRpartable"]][[1]]
  profiles__ <- res[["profiles"]][[1]]
  parframe__ <- res[["parframes"]][[1]]
  selected__ <- names(profiles__)
  if (is.null(profiles__)) {
    message("The IQRsysModel object did not contain profile information.")
    return(invisible())
  }
  profiles__ <- dplyr::bind_rows(profiles__, .id = "selected")
  profiles__ <- split(profiles__, profiles__[["whichPar"]])
  profiles__ <- lapply(seq_len(length(profiles__)), function(i__) {
    whichPar__ <- names(profiles__)[i__]
    trafo__ <- partable__[["trafo"]][match(whichPar__, partable__[["parametername"]])]
    myprofile__ <- profiles__[[i__]]
    if (trafo__ == "L") myprofile__[[whichPar__]] <- exp(myprofile__[[whichPar__]])
    if (trafo__ == "G") myprofile__[[whichPar__]] <- inv_logit(myprofile__[[whichPar__]])
    myprofile__
  })
  colors__ <- c("#000000", "#C5000B", "#0084D1", "#579D1C", "#FF950E", "#4B1F6F", "#CC79A7", "#006400", "#F0E442", "#8B4513")
  plots__ <- lapply(seq_along(profiles__), function(i__) {
    myprofile__ <- profiles__[[i__]]
    whichPar__ <- myprofile__[["whichPar"]][1]
    parvalues__ <- myprofile__[[whichPar__]]
    FLAGlog__ <- FALSE
    if (all(parvalues__ > 0) && log10(max(parvalues__)) - log10(min(parvalues__)) > 2)
      FLAGlog__ <- TRUE
    proflist__ <- split(myprofile__, myprofile__[["selected"]])
    proflist__ <- lapply(proflist__, function(p__) {
      p__ <- p__[, -1]
      p__ <- parframe(p__, obj.attributes = c("data", "prior"))
      p__
    })
    myplot__ <- plotProfile(proflist__, mode == "data")
    myplot__ <- myplot__ + theme(legend.position = "none")
    myplot__ <- suppressMessages(
      myplot__ +
        scale_y_continuous(breaks = c(0, 1, 2.71, 3.84),
                           labels = c(" 0%", "68%", "90%", "95%")))
    myplot__ <- myplot__ + ylab("Confidence Level")
    myselected__ <- unique(myprofile__[["selected"]])
    mycolors__ <- colors__[match(myselected__, selected__)]
    names(mycolors__) <- myselected__
    myplot__ <- suppressMessages(myplot__ + scale_color_manual(values = mycolors__))
    if (FLAGlog__)
      myplot__ <- myplot__ + scale_x_log10()
    return(myplot__)
  })
  legend__ <- NULL
  if (length(selected__) > 1) {
    index_selected__ <- as.numeric(sapply(strsplit(selected__, "\n", fixed = TRUE), function(x__) x__[[1]]))
    lines__ <- max(as.numeric(sapply(strsplit(selected__, "\n", fixed = TRUE), function(x__) length(x__))))
    full_plot__ <- plotProfile(res[["profiles"]][[1]], mode == "data")
    legend__ <- cowplot::get_legend(
      suppressMessages(
        full_plot__ +
          scale_color_manual(name = "-2LL, optimum",
                             values = colors__[1:length(selected__)],
                             labels = paste0(round(parframe__[["value"]][index_selected__], 3),
                                             ", ", selected__)) +
          guides(linetype = FALSE) +
          theme(legend.position = "right", legend.key.height = unit(lines__, "lines"))
      )
    )
    fig__ <- IQRoutputFigure(plots__, nrow = 3, ncol = 3,
                             opt.layout = list(legend.option = "common",
                                               legend.object = legend__,
                                               legend.relsize = 0.2))
  } else {
    fig__ <- IQRoutputFigure(plots__, nrow = 3, ncol = 3)
  }
  fig__
}
#'@export
plotWaterfall_IQRsysModel <- function(res, tol = 0.1, maxSteps = 8, maxDiff = 100) {
  if (maxSteps < 1)
    stopIQR("maxSteps must be >= 1")
  if (tol <= 0)
    stopIQR("Tolerance must be positive")
  digits__ <- -floor(log10(max(tol, 1e-5)))
  parframes__ <- res[["parframes"]][[1]]
  if (is.null(parframes__)) {
    message("The IQRsysModel object did not contain multi-start estimation information.")
    return(invisible())
  }
  parframes__[["converged"]] <- c("no", "yes")[parframes__[["converged"]] + 1]
  parframes__ <- parframes__[parframes__$value <= min(parframes__$value) + maxDiff,]
  myplot__ <- plotValues(parframes__, tol = tol)
  data__ <- attr(myplot__, "data")
  jumps__ <- attr(myplot__, "jumps")
  if (length(jumps__) > maxSteps) jumps__ <- jumps__[seq_len(maxSteps)]
  jumpdata__ <- do.call(rbind, lapply(jumps__, function(j__) {
    index__ <- j__
    value__ <- data__[j__, "value"]
    data.frame(index = index__, value = value__, optimum = as.character(match(j__, jumps__)))
  }))
  data__[["optimum"]] <- as.factor(sapply(1:nrow(data__), function(i__) tail(which(i__ >= jumps__), 1)))
  nopt__ <- length(levels(data__[["optimum"]]))
  myplot__ <- ggplot(data__, aes(x = 1:nrow(data__), y = value, pch = converged, color = optimum)) +
    geom_segment(data = jumpdata__,
                 aes(x = index, xend = index, y = -Inf, yend = value, color = optimum),
                 inherit.aes = FALSE, lty = 2) +
    geom_segment(data = jumpdata__,
                 aes(x = index, xend = Inf, y = value, yend = value, color = optimum),
                 inherit.aes = FALSE, lty = 2) +
    scale_y_continuous(sec.axis = dup_axis(breaks = round(jumpdata__$value, 3), name = NULL)) +
    scale_x_continuous(breaks = jumpdata__$index, sec.axis = dup_axis(breaks = scales::pretty_breaks(), name = NULL)) +
    expand_limits(y = min(data__[["value"]]) + tol) +
    geom_point() +
    scale_shape_manual(values = c("no" = 4, "yes" = 19)) +
    xlab("fit index (sorted by log-likelihood)") +
    ylab("objective value (-2LL)") +
    ggtitle("Final objective values of multi-start estimation") +
    scale_color_manual(values = rep_len(IQRtoolsColors[1:9], nopt__)) +
    themeIQRtools() +
    theme(axis.text.x.top = element_text(vjust = 1, margin = margin(2, 0, 6, 0)))
  attr_out__ <- list(
    data = data__,
    jumps = jumps__
  )
  out <- IQRoutputFigure(myplot__)
  attr(out, "plotData") <- attr_out__
  return(out)
}
#'@export
plotPars_IQRsysModel <- function(res, tol = 0.1, maxSteps = 8) {
  digits__ <- -floor(log10(max(tol, 1e-5)))
  out__ <- plotWaterfall_IQRsysModel(res, tol, maxSteps)
  data__ <- attr(out__, "plotData")[["data"]]
  jumps__ <- attr(out__, "plotData")[["jumps"]]
  objvalues__ <- round(data__[["value"]][jumps__], digits = digits__)
  partable__ <- res[["IQRpartable"]][[1]]
  data__[["optimum, -2LL"]] <- paste0(jumps__[sapply(1:nrow(data__), function(i__) tail(which(i__ >= jumps__), 1))],
                                      ", ",
                                      objvalues__[sapply(1:nrow(data__), function(i__) tail(which(i__ >= jumps__), 1))])
  data__[["optimum, -2LL"]] <- factor(data__[["optimum, -2LL"]], levels = unique(data__[["optimum, -2LL"]]))
  data__ <- data__[, !grepl("^ETA_", names(data__))]
  data__ <- tidyr::gather(data__, key = "parameter", value = "parvalue", attr(data__, "parameters"))
  data__[["trafo"]] <- partable__[["trafo"]][match(data__[["parameter"]], partable__[["parametername"]])]
  data__ <- dplyr::mutate(data__, parvalue = ifelse(trafo %in% "L", exp(parvalue), parvalue))
  data__ <- dplyr::mutate(data__, parvalue = ifelse(trafo %in% "G", inv_logit(parvalue), parvalue))
  plots__ <- lapply(unique(data__[["trafo"]]), function(trafo__) {
    ylab__ <- "estimated value"
    if (trafo__ == "N") ylab__ <- "estimated value (natural scale)"
    if (trafo__ == "L") ylab__ <- "estimated value (log scale)"
    if (trafo__ == "G") ylab__ <- "estimated value (logit scale)"
    subdata__ <- dplyr::filter(data__, trafo %in% trafo__)
    ncol__ <- length(unique(subdata__[["optimum, -2LL"]]))
    myplot__ <- ggplot(subdata__, aes(y = parvalue, x = parameter, color = `optimum, -2LL`)) +
      geom_point(aes(pch = converged)) +
      geom_boxplot(fill = "white", position = "identity", width = 0.33) +
      scale_shape_manual(values = c("no" = 4, "yes" = 19)) +
      scale_x_discrete(name = NULL) +
      ylab(ylab__) +
      scale_color_manual(values = rep_len(IQRtoolsColors[1:9], ncol__)) +
      themeIQRtools() +
      theme(legend.position = "none")
    if (trafo__ == "L")
      myplot__ <- myplot__ + scale_y_log10()
    myplot__ <- myplot__ + coord_flip()
    return(myplot__)
  })
  legend__ <- cowplot::get_legend(plots__[[1]] + theme(legend.position = "right"))
  counts__ <- sapply(unique(data__[["trafo"]]), function(n__) length(which(data__[["trafo"]] %in% n__)))
  counts__ <- counts__/min(counts__)
  out__ <- IQRoutputFigure(plots__,
                           opt.layout = opt.layout(ncol = 1, npage = 1,
                                                   legend.option = "common",
                                                   legend.relsize = 0.2,
                                                   legend.position = "right"))
  attr(out__, "plotData") <- data__
  return(out__)
}
#'@export
plotPred_IQRsysModel <- function(sysModel, tol = 0.1, states = NULL, conditions = NULL, IDs = NULL) {
  data__ <- sysModel[["data"]][[1]]
  parframe__ <- sysModel[["parframes"]][[1]]
  prd__ <- sysModel[["prd"]][[1]]
  times__ <- sysModel[["times"]][[1]]
  g__ <- sysModel[["g"]][[1]]
  controls(g__, NULL, "attach.input") <- TRUE
  if (is.null(parframe__))
    stopIQR("No multi-start estimation results available.")
  pars__ <- unique(parframe__, tol = tol)
  jumps__ <- match(pars__[["index"]], parframe__[["index"]])
  objvals__ <- round(parframe__[["value"]][jumps__], 3)
  prediction__ <- lapply(1:nrow(pars__), function(i__) {
    parvec__ <- as.parvec(pars__, i__)
    simulation__ <- as.data.frame(prd__(times__, parvec__), data = data__)
    simulation__ <- dplyr::filter(simulation__, time %in% times__)
  })
  names(prediction__) <- paste0(jumps__, ", ", objvals__)
  prediction__ <- dplyr::bind_rows(prediction__, .id = "optimum")
  prediction__[["optimum"]] <- factor(prediction__[["optimum"]], levels = unique(prediction__[["optimum"]]))
  if (!is.null(states))
    prediction__ <- filter(prediction__, name %in% states)
  if (!is.null(conditions))
    prediction__ <- filter(prediction__, CONDITION %in% conditions)
  if (!is.null(IDs))
    prediction__ <- filter(prediction__, ID %in% IDs)
  levels__ <- try(unique(alphnumsort(prediction__[["condition"]])), silent = TRUE)
  if (!inherits(levels__, "try-error"))
    prediction__[["condition"]] <- factor(prediction__[["condition"]], levels = levels__)
  ncols__ <- length(unique(prediction__[["optimum"]]))
  p__ <- ggplot(prediction__, aes(x = time, y = value, color = optimum, group = paste(optimum, ID))) +
    facet_wrap(~name*CONDITION, scales = "free") +
    geom_line() +
    scale_color_manual(name = "optimum, -2LL", values = rep_len(IQRtoolsColors[1:9], ncols__)) +
    themeIQRtools()
  out <- list(p__)
  attr(out, "plotData") <- prediction__
  class(out) <- "IQRslideplot"
  return(out)
}
#'@export
plotFit_IQRsysModel <- function(res, OUTPUT = 1) {
  if (is.null(res[["parframes"]][[1]])) {
    warningIQR("The IQRsysModel object did not contain estimation information.",
               "The plot shows the data and prediction as they were found in the object.")
  }
  prediction__ <- res[["prediction"]][[1]]
  datalist__ <- res[["data"]][[1]]
  data__ <- as.data.frame(datalist__)
  times__ <- sort(unique(prediction__[["time"]]))
  pars__ <- res[["pars"]][[1]]
  prd__ <- res[["prd"]][[1]]
  errfn__ <- res[["errfn"]][[1]]
  pars_pop__ <- pars__
  if (any(grepl("^ETA_", names(pars_pop__)))) {
    pars_pop__[grepl("^ETA_", names(pars_pop__))] <- 0
    prediction_pop__ <- as.data.frame(prd__(times__, pars_pop__, deriv = FALSE), errfn = errfn__, data = datalist__)
    prediction__ <- dplyr::bind_rows(
      dplyr::mutate(prediction__, which = "individual"),
      dplyr::mutate(prediction_pop__, which = "population")
    )
  }
  prediction__ <- dplyr::filter(prediction__, name == paste0("OUTPUT", OUTPUT))
  data__ <- dplyr::filter(data__, name == paste0("OUTPUT", OUTPUT))
  if (!"CONDITION" %in% names(prediction__))
    prediction__[["CONDITION"]] <- prediction__[["ID"]]
  if (!"CONDITION" %in% names(data__))
    data__[["CONDITION"]] <- data__[["ID"]]
  levels_ID__ <- try(unique(alphnumsort(c(data__[["ID"]], prediction__[["ID"]]))), silent = TRUE)
  levels_CONDITION__ <- try(unique(alphnumsort(c(data__[["CONDITION"]], prediction__[["CONDITION"]]))), silent = TRUE)
  if (!inherits(levels_ID__, "try-error")) {
    prediction__[["ID"]] <- factor(prediction__[["ID"]], levels = levels_ID__)
    data__[["ID"]] <- factor(data__[["ID"]], levels = levels_ID__)
  } else {
    prediction__[["ID"]] <- as.factor(prediction__[["ID"]])
    data__[["ID"]] <- as.factor(data__[["ID"]])
  }
  if (!inherits(levels_CONDITION__, "try-error")) {
    prediction__[["CONDITION"]] <- factor(prediction__[["CONDITION"]], levels = levels_CONDITION__)
    data__[["CONDITION"]] <- factor(data__[["CONDITION"]], levels = levels_CONDITION__)
  } else {
    prediction__[["CONDITION"]] <- as.factor(prediction__[["CONDITION"]])
    data__[["CONDITION"]] <- as.factor(data__[["CONDITION"]])
  }
  prediction__ <- split(prediction__, prediction__[["CONDITION"]], drop = TRUE)
  data__ <- split(data__, data__[["CONDITION"]], drop = TRUE)
  conditions__ <- union(names(prediction__), names(data__))
  attr_out__ <- list(
    prediction = prediction__,
    data = data__,
    conditions = conditions__
  )
  plots__ <- lapply(conditions__, function(c__) {
    mypred__ <- prediction__[[c__]]
    mydata__ <- data__[[c__]]
    tmin__ <- suppressWarnings(min(mydata__[["time"]]))
    tmax__ <- suppressWarnings(max(mydata__[["time"]]))
    if (is.infinite(tmin__))
      tmin__ <- suppressWarnings(min(mypred__[["time"]]))
    if (is.infinite(tmax__))
      tmax__ <- suppressWarnings(max(mypred__[["time"]]))
    if (is.infinite(tmin__))
      tmin__ <- 0
    if (is.infinite(tmax__))
      tmax__ <- 1
    myplot__ <- ggplot(mapping = aes(x = time, y = value,
                                     color = ID, fill = ID)) + 
      facet_wrap(~CONDITION, scales = "free") +
      theme(legend.position = c(1, 1), legend.justification = c(1, 1),
            legend.background = element_rect(fill = grDevices::rgb(1, 1, 1, .2)))
    if (!is.null(mypred__)) {
      mypred__ <- dplyr::filter(mypred__, time >= tmin__ & time <= tmax__)
      if ("which" %in% names(mypred__)) {
        if (any(!is.na(mypred__[["sigma"]])))
          myplot__ <- myplot__ +
            geom_ribbon(data = filter(mypred__, which == "individual"),
                        aes(ymin = value - sigma, ymax = value + sigma), alpha = .3, lty = 0)
        myplot__ <- myplot__ +
          geom_line(data = mypred__, aes(linetype = which, size = which) ) +
          scale_linetype_manual("Prediction", values = c("individual"=1,"population"=2)) +
          scale_size_manual("Prediction", values = c("individual"=0.6,"population"=1))
      } else {
        if (any(!is.na(mypred__[["sigma"]])))
          myplot__ <- myplot__ +
            geom_ribbon(data = mypred__,
                        aes(ymin = value - sigma, ymax = value + sigma), alpha = .3, lty = 0)
        myplot__ <- myplot__ +
          geom_line(data = mypred__)
      }
    }
    if (!is.null(mydata__)) {
      mydata__ <- dplyr::filter(mydata__, time >= tmin__ & time <= tmax__)
      if (any(!is.na(mydata__[["sigma"]])))
        myplot__ <- myplot__ +
          geom_errorbar(data = mydata__,
                        aes(ymin = value - sigma, ymax = value + sigma), width = 0)
      if (any(mydata__[["value"]] <= mydata__[["lloq"]]))
        myplot__ <- myplot__ +
          geom_point(data = mydata__, aes(pch = ifelse(value <= lloq, "yes", "no"))) +
          scale_shape_manual(name = "BLOQ", values = c(yes = 4, no = 19))
      else
        myplot__ <- myplot__ +
          geom_point(data = mydata__)
    }
    return(myplot__ + themeIQRtools())
  })
  out__ <- IQRoutputFigure(plots__, nrow = 2, ncol = 2)
  attr(out__, "plotData") <- attr_out__
  return(out__)
}
#'@export
plotWRES_IQRsysModel <- function(res, OUTPUT = 1) {
  if (is.null(res[["parframes"]][[1]])) {
    warningIQR("The IQRsysModel object did not contain estimation information.",
               "The plot shows the data and prediction as they were found in the object.")
  }
  data__ <- res[["IQRpredtable"]][[1]]
  data__ <- dplyr::filter(data__, name == paste0("OUTPUT", OUTPUT))
  attr_out__ <- data__
  data__[["ID"]] <- as.factor(data__[["ID"]])
  data__[["CONDITION"]] <- as.factor(data__[["CONDITION"]])
  IDs__ <- unique(data__[["ID"]])
  data__[["wres_indiv"]][data__[["wres_indiv"]] < -5] <- -Inf
  plot1__ <- ggplot(data__, aes(x = time, y = wres_indiv, color = ID)) +
    facet_wrap(~CONDITION, scales = "free") +
    geom_hline(yintercept = -1, lty = 2, color = c("gray60")) +
    geom_hline(yintercept = 0, lty = 2, color = c("black")) +
    geom_hline(yintercept = 1, lty = 2, color = c("gray60")) +
    geom_point(aes(pch = ifelse(value <= lloq, "yes", "no"))) +
    scale_shape_manual(name = "BLOQ", values = c(yes = 4, no = 19)) +
    ggtitle("Weighted residuals vs time", paste0("OUTPUT", OUTPUT)) +
    xlab("time") + ylab("weighted residual")
  plot2__ <- ggplot(data__, aes(x = pred_indiv, y = wres_indiv, color = ID)) +
    facet_wrap(~CONDITION, scales = "free") +
    geom_hline(yintercept = -1, lty = 2, color = c("gray60")) +
    geom_hline(yintercept = 0, lty = 2, color = c("black")) +
    geom_hline(yintercept = 1, lty = 2, color = c("gray60")) +
    geom_point(aes(pch = ifelse(value <= lloq, "yes", "no"))) +
    scale_shape_manual(name = "BLOQ", values = c(yes = 4, no = 19)) +
    ggtitle("Weighted residuals vs prediction", paste0("OUTPUT", OUTPUT)) +
    xlab("prediction") + ylab("weighted residual")
  plot1__ <- plot1__ + scale_color_manual(values = rep_len(IQRtoolsColors[1:9], length(IDs__)))
  plot2__ <- plot2__ + scale_color_manual(values = rep_len(IQRtoolsColors[1:9], length(IDs__)))
  out <- list(plot1__ + themeIQRtools(),
              plot2__ + themeIQRtools())
  attr(out, "plotData") <- attr_out__
  class(out) <- "IQRslideplot"
  return(out)
}
#'@export
plotDVPRED_IQRsysModel <- function(res, OUTPUT = 1) {
  if (is.null(res[["parframes"]][[1]])) {
    warningIQR("The IQRsysModel object did not contain estimation information.",
               "The plot shows the data and prediction as they were found in the object.")
  }
  data__ <- res[["IQRpredtable"]][[1]]
  if (is.null(data__)) {
    res <- pred_IQRsysModel(res)
    data__ <- res[["IQRpredtable"]][[1]]
  }
  data__ <- dplyr::filter(data__, name == paste0("OUTPUT", OUTPUT))
  attr_out__ <- data__
  data__[["ID"]] <- as.factor(data__[["ID"]])
  data__[["CONDITION"]] <- as.factor(data__[["CONDITION"]])
  IDs__ <- unique(data__[["ID"]])
  plot3__ <- ggplot(data__, aes(x = pred_indiv, y = value, color = ID)) +
    facet_wrap(~CONDITION, scales = "free") +
    geom_abline(slope = 1, intercept = 0, lty = 2, color = "black") +
    geom_point(aes(pch = ifelse(value <= lloq, "yes", "no"))) +
    scale_shape_manual(name = "BLOQ", values = c(yes = 4, no = 19)) +
    ggtitle("Data vs prediction", paste0("OUTPUT", OUTPUT)) +
    xlab("prediction") + ylab("data")
  plot3__ <- plot3__ + scale_color_manual(values = rep_len(IQRtoolsColors[1:9], length(IDs__)))
  out <- list(plot3__ + themeIQRtools())
  attr(out, "plotData") <- attr_out__
  class(out) <- "IQRslideplot"
  return(out)
}
#'@export
tablePars_IQRsysModel <- function(res, digits = NA, parameters = NULL, showInitial = FALSE, FLAGBICc = FALSE) {
  partable__ <- res[["IQRpartable"]][[1]]
  parsOfInterest__ <- attr(res, "sysModelEst")[["parsOfInterest"]]
  partable__ <- dplyr::filter(partable__, !parametertask %in% "dosing" | estimated == 1)
  partable__ <- dplyr::filter(partable__, !parametertask %in% "ETA")
  if (!is.null(parsOfInterest__)) {
    if (!is.null(parameters)) {
      ismatch__ <- sapply(parsOfInterest__, function(mypar__) mypar__ %in% parameters)
      parsOfInterest__ <- parsOfInterest__[ismatch__]
      partable__ <- dplyr::filter(partable__, parametername %in% parsOfInterest__ | base_par %in% parsOfInterest__ | belongs_to %in% parsOfInterest__)
    } else {
      partable__ <- dplyr::filter(partable__, parametertask != "pop" | parametername %in% parsOfInterest__ | base_par %in% parsOfInterest__)
    }
  }
  partable__[["digits"]] <- digits
  if (is.null(partable__[["sigma_linear"]])) partable__[["sigma_linear"]] <- NA
  table__ <- IQRtable(partable__,
                      stat = statSE(value = parametervalue_linear, se = sigma_linear, digits = digits),
                      "PARAMETER" = "printname",
                      "VALUE" = "value",
                      "RSE" = "rse%",
                      "COMMENT" = "Notes")
  table__[, 3] <- gsub("--%", "-", table__[, 3], fixed = TRUE)
  table__[is.na(table__[, 4]), 4] <- "-"
  table__[-1, 2] <- paste0(table__[-1, 2], ifelse(partable__[["estimated"]] == 0, " (FIX)", ""))
  if ("lower95_parametervalue_linear" %in% names(partable__)) {
    tableCI__ <- IQRtable(partable__,
                          stat = statCI(value = parametervalue_linear,
                                        lower = lower95_parametervalue_linear,
                                        upper = upper95_parametervalue_linear,
                                        digits = digits),
                          "PARAMETER" = "printname",
                          "VALUE" = "value",
                          "CI (95%)" = "[lower.95, upper.95]")
    tableCI__[, 3] <- gsub("[--, --]", "-", tableCI__[, 3], fixed = TRUE)
    table__ <- cjoin_IQRtable(table__, tableCI__[, -2])
  }
  if ("shrinkage" %in% names(partable__) && any(!is.na(partable__[["shrinkage"]]))) {
    tableShrinkage__ <- as_IQRtable(data.frame(
      PARAMETER = partable__[["printname"]],
      SHRINKAGE = ifelse(is.na(partable__[["shrinkage"]]),
                         yes = "-",
                         no = paste0(round(100*partable__[["shrinkage"]]), "%"))
    ))
    table__ <- cjoin_IQRtable(table__, tableShrinkage__)
  }
  if (showInitial) {
    parameter__ <- table__[-1, 1]
    selected__ <- res[["selected"]][[1]][1]
    if (is.null(selected__)) selected__ <- 1
    index__ <- res[["parframes"]][[1]][["index"]][selected__]
    if (is.null(index__)) index__ <- 1
    initial__ <- unlist(res[["startpars"]][[1]][index__, ])
    initial__ <- initial__[names(initial__) %in% parameter__]
    transformation__ <- partable__$trafo[match(names(initial__), partable__$parametername)]
    names(transformation__) <- names(initial__)
    initial__ <- dplyr::case_when(
      transformation__ == "N" ~ initial__,
      transformation__ == "G" ~ inv_logit(initial__),
      transformation__ == "L" ~ exp(initial__),
      TRUE ~ initial__
    )
    names(initial__) <- names(transformation__)
    if (is.na(digits))
      initial__ <- sapply(initial__, function(x__) format(x__, digits = 2))
    else
      initial__ <- sapply(initial__, function(x__) format(x__, digits = digits))
    outinitial__ <- rep("", length(parameter__))
    outinitial__[match(names(initial__), parameter__)] <- initial__
    outinitial__ <- matrix(c("INITIAL GUESS", outinitial__), ncol = 1)
    table__ <- cbind(table__[, 1], outinitial__, table__[, -1])
    class(table__) <- c("IQRtable", "matrix")
  }
  states__ <- names(getEquations(res$x[[1]])[[1]])
  ini_pars__ <- partable__$printname[partable__$parametername %in% states__ | partable__$base_par %in% states__]
  err_pars__ <- partable__$printname[partable__$parametertask %in% "error"]
  omg_pars__ <- partable__$printname[partable__$parametertask %in% "omega"]
  bet_pars__ <- partable__$printname[partable__$parametertask %in% "beta"]
  pop_pars__ <- setdiff(partable__$printname, c(ini_pars__, err_pars__, bet_pars__, omg_pars__))
  pattern <- "Z[1, ]\n "
  if (length(pop_pars__) > 0)
    pattern <- paste0(pattern, "\n **Typical parameters**\n Z[match(pop, Z[,1]), ]\n ")
  if (length(omg_pars__) > 0)
    pattern <- paste0(pattern, "\n **Inter-individual variability**\n Z[match(omg, Z[,1]), ]\n ")
  if (length(bet_pars__) > 0)
    pattern <- paste0(pattern, "\n **Parameter-Covariate relationships**\n Z[match(bet, Z[,1]), ]\n ")
  if (length(err_pars__) > 0)
    pattern <- paste0(pattern, "\n **Residual variability**\n Z[match(err, Z[,1]), ]\n ")
  metric.keys__ <- c("OBJ", "BIC", "AIC")
  if (FLAGBICc) metric.keys__ <- c("OBJ", "BICc", "AIC")
  if (!is.null(res[["metrics"]][[1]])) {
    metrics__ <- res[["metrics"]][[1]]
    metrics__ <- data.frame(
      metric = c("OBJ", "BIC", "AIC"),
      value = round(metrics__[["value"]][match(metric.keys__, metrics__[["metric"]])]),
      stringsAsFactors = FALSE
    )
    metrics__ <- as_IQRtable(metrics__)
  } else {
    metrics__ <- data.frame(
      metric = c("OBJ", "BIC", "AIC"),
      value = c("-", "-", "-"),
      stringsAsFactors = FALSE
    )
  }
  pattern <- paste0(pattern, "\n **Model metrics**\n N[-1, ]\n ")
  out__ <-   compose_IQRtable(
    Z = table__,
    N = metrics__,
    pop = pop_pars__,
    ini = ini_pars__,
    bet = bet_pars__,
    omg = omg_pars__,
    err = err_pars__,
    pattern = pattern
  )
  colnames__ <- intersect(c("PARAMETER", "INITIAL GUESS", "VALUE", "RSE", "CI (95%)", "SHRINKAGE", "COMMENT"),
                          out__[1,])
  as_IQRtable(out__[, match(colnames__, out__[1,]), drop = FALSE])
}
is_IQRsysModelEst <- function(input) {
  methods::is(input, "IQRsysModelEst")
}
is_IQRsysData <- function(input) {
  methods::is(input, "IQRsysData")
}
is_IQRsysModel <- function(input) {
  methods::is(input, "IQRsysModel")
}
pars_IQRsysModel <- function(sysModel, ..., parameters = NULL, dosing = NULL, FLAGverbose = FALSE) {
  if (!is_IQRsysModel(sysModel)) stopIQR("sysModel is not an IQRsysModel object")
  dots__ <- list(...)
  get__ <- set__ <- numeric(0)
  if (length(dots__) > 0) {
    get__ <- as.integer(which(sapply(dots__, is.character)))
    set__ <- as.integer(which(sapply(dots__, is.numeric)))
  }
  parameters.arg__ <- parameters
  dosing.arg__ <- dosing
  IQRpartable__ <- sysModel[["IQRpartable"]][[1]]
  if (length(set__) == 0) {
    parameters__ <- NULL
  } else {
    parameters__ <- unlist(
      lapply(set__, function(i__) {
        n__ <- names(dots__)[i__]
        v__ <- dots__[[i__]]
        if (is.null(names(v__))) names(v__) <- rep("", length(v__))
        is_condition <- !names(v__) %in% c("", n__, "BASE")
        names(v__)[is_condition] <- paste(n__, names(v__)[is_condition], sep = "_")
        names(v__)[!is_condition] <- n__
        if (any(duplicated(names(v__))))
          stopIQR("Vector of parameter values for ", n__, " were badly defined. ",
                  "Names should correspond to CONDITIONs or should be omitted if referring to the BASE parameter.")
        v__
      })
    )
  }
  parameters__ <- c(parameters__, parameters.arg__)[union(names(parameters__), names(parameters.arg__))]
  if (!is.null(dosing) & any(grepl("^INPUT[1-9]", names(attr(sysModel$model[[1]]$func, "equations"))))) {
    meta__ <- attr(sysModel$data[[1]], "condition.grid")
    dosing__ <- NULL
    if (is_IQRdosing(dosing)) {
      dosing__ <- do.call(rbind, lapply(seq_len(nrow(meta__)), function(i__) {
        mymeta__ <- meta__[i__,]
        mymeta__$CONDITION <- "BASE"
        rownames(dosing) <- NULL
        rownames(mymeta__) <- NULL
        cbind(mymeta__, dosing)
      }))
    } else if (is.list(dosing) && all(sapply(dosing, is_IQRdosing))) {
      available__ <- intersect(meta__$CONDITION, names(dosing))
      meta__ <- meta__[meta__$CONDITION %in% available__,]
      dosing__ <- suppressWarnings(do.call(rbind, lapply(seq_len(nrow(meta__)), function(i__) {
        mymeta__ <- meta__[i__,]
        cbind(mymeta__, dosing[[mymeta__$CONDITION]])
      })))
    }
    if (!is.null(dosing__)) {
      inputs__ <- grep("^INPUT[1-9]", names(attr(sysModel$model[[1]]$func, "equations")), value = TRUE)
      dosing_pars__ <- dMod_build_dosing_parameters(dosing__, inputs = length(inputs__))
      dosing_pars__ <- dosing_pars__[!grepl("^Tlag", names(dosing_pars__))]
      dosing_parnames__ <- setdiff(names(dosing_pars__), names(meta__))
      if (any(dosing_pars__$CONDITION %in% "BASE")) {
        base_dosing_pars__ <- dosing_pars__[match("BASE", dosing_pars__$CONDITION), dosing_parnames__]
        parameters__ <- c(parameters__, unlist(base_dosing_pars__))
      }
      conditions__ <- setdiff(dosing_pars__$CONDITION, "BASE")
      specific_dosing_pars__ <- dosing_pars__[match(conditions__, dosing_pars__$CONDITION), dosing_parnames__]
      if (nrow(specific_dosing_pars__) > 0) {
        specific_dosing_pars__ <- lapply(seq_len(nrow(specific_dosing_pars__)), function(i__) {
          structure(
            unlist(specific_dosing_pars__[i__, dosing_parnames__], use.names = FALSE),
            names = paste(dosing_parnames__, dosing_pars__$CONDITION[i__], sep = "_")
          )
        })
        parameters__ <- c(parameters__, unlist(specific_dosing_pars__))
      }
    }
  }
  if (!is.null(parameters__)) {
    not_defined__ <- setdiff(names(parameters__), IQRpartable__[["parametername"]])
    if (length(not_defined__) > 0) {
      warningIQR("The following parameters were not recognized and were ignored: ",
                 paste(not_defined__, collapse = ", "))
    }
    defined__ <- intersect(names(parameters__), IQRpartable__[["parametername"]])
    rowindex__ <- match(defined__, IQRpartable__[["parametername"]])
    IQRpartable__[["parametervalue_linear"]][rowindex__] <- parameters__[defined__]
    IQRpartable__[["parametervalue"]] <- IQRpartable__[["parametervalue_linear"]]
    IQRpartable__[["parametervalue"]][IQRpartable__$trafo == "L"] <- pmax(-750, log(IQRpartable__$parametervalue[IQRpartable__$trafo == "L"]))
    IQRpartable__[["parametervalue"]][IQRpartable__$trafo == "G"] <- logit(IQRpartable__$parametervalue[IQRpartable__$trafo == "G"])
    uncertainty_cols__ <- c("sigma", "sigma_linear", "RSE_linear",
                            "lower95_parametervalue_linear", "upper95_parametervalue_linear",
                            "lower68_parametervalue_linear", "upper68_parametervalue_linear")
    for (n__ in uncertainty_cols__) {
      if (n__ %in% names(IQRpartable__))
        IQRpartable__[[n__]][rowindex__] <- NA
    }
  }
  if (length(get__) == 0) {
    parameters_df_print__ <- IQRpartable__
  } else {
    parameters__ <- as.character(unlist(dots__[get__]))
    select__ <-
      IQRpartable__[["parametername"]] %in% parameters__ |
      IQRpartable__[["base_par"]] %in% parameters__
    parameters_df_print__ <- IQRpartable__[select__, ]
  }
  if (FLAGverbose)
    print_IQRpartable(parameters_df_print__, level = ifelse("bestfit" %in% names(sysModel), 2, 1), optimum = sysModel[["selected"]][[1]])
  if (length(set__) == 0 & is.null(parameters) & is.null(dosing)) {
    parvec__ <- parameters_df_print__[["parametervalue_linear"]]
    names(parvec__) <- parameters_df_print__[["parametername"]]
    attr(parvec__, "se") <- parameters_df_print__[["sigma_linear"]]
    attr(parvec__, "lower95") <- parameters_df_print__[["lower95_parametervalue_linear"]]
    attr(parvec__, "upper95") <- parameters_df_print__[["upper95_parametervalue_linear"]]
    return(invisible(parvec__))
  }
  sysModel <- mutate_IQRsysModel(sysModel, IQRpartable = list(IQRpartable__))
  sysModel <- grids_IQRsysModel(sysModel)
  sysModel <- spec_IQRsysModel(sysModel)
  p0__ <- sysModel[["p0"]][[1]]
  prd0__ <- sysModel[["g"]][[1]]*sysModel[["x"]][[1]]*p0__
  fixed.grid__ <- sysModel[["fixed.grid"]][[1]]
  est.grid__ <- sysModel[["est.grid"]][[1]]
  prd__ <- dMod_PRD_indiv(prd = prd0__, fixed.grid = fixed.grid__, est.grid = est.grid__)
  p__   <- dMod_P_indiv(p = p0__, fixed.grid = fixed.grid__, est.grid = est.grid__)
  sysModel <- mutate_IQRsysModel(sysModel, prd = list(prd__), p = list(p__))
  return(sysModel)
}
vcov_IQRsysModel <- function(mymodel) {
  fixed__ <- c(mymodel[["fixed_on_lower"]][[1]], mymodel[["fixed_on_upper"]][[1]])
  FLAGderiv__ <- mymodel[["options"]][[1]][["opt.method"]] %in% "trust"
  if (FLAGderiv__) {
    mymodel <- mutate_IQRsysModel(mymodel, hessian = list(structure(obj(pars)[["hessian"]], dimnames = list(names(pars), names(pars)))))
  } else {
    mymodel <- mutate_IQRsysModel(mymodel, hessian = list(dMod_approxHessian(obj, pars, diag = TRUE)))
  }
  hessian__ <- mymodel[["hessian"]][[1]]
  vcov__ <- 0*hessian__
  is_fixed__ <- colnames(hessian__) %in% names(fixed__)
  subhessian__ <- hessian__[!is_fixed__, !is_fixed__, drop = FALSE]
  subvcov__ <- try(solve(0.5*subhessian__), silent = TRUE)
  if (inherits(subvcov__, "try-error")) subvcov__ <- MASS::ginv(subhessian__)
  vcov__[!is_fixed__, !is_fixed__] <- subvcov__
  mymodel <- mutate_IQRsysModel(mymodel, vcov = list(structure(vcov__, dimnames = list(names(pars), names(pars)))))
  subhessian__ <- mymodel[["hessian"]][[1]][!is_fixed__, !is_fixed__, drop = FALSE]
  subvcov__ <- mymodel[["vcov"]][[1]][!is_fixed__, !is_fixed__, drop = FALSE]
  eigen__ <- eigen(subhessian__)
  tol__ <- sqrt(.Machine$double.eps)
  V__ <- eigen__[["vectors"]][, abs(eigen__[["values"]]) < tol__, drop = FALSE]
  identifiable__ <- apply(V__, 1, function(v) all(abs(v) < tol__))
  diag(subvcov__)[!identifiable__] <- Inf
  vcov__ <- mymodel[["vcov"]][[1]]
  vcov__[!is_fixed__, !is_fixed__] <- subvcov__
  mymodel <- mutate_IQRsysModel(mymodel, vcov = list(vcov__))
  return(mymodel)
}
pred_IQRsysModel <- function(res) {
  datalist__ <- res[["data"]][[1]]
  data_indiv__ <- as.data.frame(datalist__)
  timesD__ <- unique(data_indiv__[["time"]])
  prd__ <- res[["prd"]][[1]]
  pars__ <- res[["pars"]][[1]]
  data__ <- res[["data"]][[1]]
  errfn__ <- res[["e"]][[1]]
  prediction_indiv__ <- as.data.frame(prd__(timesD__, pars__, deriv = FALSE), errfn = errfn__, data = datalist__)
  pars_pop__ <- pars__
  pars_pop__[grepl("^ETA_", names(pars_pop__))] <- 0
  prediction_pop__ <- as.data.frame(prd__(timesD__, pars_pop__, deriv = FALSE), errfn = errfn__, data = datalist__)
  if (!"CONDITION" %in% names(prediction_indiv__))
    prediction_indiv__[["CONDITION"]] <- prediction_indiv__[["ID"]]
  if (!"CONDITION" %in% names(prediction_pop__))
    prediction_pop__[["CONDITION"]] <- prediction_pop__[["ID"]]
  if (!"CONDITION" %in% names(data_indiv__))
    data_indiv__[["CONDITION"]] <- data_indiv__[["ID"]]
  match.approx <- function(x, table, tol = getOption("approx.tol", default = 1e-6)) {
    stopifnot(is.numeric(x), is.numeric(table))
    metric <- outer(x, table, function(x, y) abs(x - y))
    position <- apply(metric, 1, which.min)
    position[abs(x - table[position]) > tol] <- NA
    return(position)
  }
  outputs__ <- as.character(unique(prediction_indiv__[["name"]]))
  predframe__ <- do.call(rbind, lapply(outputs__, function(OUTPUT) {
    pred_indiv__ <- dplyr::filter(prediction_indiv__, name == OUTPUT)
    pred_pop__   <- dplyr::filter(prediction_pop__, name == OUTPUT)
    data__       <- dplyr::filter(data_indiv__, name == OUTPUT)
    if (nrow(pred_indiv__) == 0 | nrow(data__) == 0)
      return()
    data__[["pred_indiv"]] <- NA
    data__[["pred_pop"]] <- NA
    data__[["res_indiv"]] <- NA
    data__[["res_pop"]] <- NA
    data__[["wres_indiv"]] <- NA
    data__[["wres_pop"]] <- NA
    data__[["CONDID"]] <- paste0(data__[["ID"]],"-", data__[["CONDITION"]])
    pred_indiv__[["CONDID"]] <- paste0(pred_indiv__[["ID"]],"-", pred_indiv__[["CONDITION"]])
    pred_pop__[["CONDID"]] <- paste0(pred_pop__[["ID"]],"-", pred_pop__[["CONDITION"]])
    condids__ <- as.character(unique(data__[["CONDID"]]))
    for (ci__ in condids__) {
      idxCONDID_d__ <- data__[["CONDID"]] == ci__
      idxCONDID_i__ <- pred_indiv__[["CONDID"]] == ci__
      idxCONDID_p__ <- pred_pop__[["CONDID"]] == ci__
      idxMatch_i__ <- match.approx(data__$time[idxCONDID_d__], pred_indiv__$time[idxCONDID_i__])
      idxMatch_p__ <- match.approx(data__$time[idxCONDID_d__], pred_pop__$time[idxCONDID_p__])
      y__       <- data__[["value"]][idxCONDID_d__]
      sigma_y__ <- data__[["sigma"]][idxCONDID_d__]
      value_indiv__ <- pred_indiv__[["value"]][idxCONDID_i__][idxMatch_i__]
      value_pop__   <- pred_pop__[["value"]][idxCONDID_p__][idxMatch_p__]
      sigma_value_indiv__ <- pred_indiv__[["sigma"]][idxCONDID_i__][idxMatch_i__]
      sigma_value_pop__   <- pred_pop__[["sigma"]][idxCONDID_p__][idxMatch_p__]
      res_indiv__ <- value_indiv__ - y__
      res_pop__   <- value_pop__   - y__
      wres_indiv__ <- wres_pop__ <- rep(NA, sum(idxCONDID_d__))
      idxSigObs__ <- !is.na(sigma_y__)
      wres_indiv__[idxSigObs__] <- res_indiv__[idxSigObs__]/sigma_y__[idxSigObs__]
      wres_pop__[idxSigObs__]   <- res_pop__[idxSigObs__]/sigma_y__[idxSigObs__]
      idxSigPred__ <- is.na(sigma_y__)
      wres_indiv__[idxSigPred__] <- res_indiv__[idxSigPred__]/sigma_value_indiv__[idxSigPred__]
      wres_pop__[idxSigPred__]   <- res_pop__[idxSigPred__]/sigma_value_pop__[idxSigPred__]
      data__[idxCONDID_d__, "pred_indiv"] <- value_indiv__
      data__[idxCONDID_d__, "pred_pop"]     <- value_pop__
      data__[idxCONDID_d__, "res_indiv"]  <- res_indiv__
      data__[idxCONDID_d__, "res_pop"]      <- res_pop__
      data__[idxCONDID_d__, "wres_indiv"] <- wres_indiv__
      data__[idxCONDID_d__, "wres_pop"]     <- wres_pop__
    }
    data__[["CONDID"]] <- NULL
    return(data__)
  }))
  mutate_IQRsysModel(res, IQRpredtable = list(predframe__))
}
metrics_IQRsysModel <- function(res) {
  parframes__ <- res$parframes[[1]]
  if (is.null(parframes__) || nrow(parframes__) == 0)
    return(mutate_IQRsysModel(res, metrics = list(NULL)))
  partable__ <- res$IQRpartable[[1]]
  ndata__ <- nrow(as.data.frame(res$data[[1]]))
  nsubjects__ <- length(res$data[[1]])
  neta__ <-  sum(res$IQRpartable[[1]]$parametertask == "ETA")
  npars__ <- length(res$bestfit[[1]])
  parsWithIIV__ <- unique(partable__$belongs_to[partable__$parametertask == "ETA"])
  parsF__ <- partable__$parametername[partable__$parametertask %in% c("pop", "error")]
  parsR__ <- partable__$parametername[partable__$parametertask %in% c("omega")]
  parsR__ <- c(parsR__, partable__$parametername[partable__$parametertask %in% c("beta") & partable__$belongs_to %in% partable__$belongs_to[partable__$parametertask == "ETA"]])
  parsF__ <- c(parsF__, partable__$parametername[partable__$parametertask %in% c("beta") & !partable__$belongs_to %in% partable__$belongs_to[partable__$parametertask == "ETA"]])
  dim_parsR__ <- length(which(partable__$parametername %in% parsR__ & partable__$estimated == 1))
  dim_parsF__ <- length(which(partable__$parametername %in% parsF__ & partable__$estimated == 1))
  obj__ <- data.frame(
    metric = c("OBJ", "AIC", "BIC", "BICc"),
    value = c(
      parframes__[1, "value", drop = TRUE],
      parframes__[1, "value", drop = TRUE] + 2*(dim_parsR__ + dim_parsF__),
      parframes__[1, "value", drop = TRUE] + (dim_parsR__ + dim_parsF__)*log(ndata__),
      parframes__[1, "value", drop = TRUE] + dim_parsR__*log(nsubjects__) + dim_parsF__*log(ndata__)
    )
  )
  mutate_IQRsysModel(res, metrics = list(obj__))
}
mutate_IQRsysModel <- function(sysModel, ...) {
  args0__ <- attr(sysModel, "args0")
  sysModelEst__ <- attr(sysModel, "sysModelEst")
  out__ <- dplyr::mutate(sysModel, ...)
  attr(out__, "args0") <- args0__
  attr(out__, "sysModelEst") <- sysModelEst__
  class(out__) <- class(sysModel)
  out__
}
grids_IQRsysModel <- function(sysModel) {
  IQRpartable__ <- sysModel$IQRpartable[[1]]
  data__ <- sysModel$data[[1]]
  name_loc__ <- unique(IQRpartable__$base_par[!is.na(IQRpartable__$CONDITION)])
  name_eta__ <- unique(IQRpartable__$belongs_to[IQRpartable__$parametertask == "ETA"])
  name_glob__ <- setdiff(
    unique(IQRpartable__$parametername)[is.na(IQRpartable__$CONDITION) & IQRpartable__$parametertask != "ETA"],
    c(name_loc__)
  )
  condition.grid__ <- attr(data__, "condition.grid")
  est__ <- attr(sysModel, "sysModelEst")$est
  for(mycatname__ in est__$data$catNames) {
    condition.grid__ <- dMod_expand_cat(condition.grid__,
                                        mycatname__,
                                        est__$data$catValues[[mycatname__]],
                                        est__$data$covariateCATreference[[mycatname__]])
  }
  fixed.grid__ <- est.grid__ <- condition.grid__
  est.grid__ <- est.grid__[intersect(names(est.grid__), c("ID", "CONDITION"))]
  for (n__ in name_glob__) {
    info__ <- filter(IQRpartable__, parametername == n__)
    if (info__$estimated[1] == 1) {
      est.grid__[[n__]] <- n__
    } else {
      fixed.grid__[[n__]] <- info__[["parametervalue"]]
    }
  }
  for (n__ in name_eta__) {
    info__ <- filter(IQRpartable__, belongs_to == n__)
    parametername__ <- paste("ETA", n__, est.grid__[["ID"]], sep = "_")
    if (info__$estimated[1] == 1) {
      est.grid__[[n__]] <- n__
      est.grid__[[paste0("ETA_", n__)]] <- parametername__
    } else {
      fixed.grid__[[n__]] <- IQRpartable__[["parametervalue"]][IQRpartable__$parametername == n__]
      fixed.grid__[[paste0("ETA_", n__)]] <- info__[["parametervalue"]][match(parametername__, info__[["parametername"]])]
    }
  }
  for (n__ in name_loc__) {
    fixed.grid__[[n__]] <- est.grid__[[n__]] <- NA
    info__ <- filter(IQRpartable__, parametername == n__)
    if (nrow(info__) > 0) {
      if (info__$estimated == 1) {
        est.grid__[[n__]] <- n__
      } else {
        fixed.grid__[[n__]] <- info__[["parametervalue"]]
      }
    }
    info__ <- filter(IQRpartable__, base_par == n__)
    estimated__ <- info__$estimated == 1
    base_estimated__ <- rep(FALSE, nrow(est.grid__))
    if ("BASE" %in% info__$CONDITION[estimated__]) {
      base_estimated__ <- !est.grid__$CONDITION %in% info__$CONDITION[!estimated__]
    }
    condition_estimated__ <- est.grid__$CONDITION %in% info__$CONDITION[estimated__]
    est.grid__[[n__]][condition_estimated__] <- info__$parametername[match(est.grid__$CONDITION[condition_estimated__], info__$CONDITION)]
    est.grid__[[n__]][!condition_estimated__ & !base_estimated__] <- NA
    fixed.grid__[[n__]][!condition_estimated__ & !base_estimated__] <-
      info__$parametervalue[match(est.grid__$CONDITION[!condition_estimated__ & !base_estimated__], info__$CONDITION)]
  }
  pars__ <- structure(IQRpartable__$parametervalue[IQRpartable__$estimated == 1], names = IQRpartable__$parametername[IQRpartable__$estimated == 1])
  mutate_IQRsysModel(sysModel,
                     fixed.grid = list(fixed.grid__),
                     est.grid = list(est.grid__),
                     pars = list(pars__))
}
spec_IQRsysModel <- function(sysModel) {
  spec__ <- sysModel[["parameters"]][[1]]
  IQRpartable__ <- sysModel[["IQRpartable"]][[1]]
  for (n__ in names(spec__$POPvalue0)) {
    row__ <- match(n__, IQRpartable__$parametername)
    if (length(row__) > 0) {
      spec__$POPvalue0[n__] <- IQRpartable__[row__[1], "parametervalue_linear"]
      spec__$POPestimate[n__] <- IQRpartable__[row__[1], "estimated"]
      spec__$IIVdistribution[n__] <- IQRpartable__[row__[1], "trafo"]
    }
    row__ <- match(paste0("omega_", n__), IQRpartable__$parametername)
    if (length(row__) > 0) {
      spec__$IIVvalue0[n__] <- IQRpartable__[row__[1], "parametervalue_linear"]
      spec__$IIVestimate[n__] <- IQRpartable__[row__[1], "estimated"]
      if (spec__$IIVvalue0[n__] != 0 & spec__$IIVestimate[n__] == 0) spec__$IIVestimate[n__] <- 2
    }
  }
  for (n__ in names(spec__$LOCmodel)) {
    loc_pars__ <- paste(n__, spec__$LOCmodel[[n__]], sep = "_")
    row__ <- match(loc_pars__, IQRpartable__$parametername)
    if (length(row__) > 0) {
      spec__$LOCvalues0[[n__]] <- IQRpartable__[row__, "parametervalue_linear"]
      spec__$LOCestimate[[n__]] <- IQRpartable__[row__, "estimated"]
      names(spec__$LOCvalues0[[n__]]) <- names(spec__$LOCestimate[[n__]]) <- IQRpartable__[row__, "CONDITION"]
    }
  }
  sysModel <- mutate_IQRsysModel(sysModel, parameters = list(spec__))
}
dMod_generate_dosing_data <- function(dosingTables,
                                      all_conditions,
                                      IDCOND.grid) {
  if (is_IQRdosing(dosingTables))
    dosingTables <- lapply(stats::setNames(nm = all_conditions), function(.x) dosingTables)
  if (length(setdiff(all_conditions, names(dosingTables)) > 0)) {
    dosingTables0__ <- lapply(stats::setNames(nm = setdiff(all_conditions, names(dosingTables))), function(i)
      IQRdosing(TIME = dosingTables[[1]][["TIME"]][[1]],1,0))
    dosingTables <- c(dosingTables, dosingTables0__)
  }
  data_dosing__ <- mapply(cbind, dosingTables, CONDITION = names(dosingTables),
                          MoreArgs = list(stringsAsFactors = FALSE), SIMPLIFY = FALSE)
  data_dosing__ <- mapply(function(.x,.y) {
    dT__ <- data_dosing__[[.x]]
    cbind(dT__, ID = .y, stringsAsFactors = FALSE)
  }, .x = IDCOND.grid$CONDITION, .y = IDCOND.grid$ID, SIMPLIFY = FALSE)
  data_dosing__ <- do.call(rbind, data_dosing__)
  data_dosing__ <- data.frame(data_dosing__, YTYPE = 0, NAME = paste0("DOSE_INPUT", data_dosing__[["ADM"]]), DV = 0, stringsAsFactors = FALSE)
  return(data_dosing__)
}
#'@export
run_IQRsysProjectMulti <- function (projectPath,
                                    ncores=1,
                                    Nparallel=1,
                                    FLAGgof=TRUE,
                                    FLAGgofStratify=FALSE,
                                    FLAGrequireConverged=FALSE) {
  projectPath <- lapply(projectPath, function (x) {
    if (!is_IQRsysProject(x)) return(NULL)
    x
  })
  if (Nparallel==1) {
    cat(sprintf("run_IQRsysProjectMulti: Running %d models\n",length(projectPath)))
  } else {
    cat(sprintf("Running %d models, %d in parallel at the same time (output suppressed)\n",length(projectPath),Nparallel))
  }
  cat("(Projects are only run if not run yet)\n")
  PASS_COMPLIANCEINFO_LOCAL_ENVIR__ <- globalenv()$COMPLIANCE_MODE_SCRIPT_NAME
  if (Nparallel > 1) {
    cluster__ <- parallel::makeCluster(Nparallel)
    doParallel::registerDoParallel(cluster__)
    parallel::clusterCall(cl=cluster__,function (x__) .libPaths(x__), .libPaths())
    parallel::clusterExport(cluster__,envir=environment(),varlist=ls())
    "%dopar%" <- foreach::"%dopar%"
  } else {
    DLLloaded__ <- FALSE
    "%dopar%" <- foreach::"%do%"
  }
  .packages = c("IQRtools")
  foreach::foreach (k__=1:length(projectPath), .packages=.packages, .inorder=TRUE) %dopar% {
    e__ <- globalenv()
    e__$COMPLIANCE_MODE_SCRIPT_NAME <- PASS_COMPLIANCEINFO_LOCAL_ENVIR__
    out <- try(run_IQRsysProject(projectPath=projectPath[[k__]],
                                 ncores=ncores,
                                 FLAGgof=FLAGgof,
                                 FLAGgofStratify=FLAGgofStratify,
                                 FLAGrequireConverged = FLAGrequireConverged), silent = FALSE)
    if (inherits(out, "try-error")) {
      message("An error was encountered in run_IQRsysProject(). Continuing anyway.")
      return()
    }
    out
  }
  if (Nparallel > 1) {
    parallel::stopCluster(cluster__)
    doParallel::stopImplicitCluster()
  }
  return(as_IQRsysProjectMulti(projectPath))
}
#'@export
as_IQRsysProjectMulti <- function (input, FLAGrecursive=FALSE) {
  if (is_IQRsysProjectMulti(input)) {
    return(input)
  }
  output__ <- list()
  if ("character" %in% class(input)) {
    for (k__ in seq_along(input)) {
      if (!dir.exists(input[k__]))
        stopIQR("provided path does not exist.")
      if (is_IQRsysProject(input[k__])) {
        output__[[length(output__)+1]] <- as_IQRsysProject(input[k__])
      } else {
        dirs__ <- list.dirs(input[k__],recursive=FLAGrecursive)
        dirs__ <- dirs__[!grepl(".Internals",dirs__)]
        lapply(dirs__,function(x) {if (is_IQRsysProject(x)) {
          output__[[length(output__)+1]] <<- as_IQRsysProject(x) }
        })
      }
    }
  } else {
    tst <- sapply(input, function (x) is_IQRsysProject(x))
    if (all(tst)) {
      output__ <- input
    } else {
      stopIQR("argument 'input' of incorrect type")
    }
  }
  names(output__) <- unclass(output__)
  attr(output__,"class") <- "IQRsysProjectMulti"
  return(output__)
}
#'@export
is_IQRsysProjectMulti <- function (input) {
  methods::is(input,"IQsysProjectMulti")
}
#'@export
as_IQRsysProject <- function (projectPath) {
  if (!is_IQRsysProject(projectPath))
    stopIQR("Provided path does not point to an IQRsysProject")
  output__ <- as_IQRnlmeProject(projectPath)
  attr(output__,"class") <- "IQRsysProject"
  return(output__)
}
#'@export
is_IQRsysProject <- function (projectPath) {
  testfile__ <- file.path(projectPath, 'project_sysfit.R')
  return(file.exists(testfile__))
}
#'@export
print.IQRsysProject <- function(x, ...) {
  if (!hasrun_IQRsysProject(x)) {
    cat(paste0("IQRsysProject: ",x,"\n"))
    cat("Project has not been run yet (most likely)\n\n")
    return()
  }
  cat(paste0("\n\nIQRsysProject: ",x,"\n\n"))
}
#'@export
hasrun_IQRsysProject <- function(projectPath) {
  file.exists(file.path(projectPath,"RESULTSORIG","project_result.sysfit"))
}
#'@export
IQRsysProject <- function(est,
                          projectPath,
                          comment = "",
                          keepProjectFolder = FALSE,
                          SIMOPT.method    = "lsodes",
                          SIMOPT.atol      = 1e-6,
                          SIMOPT.rtol      = 1e-6,
                          SIMOPT.hmin      = 0,
                          SIMOPT.hmax      = NULL,
                          SIMOPT.hini      = 0,
                          SIMOPT.maxsteps  = 5000,
                          SIMOPT.nauxtimes = 0,
                          SIMOPT.cores     = 1,
                          opt.method = "trust",
                          opt.nfits = 1,
                          opt.sd = 1,
                          opt.rinit = 1,
                          opt.rmax = 10,
                          opt.iterlim = 100,
                          opt.prior_sigma = 10,
                          opt.parlower = NULL,
                          opt.parupper = NULL,
                          algOpt.SEED = 123456,
                          FLAGprofileLL = F,
                          FLAGkeepFits = F,
                          FLAGchecks = T,
                          ...
) {
  argnames__ <- names(formals())
  SIMOPTnames__ <- grep("^SIMOPT\\.", argnames__, value = TRUE)
  OPTnames__ <- grep("^opt\\.", argnames__, value = TRUE)
  SIMOPT__ <- OPT__ <- list()
  for (n__ in SIMOPTnames__) SIMOPT__[[n__]] <- get(n__)
  for (n__ in OPTnames__) OPT__[[n__]] <- get(n__)
  if (FLAGprofileLL & !opt.method %in% "trust") {
    stopIQR("When derivative-free optimization method is used, profiles cannot be computed. Set FLAGprofileLL to FALSE or use trust as optim.method.")
  }
  if (!is_IQRsysEst(est))
    stopIQR("Input argument 'est' needs to be of class IQRsysEst")
  estInputOriginal__ <- est
  if (!is.character(projectPath))
    stopIQR("projectPath argument needs to be a character string with a absolute (avoid) or relative (better) path definition")
  est$projectPath <- projectPath
  oldpath__ <- getwd()
  if (!keepProjectFolder) aux_rmdir(est$projectPath)
  tryCatch({
    aux_mkdir(est$projectPath)
    setwd(est$projectPath)
  }, error = function (err) {
    aux_rmdir(est$projectPath)
    aux_mkdir(est$projectPath)
    setwd(est$projectPath)
  })
  aux_rmdir("RESULTS")
  aux_rmdir("RESULTSORIG")
  aux_mkdir("RESULTS")
  aux_mkdir("RESULTSORIG")
  setwd(oldpath__)
  oldpath__ <- getwd()
  setwd(est$projectPath)
  path__ <- getwd()
  if (nchar(path__) != nchar(aux_strrep(path__," ",""))) {
    setwd(oldpath__)
    aux_rmdir(est$projectPath)
    if (allowed_spaces_IQR()) {
      stopIQR("The absolute path to the project contains spaces. This is not allowed (thanks to NONMEM)")
    } else {
      warningIQR("The absolute path to the project contains spaces. If you plan to use NONMEM - then it will not work!")
    }
  }
  setwd(oldpath__)
  oldpath__ <- getwd()
  setwd(est$projectPath)
  absPathProject__ <- getwd()
  setwd(oldpath__)
  setwd(aux_fileparts(est$data$datafile)$pathname)
  absPathData__ <- getwd()
  setwd(oldpath__)
  fromFolder__ <- absPathProject__
  toFolder__ <- absPathData__
  relPathData__ <- aux_getRelPath(fromFolder__,toFolder__)
  est$data$relPathFromProject <- "."  
  est$data$fileName <- paste0(aux_fileparts(est$data$datafile)$filename,
                              aux_fileparts(est$data$datafile)$fileext)
  file.copy(est$data$datafile, est$projectPath)
  atrfile <- sub("\\.csv$", ".atr", est$data$datafile)
  if (file.exists(atrfile)) {
    file.copy(atrfile, est$projectPath)
  }
  est$data$datafile <- file.path(est$data$relPathFromProject, est$data$fileName)
  dMod_gen_IQRsysProject(est,
                         SIMOPT__,
                         OPT__,
                         algOpt.SEED,
                         FLAGprofileLL,
                         FLAGkeepFits,
                         FLAGchecks,
                         comment = comment)
  text__ <- export_IQRmodel(attr(est$model, "original"))
  aux_filewrite(text__,file.path(est$projectPath,"model.txt"))
  saveRDS(est, file = file.path(est$projectPath,"project.est"))
  output__ <- as_IQRsysProject(est$projectPath)
  return(output__)
}
#'@export
run_IQRsysProject <- function(projectPath,
                              ncores = 1,
                              FLAGrequireConverged = TRUE,
                              FLAGgof = TRUE,
                              FLAGgofStratify=FALSE,
                              FLAGclean=TRUE) {
  if (!is_IQRsysProject(projectPath))
    tryCatch(projectPath <- as_IQRsysProject(projectPath), error = function(x) x)
  oldPath__ <- getwd() 
  setwd(projectPath)   
  absProjectPath__ <- getwd() 
  absProjectPath__ <- as_IQRsysProject(absProjectPath__)
  if (hasrun_IQRsysProject(projectPath)) {
    message("IQRsysProject was already run - not rerun")
    return(invisible(NULL))
  }
  setwd(absProjectPath__)
  terminationMessage <- ""
  if (!hasrun_IQRsysProject(file.path(oldPath__,projectPath)))
    tryCatch({
      source("project_sysfit.R", local = TRUE)
    }, error = function (err__) {
      terminationMessage <<- paste0("dMod stopped with the following error message:\n",err__$message)
    })
  setwd(absProjectPath__)
  if (terminationMessage != "") {
    setwd(oldPath__)
    suppressWarnings(try(detach("package:dMod", unload=TRUE, character.only = TRUE), silent = TRUE))
    suppressWarnings(try(detach("package:cOde", unload=TRUE, character.only = TRUE), silent = TRUE))
    suppressWarnings(try(detach("package:deSolve", unload=TRUE, character.only = TRUE), silent = TRUE))
    stopIQR(terminationMessage)
  }
  mymodel__ <- readRDS("RESULTSORIG/project_result.sysfit")
  digits__ <- 3
  table__ <- tablePars_IQRsysModel(mymodel__, digits = digits__)
  filenames__ <- c("project_parameters_table.txt", "RESULTS/project_results.txt")
  for (f__ in filenames__) {
    IQRoutputTable(as.data.frame(table__),
                   xfooter = paste(aux_getRelPath(oldPath__, absProjectPath__),
                                   "RSE (relative standard error)",
                                   "CI (confidence interval)",
                                   paste("Significant digits:", digits__),
                                   "omega values reported in standard deviations.",
                                   sep = ", "),
                   xtitle = "Sysfit parameter estimates",
                   filename = f__)
  }
  standardOutput_IQRsysModel(mymodel__, projectPath = projectPath)
  setwd(oldPath__)
  PROJECTINFO     <- parseNLMEprojectHeader(projectPath)
  outputNumberALL <- 1:length(PROJECTINFO$OUTPUTS)
  outputNamesALL  <- PROJECTINFO$OUTPUTS
  setwd(absProjectPath__)
  if (FLAGgof) {
    cat("Producing SYSFIT GOF plots ------------------\n")
    tryCatch({
      aux_mkdir("RESULTS/GOF_GENERAL")
      setwd(file.path(absProjectPath__, "RESULTS", "GOF_GENERAL"))
      if (!is.null(mymodel__[["parframes"]][[1]])) {
        plots__ <- plotWaterfall_IQRsysModel(mymodel__)
        IQRoutputFigure(plots__, filename = "11_Sys_Objvalues_Multistart.pdf",scale=1.5)
      }
      if (!is.null(mymodel__[["parframes"]][[1]])) {
        plots__ <- plotPars_IQRsysModel(mymodel__)
        IQRoutputFigure(plots__,filename = "12_Sys_Parameters_Multistart.pdf",scale=1.5)
      }
      if (!is.null(mymodel__[["profiles"]][[1]])) {
        plots__ <- plotProfile_IQRsysModel(mymodel__)
        IQRoutputFigure(plots__,filename = "13_Sys_Profile_Likelihood.pdf",scale=2)
      }
      setwd(absProjectPath__)
      outputs__ <- paste0("OUTPUT", outputNumberALL)
      for (i__ in seq_along(outputs__)) {
        myfolder__ <- paste0("RESULTS/GOF_OUTPUT_", outputNumberALL[i__], "_", outputNamesALL[i__])
        aux_mkdir(myfolder__)
        setwd(myfolder__)
        setwd(absProjectPath__)
      }
    }, error = function(e__) {
      warningIQR("Problem with generation of SYSFIT diagnostic plots. Please check if model run had issues.")
    })
    cat("---------------------------------------------\n")
  }
  setwd(oldPath__)
  if (FLAGgof) {
    tryCatch({
      suppressWarnings(plot.IQRnlmeProject(projectPath,
                                           FLAGgofStratify=FLAGgofStratify,
                                           CORR_THRESHOLD=0.3,
                                           pathname=file.path(projectPath, "RESULTS")))
    }, error = function(e__) {
      warningIQR("Problem with generation of NLME-type of diagnostic plots. Please check if model run had issues.")
    })
  }
  setwd(oldPath__)
  relProjectPath__ <- aux_getRelPath(oldPath__, absProjectPath__)
  relDataPath__ <- file.path(relProjectPath__, basename(attr(mymodel__, "sysModelEst")[["data"]][["datafile"]]))
  attr(mymodel__, "sysModelEst")[["data"]][["datafile"]] <- relDataPath__
  class(mymodel__) <- union("optIQRsysModel", class(mymodel__))
  if (FLAGclean) {
    unlink(paste0(absProjectPath__,"/RESULTSORIG/project_result.sysfit"),force = TRUE)
    unlink(paste0(absProjectPath__,"/project_model.sysfit"),force = TRUE)
  }
  return(mymodel__)
}
#'@export
load_IQRsysProject <- function(projectPath, FLAGresultsOnly = FALSE) {
  stopifnot(is_IQRsysProject(projectPath))
  mywd__ <- getwd()
  setwd(projectPath)
  mymodel__ <- NULL
  if (file.exists("RESULTSORIG/project_result.sysfit"))
    mymodel__ <- readRDS("RESULTSORIG/project_result.sysfit")
  else
    warningIQR("Project result not available. Might lead to problems in recompilation")
  if (FLAGresultsOnly) {
    setwd(mywd__)
    return(mymodel__)
  }
  cat("Rebuilding IQRsysModel ...\n\n")
  est__ <- readRDS("project.est")
  options__ <- mymodel__[["options"]][[1]]
  newmodel__ <- do.call("dMod_nlmeEst2dModFrame", c(list(est = est__), options__))
  newmodel__[["dllfolder"]][[1]] <- tempdirIQR()
  cat("\n")
  cat("... successful.\n")
  if (!is.null(mymodel__)) {
    cat("Transferring results of previously executed project ... ")
    keep__ <- c("g", "x", "p", "data", "e", "p0", "model", "prd", "obj_data", "obj", "constr_fit", "dllpaths", "dllfolder")
    for (n__ in setdiff(names(mymodel__), keep__)) newmodel__[[n__]] <- mymodel__[[n__]]
    sysModelEst__ <- attr(newmodel__, "sysModelEst")
    if (!is.null(sysModelEst__[["data"]][["datafile"]]))
      sysModelEst__[["data"]][["datafile"]] <- file.path(projectPath, sysModelEst__[["data"]][["datafile"]])
    if (!is.null(newmodel__[["obj"]]))
      modelname(newmodel__[["obj"]][[1]]) <- file.path(tempdirIQR(), modelname(newmodel__[["obj"]][[1]]))
    attr(newmodel__, "sysModelEst") <- sysModelEst__
    attr(newmodel__, "args0") <- attr(mymodel__, "args0")
    class(newmodel__) <- union("optIQRsysModel", class(newmodel__))
    cat("done.\n")
  }
  setwd(mywd__)
  return(newmodel__)
}
#'@export
duplicate_IQRsysProject <- function(projectSource, projectDestination) {
  projectSource <- as_IQRsysProject(projectSource)
  aux_rmdir(projectDestination)
  aux_mkdir(projectDestination)
  oldpath__ <- getwd()
  setwd(projectDestination)
  absPathDestination__ <- getwd()
  setwd(oldpath__)
  oldpath__ <- getwd()
  setwd(projectSource)
  file.copy(from=".", to=absPathDestination__,recursive=TRUE)
  setwd(oldpath__)
  relPathSourceDataInfo__ <- aux_fileparts(parseNLMEprojectHeader(projectSource)$DATA)
  datafile__ <- paste0(relPathSourceDataInfo__$filename,relPathSourceDataInfo__$fileext)
  dataRelPathSource__ <- relPathSourceDataInfo__$pathname
  oldpath__ <- getwd()
  setwd(projectSource)
  setwd(dataRelPathSource__)
  absDataPath__ <- getwd()
  setwd(oldpath__)
  oldpath__ <- getwd()
  setwd(projectDestination)
  absDestinationPath__ <- getwd()
  setwd(oldpath__)
  FLAGcopyData <- FALSE
  if (FLAGcopyData | dataRelPathSource__==".") {
    dataRelPathDestination__ <- "."
  } else {
    dataRelPathDestination__ <- aux_getRelPath(absDestinationPath__,absDataPath__)
  }
  content__ <- aux_fileread(paste0(projectDestination,"/project_sysfit.R"))
  content__ <- aux_strrep(content__,paste0(dataRelPathSource__,"/"),paste0(dataRelPathDestination__,"/"))
  aux_filewrite(content__,paste0(projectDestination,"/project_sysfit.R"))
  return(as_IQRsysProject(projectDestination))
}
#'@export
getOptTrace_IQRsysProject <- function(projectPath, index = NULL) {
  if (!is_IQRsysProject(projectPath))
    stopIQR("Project path does not point to an IQRsysProject.")
  if (!file.exists(file.path(projectPath, "RESULTSORIG")))
    stopIQR("Project has not yet run.")
  trialfolder__ <- tail(list.files(file.path(projectPath, "RESULTSORIG"), pattern = "^trial-"), 1)
  tracefiles__ <- list.files(file.path(projectPath, "RESULTSORIG", trialfolder__), pattern = "_optTrace\\.csv")
  fitindex__ <- sapply(tracefiles__, function(t__) strsplit(t__, "_", fixed = TRUE)[[1]][1])
  if (!is.null(index)) fitindex__ <- fitindex__[as.numeric(fitindex__) %in% index]
  traces__ <- do.call(rbind, lapply(fitindex__, function(f__) {
    data__ <- IQRloadCSVdata(paste0(file.path(projectPath, "RESULTSORIG", trialfolder__, f__), "_optTrace.csv"))
    data.frame(Index = as.numeric(f__), data__)
  }))
  rownames(traces__) <- NULL
  results__ <- readRDS(file.path(projectPath, "project_model.sysfit"))
  partable__ <- results__[["IQRpartable"]][[1]]
  partable__ <- partable__[partable__[["estimated"]] == 1, c("parametername", "trafo", "parametertask", "belongs_to")]
  parameters__ <- partable__[["parametername"]]
  for (i__ in seq_along(parameters__)) {
    parname__ <- partable__[["parametername"]][i__]
    trafo__ <- partable__[["trafo"]][i__]
    traces__[[parname__]] <- dplyr::case_when(trafo__ == "L" ~ exp(traces__[[parname__]]),
                                              trafo__ == "G" ~ inv_logit(traces__[[parname__]]),
                                              trafo__ == "N" ~ traces__[[parname__]])
  }
  traces__ <- data.frame(traces__[ , setdiff(names(traces__), parameters__)],
                         Parameter = rep(parameters__, each = nrow(traces__)),
                         Value = unlist(traces__[, parameters__]),
                         stringsAsFactors = FALSE)
  names(partable__) <- c("Parameter", "Trafo", "Task", "Parent")
  dplyr::left_join(traces__, partable__, by = "Parameter")
}
#'@export
plot_IQRoptTrace <- function(trace, cutObj = 20) {
  obj <- trace[!duplicated(paste(trace[["Index"]], trace[["Iteration"]])),]
  obj[["Parent"]] <- "obj"
  obj[["Task"]] <- "obj"
  obj[["Value"]] <- obj[["Obj"]]
  obj[["Trafo"]] <- "N"
  obj[["Parameter"]] <- "Objective Value"
  obj[["Value"]][obj[["Value"]] > min(obj[["Value"]] + cutObj, na.rm = TRUE)] <- Inf
  trace <- rbind(trace, obj)
  trace[["Parameter"]] <- factor(
    trace[["Parameter"]],
    levels = c("Objective Value", sort(setdiff(trace[["Parameter"]], "Objective Value")))
  )
  IQRggplot(trace, aes(x = Iteration, y = Value, group = Index, color = Task)) +
    facet_wrap(~Parameter, scales = "free_y") +
    geom_line() +
    scale_color_IQRtools()
}
#'@export
getPars_IQRoptTrace <- function(trace,
                                index = trace$Index[1],
                                iteration = max(trace$Iteration[trace$Index == index])) {
  trace__ <- trace[trace$Index == index[1] & trace$Iteration == iteration[1],]
  if (nrow(trace__) == 0) stopIQR("The requested index or iteration is not contained in trace data.")
  structure(as.numeric(trace__$Value), names = as.character(trace__$Parameter))
}
#'@export
sample_IQRsysModel <- function(sysmodel, covariates = NULL, parameters = NULL, returnCovariates = FALSE) {
  sysobj__ <- sysmodel
  sysModelEst__ <- attr(sysobj__, "sysModelEst")
  model0__ <- attr(sysModelEst__[["model"]], "original")
  partable__ <- sysobj__$IQRpartable[[1]]
  covtable__ <- unique(partable__[partable__$parametertask == "beta", c("covname", "covref")])
  parsOfInterest__ <- unlist(sysModelEst__[["parsOfInterest"]])
  if (!is.null(parameters)) parsOfInterest__ <- parameters
  var.equations__ <- sapply(model0__[["variables"]], function(x__) x__[["formula"]])
  var.equations__ <- var.equations__[intersect(names(var.equations__), parsOfInterest__)]
  par.equations__ <- getEquations(sysobj__[["p0"]][[1]])[[1]]
  par.equations__ <- par.equations__[intersect(names(par.equations__), union(parsOfInterest__,
                                                                             getSymbols(var.equations__)))]
  varpar.equations__ <- resolveRecurrence(c(par.equations__, var.equations__))[parsOfInterest__]
  symbols__ <- getSymbols(varpar.equations__)
  etas__ <- symbols__[grepl("^ETA_", symbols__)]
  etas.values__ <- structure(rep(0, length(etas__)), names = etas__)
  bestfit.values__ <- sysobj__[["pars"]][[1]]
  fixedgrid__ <- sysobj__[["fixed.grid"]][[1]]
  states__ <- names(model0__[["states"]])
  parameters__ <- names(model0__[["parameters"]])
  parameters.values__ <- sapply(parameters__, function(x__) model0__[["parameters"]][[x__]][["value"]])
  states.values__ <- sapply(states__, function(x__) as.numeric(model0__[["states"]][[x__]][["IC"]]))
  catNames__ <- sysModelEst__[["data"]][["catNames"]]
  covNames__ <- sysModelEst__[["data"]][["covNames"]]
  covNames__ <- covNames__[covNames__ %in% covtable__[["covname"]]]
  catNames__ <- catNames__[sapply(catNames__, function(.) any(grepl(paste0("^", ., "_"), covtable__[["covname"]])))]
  cov.expanded__ <- as.list(do.call(cbind, lapply(covNames__, function(myname__) {
    out__ <- rep(covtable__[["covref"]][covtable__[["covname"]] == myname__], nrow(covariates))
    if (myname__ %in% names(covariates))
      out__[!is.na(covariates[[myname__]])] <- covariates[[myname__]][!is.na(covariates[[myname__]])]
    return(out__)
  })))
  cat.expanded__ <- as.list(do.call(cbind, lapply(catNames__, function(myname__) {
    catlevels__ <- unique(covtable__[["covname"]][grepl(paste0("^", myname__, "_[1-9]"), covtable__[["covname"]])])
    out__ <- as.data.frame(matrix(0, nrow = nrow(covariates), ncol = length(catlevels__), dimnames = list(NULL, catlevels__)))
    if (myname__ %in% names(covariates)) {
      catvalues__ <- tail(strsplit(catlevels__, "_")[[1]], 1)
      for (i__ in 1:length(catlevels__)) {
        out__[ , i__] <- as.numeric(covariates[[myname__]] == catvalues__[i__])
      }
    }
    return(out__)
  })))
  catcov.expanded__ <- as.data.frame(c(cov.expanded__, cat.expanded__))
  par2out <- function(estimated_pars) {
    output__ <- do.call(rbind, lapply(1:nrow(covariates), function(i__) {
      mycatcov.expanded__ <- catcov.expanded__[i__, , drop = FALSE]
      cols__ <- names(mycatcov.expanded__)[sapply(strsplit(names(mycatcov.expanded__), "_"), function(x__) paste(x__[-length(x__)], collapse = "_")) %in% names(covariates)]
      fixedgrid.values__ <- fixedgrid__[, intersect(names(fixedgrid__), c(cols__, symbols__)), drop = FALSE]
      indx__ <- try({
        which(Reduce("&", lapply(cols__, function(n__) {
          as.numeric(fixedgrid.values__[[n__]]) == as.numeric(mycatcov.expanded__[[n__]])
        })))
      }, silent = TRUE)
      if (inherits(indx__, "try-error") | length(indx__) == 0) {
        warningIQR("Requested covariate combination was not used in model. Use output parameter values with caution.")
        indx__ <- 1
      }
      parlist__ <- c(as.list(mycatcov.expanded__),
                     as.list(etas.values__), as.list(estimated_pars),
                     as.list(fixedgrid.values__[indx__[1], intersect(names(fixedgrid.values__), symbols__), drop = FALSE]),
                     as.list(parameters.values__),
                     as.list(states.values__))
      myexpr__ <- parse(text = paste0("c(", paste(varpar.equations__, collapse = ", "), ")"))
      with(parlist__, eval(myexpr__))
    }))
    colnames(output__) <- names(varpar.equations__)
    output__ <- as.data.frame(output__)
    return(output__)
  }
  output__ <- par2out(bestfit.values__)
  vcov__ <- sysobj__[["vcov"]][[1]]
  sd__ <- NULL
  rse__ <- NULL
  if (length(vcov__) > 0) {
    N__ <- 1000
    set.seed(0)
    sample__ <- MASS::mvrnorm(n = N__, mu = bestfit.values__, Sigma = vcov__)
    parvalues__ <- lapply(1:N__, function(i__) par2out(sample__[i__,]))
    ncol__ <- ncol(output__)
    sd__ <- lapply(seq_len(ncol__), function(i__) {
      values__ <- sapply(parvalues__, function(d__) d__[[i__]])
      values__ <- apply(values__, 1, function(x) sd(x, na.rm = TRUE))
      values__
    })
    names(sd__) <- names(output__)
    sd__ <- as.data.frame(sd__)
    rse__ <- sd__/output__ 
  }
  if (returnCovariates) {
    output__ <- cbind(covariates, output__)
    if (!is.null(sd__)) sd__ <- cbind(covariates, sd__)
    if (!is.null(rse__)) rse__ <- cbind(covariates, rse__)
  }
  attr(output__, "se") <- sd__
  attr(output__, "rse") <- rse__
  return(output__)
}
#'@export
plot.IQRsysProject <- function(x, ..., FLAGgofStratify=FALSE, CORR_THRESHOLD=0.3,nindiv=16) {
  NULL
}
parseSYSprojectHeader <- function(projectPath) {
  if (!is_IQRsysProject(projectPath))
    stopIQR("Provided projectPath does not point to an IQRsysProject")
  project__ <- readLines(file.path(projectPath, "project_sysfit.R"))
  project__ <- paste0(project__, collapse = "\n")
  ixstart__ <- aux_strFindAll(project__,'# ==PROJECT HEADER START===================================================')$end
  ixend__   <- aux_strFindAll(project__,  '# ==PROJECT HEADER END=====================================================')$start
  if (is.null(ixstart__) | is.null(ixend__)) stopIQR('Project header could not be found in project_NLMIXR.R file.')
  headertext__ <- aux_strtrim(substr(project__,ixstart__+1,ixend__-1))
  headertext__  <- aux_strrep(headertext__,"# ","")
  headertext__  <- aux_strrep(headertext__,"\n",",\n")
  projectinfo__ <- eval(parse(text=paste0("list(",headertext__,")")))
  projectinfo__ <- lapply(projectinfo__,function(x) aux_explodePC(x))
  return(projectinfo__)
}
sampleSYSFITpopulationParameters <- function(input, FLAG_SAMPLE=FALSE, verbose=TRUE) {
  if (is.na(input$objectivefunction$OBJ)) return(NULL)
  output__                                      <- list()
  output__$type                                 <- 'SYSFIT'
  output__$path                                 <- input$path
  paramNames                                  <- input$parameters$names
  values                                      <- input$parameters$values
  covariance                                  <- input$parameters$covariancematrix
  covariance[abs(covariance)<100*.Machine$double.eps] <- 0
  covariance <- matrix(covariance,ncol=ncol(input$parameters$covariancematrix))
  rownames(covariance) <- rownames(input$parameters$covariancematrix)
  colnames(covariance) <- colnames(input$parameters$covariancematrix)
  RUN__ <- TRUE
  count__ <- 1
  while (RUN__) {
    if (FLAG_SAMPLE) {
      if (!is.null(covariance)) {
        samples__ <- mvrnorm(n=1,mu=values,Sigma=covariance)
      } else {
        if (verbose) {
          warningIQR('The covariance matrix was not determined => No sampling of population parameters from uncertainty distributions.')
        }
        samples__ <- values
      }
    } else {
      samples__ <- values
      if (verbose) {
        message('No sampling of population parameters from uncertainty distributions.')
      }
    }
    ixo__     <- which(grepl('omega\\(',paramNames))
    n       <- paramNames[ixo__]
    ixno__    <- which(aux_strFindAll(n,',')$start < 0)
    no      <- n[ixno__]
    ixouseo__ <- sapply(no, function(x) which(x==paramNames))
    no      <- gsub(x=no,pattern='omega\\(',replacement='')
    no      <- gsub(x=no,pattern='\\)',replacement='')
    output__$randomEffects$names  <- no
    output__$randomEffects$values <- abs(samples__[ixouseo__])
    covariancematrix <- diag(samples__[ixouseo__])^2
    ixo__              <- which(grepl('omega\\(',paramNames))
    n                <- paramNames[ixo__]
    ixousec__          <- which(aux_strFindAll(n,',')$start > 0)
    if (length(ixousec__) > 0) {
      warningIQR(paste(
        "Found off-diagonal random effects.",
        "This is currently not supported by SYSFIT.",
        "Offdiagonal elements were ignored."))
    }
    suppressWarnings(output__$randomEffects$covariancematrix <- nearPD(covariancematrix))
    if (min(eigen(covariancematrix)$values) >= 0) RUN__ <- FALSE
    count__ <- count__ + 1
    if (count__ > 100) {
      warningIQR("Difficulty to get a possemidef covariance matrix.")
      break()
    }
  }
  corrmatrix <- matrix(NaN, ncol=ncol(covariancematrix),nrow=ncol(covariancematrix))
  for (krow in 1:nrow(corrmatrix)) {
    for (kcol in 1:ncol(corrmatrix)) {
      corrmatrix[krow,kcol] <- covariancematrix[krow,kcol]/sqrt(abs(covariancematrix[krow,krow]*covariancematrix[kcol,kcol]))
    }
  }
  output__$randomEffects$correlationmatrix <- corrmatrix
  ix__ <- c(ixouseo__, ixousec__)
  ix_keep <- setdiff(seq_along(samples__),ix__)
  samples__ <- samples__[ix_keep]
  paramNames <- paramNames[ix_keep]
  output__$fixedEffects$names <- output__$randomEffects$names
  ixfe <- c()
  for (k__ in seq_along(output__$fixedEffects$names)) {
    ix__ <- which(output__$fixedEffects$names[k__]==paramNames)
    output__$fixedEffects$values[k__] <- samples__[ix__]
    ixfe <- c(ixfe, ix__)
  }
  ix_keep <- setdiff(seq_along(samples__),ixfe)
  samples__ <- samples__[ix_keep]
  paramNames <- paramNames[ix_keep]
  output__$randomEffects$transformation     <- input$trans_randeffects
  output__$randomEffects$inv_transformation <- input$inv_trans_randeffects
  output__$fixedEffects$transformation      <- input$trans_randeffects
  Noutput <- length(input$residualerrormodels)
  removeIX <- c()
  for (k__ in 1:Noutput) {
    outputInfo   <- list()
    outputInfo$alias <- input$residualerrormodels[k__]
    outputInfo$abcr <- c(NA,NA)
    ix__ <- which(paste0('error_ADD', k__)==paramNames)
    if (length(ix__)>0) {
      outputInfo$abcr[1] <- samples__[ix__]
      removeIX <- c(removeIX, ix__)
    }
    ix__ <- which(paste0('error_PROP', k__)==paramNames)
    if (length(ix__)>0) {
      outputInfo$abcr[2] <- samples__[ix__]
      removeIX <- c(removeIX, ix__)
    }
    output__$residualErrorModel[[k__]] <- outputInfo
  }
  ix_keep <- setdiff(seq_along(samples__),removeIX)
  samples__ <- samples__[ix_keep]
  paramNames <- paramNames[ix_keep]
  for (k__ in 1:Noutput) {
    ix__ <- which(output__$residualErrorModel[[k__]]$alias=='abs')
    if (length(ix__)>0) {
      output__$residualErrorModel[[k__]]$formula <- 'abcr(1).*ones(size(f))'
      output__$residualErrorModel[[k__]]$FlagTransf <- 0
    }
    ix__ <- which(output__$residualErrorModel[[k__]]$alias=='rel')
    if (length(ix__)>0) {
      output__$residualErrorModel[[k__]]$formula <- 'abcr(2).*f'
      output__$residualErrorModel[[k__]]$FlagTransf <- 0
    }
    ix__ <- which(output__$residualErrorModel[[k__]]$alias=='absrel')
    if (length(ix__)>0) {
      output__$residualErrorModel[[k__]]$formula <- 'abcr(1) + abcr(2).*f'
      output__$residualErrorModel[[k__]]$FlagTransf <- 0
    }
  }
  ix__            <- which(grepl('beta_',paramNames))
  covariates      <- paramNames[ix__]
  covariatevalues <- samples__[ix__]
  ix_keep    <- setdiff(seq_along(samples__),ix__)
  samples__    <- samples__[ix_keep]
  paramNames   <- paramNames[ix_keep]
  categorical_covariates              <- list()
  categorical_covariates$parameter    <- c()
  categorical_covariates$covariate    <- c()
  for (k__ in seq_along(input$PROJECTINFO$BETACATNAMES)) {
    if (nchar(input$PROJECTINFO$BETACATNAMES[k__]) > 0) {
      bcnk <- input$PROJECTINFO$BETACATNAMES[k__]
      bcnk <- aux_strrep(bcnk,'beta_','')
      bcnk <- aux_strrep(bcnk,'(',',')
      bcnk <- aux_strrep(bcnk,')','')
      terms <- aux_explodePC(bcnk)
      xx <- aux_strrep(input$PROJECTINFO$BETACATCATEGORIES[k__],"[","c(")
      xx <- aux_strrep(xx,"]",")")
      xx <- aux_strrep(xx," ",",")
      categories <- eval(parse(text=xx))
      reference  <- as.double(input$PROJECTINFO$BETACATREFERENCE[k__])
      categorical_covariates$parameter <- c(categorical_covariates$parameter, terms[1])
      categorical_covariates$covariate <- c(categorical_covariates$covariate, terms[2])
      categorical_covariates$categories[[k__]] <- categories
      categorical_covariates$reference <- c(categorical_covariates$reference, reference)
      values_cat <- c()
      for (k2__ in seq_along(categories)) {
        n <- aux_strrep(input$PROJECTINFO$BETACATNAMES[k__],')',sprintf('==%d)',categories[k2__]))
        if (categories[k2__] == reference) {
          value <- 0
        } else {
          value <- covariatevalues[which(n==covariates)]
        }
        values_cat <- c(values_cat,value)
      }
      categorical_covariates$value[[k__]] <- values_cat
    }
  }
  continuous_covariates <- list()
  continuous_covariates$parameter <- c()
  continuous_covariates$covariate <- c()
  continuous_covariates$formula   <- c()
  continuous_covariates$value     <- c()
  for (k__ in seq_along(input$PROJECTINFO$BETACOVNAMES)) {
    if (nchar(input$PROJECTINFO$BETACOVNAMES[k__]) > 0) {
      bcnk <- input$PROJECTINFO$BETACOVNAMES[k__]
      bcnk <- aux_strrep(bcnk,'beta_','')
      bcnk <- aux_strrep(bcnk,'(',',')
      bcnk <- aux_strrep(bcnk,')','')
      terms <- aux_explodePC(bcnk)
      continuous_covariates$parameter  <- c(continuous_covariates$parameter, terms[1])
      continuous_covariates$covariate  <- c(continuous_covariates$covariate, terms[2])
      continuous_covariates$formula    <- c(continuous_covariates$formula, input$PROJECTINFO$BETACOVTRANS[k__])
      continuous_covariates$value      <- c(continuous_covariates$value, covariatevalues[which(input$PROJECTINFO$BETACOVNAMES[k__]==covariates)])
    }
  }
  continuous <- list()
  for (k__ in seq_along(continuous_covariates$parameter)) {
    if (k__==1) {
      continuous[[k__]] <- list(
        parameter  = continuous_covariates$parameter[k__],
        covariates = continuous_covariates$covariate[k__],
        values     = continuous_covariates$value[k__],
        formula    = continuous_covariates$formula[k__])
    } else {
      ix__ <- which(continuous_covariates$parameter[k__]==sapply(continuous,function(x) x$parameter))
      if (length(ix__) == 0) {
        continuous[[length(continuous)+1]] <- list(
          parameter  = continuous_covariates$parameter[k__],
          covariates = continuous_covariates$covariate[k__],
          values     = continuous_covariates$value[k__],
          formula    = continuous_covariates$formula[k__])
      } else {
        continuous[[ix__]]$covariates <- c(continuous[[ix__]]$covariates, continuous_covariates$covariate[k__])
        continuous[[ix__]]$values     <- c(continuous[[ix__]]$values, continuous_covariates$value[k__])
        continuous[[ix__]]$formula    <- c(continuous[[ix__]]$formula, continuous_covariates$formula[k__])
      }
    }
  }
  output__$covariates$continuous <- continuous
  categorical <- list()
  for (k__ in seq_along(categorical_covariates$parameter)) {
    if (k__==1) {
      information <- list()
      information[[1]] <- list(
        categories = categorical_covariates$categories[k__],
        values = categorical_covariates$value[k__]
      )
      categorical[[k__]] <- list(
        parameter  = categorical_covariates$parameter[k__],
        covariates = categorical_covariates$covariate[k__],
        information = information)
    } else {
      ix__ <- which(categorical_covariates$parameter[k__]==sapply(categorical,function(x) x$parameter))
      if (length(ix__) == 0) {
        information <- list()
        information[[1]] <- list(categories = categorical_covariates$categories[k__],
                                 values = categorical_covariates$value[k__]
        )
        categorical[[length(categorical)+1]] <- list(
          parameter  = categorical_covariates$parameter[k__],
          covariates = categorical_covariates$covariate[k__],
          information = information)
      } else {
        information <- list()
        information[[1]] <- list(
          categories = categorical_covariates$categories[k__],
          values = categorical_covariates$value[k__]
        )
        categorical[[ix__]]$covariates <- c(categorical[[ix__]]$covariates, categorical_covariates$covariate[k__])
        categorical[[ix__]]$information[[length(categorical[[ix__]]$information)+1]] <- list(
          categories = categorical_covariates$categories[k__],
          values = categorical_covariates$value[k__]
        )
      }
    }
  }
  output__$covariates$categorical <- categorical
  if (length(paramNames)>0) {
    if (!is.na(paramNames[1])) {
      warningIQR('The NONMEM output contained information that are currently not handled.')
    }
  }
  values  <- output__$fixedEffects$values
  trans   <- output__$fixedEffects$transformation
  tvalues <- c()
  for (k__ in seq_along(values)) {
    phi <- values[k__]
    tvalues[k__] <- eval(parse(text=trans[k__]))
  }
  output__$fixedEffects$values <- tvalues
  output__$fixedEffects$transformation <- NULL
  return(output__)
}
dMod_fill_sysEst_parameters <- function(POPvalues0, POPestimate = NULL, IIVdistribution = NULL, IIVvalues0 = NULL, IIVestimate = NULL) {
  if (!is.numeric(c(POPvalues0, IIVvalues0)))
    stopIQR("Initial guesses are not numeric.")
  if (any(!(c(IIVestimate, 1) %in% 0:2)))
    stopIQR("IIVestimate parameters not 0, 1 or 2")
  if (any(!(c(POPestimate, 1) %in% 0:1)))
    stopIQR("IIVestimate parameters not 0, 1 or logical")
  if (any(!(c(IIVdistribution, "L") %in% c("N", "G", "L"))))
    stopIQR("Not all IIVdistribution parameters are N, G or L")
  if (any(!(c(names(POPestimate),
              names(IIVdistribution),
              names(IIVvalues0),
              names(IIVestimate),
              names(POPvalues0)) %in% names(POPvalues0))))
    stopIQR("Parameters which are not defined in POPvalues0 have been passed into another argument.")
  n__ <- names(POPvalues0)
  POPestimate__ <- stats::setNames(rep(1, length(n__)), n__)
  IIVdistribution__ <- stats::setNames(rep("L", length(n__)), n__)
  IIVvalues0__ <- stats::setNames(rep(1, length(n__)), n__)
  IIVestimate__ <- stats::setNames(rep(0, length(n__)), n__)
  if (!is.null(POPestimate))
    POPestimate__[intersect(names(POPestimate), n__)] <- POPestimate[intersect(names(POPestimate), n__)]
  if (!is.null(IIVdistribution))
    IIVdistribution__[intersect(names(IIVdistribution), n__)] <- IIVdistribution[intersect(names(IIVdistribution), n__)]
  if (!is.null(IIVvalues0))
    IIVvalues0__[intersect(names(IIVvalues0), n__)] <- IIVvalues0[intersect(names(IIVvalues0), n__)]
  if (!is.null(IIVestimate))
    IIVestimate__[intersect(names(IIVestimate), n__)] <- IIVestimate[intersect(names(IIVestimate), n__)]
  out__ <- list(POPvalues0 = POPvalues0, POPestimate = POPestimate__,
                IIVdistribution = IIVdistribution__,
                IIVvalues0 = IIVvalues0__, IIVestimate = IIVestimate__)
  out__ <- lapply(out__, function(i) {
    i[order(names(i))]
  })
  return(out__)
}
dMod_fill_LOC_fields <- function(myNlmeEst__) {
  all_conditions <- unique(c(
    unlist(myNlmeEst__$modelSpec$LOCmodel),
    unlist(lapply(myNlmeEst__$modelSpec$LOCvalues0, names)),
    myNlmeEst__$modelSpec$CONDITIONS,
    myNlmeEst__$data$dataModeling$CONDITION)
  )
  LOCmodel__   <- myNlmeEst__$modelSpec$LOCmodel
  LOCvalues0__ <- myNlmeEst__$modelSpec$LOCvalues0[names(LOCmodel__)]
  if (is.null(LOCvalues0__))
    LOCvalues0__ <- structure(vector("list", length = length(LOCmodel__)), names = names(LOCmodel__))
  LOCestimate__ <- myNlmeEst__$modelSpec$LOCestimate[names(LOCmodel__)]
  if (is.null(LOCestimate__))
    LOCestimate__ <- structure(vector("list", length = length(LOCmodel__)), names = names(LOCmodel__))
  LPOPvalues0__  <-  myNlmeEst__$modelSpec$POPvalues0[names(LOCmodel__)]
  LPOPestimate__ <-  myNlmeEst__$modelSpec$POPestimate[names(LOCmodel__)]
  LOCvalues0__ <- mapply(function(L, P, all_conditions) {
    nm <- all_conditions[!all_conditions %in% names(L)]
    defaults__ <- stats::setNames(rep(P, length(nm)), nm)
    return(c(L, defaults__)[all_conditions])
  }, L = LOCvalues0__, P = LPOPvalues0__, MoreArgs = list(all_conditions = all_conditions), SIMPLIFY = FALSE)
  LOCestimate__ <- mapply(function(L, P, all_conditions) {
    nm <- all_conditions[!all_conditions %in% names(L)]
    defaults__ <- stats::setNames(rep(P, length(nm)), nm)
    return(c(L, defaults__)[all_conditions])
  }, L = LOCestimate__, P = LPOPestimate__, MoreArgs = list(all_conditions = all_conditions), SIMPLIFY = FALSE)
  LOCparameters__ <- mapply(function(LM, all_conditions, parameter) {
    nm <- setdiff(all_conditions, LM)
    defaults__ <- stats::setNames(rep(parameter, length(nm)), nm = nm)
    uniques__ <- stats::setNames(paste0(parameter, "_", LM), nm = LM)
    return(c(uniques__, defaults__)[all_conditions])
  }, parameter = names(LOCmodel__), LM = LOCmodel__, MoreArgs = list(all_conditions = all_conditions), SIMPLIFY = FALSE)
  out__ <- list(CONDITIONS    = all_conditions,
                LOCmodel      = LOCmodel__,
                LOCvalues0    = LOCvalues0__,
                LOCestimate   = LOCestimate__,
                LOCparameters = LOCparameters__)
  return(out__)
}
dMod_convert2dMod_errorModel__ <- function(errorModel) {
  log_safe <- function(x__) ifelse(x__ <= 0, -750, log(x__))
  error_eqns__ <- lapply(seq_along(errorModel), function(i) {
    myname__ <- names(errorModel)[i]
    output_index__ <- gsub("OUTPUT", "", names(errorModel)[i])
    type__ <- errorModel[[i]][[1]]
    if (type__ == "abs"){
      out_eqn__ <- structure(paste0("error_ADD", output_index__), names = myname__)
      attr(out_eqn__, "values0") <- structure(log_safe(as.numeric(errorModel[[i]][[2]])),
                                              names = paste0("error_ADD", output_index__))
      return(out_eqn__)}
    if (type__ == "rel"){
      out_eqn__ <- structure(paste0("error_PROP", output_index__, " * ", myname__), names = myname__)
      attr(out_eqn__, "values0") <- structure(log_safe(as.numeric(errorModel[[i]][[2]])),
                                              names = paste0("error_PROP", output_index__))
      return(out_eqn__)}
    if (type__ == "absrel")    {
      out_eqn__ <- structure(paste0("sqrt(error_ADD", output_index__, "^2 + ", "error_PROP", output_index__, "^2 * ", myname__, "^2)"), names = myname__)
      attr(out_eqn__, "values0") <- structure(log_safe(as.numeric(unlist(errorModel[[i]][c(2,3)]))),
                                              names = paste0(c("error_ADD", "error_PROP"), output_index__))
      return(out_eqn__)}
    if (!(type__ %in% c("abs", "rel", "absrel")))
      stopIQR("errorModel type not defined")
  })
  names(error_eqns__) <- names(errorModel)
  out__ <- do.call(c, unname(error_eqns__))
  attr(out__, "values0") <- do.call(c, lapply(unname(error_eqns__), function(error_eqn) attr(error_eqn, "values0")))
  return(out__)
}
dMod_build_dosing_events <- function(inputs__, ndoses) {
  N__ <- length(inputs__)
  events__ <- do.call(rbind, lapply(1:N__, function(i__) {
    input__ <- inputs__[i__]
    ndose__ <- ndoses[i__]
    var__    <- rep(input__, 2*ndose__) 
    time__   <- c(paste("ton" , input__, seq_len(ndose__), sep = "_"),
                  paste("toff", input__, seq_len(ndose__), sep = "_"))
    value__  <- c(paste("xon" , input__, seq_len(ndose__), sep = "_"),
                  rep("0", ndose__))
    method__ <- rep("replace", length(var__))
    data.frame(var = var__, time = time__, value = value__, method = method__, stringsAsFactors = FALSE)
  }))
  as.eventlist(events__)
}
dMod_build_dosing_trafo <- function(max_scheme) {
  as.eqnvec(do.call(c,lapply(1:length(max_scheme), function(ADM__) {
    nDOSE__ <- max_scheme[ADM__]
    TIME <- paste0("TIME", ADM__, "_", 1:nDOSE__)
    Tlag <- paste0("Tlag", ADM__)
    TINF <- paste0("TINF", ADM__, "_", 1:nDOSE__)
    AMT <-  paste0("AMT", ADM__, "_", 1:nDOSE__)
    names.ton__ <- paste0("ton_INPUT", ADM__, "_", 1:nDOSE__)
    ton__ <- paste0("(", TIME, " + ", Tlag, ")")
    names.toff__ <- paste0("toff_INPUT", ADM__, "_", 1:nDOSE__)
    toff__ <- paste0("(", TIME, " + ", TINF, " + ",Tlag, ")")
    names.xon__ <- paste0("xon_INPUT", ADM__, "_", 1:nDOSE__)
    xon__ <- paste0("(", AMT, "/", TINF, ")")
    stats::setNames(c(ton__, toff__, xon__), c(names.ton__, names.toff__, names.xon__))
  })))
}
dMod_build_dosing_parameters <- function(dataNLME, inputs = NULL) {
  if ("EVID" %in% names(dataNLME)) {
    data_dosing__ <- dataNLME[dataNLME[["EVID"]] == 1,]
  } else {
    data_dosing__ <- dataNLME[!is.na(dataNLME[["ADM"]]) && dataNLME[["ADM"]] > 0,]
  }
  merge_by_var1__<- function(x, y) merge(x, y, by = "Var1", all = TRUE)
  nDoses_per_ADM__ <- Reduce("merge_by_var1__", lapply(split(data_dosing__, data_dosing__[["ADM"]]), function(ADM__) {
    as.data.frame(table(ADM__[, "ID"]))
  }))
  max_nDoses_per_ADM__ <- data.frame(
    ADM = 1:(ncol(nDoses_per_ADM__) - 1),
    N  = apply(
      as.matrix(nDoses_per_ADM__[, -1, drop = FALSE]),
      2, 
      function(x) {
        x[is.na(x)] <- 0
        max(x)
      })
  )
  if (!is.null(inputs)) {
    missingADM__ <- setdiff(seq_len(inputs), max_nDoses_per_ADM__[["ADM"]])
    if (length(missingADM__) > 0) {
      addDoses__ <- data.frame(ADM = missingADM__, N = 1)
      max_nDoses_per_ADM__ <- rbind(max_nDoses_per_ADM__, addDoses__)
    }
  }
  IDs__ <- unique(dataNLME[["ID"]])
  dosing_pars__ <- lapply(IDs__, function(myID__) {
    d__ <- data_dosing__[data_dosing__[["ID"]] %in% myID__, ]
    d__ <- d__[order(d__[["TIME"]], d__[["ADM"]]),]
    unlist(lapply(seq_along(max_nDoses_per_ADM__[["ADM"]]), function(i__) {
      ADM__ <- max_nDoses_per_ADM__[["ADM"]][i__]
      dminor__ <- d__[d__[["ADM"]] == ADM__,]
      seqN__ <- seq_len(max_nDoses_per_ADM__[i__, "N"])
      TIME__ <- structure(dminor__[["TIME"]][seqN__], names = paste0("TIME", ADM__, "_", seqN__))
      AMT__  <- structure(dminor__[["AMT" ]][seqN__], names = paste0("AMT" , ADM__, "_", seqN__))
      TINF__ <- structure(dminor__[["TINF"]][seqN__], names = paste0("TINF", ADM__, "_", seqN__))
      Tlag__ <- structure(0, names = paste0("Tlag", ADM__))
      TINF__[TINF__ == 0] <- 1e-4
      AMT__[is.na(AMT__)] <- 0
      TINF__[is.na(TINF__)] <- 1
      if (all(is.na(TIME__))) TIME__[1] <- 0
      if (any(is.na(TIME__))) {
        for (i__ in which(is.na(TIME__))) {
          last__ <- max(1, tail(which(!is.na(TIME__[1:i__])), 1))
          TIME__[i__] <- TIME__[last__] + TINF__[last__]
        }
      }
      as.data.frame(as.list(c(TIME__, AMT__, TINF__, Tlag__)))
    }))
  })
  dosing_pars__ <- as.data.frame(do.call("rbind", dosing_pars__))
  dosing_pars__ <- cbind(data.frame(ID = IDs__), dosing_pars__)
  if ("CONDITION" %in% names(data_dosing__))
    dosing_pars__[["CONDITION"]] <- dataNLME[["CONDITION"]][match(dosing_pars__[["ID"]], dataNLME[["ID"]])]
  attr(dosing_pars__, "max_nDoses_per_ADM") <- max_nDoses_per_ADM__
  return(dosing_pars__)
}
dMod_build_IIVpars <- function(parameters, conditions) {
  ETApars.est__ <- lapply(names(parameters[["IIVvalues0"]])[parameters[["IIVestimate"]] == 1],
                          function(myiiv) {paste("ETA", myiiv, conditions, sep = "_")})
  names(ETApars.est__) <- names(parameters[["IIVvalues0"]])[parameters[["IIVestimate"]] == 1]
  ETApars.fix__ <- lapply(names(parameters[["IIVvalues0"]])[parameters[["IIVestimate"]] == 2],
                          function(myiiv) {paste("ETA", myiiv, conditions, sep = "_")})
  names(ETApars.fix__) <- names(parameters[["IIVvalues0"]])[parameters[["IIVestimate"]] == 2]
  omegapars.est__ <- lapply(names(parameters[["IIVvalues0"]])[parameters[["IIVestimate"]] == 1], function(myiiv) {
    stats::setNames(rep(paste0("omega_", myiiv), length(ETApars.est__[[myiiv]])), ETApars.est__[[myiiv]]) })
  omegapars.fix__ <- lapply(names(parameters[["IIVvalues0"]])[parameters[["IIVestimate"]] == 2], function(myiiv) {
    stats::setNames(rep(paste0("omega_", myiiv), length(ETApars.fix__[[myiiv]])), ETApars.fix__[[myiiv]]) })
  ETAnames.est__ <- unlist(ETApars.est__, use.names = FALSE)
  ETAnames.fix__ <- unlist(ETApars.fix__, use.names = FALSE)
  omeganames.est__ <- unique(unlist(omegapars.est__, use.names = FALSE))
  omeganames.fix__ <- unique(unlist(omegapars.fix__, use.names = FALSE))
  ETA.est__ <- stats::setNames(rep(0, length(ETAnames.est__)), ETAnames.est__)
  ETA.fix__ <- stats::setNames(rep(0, length(ETAnames.fix__)), ETAnames.fix__)
  omega.est__ <- with(parameters, stats::setNames(log(IIVvalues0[IIVestimate == 1]), omeganames.est__))
  omega.fix__ <- with(parameters, stats::setNames(log(IIVvalues0[IIVestimate == 2]), omeganames.fix__))
  pars <- c(ETA.est__, ETA.fix__, omega.est__)
  fixed    <- c(omega.fix__)
  est.grid_IIV__ <- NULL
  if ((length(ETApars.est__) > 0) || (length(ETApars.fix__) > 0) || (length(omega.est__) > 0))
    est.grid_IIV__ <- data.frame(ID = conditions, stringsAsFactors = FALSE)
  if (length(ETApars.est__) > 0)
    est.grid_IIV__ <- cbind(est.grid_IIV__, stats::setNames(as.data.frame(ETApars.est__, stringsAsFactors = FALSE), paste0("ETA_", names(ETApars.est__))))
  if (length(ETApars.fix__) > 0)
    est.grid_IIV__ <- cbind(est.grid_IIV__, stats::setNames(as.data.frame(ETApars.fix__, stringsAsFactors = FALSE), paste0("ETA_", names(ETApars.fix__))))
  if (length(omega.est__) > 0)
    est.grid_IIV__ <- cbind(est.grid_IIV__, stats::setNames(as.data.frame(t(names(omega.est__)), stringsAsFactors = FALSE), names(omega.est__)))
  fixed.grid_IIV__ <- NULL
  if (length(omega.fix__) > 0)
    fixed.grid_IIV__ <- data.frame(ID = conditions, stringsAsFactors = FALSE)
  if (length(omega.fix__) > 0)
    fixed.grid_IIV__ <- cbind(fixed.grid_IIV__, as.data.frame(t(omega.fix__)))
  return(list(pars = pars, fixed = fixed,
              est.grid_IIV = est.grid_IIV__,
              fixed.grid_IIV = fixed.grid_IIV__,
              ETA.est = ETA.est__,
              ETA.fix = ETA.fix__,
              omega.est = omega.est__,
              omega.fix = omega.fix__,
              ETApars.est = ETApars.est__,
              ETApars.fix = ETApars.fix__,
              omegapars.est = omegapars.est__,
              omegapars.fix = omegapars.fix__))
}
dMod_make_pars <- function(est.vec, condition, est.grid, p) {
  est_lookup__ <- unlist(est.grid[est.grid$ID == condition, !(names(est.grid) %in% c("ID", "CONDITION"))])
  est_pars__ <- stats::setNames(est.vec[est_lookup__], nm = names(est_lookup__))
  est_pars__ <- est_pars__[!is.na(est_pars__)]
  est_pars__ <- est_pars__[names(est_pars__) %in% getParameters(p)]
  return(est_pars__)
}
dMod_make_fixed <- function(condition, fixed.grid, p) {
  fixed_pars__ <- unlist(fixed.grid[fixed.grid$ID == condition, !(names(fixed.grid) %in% c("ID", "CONDITION"))])
  fixed_pars__ <- fixed_pars__[!is.na(fixed_pars__)]
  fixed_pars__ <- fixed_pars__[names(fixed_pars__) %in% getParameters(p)]
  return(fixed_pars__)
}
dMod_init_empty_objlist <- function(pars, deriv = TRUE) {
  if (!deriv)
    return(objlist(0,NULL,NULL))
  objlist(value = 0,
          gradient = setNames(rep(0, length(pars)), names(pars)),
          hessian = matrix(0, nrow = length(pars), ncol = length(pars),
                           dimnames = list(names(pars), names(pars))))
}
dMod_rename_objlist <- function(myobjlist, condition, est.grid) {
  est_lookup__ <- unlist(est.grid[est.grid$ID == condition,])
  if (!is.null(myobjlist$gradient)){
    grad_names__ <- stats::setNames(names(myobjlist$gradient), names(myobjlist$gradient))
    est_lookup_used__ <- names(est_lookup__)[names(est_lookup__) %in% grad_names__]
    grad_names__[est_lookup_used__] <- est_lookup__[est_lookup_used__]
    names(myobjlist$gradient) <- grad_names__
  }
  if (!is.null(myobjlist$hessian))
    dimnames(myobjlist$hessian) <- list(grad_names__, grad_names__)
  return(myobjlist)
}
#'@export
#'@importFrom stats setNames
dMod_res <- function(data, out, err = NULL) {
  match.num <- function(x, y, tol = 1e-8) {
    digits <- -log10(tol)
    match(round(x, digits), round(y, digits))
  }
  data$name <- as.character(data$name)
  times <- sort(unique(data$time))
  names <- unique(data$name)
  data.time <- match.num(data$time, times)
  if (any(is.na(data.time)))
    stopIQR("Missing data time in prediction. Possible reason: premature termination of integration. Try setting different integrator tolerances.")
  data.name <- match(data$name, names)
  out.time <- match.num(times, out[,1])
  out.name <- match(names, colnames(out))
  timeIndex <- out.time[data.time]
  nameIndex <- out.name[data.name]
  prediction <- sapply(1:nrow(data), function(i) out[timeIndex[i], nameIndex[i]])
  deriv <- attr(out, "deriv")
  deriv.data <- NULL
  if (!is.null(deriv)) {
    pars <- unique(unlist(lapply(strsplit(colnames(deriv)[-1], split = ".", fixed = TRUE), function(i) i[2])))
    sensnames <- as.vector(outer(names, pars, paste, sep = "."))
    names.sensnames <- t(matrix(1:length(sensnames), nrow = length(names), ncol = length(pars)))
    sensnames.deriv <- match(sensnames, colnames(deriv))
    derivnameIndex <- matrix(sensnames.deriv[names.sensnames[, data.name]], ncol = length(data.name))
    deriv.prediction <- do.call(rbind, lapply(1:nrow(data), function(i) submatrix(deriv, timeIndex[i], derivnameIndex[, i])))
    colnames(deriv.prediction) <- pars
    deriv.data <- data.frame(time = data$time, name = data$name, deriv.prediction)
  }
  if (any(is.na(data$sigma)) & is.null(err))
    stopIQR("In data, some sigmas are NA and no errmodel exists for the respective condition. Please fix data$sigma or supply errmodel.")
  sNAIndex <- is.na(data$sigma)
  if (!any(sNAIndex))
    err <- NULL 
  if (!is.null(err)) {
    time.err <- match.num(times, err[,1])
    name.err <- match(names, colnames(err))
    timeIndex <- time.err[data.time]
    nameIndex <- name.err[data.name]
    errprediction <- sapply(1:nrow(data), function(i) err[timeIndex[i], nameIndex[i]])
    if (any(sNAIndex & is.na(errprediction)))
      stopIQR("errmodel predicts NA for some observables with is.na(data$sigma).")
    data$sigma[sNAIndex] <- errprediction[sNAIndex]
  }
  deriv.err <- attr(err, "deriv")
  deriv.err.data <- NULL
  if (!is.null(err) && !is.null(deriv.err)) {
    pars <- unique(unlist(lapply(strsplit(colnames(deriv.err)[-1], split = ".", fixed = TRUE), function(i) i[2])))
    sensnames <- as.vector(outer(names, pars, paste, sep = "."))
    names.sensnames <- t(matrix(1:length(sensnames), nrow = length(names), ncol = length(pars)))
    sensnames.deriv <- match(sensnames, colnames(deriv.err))
    derivnameIndex <- matrix(sensnames.deriv[names.sensnames[, data.name]], ncol = length(data.name))
    deriv.prediction <- do.call(rbind, lapply(1:nrow(data), function(i) submatrix(deriv.err, timeIndex[i], derivnameIndex[, i])))
    colnames(deriv.prediction) <- pars
    deriv.prediction[is.na(deriv.prediction)] <- 0
    deriv.prediction[!sNAIndex, ] <- 0 
    deriv.err.data <- data.frame(time = data$time, name = data$name, deriv.prediction)
  }
  data$value <- pmax(data$value, data$lloq)
  is.bloq <- data$value <= data$lloq
  residuals <- prediction - data$value
  weighted.residuals <- (prediction - data$value)/data$sigma
  weighted.0 <- prediction/data$sigma
  data[["prediction"]] <- prediction
  data[["residual"]] <- residuals
  data[["weighted.residual"]] <- weighted.residuals
  data[["weighted.0"]] <- weighted.0
  data[["bloq"]] <- is.bloq
  dMod_objframe(data, deriv = deriv.data, deriv.err = deriv.err.data)
}
dMod_objframe <- function(mydata, deriv = NULL, deriv.err = NULL) {
  mydata <- as.data.frame(mydata)
  correct.names <- c("time", "name", "value", "prediction",
                     "sigma", "residual", "weighted.residual", "bloq", "weighted.0") 
  ok <- all(correct.names %in% names(mydata))
  if (!ok) stopIQR("mydata does not have required names")
  out <- mydata[, correct.names]
  attr(out, "deriv") <- deriv
  attr(out, "deriv.err") <- deriv.err
  class(out) <- c("objframe", "data.frame")
  return(out)
}
#'@importFrom stats pnorm dnorm
dMod_nll_ALOQ <- function(nout,
                          derivs,
                          derivs.err,
                          opt.BLOQ = c("M3", "M4NM", "M4BEAL", "M1"),
                          opt.hessian = c(
                            ALOQ_part1 = TRUE,
                            ALOQ_part2 = TRUE,
                            ALOQ_part3 = TRUE
                          )) {
  wr <- nout$weighted.residual
  w0 <- nout$weighted.0
  s  <- nout$sigma
  obj <- sum(wr^2)
  if (!is.null(derivs.err))
    obj <- obj + sum(log(2*pi*s^2))
  if (opt.BLOQ %in% "M4BEAL")
    obj <- obj + 2 * sum(stats::pnorm(w0, log.p = TRUE))
  grad <- NULL
  hessian <- NULL
  if (!is.null(derivs) && nrow(derivs) > 0) {
    dxdp <- as.matrix(derivs[, -(1:2), drop = FALSE])
    dsdp <- 0 * dxdp
    if (!is.null(derivs.err))
      dsdp <- as.matrix(derivs.err[, -(1:2), drop = FALSE])
    dwrdp <- 1/s*dxdp - wr/s*dsdp
    dw0dp <- 1/s*dxdp - w0/s*dsdp
    dlogsdp <- (1/s)*dsdp 
    G_by_Phi <- function(w) exp(stats::dnorm(w, log = TRUE)- stats::pnorm(w, log.p = TRUE))
    grad <- as.vector(2*matrix(wr, nrow = 1) %*% dwrdp + 2*apply(dlogsdp,2, sum))
    if (opt.BLOQ %in% "M4BEAL")
      grad <- grad + as.vector((2 * matrix(G_by_Phi(w0), nrow = 1)) %*% (dw0dp))
    names(grad) <- colnames(dxdp)
    hessian <- matrix(0, nrow = ncol(dwrdp), ncol = ncol(dwrdp), dimnames = list(colnames(dwrdp), colnames(dwrdp)))
    hessian <- hessian + 2 * t(dwrdp) %*% dwrdp 
    if (opt.hessian["ALOQ_part1"])  
      hessian <- hessian + 2 * (t(-wr/s^2 * dxdp) %*% dsdp + t(-wr/s^2 * dsdp) %*% dxdp) 
    if (opt.hessian["ALOQ_part2"])  
      hessian <- hessian + 2 * t(2 * wr^2/(s^2) * dsdp)%*%dsdp 
    if (opt.hessian["ALOQ_part3"]) 
      hessian <- hessian - 2 * t(dlogsdp) %*% dlogsdp
    if (opt.BLOQ %in% "M4BEAL") {
      hessian <- hessian + 2 * t((-w0 * G_by_Phi(w0) - G_by_Phi(w0)^2) * dw0dp) %*% dw0dp 
      hessian <- hessian + 2 * t(G_by_Phi(w0) * (-1)/(s^2) * dxdp ) %*% dsdp + 2 * t(G_by_Phi(w0) * (-1)/(s^2) * dsdp ) %*% dxdp 
      if (opt.hessian["ALOQ_part1"]) 
        hessian <- hessian + 2 * t(2 * G_by_Phi(w0) * w0/(s^2) * dsdp)%*%dsdp
    }
  }
  objlist(value = obj, gradient = grad, hessian = hessian)
}
#'@importFrom stats pnorm dnorm
dMod_nll_BLOQ <- function(nout.bloq,
                          derivs.bloq,
                          derivs.err.bloq,
                          opt.BLOQ = c("M3", "M4NM", "M4BEAL", "M1"),
                          opt.hessian = c(
                            BLOQ_part1 = TRUE,
                            BLOQ_part2 = TRUE,
                            BLOQ_part3 = TRUE
                          )) {
  if (opt.BLOQ %in% c("M4NM", "M4BEAL") & any(nout.bloq$value < 0))
    stopIQR("M4-Method cannot handle LLOQ < 0. Possible solutions:
      * Use M3 which allows negative LLOQ (recommended)
      * If you are working with log-transformed DV, exponentiate DV and LLOQ\n")
  wr <- nout.bloq$weighted.residual
  w0 <- nout.bloq$weighted.0
  s  <- nout.bloq$sigma
  if (opt.BLOQ == "M3"){
    objvals.bloq <- -2*stats::pnorm(-wr, log.p = TRUE)
  }
  if (opt.BLOQ %in% c("M4NM", "M4BEAL")){
    objvals.bloq <- -2*log(1 - stats::pnorm(wr) / stats::pnorm(w0))
    intercept = ifelse(log(w0-wr) > 0, 1.8, -1.9 * log(w0-wr) +0.9)
    lin = ifelse(log(w0-wr) > 0, 0.9, 0.5 )
    objvals.bloq[!is.finite(objvals.bloq)] <-  (intercept + lin * w0 + 0.95 * w0^2)[!is.finite(objvals.bloq)]
  }
  obj.bloq <- sum(objvals.bloq)
  grad.bloq <- NULL
  hessian.bloq <- NULL
  if (!is.null(derivs.bloq) && nrow(derivs.bloq) > 0){
    dxdp <- as.matrix(derivs.bloq[, -(1:2), drop = FALSE])
    dsdp <- 0 * dxdp
    if (!is.null(derivs.err.bloq))
      dsdp <- as.matrix(derivs.err.bloq[, -(1:2), drop = FALSE])
    dwrdp <- 1/s*dxdp - wr/s*dsdp
    dw0dp <- 1/s*dxdp - w0/s*dsdp
    dlogsdp <- (1/s)*dsdp 
    G_by_Phi <- function(w1, w2 = w1) exp(stats::dnorm(w1, log = TRUE) - stats::pnorm(w2, log.p = TRUE))
    if (opt.BLOQ == "M3"){
      grad.bloq <- -2 * as.vector(matrix( G_by_Phi(-wr), nrow = 1) %*% (-dwrdp)) 
    }
    if (opt.BLOQ %in% c("M4NM", "M4BEAL")){
      grad.bloq <-             as.vector(matrix(2 / (1/G_by_Phi(wr,w0) - 1/G_by_Phi(wr,wr)), nrow = 1) %*% dwrdp)
      grad.bloq <- grad.bloq - as.vector(matrix(2 / (1/G_by_Phi(w0,w0) - 1/G_by_Phi(w0,wr)), nrow = 1) %*% dw0dp)
      grad.bloq <- grad.bloq + as.vector(matrix(2 * G_by_Phi(w0), nrow = 1) %*% dw0dp)
    }
    names(grad.bloq) <- colnames(dxdp)
    if (opt.BLOQ %in% "M3") {
      hessian.bloq <- matrix(0, nrow = ncol(dxdp), ncol = ncol(dxdp), dimnames = list(colnames(dxdp), colnames(dxdp)))
      if (opt.hessian["BLOQ_part1"])
        hessian.bloq <- hessian.bloq + 2 * t((-wr * G_by_Phi(-wr) + G_by_Phi(-wr)^2) * dwrdp) %*% dwrdp 
      if (opt.hessian["BLOQ_part2"]){
        hessian.bloq <- hessian.bloq - 2 * t(G_by_Phi(-wr) * (+1)/(s^2) * dxdp) %*% dsdp 
        hessian.bloq <- hessian.bloq - 2 * t(G_by_Phi(-wr) * (+1)/(s^2) * dsdp) %*% dxdp
      }
      if (opt.hessian["BLOQ_part3"])
        hessian.bloq <- hessian.bloq - 2 *  t(G_by_Phi(-wr) * (2 * (-wr))/(s^2) * dsdp)%*%dsdp
    }
    if (opt.BLOQ %in% c("M4NM", "M4BEAL")) {
      d_dp_sq <- function(A, w = wr, sign = 1) {
        dwdp <- 1/s*dxdp - w/s*dsdp
        out <- matrix(0, nrow = ncol(dxdp), ncol = ncol(dxdp), dimnames = list(colnames(dxdp), colnames(dxdp)))
        out <- out + t(A * dwdp) %*% dwdp
      }
      d2_dp2 <- function(A, w = wr, sign = 1) {
        out <- matrix(0, nrow = ncol(dxdp), ncol = ncol(dxdp), dimnames = list(colnames(dxdp), colnames(dxdp)))
        out <- out + t(A * (-1*sign)/(s^2) * dxdp) %*% dsdp
        out <- out + t(A * (-1*sign)/(s^2) * dsdp) %*% dxdp
        out <- out + t(A * (2 * (w*sign))/(s^2) * dsdp) %*% dsdp
      }
      stable <- function(wn, w0, wr) {
        if(!(identical(wn, w0) | identical(wn, wr)))
          stopIQR("The first argument wn needs to be identical to either the second or third")
        out <- stats::dnorm(wn)/(stats::pnorm(w0)-stats::pnorm(wr))
        if (identical(wn, w0)){
          out[is.infinite(out)] <- 0
          return(out)
        }
        if (identical(wn, wr)){
          out[is.infinite(out)] <- 1/(w0-wr) + wr 
          return(out)
        }
      }
      part1 <- d_dp_sq(-wr * stable(wr,w0,wr)) +
        d2_dp2(stable(wr,w0,wr)) -
        (d_dp_sq(-w0 * stable(w0,w0,wr), w = w0) +
           d2_dp2(stable(w0,w0,wr), w = w0))
      part1 <- 2 * part1
      part2 <- stable(wr,w0,wr) * dwrdp - stable(w0,w0,wr) * dw0dp
      part2 <- -2 * t(part2) %*% part2
      part3 <- d_dp_sq(-w0 * G_by_Phi(w0) - (G_by_Phi(w0))^2, w = w0) + d2_dp2(G_by_Phi(w0), w = w0)
      part3 <- 2 * part3
      hessian.bloq <- matrix(0, nrow = ncol(dxdp), ncol = ncol(dxdp), dimnames = list(colnames(dxdp), colnames(dxdp)))
      if (opt.hessian["BLOQ_part1"])
        hessian.bloq <- hessian.bloq + part1
      if (opt.hessian["BLOQ_part2"])
        hessian.bloq <- hessian.bloq + part2
      if (opt.hessian["BLOQ_part3"])
        hessian.bloq <- hessian.bloq + part3
    }
  }
  out <- objlist(value = obj.bloq, gradient = grad.bloq, hessian = hessian.bloq)
  return(out)
}
dMod_normL2_indiv <- function(data, prd, errmodel = NULL, fixed.grid, est.grid,
                              SIMOPT.nauxtimes = 500, SIMOPT.cores = 1, opt.method = "trust",
                              attr.name = "data") {
  x.conditions__ <- fixed.grid[["ID"]]
  data.conditions__ <- names(data)
  if (!all(data.conditions__ %in% x.conditions__))
    stopIQR("The prediction function does not provide predictions for all conditions in the data.")
  timesD <- sort(unique(c(0, do.call(c, lapply(data, function(d) d$time)))))
  timesD <- sort(union(timesD, seq(min(timesD), max(timesD), length.out = SIMOPT.nauxtimes)))
  controls <- list(timesD = timesD, attr.name = attr.name, conditions = x.conditions__)
  force(errmodel)
  e.conditions__ <- names(attr(errmodel, "mappings"))
  force(fixed.grid)
  force(est.grid)
  if (Sys.info()["sysname"] == "Windows" & SIMOPT.cores > 1) {
    warningIQR("Parallelization of conditions on Windows not yet implemented. Unsing only 1 core.")
    SIMOPT.cores <- 1
  }
  useDerivs <- ifelse(opt.method %in% "trust", TRUE, FALSE)
  opt.BLOQ = c("M3", "M4NM", "M4BEAL", "M1")
  opt.hessian = c(
    ALOQ_part1 = TRUE, ALOQ_part2 = TRUE, ALOQ_part3 = TRUE,
    BLOQ_part1 = TRUE, BLOQ_part2 = TRUE, BLOQ_part3 = TRUE,
    PD = TRUE  
  )
  controls <- c(controls, list(opt.BLOQ = opt.BLOQ, opt.hessian = opt.hessian)  )
  myfn__ <- function(..., fixed = NULL, deriv=useDerivs, conditions = controls$conditions, env = NULL,
                     opt.BLOQ    = controls$opt.BLOQ,
                     opt.hessian = controls$opt.hessian) {
    opt.hessian0 <- c(
      ALOQ_part1 = TRUE, ALOQ_part2 = TRUE, ALOQ_part3 = TRUE,
      BLOQ_part1 = TRUE, BLOQ_part2 = TRUE, BLOQ_part3 = TRUE,
      PD = FALSE 
    )
    opt.hessian <- c(opt.hessian, opt.hessian0[setdiff(names(opt.hessian0), names(opt.hessian))])
    opt.BLOQ    <- opt.BLOQ[1]
    arglist__    <- list(...)
    arglist__    <- arglist__[match.fnargs(arglist__, "pars")]
    pouter__     <- arglist__[[1]]
    fixedouter__ <- fixed
    if (!is.null(attr(pouter__, "deriv"))){
      pouter__ <- unclass(pouter__)
      attr(pouter__, "deriv") <- NULL
    }
    outlist__ <- parallel::mclapply(conditions, function(cn__) {
      fixed <- fixedouter__
      pars_from_grid__ <- dMod_make_pars(c(pouter__, fixed), cn__, est.grid, prd)
      fixed_from_grid__ <- dMod_make_fixed(cn__, fixed.grid, prd)
      pars <- pars_from_grid__[!(names(pars_from_grid__) %in% names(fixed))]
      fixed <- c(fixed_from_grid__, fixed)
      cn_prd__ <- intersect(cn__, getConditions(prd))
      if (length(cn_prd__) == 0 & !is.null(cn_prd__)) {
        cn_prd__ <- getConditions(prd)[1]
        warningIQR("Condition ", cn__, " not provided with prd. Used first condition of prd instead.")
      }
      prediction <- prd(times = controls$timesD, pars = pars, fixed = fixed, deriv = deriv, conditions = cn_prd__)
      if (nrow(prediction[[1]]) < length(timesD))
        warningIQR("Integrator has problems reaching tmax. Try increasing SIMOPT.nauxtimes")
      if (length(intersect(data[[cn__]][["name"]], colnames(prediction[[1]]))) == 0){
        mywrss__ <- dMod_init_empty_objlist(pars, deriv = deriv)
        mywrss__ <- dMod_rename_objlist(mywrss__, cn__, est.grid)
        return(mywrss__)
      }
      err <- NULL
      if ((!is.null(errmodel) & is.null(e.conditions__)) | (!is.null(e.conditions__) && (cn__ %in% e.conditions__)))
        err <- errmodel(out = prediction[[1]], pars = getParameters(prediction[[1]]), conditions = cn__)
      nout <- dMod_res(data[[cn__]], prediction[[1]], err[[cn__]])
      is.bloq <- nout$bloq
      nout.bloq <- nout[is.bloq, , drop = FALSE]
      nout <- nout[!is.bloq, , drop = FALSE]
      derivs <- derivs.err <- derivs.bloq <- derivs.err.bloq <- NULL
      derivs <- attr(nout, "deriv")
      derivs.err <- attr(nout, "deriv.err")
      if (!is.null(derivs) ) {
        derivs.bloq <- derivs[is.bloq, , drop = FALSE]
        derivs <- derivs[!is.bloq, , drop = FALSE]
      }
      if (!is.null(derivs.err)) {
        derivs.err.bloq <- derivs.err[is.bloq, , drop = FALSE]
        derivs.err <- derivs.err[!is.bloq, , drop = FALSE]
      }
      mywrss__ <- dMod_init_empty_objlist(pars, deriv = deriv)
      if (!all(is.bloq))
        mywrss__ <- mywrss__ + dMod_nll_ALOQ(nout, derivs, derivs.err, opt.BLOQ = opt.BLOQ, opt.hessian = opt.hessian)
      if (any(is.bloq) && (!opt.BLOQ == "M1")){
        mydMod_wrss_BLOQ <- dMod_nll_BLOQ(nout.bloq, derivs.bloq, derivs.err.bloq, opt.BLOQ = opt.BLOQ, opt.hessian = opt.hessian)
        mywrss__ <- mywrss__ + mydMod_wrss_BLOQ
      }
      mywrss__ <- dMod_rename_objlist(mywrss__, cn__, est.grid)
      return(mywrss__)
    }, mc.cores = SIMOPT.cores)
    failed__ <- sapply(outlist__, is.character)
    if (all(!failed__)) {
      msg__ <- "OK: Evaluation of objective function successful for all IDs\n"
    } else if (all(failed__)) {
      msg__ <- "FAILED: Evaluation of objective function failed for all IDs\n"
    } else {
      msg__ <- paste0("FAILED: Objective function could not be evaluated for the following IDs: ", paste(conditions[failed__], collapse = ", "), "\n")
    }
    cat(msg__, file = "project_obj.log", append = TRUE)
    outlist__ <- outlist__[!failed__]
    if (length(outlist__) == 0)
      stopIQR("Objective function could not be evaluated for any of the conditions.")
    values__    <- lapply(outlist__, function(x__) x__[["value"]]+1e6*sum(failed__))
    gradients__ <- lapply(outlist__, function(x__) x__[["gradient"]])
    hessians__  <- lapply(outlist__, function(x__) x__[["hessian"]])
    out__ <- dMod_init_empty_objlist(pouter__, deriv = deriv)
    out__[["value"]] <- Reduce("+", values__)
    if (deriv) {
      for (grad in gradients__) out__$gradient[names(grad)] <- out__$gradient[names(grad)] + grad
      for (hes in hessians__)   out__$hessian[rownames(hes), colnames(hes)] <- out__$hessian[rownames(hes), colnames(hes)] + hes
    }
    nearPD2__ <- function (x, corr = FALSE){
      X__ <- 0.5*(x+t(x))
      e__ <- eigen(X__)
      eV__ <- e__$values
      if (min(eV__) > 0) return(X__)
      eV__[eV__<0] <- 0
      const__ <- length(eV__)
      Xout__ <- e__$vectors %*% diag(eV__) %*% (ginv(e__$vectors*const__)*const__)
      if (corr) diag(Xout__) <- 1
      return(Xout__)
    }
    if (opt.hessian["PD"] & deriv) {
      poshessian__ <- try(nearPD2__(out__$hessian), silent = TRUE)
      if (!inherits(poshessian__, "try-error")) {
        out__$hessian <- structure(nearPD2__(out__$hessian), dimnames = dimnames(out__$hessian))
      }
    }
    attr(out__, controls$attr.name) <- out__$value
    return(out__)
  }
  class(myfn__) <- c("objfn", "fn")
  attr(myfn__, "conditions") <- data.conditions__
  attr(myfn__, "parameters") <- attr(prd, "parameters")
  attr(myfn__, "modelname") <- modelname(prd, errmodel)
  return(myfn__)
}
dMod_PRD_indiv <- function(prd, fixed.grid, est.grid) {
  prd_conditions <- as.character(fixed.grid$ID)
  if (length(prd_conditions) == 0)
    stopIQR("fixed.grid must contain ID column for conditions")
  P2X_indiv__ <- function(..., fixed = NULL, deriv = FALSE, conditions = prd_conditions) {
    arglist__ <- list(...)
    arglist__ <- arglist__[match.fnargs(arglist__, c("times", "pars"))]
    times <- arglist__[[1]]
    pouter__ <- arglist__[[2]]
    fixedouter__ <- fixed
    if (!is.null(attr(pouter__, "deriv"))){
      pouter__ <- unclass(pouter__)
      attr(pouter__, "deriv") <- NULL
    }
    if (is.null(conditions))
      conditions <- prd_conditions
    if (!is.null(conditions) & !is.null(prd_conditions))
      conditions <- intersect(conditions, prd_conditions)
    prediction <- lapply(conditions, function(cn__) {
      fixed <- fixedouter__
      pars_from_grid__ <- dMod_make_pars(c(pouter__, fixed), cn__,est.grid, prd)
      fixed_from_grid__ <- dMod_make_fixed(cn__, fixed.grid, prd)
      pars <- pars_from_grid__[!(names(pars_from_grid__) %in% names(fixed))]
      fixed <- c(fixed_from_grid__, fixed)
      cn_prd__ <- intersect(cn__, getConditions(prd))
      if (length(cn_prd__) == 0 & !is.null(cn_prd__)) {
        cn_prd__ <- getConditions(prd)[1]
        warningIQR("Condition ", cn__, " not provided with prd. Used first condition of prd instead.")
      }
      pred_cn <- try(prd(times = times, pars = unclass(pars), fixed = fixed, deriv = deriv, conditions = cn_prd__)[[1]])
      if (inherits(pred_cn, "try-error")) warningIQR("\n\n********Problem encountered while simulating ID ", cn__ ,".********\n\n\n")
      pred_cn
    })
    prediction <- as.prdlist(prediction, names = conditions)
    return(prediction)
  }
  attr(P2X_indiv__, "modelname") <- modelname(prd)
  attr(P2X_indiv__, "parameters") <- getSymbols(unlist(est.grid[!names(est.grid)%in%c("ID", "CONDITION")]))
  attr(P2X_indiv__, "conditions") <- prd_conditions
  class(P2X_indiv__) <- c("prdfn", "fn")
  return(P2X_indiv__)
}
dMod_P_indiv <- function(p, fixed.grid, est.grid) {
  p_conditions__ <- as.character(fixed.grid$ID)
  if (length(p_conditions__) == 0)
    stopIQR("fixed.grid must contain ID column for conditions")
  P2P_indiv__ <- function(..., fixed = NULL, deriv = FALSE, conditions = p_conditions__) {
    arglist__ <- list(...)
    arglist__ <- arglist__[match.fnargs(arglist__, c("pars"))]
    pouter__ <- arglist__[[1]]
    fixedouter__ <- fixed
    if (!is.null(attr(pouter__, "deriv"))){
      pouter__ <- unclass(pouter__)
      attr(pouter__, "deriv") <- NULL
    }
    pinner__ <- NULL
    for (cn__ in conditions) {
      fixed <- fixedouter__
      pars_from_grid__ <- dMod_make_pars(c(pouter__, fixed), cn__, est.grid, p)
      fixed_from_grid__ <- dMod_make_fixed(cn__, fixed.grid, p)
      pars <- pars_from_grid__[!(names(pars_from_grid__) %in% names(fixed))]
      fixed <- c(fixed_from_grid__, fixed)
      cn_p__ <- NULL
      if (length(getConditions(p)) > 1) {
        warningIQR("p contains more than one condition, only the first is used")
        cn_p__ <- getConditions(p)[1]
      }
      pinner__ <- c(pinner__, try(stats::setNames(p(pars = pars, fixed = fixed, deriv = deriv, conditions = cn_p__),
                                                  nm = cn__)))
    }
    return(pinner__)
  }
  attr(P2P_indiv__, "parameters") <- getSymbols(unlist(est.grid[!names(est.grid)%in%c("ID", "CONDITION")]))
  attr(P2P_indiv__, "conditions") <- p_conditions__
  class(P2P_indiv__) <- c("parfn", "fn")
  return(P2P_indiv__)
}
dMod_build_LOC.grids <- function(myNlmeEst__, IDCOND.grid) {
  all_conditions <- unique(c(myNlmeEst__$modelSpec$CONDITIONS, IDCOND.grid$CONDITION))
  filled_LOC_fields__ <- dMod_fill_LOC_fields(myNlmeEst__)
  myNlmeEst__$modelSpec[names(filled_LOC_fields__)] <- filled_LOC_fields__
  LOCmodel__ <- myNlmeEst__$modelSpec$LOCmodel
  LOCvalues0 <- myNlmeEst__$modelSpec$LOCvalues0[names(LOCmodel__)]
  LOCestimate__ <- myNlmeEst__$modelSpec$LOCestimate[names(LOCmodel__)]
  LOCparameters__ <- myNlmeEst__$modelSpec$LOCparameters[names(LOCmodel__)]
  fixed.grid_LOC__ <- Reduce(merge, mapply(function(parameter, LV, LE, IDCOND.grid) {
    IDCOND.grid <- cbind(IDCOND.grid, LV[IDCOND.grid$CONDITION], stringsAsFactors = FALSE)
    names(IDCOND.grid)[3] <- parameter
    IDCOND.grid[[3]][LE[IDCOND.grid$CONDITION] == 1] <- NA
    return(IDCOND.grid)
  }, parameter = names(LOCmodel__), LV = LOCvalues0, LOCestimate__, MoreArgs = list(IDCOND.grid =IDCOND.grid), SIMPLIFY = FALSE))
  est.grid_LOC__ <- Reduce(merge, mapply(function(parameter, LP, LE, IDCOND.grid) {
    IDCOND.grid <- cbind(IDCOND.grid, LP[IDCOND.grid$CONDITION], stringsAsFactors = FALSE)
    names(IDCOND.grid)[3] <- parameter
    IDCOND.grid[[3]][LE[IDCOND.grid$CONDITION] == 0] <- NA
    return(IDCOND.grid)
  }, parameter = names(LOCmodel__), LP = LOCparameters__, LOCestimate__, MoreArgs = list(IDCOND.grid =IDCOND.grid), SIMPLIFY = FALSE))
  est.vec_LOC__ <- Reduce(c, mapply(function(LV, LP, LE) {
    stats::setNames(LV, LP)[LE == 1]
  }, LV = LOCvalues0, LP = LOCparameters__, LE = LOCestimate__, SIMPLIFY = FALSE))
  est.vec_LOC__ <- est.vec_LOC__[!duplicated(names(est.vec_LOC__))]
  out__ <- list(fixed.grid_LOC = fixed.grid_LOC__,
                est.grid_LOC   = est.grid_LOC__,
                est.vec_LOC    = est.vec_LOC__)
  return(out__)
}
dMod_expand_cat <- function(condition.grid, catName, catValues, ref) {
  if (any(grepl("[ /()+-]", as.character(catValues))))
    stopIQR(paste0("The levels", grep("[ /()]", as.character(catValues)), "of categorical variable", catName, "contains unallowed symbols"))
  catValues_wo_ref__ <- catValues[!catValues==ref]
  aux_pars__ <- paste0(catName, "_", catValues_wo_ref__)
  named_indices__ <- stats::setNames(seq_along(aux_pars__), aux_pars__)
  aux_cols <- lapply(named_indices__, function(i) {
    as.integer(condition.grid[[catName]] == catValues[!catValues == ref][i])
  })
  aux_cols <- do.call(cbind, aux_cols)
  out__ <- cbind(condition.grid[names(condition.grid) != catName], aux_cols)
  aux_cols_old__ <- attr(condition.grid, "aux_cols")
  aux_cols_attr__ <- c(attr(condition.grid, "aux_cols"), colnames(aux_cols))
  attr(out__, "aux_cols") <- c(aux_cols_old__, list(aux_cols_attr__))
  levels_mapping_old__ <- attr(condition.grid, "levels_mapping")
  levels_mapping <- do.call(c, lapply(as.list(as.data.frame(aux_cols)), function(.x) {condition.grid[[catName]][as.logical(.x)]}))
  attr(out__, "levels_mapping") <- c(levels_mapping_old__, list(levels_mapping))
  return(out__)
}
dMod_cov_multiplier <- function(cov, ref, to_which, scale = c("L", "N", "G")) {
  if (scale == "N")
    return(paste0("(", "log(", cov , " / ", ref, ") * beta_", to_which, "_", cov,")"))
  if (scale != "N")
    return(paste0("(", "log(", cov , " / ", ref, ") * beta_", to_which, "_", cov,")"))
}
dMod_cat_multiplier <- function(cat, catValues, ref, to_which, scale = c("L", "N", "G")) {
  catValues_wo_ref__ <- catValues[!catValues==ref]
  cov_pars__ <- paste0(cat, "_", catValues_wo_ref__)
  beta_pars__ <- paste0("beta", "_", to_which, "_", cov_pars__)
  if (scale == "N")
    return(paste(paste0("(", beta_pars__, " * ", cov_pars__, ")"), collapse = " + "))
  if (scale != "N")
    return(paste(paste0("(", beta_pars__, " * ", cov_pars__, ")"), collapse = " + "))
}
dMod_get_fixed_betas <- function(COVestimate, covNames, covariateModelValues, catNames, catValues, covariateCATreference) {
  do.call(c, lapply(seq_along(COVestimate), function(i) {
    is_fixed__ <- (COVestimate[[i]] == 0)
    if (sum(is_fixed__) == 0)
      return(NULL)
    to_which <- names(COVestimate)[i]
    fixed_covariates__ <- names(COVestimate[[i]])[is_fixed__]
    cov <- do.call(c, lapply(fixed_covariates__[fixed_covariates__%in%covNames], function(mycov) {
      cov_value__ <- covariateModelValues[[i]][mycov]
      names(cov_value__) <- paste0("beta_", to_which, "_", mycov)
      return(cov_value__) }))
    cat <- do.call(c, lapply(fixed_covariates__[fixed_covariates__%in%catNames], function(mycov) {
      cat_value__ <- covariateModelValues[[i]][mycov]
      cat_ref__ <- covariateCATreference[mycov]
      if (length(catValues[[mycov]])>2)
        stopIQR("Fixing betas of categorical variables with more than two categroies are not yet implemented") 
      names(cat_value__) <- paste0("beta_", to_which, "_", mycov, "_", setdiff(catValues[[mycov]], cat_ref__))
      return(cat_value__)}))
    return(c(cov, cat))
  }))
}
dMod_get_default_betas <- function(myNlmeEst__, cov_trafo) {
  fixed_betas__ <- attr(cov_trafo, "fixed")
  covdefaultsALL__ <- unlist(lapply(names(myNlmeEst__$modelSpec$covariateModelValues), function(n__) {
    values__ <- myNlmeEst__$modelSpec$covariateModelValues[[n__]]
    names(values__) <- paste("beta", n__, names(values__), sep = "_")
    return(values__)
  }))
  covdefaults__ <- covdefaultsALL__[!duplicated(names(covdefaultsALL__))]
  covdefaults__ <- covdefaults__[sapply(names(covdefaults__), function(n__) !any(grepl(paste0("^", n__), names(fixed_betas__))))]
  if (length(covdefaults__) == 0) return()
  possible_covpars_regex__ <- paste0("(", paste0(names(covdefaults__), collapse = "|"), "|", paste0(paste0(names(covdefaults__), "_[1-9]+$"), collapse = "|"), ")")
  covparnames__ <- getSymbols(cov_trafo)[grepl(possible_covpars_regex__, getSymbols(cov_trafo))]
  index__ <- match(sub("_[1-9]+$", "", covparnames__), names(covdefaults__))
  betas__ <- covdefaults__[index__]
  names(betas__) <- covparnames__
  return(betas__)
}
dMod_build_covcat_trafo <- function(myNlmeEst__) {
  covariateModel__        <- myNlmeEst__$modelSpec$covariateModel
  COVestimate             <- myNlmeEst__$modelSpec$COVestimate
  covariateModelValues    <- myNlmeEst__$modelSpec$covariateModelValues
  covNames                <- myNlmeEst__$data$covNames
  covariateMedianValues__ <- myNlmeEst__$data$covariateMedianValues
  covariateMedianValues__[intersect(covNames, names(myNlmeEst__$modelSpec$COVcentering))] <-
    myNlmeEst__$modelSpec$COVcentering[intersect(covNames, names(myNlmeEst__$modelSpec$COVcentering))]
  catNames                <- myNlmeEst__$data$catNames
  covariateCATreference__ <- myNlmeEst__$data$covariateCATreference
  covariateCATreference__[intersect(catNames, names(myNlmeEst__$modelSpec$COVcentering))] <-
    myNlmeEst__$modelSpec$COVcentering[intersect(catNames, names(myNlmeEst__$modelSpec$COVcentering))]
  catValues               <- myNlmeEst__$data$catValues
  cov_trafo <- vapply(names(covariateModel__), function(mypar) {
    scale <- myNlmeEst__$modelSpec$IIVdistribution[mypar]
    cov_expressions__ <- vapply(covariateModel__[[mypar]], function(mycov) {
      if(mycov %in% covNames)
        return(dMod_cov_multiplier(cov = mycov,
                                   ref = covariateMedianValues__[mycov],
                                   to_which = mypar,
                                   scale = scale))
      if (mycov %in% catNames) {
        return(dMod_cat_multiplier(cat = mycov,
                                   catValues = catValues[[mycov]],
                                   ref = covariateCATreference__[mycov],
                                   to_which = mypar,
                                   scale = scale))
      }
    }, FUN.VALUE = "")
    separator__ <- switch(scale, L = " + ", G = " + ", N = " + ")
    mytrafo__ <- paste(mypar, paste0(cov_expressions__, collapse = separator__), sep = separator__)
    if (scale == "N")
      mytrafo__ <- paste0("(", mytrafo__, ")")
    if (scale == "L")
      mytrafo__ <- paste0("exp(", mytrafo__, ")")
    if (scale == "G")
      mytrafo__ <- paste0("exp(", mytrafo__, ") / (1 + exp(", mytrafo__, "))")
    return(mytrafo__)
  }, FUN.VALUE = "")
  names(cov_trafo) <- names(covariateModel__)
  fixed_beta_pars__ <- dMod_get_fixed_betas(COVestimate, covNames, covariateModelValues, catNames, catValues, covariateCATreference__)
  attr(cov_trafo, "fixed") <- fixed_beta_pars__
  return(cov_trafo)
}
dMod_handle_default_options <- function(OPT, SIMOPT) {
  SIMOPT0__ <- list(
    SIMOPT.method    = "lsodes",
    SIMOPT.atol      = 1e-6,
    SIMOPT.rtol      = 1e-6,
    SIMOPT.hmin      = 0,
    SIMOPT.hmax      = 10000,
    SIMOPT.hini      = 0,
    SIMOPT.maxsteps  = 5000,
    SIMOPT.nauxtimes = 0,
    SIMOPT.cores     = 1
  )
  OPT0__ <- list(
    opt.method = "trust",
    opt.nfits = 10,
    opt.sd = 1,
    opt.rinit = 1,
    opt.rmax = 10,
    opt.iterlim = 100,
    opt.prior_sigma = 10,
    opt.parlower = -Inf,
    opt.parupper = Inf
  )
  if (length(c(setdiff(names(OPT), names(OPT0__)),setdiff(names(SIMOPT), names(SIMOPT0__)))))
    stopIQR("Unkown options: ", paste0(c(setdiff(names(OPT), names(OPT0__)),setdiff(names(SIMOPT), names(SIMOPT0__))), collapse = ", "),
            "\n Please add these to dMod_handle_default_options()")
  OPT0__[names(OPT)] <- OPT
  SIMOPT0__[names(SIMOPT)] <- SIMOPT
  return(list(OPT = OPT0__, SIMOPT = SIMOPT0__))
}
dMod_check_mstrust_results <- function(mymodel, opt.parlower, opt.parupper) {
  myparframe__ <- mymodel$parframes[[1]]
  mybestfit__  <- unclass(mymodel$bestfit[[1]])
  myparlower__ <- opt.parlower
  myparupper__ <- opt.parupper
  less_than_10_percent_converged__ <- sum(myparframe__$converged) < floor(0.1*nrow(myparframe__))
  is_converged_bestfit__ <- myparframe__$converged[1]
  on_lower_boundary_bestfit__ <- abs(myparlower__ - mybestfit__[names(myparlower__)]) < sqrt(.Machine$double.eps)
  on_lower_boundary_bestfit__ <- on_lower_boundary_bestfit__[!is.na(on_lower_boundary_bestfit__)]
  if (length(on_lower_boundary_bestfit__) == 0 || all(!on_lower_boundary_bestfit__)) on_lower_boundary_bestfit__ <- NULL
  on_upper_boundary_bestfit__ <- abs(myparupper__ - mybestfit__[names(myparupper__)]) < sqrt(.Machine$double.eps)
  on_upper_boundary_bestfit__ <- on_upper_boundary_bestfit__[!is.na(on_upper_boundary_bestfit__)]
  if (length(on_upper_boundary_bestfit__) == 0 || all(!on_upper_boundary_bestfit__)) on_upper_boundary_bestfit__ <- NULL
  checks__ <- list(less_than_10_percent_converged = less_than_10_percent_converged__,
                   is_converged_bestfit      = is_converged_bestfit__,
                   on_lower_boundary_bestfit = on_lower_boundary_bestfit__,
                   on_upper_boundary_bestfit = on_upper_boundary_bestfit__)
  return(checks__)
}
dMod_appendLowerOmega <- function(mymodel, opt.omega_min) {
  lower_omega__ <- with(mymodel[["parameters"]][[1]], {
    has_omega__ <- names(IIVestimate)[IIVestimate == 1]
    if (length(has_omega__) == 0 )
      return(NULL)
    out__ <- structure(rep(log(opt.omega_min), length(has_omega__)), names = has_omega__)
    names(out__) <- paste0("omega_", names(out__))
    return(out__)
  })
  mymodel$lower_omega <- list(lower_omega__)
  return(mymodel)
}
#'@export
dMod_nlmeEst2dModFrame <- function(est,
                                   algOpt.SEED = 12345,
                                   ...) {
  options__ <- c(list(...), list(algOpt.SEED = algOpt.SEED))
  SIMOPT__ <- options__[grepl("^SIMOPT\\.", names(options__))]
  OPT__ <- options__[grepl("^opt\\.", names(options__))]
  SIMOPT__ <- dMod_handle_default_options(OPT__, SIMOPT__)$SIMOPT
  OPT__ <- dMod_handle_default_options(OPT__, SIMOPT__)$OPT
  cat("Convert data to dMod ...... ")
  datanames__ <- stats::setNames(nm = c("datafile", "catNames", "covNames", "regressorNames"))
  data__ <- lapply(datanames__, function(i) est$data[[i]])
  data__[["inputs"]] <- max(1, length(est[["model"]][["inputs"]])) 
  data__$datafile <- file.path(est$data$relPathFromProject, est$data$fileName)
  my_dMod_data__ <- do.call(convert2dMod_IQRdata, data__)
  cat("done. \n")
  cat("Convert model to basic dMod model ..... ")
  FLAGderiv__ <- ifelse(OPT__[["opt.method"]] %in% "trust", TRUE, FALSE)
  basic_dMod_model__ <- convert2dMod_IQRmodel(est = est,
                                              my_dMod_data = my_dMod_data__,
                                              options = SIMOPT__,
                                              FLAGderiv = FLAGderiv__)
  cat("done. \n")
  cat("Link basic dMod model to data ...... ")
  mymodel <- combine_model_and_data(basic_dMod_model__,
                                    my_dMod_data__,
                                    est,
                                    SIMOPT.nauxtimes = SIMOPT__[["SIMOPT.nauxtimes"]],
                                    SIMOPT.cores = SIMOPT__[["SIMOPT.cores"]])
  cat("done. \n")
  mymodel <- dplyr::mutate(mymodel, constr_fit = list(NULL))
  if (!is.null(OPT__[["opt.prior_sigma"]])) {
    mu__ <- mymodel[["pars"]][[1]]
    mu__ <- mu__[!grepl("^(omega|ETA)", names(mu__))]
    mymodel <- dplyr::mutate(mymodel,
                             constr_fit = list(constraintL2(mu__, sigma = OPT__[["opt.prior_sigma"]])),
                             obj = list(obj + constr_fit))
  }
  startpars__ <- msParframe(mymodel$pars[[1]], n = OPT__[["opt.nfits"]], seed = algOpt.SEED, sd = OPT__[["opt.sd"]])
  mymodel <- dplyr::mutate(mymodel, startpars = list(startpars__))
  mymodel <- dplyr::mutate(mymodel, options = list(options__))
  .so__ <- .Platform$dynlib.ext
  modelnames__ <- Reduce(union, lapply(unlist(mymodel, FALSE), function(i) {if (inherits(i, "fn")) return(modelname(i))}))
  modelnames__ <- paste0(modelnames__, .so__)
  mymodel <- dplyr::mutate(mymodel, dllpaths = list(modelnames__), dllfolder = list(tempdirIQR()))
  args0.est__ <- attr(est, "args0")
  args0__ <- list()
  args0__[["Scenario"]] <- args0.est__[["Scenario"]]
  sysModelEst__ <- list()
  sysModelEst__[["model"]]              <- args0.est__[["model"]]
  sysModelEst__[["dosing"]]             <- args0.est__[["dosing"]]
  sysModelEst__[["data"]]               <- args0.est__[["data"]]
  sysModelEst__[["estSpec"]]            <- args0.est__[["estSpec"]]
  sysModelEst__[["modelSpec"]]          <- args0.est__[["modelSpec"]]
  sysModelEst__[["parsOfInterest"]]     <- args0.est__[["parsOfInterest"]]
  sysModelEst__[["data"]][["datafile"]] <- est$data$datafile
  sysModelEst__[["est"]]                <- est
  sysModelEst__[["IQRsysData"]]         <- import_IQRsysData(est$data$datafile)
  attr(mymodel, "args0")       <- args0__
  attr(mymodel, "sysModelEst") <- sysModelEst__
  class(mymodel) <- unique(c("IQRsysModel", class(mymodel)))
  return(mymodel)
}
#'@export
dMod_sysProject_computations <- function(mymodel, ncores, FLAGrequireConverged = TRUE, FLAGkeepFits = FALSE, FLAGchecks = TRUE, FLAGprofileLL = FALSE) {
  loadedPackages__ <- .packages()
  if (ncores > 1 & any(c("RevoUtils", "RevoUtilsMath") %in% loadedPackages__))
    stopIQR("Loaded packages RevoUtils/RevoUtilsMath can cause problems with IQRtools when running with ncores > 1. Please unload with\n",
            "    detach('package:RevoUtils', unload=TRUE)\n",
            "    detach('package:RevoUtilsMath', unload=TRUE)\n",
            "before continuing.")
  SIMOPT.cores <- mymodel[["options"]][[1]][["SIMOPT.cores"]]
  if (SIMOPT.cores > 1 & ncores > 1) {
    ncores <- 1
    warningIQR("When SIMOPT.cores > 1, fits cannot be parallelized. Argument ncores has been changed to 1.")
  }
  myattributes__ <- attributes(mymodel)
  partable__ <- mymodel[["IQRpartable"]][[1]]
  pars__ <- mymodel[["pars"]][[1]]
  opt.method <- mymodel[["options"]][[1]][["opt.method"]]
  opt.rinit <- mymodel[["options"]][[1]][["opt.rinit"]]
  opt.rmax <- mymodel[["options"]][[1]][["opt.rmax"]]
  opt.iterlim <- mymodel[["options"]][[1]][["opt.iterlim"]]
  opt.parlower <- mymodel[["options"]][[1]][["opt.parlower"]]
  opt.parupper <- mymodel[["options"]][[1]][["opt.parupper"]]
  if (length(opt.parlower) > 0) {
    names__ <- intersect(names(opt.parlower), names(pars__))
    if (length(names__) > 0) {
      opt.parlower <- sapply(names__, function(n__) {
        switch(partable__$trafo[partable__$parametername == n__],
               "N" = opt.parlower[n__],
               "L" = log(opt.parlower[n__]),
               "G" = logit(opt.parlower[n__]))
      })
      names(opt.parlower) <- names__
    } else opt.parlower <- NULL
  }
  if (length(opt.parupper) > 0) {
    names__ <- intersect(names(opt.parupper), names(pars__))
    if (length(names__) > 0) {
      opt.parupper <- sapply(names__, function(n__) {
        switch(partable__$trafo[partable__$parametername == n__],
               "N" = opt.parupper[n__],
               "L" = log(opt.parupper[n__]),
               "G" = logit(opt.parupper[n__]))
      })
      names(opt.parupper) <- names__
    } else opt.parupper <- NULL
  }
  if (FLAGchecks) {
    cat("\n----------------------------------------------------------------------------------------\n", file = "project_obj.log", append = TRUE)
    cat("Initial Checking of Objective Function\n", file = "project_obj.log", append = TRUE)
    cat("----------------------------------------------------------------------------------------\n\n", file = "project_obj.log", append = TRUE)
    testmodel__ <- lapply(mymodel, function(x) x[[1]])
    cat("Checking generated functions ")
    is_df <- opt.method %in% c("hjkb", "nmkb")
    if (is_df) cat("[     ]") else cat("[       ]")
    with(testmodel__, {
      is_inf.pars__ <- !is.finite(pars)
      if (any(is_inf.pars__)) stopIQR("Parameters (", paste(names(pars)[is_inf.pars__], collapse = ", "), ") were initialized with infinite values.")
      if (is_df) cat("\b\b\b\b\b\b\b[-    ]") else cat("\b\b\b\b\b\b\b\b\b[-      ]")
      dummy__ <- run_silent_IQR(try(p(pars), silent = TRUE))
      if (any(sapply(dummy__, function(d__) inherits(d__, "try-error"))))
        stopIQR("Evaluation of model parameterization yields error. Possible sources:\n",
                "  * Initial conditions in STATES section of IQR model file\n",
                "  * VARIABLES section of IQR model file\n",
                "  * INITIAL ASSIGNMENTS section of IQR model file\n",
                "  * IIVdistribution in model spec")
      if (is_df) cat("\b\b\b\b\b\b\b[--   ]") else cat("\b\b\b\b\b\b\b\b\b[--     ]")
      dummy__ <- run_silent_IQR(try(prd(times, pars, deriv = FALSE), silent = TRUE))
      if (inherits(dummy__, "try-error")) stopIQR("Simulation of model without derivatives failed. Possible sources:\n",
                                                  "  * Bad parameter guesses or initial values\n",
                                                  "  * Integration tolerances\n",
                                                  "Try to simulate with sim_IQRsysModel() to find appropriate initial guesses and integrator settings.")
      else {
        infinits <- lapply(dummy__, function(d__) sapply(as.data.frame(d__), function(x__) any(is.infinite(x__))))
        inf_cond <- names(dummy__)[sapply(infinits, function(i__) any(i__))]
        inf_state <- Reduce(union, lapply(infinits, function(i__) names(i__)[i__]))
        if (length(inf_cond) > 0) stopIQR("Infinite prediction detected.\n",
                                          "Affected states: ", paste(inf_state, collapse = ", "), "\n",
                                          "Affected conditions: ", paste(inf_cond, collapse = ", "), "\n")
      }
      if (is_df) cat("\b\b\b\b\b\b\b[---  ]") else cat("\b\b\b\b\b\b\b\b\b[---    ]")
      dummy__ <- run_silent_IQR(try((e*prd)(times, pars, deriv = FALSE), silent = TRUE))
      if (hasError(dummy__)) stopIQR("Error model cannot be evaluated. Please check \"errorModel\" in your modelSpec.",
                                     getErrorMessage(dummy__))
      sigma__ <- as.data.frame(dummy__)
      is_inf.sigma__ <- !is.finite(sigma__[["value"]])
      if (any(is_inf.sigma__)) stopIQR("Error model cannot be evaluated for following outputs: ", paste(unique(sigma__[["name"]][is_inf.sigma__]), sep = ", "), ".\n",
                                       "Check the model predictions for these outputs.")
      is_neg.sigma__ <- (sigma__[["value"]] < 0)
      if (any(is_neg.sigma__)) stopIQR("Error model predicts negative values for the following outputs: ", paste(unique(sigma__[["name"]][is_neg.sigma__]), sep = ", "), ".\n",
                                       "Probably a relative error model is used and the predicted OUTPUT is negative.")
      is_zero.sigma__ <- (sigma__[["value"]] == 0)
      if (any(is_zero.sigma__)) stopIQR("Error model predicts 0 for the following outputs: ", paste(unique(sigma__[["name"]][is_zero.sigma__]), sep = ", "), ".\n",
                                        "Probably a relative error model is used and the predicted OUTPUT is 0.\n",
                                        "Please change error model to \"absrel\" or remove observation records from data set that ",
                                        "correspond to 0 predictions.")
      if (is_df) cat("\b\b\b\b\b\b\b[---- ]") else cat("\b\b\b\b\b\b\b\b\b[----   ]")
      dummy__ <- run_silent_IQR(try(obj(pars, deriv = FALSE), silent = TRUE))
      if (hasError(dummy__)) {
        stopIQR("Error in evaluating objective function.", toString(getErrorMessage(dummy__)))
      }
      if (!is.finite(dummy__[["value"]])) {
        stopIQR("Objective function cannot be evaluated to a finite value.")
      }
      if (is_df) cat("\b\b\b\b\b\b\b[-----]") else cat("\b\b\b\b\b\b\b\b\b[-----  ]")
      if (!is_df) {
        dummy__ <- run_silent_IQR(try(prd(times, pars, deriv = TRUE), silent = TRUE))
        if (any(sapply(dummy__, function(d__) inherits(d__, "try-error"))))
          stopIQR("Simulation of model sensitivities failed. Known sources:\n",
                  "  * Typically estimation of hill parameters and exponents.\n",
                  "  * Generally, estimation of parameters involved in expressions x^p (x is a state, p is the parameter to be estimated)\n",
                  "The problem for the above cases occurs when x = 0. Conserider shiftig x by a small value eps -> (x+eps)^p in ",
                  "your model file.")
        cat("\b\b\b\b\b\b\b\b\b[------ ]")
        dummy__ <- run_silent_IQR(try(obj(pars, deriv = TRUE), silent = TRUE))
        if (hasError(dummy__)) {
          stopIQR("Error in evaluating objective function with derivatives.", toString(getErrorMessage(dummy__)))
        }
        is_inf.grad__ <- !is.finite(dummy__[["gradient"]])
        if (any(is_inf.grad__)) stopIQR("Non-finite gradient of the objective function for the following parameters: ", paste(names(dummy__[["gradient"]])[is_inf.grad__]), collapse = ", ", ".\n",
                                        "Reason unknown.")
        cat("\b\b\b\b\b\b\b\b\b[-------]")
      }
    })
    cat(" ... done.\n")
  }
  cat("\n----------------------------------------------------------------------------------------\n", file = "project_obj.log", append = TRUE)
  cat("Parameter Estimation\n", file = "project_obj.log", append = TRUE)
  cat("----------------------------------------------------------------------------------------\n\n", file = "project_obj.log", append = TRUE)
  cat("Running fits ... ")
  fits__ <- mstrust(mymodel$obj[[1]],
                    mymodel$startpars[[1]],
                    rinit   = opt.rinit,
                    rmax    = opt.rmax,
                    iterlim = opt.iterlim,
                    cores = ncores,
                    optmethod = opt.method,
                    parlower = opt.parlower,
                    parupper = opt.parupper,
                    printIter = TRUE,
                    traceFile = "optTrace.csv")
  mymodel <- dplyr::mutate(mymodel, fits = list(fits__))
  cat("done.\n")
  cat("Augmenting result by derived quantities ... ")
  mymodel <- appendParframes(mymodel, keepFits = FLAGkeepFits)
  mymodel <- dplyr::mutate(mymodel,
                           bestfit = list(as.parvec(parframes)),
                           pars = list(as.parvec(parframes)),
                           selected = list(1))
  mstrustchecks__ <- dMod_check_mstrust_results(mymodel, opt.parlower, opt.parupper)
  if (mstrustchecks__$less_than_10_percent_converged)
    warningIQR("\nLess than 10% of fits converged. Try to run the project with more iterations.")
  if (!mstrustchecks__$is_converged_bestfit)
    warningIQR("\nBest fit is not converged. Try to run the project with more iterations.")
  if (!mstrustchecks__$is_converged_bestfit & FLAGprofileLL)
    warningIQR("\nProfiles are not going to be computed due to unconverged fit.")
  fixed_on_lower__ <- fixed_on_upper__ <- NULL
  if (!is.null(mstrustchecks__$on_lower_boundary_bestfit)) {
    message("\nSome parameters have reached their lower boundary in the best fit.")
    fixed_on_lower__ <- opt.parlower[mstrustchecks__$on_lower_boundary_bestfit]
  }
  if (!is.null(mstrustchecks__$on_upper_boundary_bestfit)) {
    message("\nSome parameters have reached their upper boundary in the best fit.")
    fixed_on_upper__ <- opt.parupper[mstrustchecks__$on_upper_boundary_bestfit]
  }
  fixed__ <- c(fixed_on_lower__, fixed_on_upper__)
  mymodel <- dplyr::mutate(mymodel,
                           fixed_on_lower = list(fixed_on_lower__),
                           fixed_on_upper = list(fixed_on_upper__))
  mymodel <- vcov_IQRsysModel(mymodel)
  cat("done.\n")
  saveRDS(mymodel, file = "project_result.sysfit")
  mymodel <- dplyr::mutate(mymodel, profiles = list(NULL))
  if (FLAGprofileLL & mstrustchecks__$is_converged_bestfit) {
    cat("\n----------------------------------------------------------------------------------------\n", file = "project_obj.log", append = TRUE)
    cat("Profile Likelihood\n", file = "project_obj.log", append = TRUE)
    cat("----------------------------------------------------------------------------------------\n\n", file = "project_obj.log", append = TRUE)
    cat("Running profile likelihood ... ")
    if (length(mymodel[["fixed_on_lower"]][[1]]) + length(mymodel[["fixed_on_upper"]][[1]]) > 0)
      message("\nEstimated parameters on bounds are fixed during profile likelihood computation.")
    mymodel <- dplyr::mutate(mymodel,
                             profiles = list(
                               structure(
                                 list(profile(obj,
                                              bestfit, 
                                              whichPar = names(bestfit)[!(grepl("^(omega|ETA|error_ADD|error_PROP)", names(bestfit))|names(bestfit)%in%names(c(fixed_on_lower, fixed_on_upper)))],
                                              cores = ncores,
                                              verbose = FALSE,
                                              fixed = c(fixed_on_lower, fixed_on_upper))),
                                 names = "1" 
                               )))
    cat("done.\n")
  }
  attr(mymodel, "sysModelEst") <- myattributes__[["sysModelEst"]]
  attr(mymodel, "args0") <- myattributes__[["args0"]]
  class(mymodel) <- unique(c("IQRsysModel", class(mymodel)))
  mymodel$pars[[1]] <- structure(as.numeric(mymodel$bestfit[[1]]),
                                 names = names(mymodel$bestfit[[1]]))
  mymodel <- pred_IQRsysModel(mymodel)
  mymodel <- partable_IQRsysModel(mymodel)
  mymodel <- sim_IQRsysModel(mymodel)
  mymodel <- metrics_IQRsysModel(mymodel)
  return(mymodel)
}
genModelParInfo_SYSFIT <- function(est) {
  param_fixed__ <- names(est$modelSpec$POPestimate)[est$modelSpec$POPestimate==0]
  param_estimated__ <- names(est$modelSpec$POPestimate)[est$modelSpec$POPestimate==1]
  cov_param__ <- as.vector(unlist(sapply(names(est$modelSpec$covariateModel), function(x) {
    paste0("beta_",x,"_",est$modelSpec$covariateModel[[x]])
  })))
  cov_param_estimated__ <- cov_param__[unlist(sapply(names(est$modelSpec$covariateModel), function(x) {
    est$modelSpec$COVestimate[[x]]
  }))==1]
  cov_param_fixed__ <- setdiff(cov_param__,cov_param_estimated__)
  covariates__ <- unique(unlist(est$modelSpec$covariateModel))
  covariateValues__ <- as.vector(unlist(sapply(names(est$modelSpec$covariateModel), function(x) {
    est$modelSpec$covariateModelValues[[x]]
  })))
  names(covariateValues__) <- cov_param__
  output__ <- ""
  covParamFixedText <- sapply(cov_param_fixed__, function (x) {
    paste0("    ",x," <- ",covariateValues__[x])
  })
  if (length(covParamFixedText) > 0) {
    output__ <- paste0(output__,"# Fixed covariate coefficients")
    output__ <- paste0(output__,"\n",paste0(covParamFixedText,collapse="\n"))
    output__ <- paste0(output__,"\n\n")
  }
  paramTextEstimated <- sapply(param_estimated__, function (param__) {
    covModel <- ""
    if (!is.null(est$modelSpec$covariateModel[[param__]])) {
      cats__ <- intersect(est$data$catNames,est$modelSpec$covariateModel[[param__]])
      covs__ <- intersect(est$data$covNames,est$modelSpec$covariateModel[[param__]])
      catText__ <- sapply(cats__, function (cat) {
        paste0(" + beta_",param__,"_",cat,"*",cat)
      })
      if (length(catText__) > 0) {
        catText__ <- paste0(catText__,collapse="")
      } else {
        catText__ <- ""
      }
      covText__ <- sapply(covs__, function (cov) {
        covReference__ <- est$data$covariateMedianValues[cov] 
        paste0(" + beta_",param__,"_",cov,"*log(",cov,"/",covReference__,")")
      })
      if (length(covText__) > 0) {
        covText__ <- paste0(covText__,collapse="")
      } else {
        covText__ <- ""
      }
      covModel <- paste0(covText__,catText__)
    }
    if (est$modelSpec$IIVestimate[param__]!=0) {
      etaText__ <- paste0(" + ETA_",param__)
    } else {
      etaText__ <- ""
    }
    out__ <- paste0("    ",param__," <- ")
    if (est$modelSpec$IIVdistribution[param__]=="G") {
      x__ <- paste0("trans",param__,etaText__,covModel)
      out__ <- paste0(out__,"exp(",x__,")/(1+exp(",x__,"))")
    }
    if (est$modelSpec$IIVdistribution[param__]=="L") out__ <- paste0(out__,"exp(trans",param__,etaText__,covModel,")")
    if (est$modelSpec$IIVdistribution[param__]=="N") out__ <- paste0(out__,"trans",param__,etaText__,covModel)
    out__
  })
  output__ <- paste0(output__,"# Parameter transformations\n")
  output__ <- paste0(output__,paste0(paramTextEstimated,collapse="\n"))
  output__ <- paste0(output__,"\n\n")
  paramTextFixed__ <- sapply(param_fixed__, function (param__) {
    covModel <- ""
    if (!is.null(est$modelSpec$covariateModel[[param__]])) {
      cats__ <- intersect(est$data$catNames,est$modelSpec$covariateModel[[param__]])
      covs__ <- intersect(est$data$covNames,est$modelSpec$covariateModel[[param__]])
      catText__ <- sapply(cats__, function (cat) {
        paste0(" + beta_",param__,"_",cat,"*",cat)
      })
      if (length(catText__) > 0) {
        catText__ <- paste0(catText__,collapse="")
      } else {
        catText__ <- ""
      }
      covText__ <- sapply(covs__, function (cov) {
        covReference__ <- est$data$covariateMedianValues[cov] 
        paste0(" + beta_",param__,"_",cov,"*log(",cov,"/",covReference__,")")
      })
      if (length(covText__) > 0) {
        covText__ <- paste0(covText__,collapse="")
      } else {
        covText__ <- ""
      }
      covModel <- paste0(covText__,catText__)
    }
    value <- est$modelSpec$POPvalues[param__]
    out__ <- paste0("    ",param__," <- ")
    if (est$modelSpec$IIVdistribution[param__]=="G") out__ <- paste0(out__,"inv_logit(logit(",value,")",covModel,")")
    if (est$modelSpec$IIVdistribution[param__]=="L") out__ <- paste0(out__,"exp(log(",value,")",covModel,")")
    if (est$modelSpec$IIVdistribution[param__]=="N") out__ <- paste0(out__,value,covModel,param__)
    out__
  })
  if (length(paramTextFixed__) > 0) {
    output__ <- paste0(output__,"# Fixed model parameters (considered in modelSpec)")
    output__ <- paste0(output__,"\n",paste0(paramTextFixed__,collapse="\n"))
    output__ <- paste0(output__,"\n")
  }
  cov_param__ <- sapply(cov_param__, function (x) {
    x <- aux_strrep(x,"beta_","")
    x <- aux_strrep(x,"_","(")
    paste0("beta_",x,")")
  })
  cov_param_estimated__ <- sapply(cov_param_estimated__, function (x) {
    x <- aux_strrep(x,"beta_","")
    x <- aux_strrep(x,"_","(")
    paste0("beta_",x,")")
  })
  paramsAll__ <- c(names(est$modelSpec$POPestimate),
                   cov_param__)
  paramsEstimated__ <- c(est$modelSpec$POPestimate,
                         as.numeric(cov_param__ %in% cov_param_estimated__))
  paramsInit__ <- c(est$modelSpec$POPvalues0,
                    covariateValues__)
  if (!is.null(est$data$covNames)) {
    covParam__ <- cov_param__[sort(unique(unlist(sapply(est$data$covNames, function (x) {
      which(grepl(paste0("(",x),cov_param__,fixed=TRUE))
    }))))]
  } else {
    covParam__ <- NULL
  }
  if (!is.null(est$data$catNames)) {
    catParam__ <- cov_param__[sort(unique(unlist(sapply(est$data$catNames, function (x) {
      which(grepl(paste0("(",x),cov_param__,fixed=TRUE))
    }))))]
  } else {
    catParam__ <- NULL
  }
  covTrans__ <- sapply(covParam__, function (cov) {
    ix__ <- gregexpr("(",cov,fixed=TRUE)[[1]][1]
    cov <- substr(cov,ix__+1,nchar(cov)-1)
    covReference__ <- est$data$covariateMedianValues[cov]
    paste0("log(cov/",covReference__,")")
  })
  catRef__ <- rep(0,length(catParam__))
  names(catRef__) <- catParam__
  betacatcategories__ <- lapply(catParam__, function (x) {
    c(0,1)
  })
  names(betacatcategories__) <- catParam__
  return(list(output=output__,
              parameters = paramsAll__,
              paramsEstimated = paramsEstimated__,
              paramsInit = paramsInit__,
              covariatesused = covariates__,
              covParam = covParam__,
              covTrans = covTrans__,
              catParam = catParam__,
              catRef   = catRef__, 
              betacatcategories = betacatcategories__ 
  )
  )
}
dMod_process_bestfit <- function(mymodel) {
  bestfit__  <- unclass(mymodel$bestfit[[1]])
  attr(bestfit__, "deriv") <- NULL
  sigma__ <- sqrt(diag(mymodel[["vcov"]][[1]]))
  fixed.grid <- mymodel$fixed.grid[[1]]
  parameters <- mymodel$parameters[[1]]
  pars <- list()
  pars$ETA        <- bestfit__[grepl("^ETA_", names(bestfit__))]
  pars$omega      <- bestfit__[grepl("^omega_", names(bestfit__))]
  pars$beta       <- bestfit__[grepl("^beta_", names(bestfit__))]
  pars$error      <- bestfit__[grepl("^error", names(bestfit__))]
  pars$pop        <- bestfit__[!grepl("^(ETA|omega|beta|error)", names(bestfit__))]
  fixed <- list()
  fixed$ETA        <- NULL
  fixed$omega      <- unlist(fixed.grid[1, grepl("omega_", names(fixed.grid))])
  fixed$beta       <- NULL
  fixed$errors     <- NULL
  fixed$pop        <- unlist(fixed.grid[1,intersect(names(fixed.grid), names(parameters[[1]]))])
  pars <- do.call(rbind,lapply(seq_along(pars), function(.x) {
    if(length(pars[[.x]]) == 0)
      return(NULL)
    data.frame(parametername = names(pars[[.x]]),
               parametervalue = pars[[.x]],
               sigma = sigma__[names(pars[[.x]])],
               parametertask = names(pars)[.x],
               estimated = 1,
               stringsAsFactors = FALSE)
  }))
  fixed <- do.call(rbind,lapply(seq_along(fixed), function(.x) {
    if(length(fixed[[.x]]) == 0)
      return(NULL)
    data.frame(parametername = names(fixed[[.x]]),
               parametervalue = fixed[[.x]],
               sigma = NA,
               parametertask = names(fixed)[.x],
               estimated = 0,
               stringsAsFactors = FALSE)
  }))
  parameters_df <- rbind(pars, fixed)
  parameters_df$RSE <- abs(parameters_df$sigma / parameters_df$parametervalue) * 100
  belongs_to <- function(parametername, parameters) {
    gsub(paste0(".*(",paste0(names(parameters[[1]]), collapse = "|"), ").*"), "\\1", parametername)
  }
  parameters_df$belongs_to <- belongs_to(parameters_df$parametername, parameters)
  covariate_levels <- function(parametername, belongs_to) {
    out__ <- rep(NA, length(belongs_to))
    is_beta__ <- grepl("^beta", parametername)
    betas__ <- parametername[is_beta__]
    to_which <- unique(belongs_to[is_beta__])
    betas__ <- gsub(paste0("^beta_(",paste0(to_which, collapse = "|"), ")_"), "", betas__)
    out__[is_beta__] <- betas__
    return(out__)
  }
  parameters_df$covariate_levels <- covariate_levels(parameters_df$parametername, parameters_df$belongs_to)
  parameters_df$trafo <- parameters$IIVdistribution[parameters_df$belongs_to]
  parameters_df$trafo[parameters_df$parametertask %in% c("error")] <- "L"
  parameters_df$parametervalue_linear <-
    vapply(seq_len(nrow(parameters_df)), function(.x) switch(parameters_df$trafo[.x], N = parameters_df$parametervalue[.x], L = exp(parameters_df$parametervalue[.x]), G = inv_logit(parameters_df$parametervalue[.x]), `NA` = exp(parameters_df$parametervalue[.x])), FUN.VALUE = 1)
  dinv_logit__ <- function(x) (exp(x))/((1+exp(x))^2)
  parameters_df$sigma_linear <- vapply(seq_len(nrow(parameters_df)),
                                       function(.x) switch(parameters_df$trafo[.x],
                                                           N = parameters_df$sigma[.x],
                                                           L = sqrt((exp(parameters_df$parametervalue[.x])*parameters_df$sigma[.x])^2),
                                                           G = sqrt((dinv_logit__(parameters_df$parametervalue[.x])*parameters_df$sigma[.x])^2),
                                                           `NA` = sqrt((exp(parameters_df$parametervalue[.x])*parameters_df$sigma[.x])^2)),
                                       FUN.VALUE = 1)
  parameters_df$RSE_linear <- abs(parameters_df$sigma_linear / parameters_df$parametervalue_linear) * 100
  parameters_df$description_name <- vapply(seq_len(nrow(parameters_df)),
                                           function(.x) switch(parameters_df$trafo[.x],
                                                               N = parameters_df$parametername[.x],
                                                               L = paste0("log(",parameters_df$parametername[.x], ")"),
                                                               G = paste0("logit(",parameters_df$parametername[.x], ")"),
                                                               `NA` = parameters_df$parametername[.x]),
                                           FUN.VALUE = character(1L))
  printname <- function(parametertask, belongs_to, covariate_levels, parametername) {
    out__ <- paste0(parametertask, "(", belongs_to, ")")
    is_beta__ <- !is.na(covariate_levels)
    betas__ <- parametername[is_beta__]
    to_which <- unique(belongs_to[is_beta__])
    betas__ <- gsub(paste0("(beta_(",paste0(to_which, collapse = "|"), ")).*"), "\\1", betas__)
    betas__ <- paste0(betas__, "(", covariate_levels[is_beta__], ")")
    out__[is_beta__] <- betas__
    return(out__)
  }
  parameters_df$printname <- printname(parameters_df$parametertask, parameters_df$belongs_to, parameters_df$covariate_levels, parameters_df$parametername)
  parameters_df$printname[parameters_df$parametertask %in% c("pop", "error")] <- parameters_df$parametername[parameters_df$parametertask %in% c("pop", "error")]
  parameters_df$distribution_info <- vapply(parameters_df$trafo, function(i) {switch(i, N = "(psi)", L = "log(psi)", G = "logit(psi)")}, FUN.VALUE = "1")
  pops__ <- parameters_df[parameters_df$parametertask == "pop", "parametername", drop = TRUE]
  omegas__ <- parameters_df[parameters_df$parametertask == "omega", "belongs_to", drop = TRUE]
  noIIV__ <- setdiff(pops__, omegas__)
  est <- readRDS(file.path("project.est"))
  data <- IQRloadCSVdata(file.path(est$data$relPathFromProject, basename(est$data$datafile)))
  IDs__ <- unique(data$ID)
  df_eta__ <- df_omega__ <- NULL
  if (length(noIIV__) > 0) {
    df_eta__ <- data.frame(parametername = paste0("ETA_", outer(noIIV__, IDs__, paste, sep = "_")),
                           parametervalue = 0,
                           sigma = NA,
                           parametertask = "ETA",
                           estimated = 0,
                           RSE = NA,
                           belongs_to = rep(noIIV__, length(IDs__)),
                           covariate_levels = NA,
                           trafo = "x",
                           parametervalue_linear = 0,
                           sigma_linear = NA,
                           RSE_linear = NA,
                           description_name = paste0("ETA_", outer(noIIV__, IDs__, paste, sep = "_")),
                           printname = paste0("ETA(", noIIV__, ")"),
                           distribution_info = "x",
                           stringsAsFactors = FALSE
    )
    df_omega__ <- data.frame(parametername = paste0("omega_", noIIV__),
                             parametervalue = 0,
                             sigma = NA,
                             parametertask = "omega",
                             estimated = 0,
                             RSE = NA,
                             belongs_to = noIIV__,
                             covariate_levels = NA,
                             trafo = "x",
                             parametervalue_linear = 0,
                             sigma_linear = NA,
                             RSE_linear = NA,
                             description_name = paste0("omega_", noIIV__),
                             printname = paste0("omega(", noIIV__, ")"),
                             distribution_info = "x",
                             stringsAsFactors = FALSE
    )
  }
  parameters_df <- rbind(parameters_df, df_eta__, df_omega__)
  parameters_df <- parameters_df[order(parameters_df$parametername),]
  return(parameters_df)
}
dMod_rawParameterInfo <- function(parameters_df, parsOfInterest) {
  list(
    fixedEffects = with(parameters_df, {
      mynames__ <- intersect(parsOfInterest, parametername)
      index__ <- match(mynames__, parametername)
      out <- list()
      out[["names"]] <- mynames__
      out[["trans"]] <- rep("", length(mynames__))
      out[["invtrans"]] <- rep("", length(mynames__))
      out[["estimated"]] <- estimated[index__]
      out[["values"]] <- parametervalue_linear[index__]
      out[["stderr"]] <- sigma_linear[index__]
      out[["rse"]] <- 100*RSE_linear[index__]
      out[["distribution_info"]] <- as.character(c("L" = "log(psi)", "N" = "(psi)", "G" = "logit(psi)")[trafo[index__]])
      out
    }),
    fixedEffects_transformed = with(parameters_df, {
      mynames__ <- intersect(parsOfInterest, parametername)
      index__ <- match(mynames__, parametername)
      out <- list()
      out[["names"]] <- mynames__
      out[["trans"]] <- as.character(c("L" = "exp(phi)", "N" = "(phi)", "G" = "exp(phi)/(1+exp(phi))")[trafo[index__]])
      out[["invtrans"]] <- as.character(c("L" = "log(psi)", "N" = "(psi)", "G" = "logit(psi)")[trafo[index__]])
      out[["estimated"]] <- estimated[index__]
      out[["values"]] <- parametervalue[index__]
      out[["stderr"]] <- sigma[index__]
      out[["rse"]] <- 100*sigma[index__]/abs(parametervalue[index__])
      out[["distribution_info"]] <- as.character(c("L" = "log(psi)", "N" = "(psi)", "G" = "logit(psi)")[trafo[index__]])
      out
    }),
    randomEffects = with(parameters_df, {
      out <- list()
      out[["names"]] <- paste0("omega(", intersect(parsOfInterest, parametername), ")")
      out[["estimated"]] <- rep(0, length(out[["names"]]))
      out[["values"]] <- rep(0, length(out[["names"]]))
      out[["stderr"]] <- rep(NA, length(out[["names"]]))
      out[["rse"]] <- rep(NA, length(out[["names"]]))
      if (any("omega" %in% parametertask)) {
        index__ <- which(parametertask == "omega")
        out[["estimated"]][match(printname[index__], out[["names"]])] <- c(2, 1)[estimated[index__]+1]
        out[["values"]][match(printname[index__], out[["names"]])] <- parametervalue_linear[index__]
        out[["stderr"]][match(printname[index__], out[["names"]])] <- sigma_linear[index__]
        out[["rse"]] <- 100*out[["stderr"]]/abs(out[["values"]])
      }
      out
    }),
    correlation = list(
      names = character(0),
      stderr = logical(0),
      rse = logical(0)
    ),
    covariate = with(parameters_df, {
      index__ <- which(parametertask == "beta")
      out <- list()
      out[["names"]] <- printname[index__]
      out[["estimated"]] <- estimated[index__]
      out[["values"]] <- parametervalue_linear[index__]
      out[["stderr"]] <- sigma_linear[index__]
      out[["rse"]] <- 100*RSE_linear[index__]
      out
    }),
    errorParameter = with(parameters_df, {
      index__ <- which(parametertask == "error")
      out <- list()
      out[["names"]] <- printname[index__]
      out[["estimated"]] <- estimated[index__]
      out[["values"]] <- parametervalue_linear[index__]
      out[["stderr"]] <- sigma_linear[index__]
      out[["rse"]] <- 100*RSE_linear[index__]
      out
    })
  )
}
standardOutput_IQRsysModel <- function(mymodel,
                                       SIGDIG = 3, 
                                       FLAGreport = FALSE,
                                       projectPath = "."
) {
  pathInformation__ <- projectPath
  setwd(attr(projectPath, "absProjectPath"))
  projectPath <- "."
  parameters_df <- mymodel[["IQRpartable"]][[1]]
  parsOfInterest__ <- attr(mymodel, "sysModelEst")[["parsOfInterest"]]
  parsLOC__ <- parameters_df[["parametername"]][!is.na(parameters_df[["CONDITION"]]) & !parameters_df[["CONDITION"]] %in% "BASE"]
  if (length(parsOfInterest__) == 0)
    parsOfInterest__ <- unique(parameters_df[["parametername"]][parameters_df[["parametertask"]] == "pop"])
  else
    parsOfInterest__ <- intersect(parsOfInterest__, parameters_df[["parametername"]])
  parsOfInterestNoLOC__ <- setdiff(parsOfInterest__, parsLOC__)
  pred_df__ <- mymodel[["IQRpredtable"]][[1]]
  rawParameterInfo <- dMod_rawParameterInfo(parameters_df, parsOfInterest__)
  est <- readRDS(file.path(projectPath, "project.est"))
  data <- IQRloadCSVdata(file.path(projectPath, est$data$relPathFromProject, basename(est$data$datafile)))
  covariate_df__ <- data[c("ID", est$data$covNames, est$data$catNames, "USUBJID")]
  covariate_df__ <- do.call(rbind, lapply(split(covariate_df__, covariate_df__[["USUBJID"]]), function(d) d[1,]))
  atrcontents__ <- list()
  atrcontents__$paramNames <- parsOfInterest__
  atrcontents__$localNames <- intersect(parsLOC__, parsOfInterest__)
  atrcontents__$covariateNames <- c(est$data$covNames, est$data$catNames)
  atrcontents__$covNames <- est$data$covNames
  atrcontents__$catNames <- est$data$catNames
  atrcontents__$ALT_NAMES_USED <- c("XPRED", "XWRES", "IPRED", "IWRES")
  atrcontents__$ALT_NAMES_ORIG <- c("XPRED", "XWRES", "IPRED", "IWRES")
  IDs__ <- unique(data[["ID"]])
  ETAs__ <- c(list(IDs__), lapply(parsOfInterest__, function(x__) rep(0, length(IDs__))))
  names(ETAs__) <- c("ID", parsOfInterest__)
  ETAs__ <- as.data.frame(ETAs__)
  for (par__ in parsOfInterest__) {
    for (id__ in IDs__) {
      candidate__ <- paste("ETA", par__, id__, sep = "_")
      if (candidate__ %in% parameters_df[["parametername"]]) {
        ETAs__[match(id__, IDs__), par__] <- parameters_df[["parametervalue"]][match(candidate__, parameters_df[["parametername"]])]
      }
    }
  }
  names(ETAs__) <- c("ID", paste("ETA", parsOfInterest__, sep = "_"))
  ETAs__ <- merge(ETAs__, covariate_df__, by = "ID")
  attributes(ETAs__) <- c(attributes(ETAs__), atrcontents__)
  IQRsaveCSVdata(ETAs__, file.path(projectPath, "RESULTS/project_eta.csv"))
  myenv__ <- environment(mymodel$p[[1]])
  assign("prd_conditions", myenv__$p_conditions, pos = myenv__)
  pars_indiv__ <- with(unlist(mymodel,FALSE), {p(bestfit)})
  pars_indiv__ <- do.call(rbind, mapply(
    function(pars_i, nmpars_i) {as.data.frame(t(c(ID = nmpars_i, unclass(pars_i))))},
    pars_i = pars_indiv__, nmpars_i = as.numeric(names(pars_indiv__)), SIMPLIFY = FALSE))
  if (length(parsOfInterestNoLOC__) == 0) stopIQR("Check if you have defined POPvalues0 in the IQRsysModel call.\n")
  pars_indiv__ <- merge(pars_indiv__[, c("ID", parsOfInterestNoLOC__)], covariate_df__, by = "ID")
  attributes(pars_indiv__) <- c(attributes(pars_indiv__), atrcontents__)
  IQRsaveCSVdata(pars_indiv__, file.path(projectPath, "RESULTS/project_indiv.csv"))
  data_pred_df__ <- data[!is.na(data$YTYPE),
                         c("ID", est$data$covNames, est$data$catNames, "USUBJID",
                           "TIME", "TIMEPOS", "TAD", "YTYPE","MDV", "EVID", "CENS", "DV")]
  pred_pred_df__ <- dplyr::transmute(pred_df__,
                                     ID = ID,
                                     TIME = time,
                                     YTYPE = gsub("^OUTPUT", "", name),
                                     DV = value,
                                     IPRED = pred_indiv,
                                     IRES = res_indiv,
                                     IWRES = wres_indiv,
                                     XPRED = pred_pop,
                                     XRES = res_pop,
                                     XWRES = wres_pop,
                                     NPDE = NA)
  pred_pred_df__ <- merge(data_pred_df__, pred_pred_df__, by = c("ID", "TIME", "YTYPE", "DV"))
  pred_pred_df__ <- dplyr::rename(pred_pred_df__, TIME2 = TIMEPOS)
  attributes(pred_pred_df__) <- c(attributes(pred_pred_df__), atrcontents__)
  IQRsaveCSVdata(pred_pred_df__, file.path(projectPath, "RESULTS/project_pred.csv"))
  h__ <- parseSYSprojectHeader(projectPath)
  RESULT__ <- list()
  RESULT__$COMMENT                         <- h__$COMMENT
  RESULT__$TOOL                            <- "SYSFIT"
  RESULT__$model                           <- unclass(projectPath)
  RESULT__$OBJ                             <- mymodel[["metrics"]][[1]][["value"]][1]
  RESULT__$BIC                             <- mymodel[["metrics"]][[1]][["value"]][2]
  RESULT__$AIC                             <- mymodel[["metrics"]][[1]][["value"]][3]
  RESULT__$parameternames                  <- parameters_df$parametername
  RESULT__$parametervalues                 <- parameters_df$parametervalue
  RESULT__$stderrors                       <- parameters_df$sigma
  RESULT__$correlationmatrixRandomEffects  <- NULL 
  RESULT__$rawParameterInfo                <- rawParameterInfo
  RESULT__$projectHeader                   <- h__
  ATRinfo__ <- loadATRinfo_csvData(h__$DATA)
  parameters_noETAs_df__ <- dplyr::filter(parameters_df, parametertask != "ETA")
  vcov_noETAs__ <- mymodel$vcov[[1]]
  vcov_noETAs__ <- vcov_noETAs__[!grepl("ETA_",rownames(vcov_noETAs__)),!grepl("ETA_",colnames(vcov_noETAs__)), drop = FALSE]
  corr_noETAs__ <- vcov_noETAs__ / sqrt(abs(outer(diag(vcov_noETAs__), diag(vcov_noETAs__))))
  project_results__ <- list()
  project_results__$type <- RESULT__$TOOL
  project_results__$method <- "mstrust"
  project_results__$termination_info <- ""
  project_results__$path <- pathInformation__
  project_results__$parameters <- with(parameters_noETAs_df__, {
    index <- c(
      match(parsOfInterest__, parametername),
      which(parametertask == "beta"),
      which(parametertask == "error")
    )
    myparnames <- c(printname[index], paste0("omega(", parsOfInterest__,")"))
    myparflagestimated <- c(estimated[index], rep(0, length(parsOfInterest__)))
    myparvalues <- c(parametervalue[index], rep(0, length(parsOfInterest__)))
    mypartransformation <- as.character(c(
      c("N" = "(phi)", "L" = "exp(phi)", "G" = "exp(phi)/(1+exp(phi))")[trafo[index]],
      rep("", length(parsOfInterest__))
    ))
    mypartransformation[!myparnames %in% parsOfInterest__] <- "" 
    mystandarderrors <- c(sigma[index], rep(0, length(parsOfInterest__)))
    if (any(parametertask == "omega")) {
      omegaindex <- match(printname[parametertask == "omega"], myparnames)
      myparflagestimated[omegaindex] <- c(2, 1)[estimated[parametertask == "omega"]+1]
      myparvalues[omegaindex] <- parametervalue_linear[parametertask == "omega"]
      mystandarderrors[omegaindex] <- sigma[parametertask == "omega"]
    }
    myvcov <- mycor <- matrix(0, nrow = length(myparnames),
                              ncol = length(myparnames),
                              dimnames = list(myparnames, myparnames))
    for (n__ in rownames(vcov_noETAs__)) {
      vec__ <- vcov_noETAs__[n__, ]
      myvcov[printname[match(n__, parametername)],
             printname[match(rownames(vcov_noETAs__), parametername)]] <- vec__
      vec__ <- corr_noETAs__[n__, ]
      mycor[printname[match(n__, parametername)],
            printname[match(rownames(corr_noETAs__), parametername)]] <- vec__
    }
    mystandarderrors[is.na(mystandarderrors)] <- 0
    myvcov[is.na(myvcov)] <- 0
    mycor[is.na(mycor)] <- 0
    diag(mycor) <- 1
    list(
      note = "Parameters are given on the scale at which they are estimated, they are not transformed to linear scale",
      names = myparnames,
      FLAGestimated = myparflagestimated,
      transformation = mypartransformation,
      values = myparvalues,
      stderrors = mystandarderrors,
      correlationmatrix = mycor,
      covariancematrix = myvcov
    )
  })
  project_results__$objectivefunction <- stats::setNames(as.list(mymodel[["metrics"]][[1]][["value"]]),
                                                         mymodel[["metrics"]][[1]][["metric"]])
  project_results__$residualerrormodels <- as.character(do.call(c,lapply(est$modelSpec$errorModel, "[[", 1)))
  project_results__$inv_trans_randeffects <- as.character(
    c("L" = "log(psi)",
      "N" = "(psi)",
      "G" = "logit(psi)")[parameters_df$trafo[match(parsOfInterest__, parameters_df$parametername)]])
  project_results__$trans_randeffects <- as.character(
    c("L" = "exp(phi)",
      "N" = "(phi)",
      "G" = "exp(phi)/(1+exp(phi))")[parameters_df$trafo[match(parsOfInterest__, parameters_df$parametername)]])
  covNames                   <- RESULT__$projectHeader$COVNAMES
  covTransformation__          <- RESULT__$projectHeader$BETACOVTRANS
  catNames                   <- RESULT__$projectHeader$CATNAMES
  catReference__               <- RESULT__$projectHeader$BETACATREFERENCE
  catCategories__              <- RESULT__$projectHeader$BETACATREFERENCE
  project_results__$covariates <- list(
    covNames = covNames,
    covTransformation = covTransformation__,
    catNames = catNames,
    catReference = catReference__,
    catCategories = catCategories__
  )
  project_results__$PROJECTINFO <- RESULT__$projectHeader
  project_results__$rawParameterInfo <- rawParameterInfo
  IQRoutputFile(deparse(project_results__), file.path(projectPath, "RESULTS/project_results.x"))
}
dMod_approxHessian <- function(objfn, pars, h = 1e-4, diag = TRUE, ...) {
  if (!diag)
    stopIQR("Approximation of non-diagonal entries not implemented for sake of speed")
  nm__ <- names(pars)
  ln__ <- length(pars)
  fn__ <- function(x__) {
    args <- list(...)
    args[["pars"]] <- x__
    args[["deriv"]] <- FALSE
    do.call(objfn, args)[["value"]]
  }
  if (diag) {
    values__ <- lapply(seq_len(ln__), function(i__) {
      hvec__ <- rep(0, ln__)
      hvec__[i__] <- h
      (fn__(pars+hvec__) - 2*fn__(pars) + fn__(pars-hvec__))/h^2
    })
    values__ <- unlist(values__)
    hessian__ <- diag(values__)
  }
  if (!diag) {
    cat("Computing Hessian Matrix")
    hessian__ <- numDeriv::hessian(fn__, x = pars)
    hessian__ <- nearPD(hessian__)
  }
  rownames(hessian__) <- nm__
  colnames(hessian__) <- nm__
  return(hessian__)
}
convert2dMod_IQRdata <- function(datafile = NULL,
                                 catNames = NULL,
                                 covNames = NULL,
                                 regressorNames = NULL,
                                 keep = "CONDITION",
                                 split.by = "ID",
                                 inputs = NULL) {
  mydata0__ <- IQRsysData(datafile, covNames, catNames, regressorNames)
  keep__ <- unique(c(catNames, covNames,  keep))
  keep__ <- intersect(keep__, names(mydata0__))
  mydata__ <- mydata0__
  forcings__ <- do.call(rbind, lapply(regressorNames, function(i) {
    data.frame(name = i,
               time = mydata__[["TIME"]],
               value = mydata__[[i]],
               ID = mydata__[["ID"]],
               stringsAsFactors = FALSE) 
  }))
  determine_constant_forcings__ <- function(forcings) {
    n_unique_values__ <- by(forcings, list(forcings$ID, forcings$name), function(i) {
      nforc__ <- length(unique(i$value))
    }, simplify = TRUE)
    apply(n_unique_values__, 2, function(j) {all(j == 1)})
  }
  is_constant_forcing__ <- determine_constant_forcings__(forcings__)
  constant_forcings__ <- constant_forcnames__ <- NULL
  if (any(is_constant_forcing__)) {
    constant_forcnames__ <- names(is_constant_forcing__)[is_constant_forcing__]
    constant_forcings__ <- unique(forcings__[forcings__$name %in% constant_forcnames__, names(forcings__) != "time"])
  }
  time_varying_forcings__ <- time_varying_forcnames__ <- NULL
  if (any(!is_constant_forcing__)) {
    time_varying_forcnames__ <- names(is_constant_forcing__)[!is_constant_forcing__]
    time_varying_forcings__ <- forcings__[forcings__$name %in% time_varying_forcnames__, ]
  }
  keep__ <- union(keep__, constant_forcnames__)
  mydata__ <- mydata0__
  if (nrow(mydata__) > 0) {
    if (is.null(mydata__$LLOQ))
      mydata__$LLOQ <- -Inf
    mydata__$LLOQ[is.na(mydata__$LLOQ)] <- -Inf
    mydata__$LLOQ[mydata__$CENS == 1] <- mydata__$DV[mydata__$CENS == 1]
    if (!is.null(mydata__$MDV))
      mydata__ <- mydata__[mydata__$MDV != 1 | (mydata__$MDV == 1 & (is.na(mydata__$YTYPE) | mydata__$YTYPE %in% 0)),]
    data <- with(mydata__, {
      YTYPE[is.na(YTYPE)] <- 0
      data.frame(
        name = paste0("OUTPUT", YTYPE)[seq_along(TIME)],
        time = TIME,
        value = DV,
        sigma = rep(NA, length(TIME)),
        lloq = LLOQ,
        ID = ID)
    })
    if (!is.null(keep__)) {
      data <- cbind(data, mydata__[, keep__, drop = FALSE])
    }
    data <- as.datalist(data, split.by = split.by, keep.covariates = keep__)
    condition.grid <- attr(data, "condition.grid")
    data <- as.datalist(lapply(data, function(d) d[d[["name"]] != "OUTPUT0",]),
                        names = names(data),
                        condition.grid = attr(data, "condition.grid"))
  }
  dosing_pars__ <- trafo__ <- max_scheme__ <- NULL
  mydosing__ <- mydata0__[is.na(mydata0__[["YTYPE"]]) | mydata0__[["YTYPE"]] %in% 0, unique(c("TIME", "ID", "AMT", "ADM", "TINF", keep__))]
  if (nrow(mydosing__) > 0) {
    dosing_pars__ <- dMod_build_dosing_parameters(mydata0__, inputs = inputs)
    max_scheme__ <- structure(
      attr(dosing_pars__, "max_nDoses_per_ADM")[["N"]],
      names = paste0("INPUT", attr(dosing_pars__, "max_nDoses_per_ADM")[["ADM"]])
    )
    trafo__ <- dMod_build_dosing_trafo(max_scheme__)
    dosing_pars__ <- dosing_pars__[, setdiff(names(dosing_pars__), "CONDITION")]
    all_IDs__ <- names(data)
    no_dosing_IDs__ <- setdiff(all_IDs__, as.character(dosing_pars__$ID))
    dosing_pars__ <- dosing_pars__[match(all_IDs__, dosing_pars__$ID),] 
  }
  output__ <- data
  attr(output__, "constant_regressors") <- constant_forcings__
  attr(output__, "time_varying_regressors") <- time_varying_forcings__
  attr(output__, "dosing_pars") <- dosing_pars__
  attr(output__, "dosing_trafo") <- trafo__
  attr(output__, "max_scheme") <- max_scheme__
  attr(output__, "catNames") <- catNames
  attr(output__, "covNames") <- covNames
  attr(output__, "regressorNames") <- regressorNames
  attr(output__, "keep") <- keep__
  return(output__)
}
combine_model_and_data <- function(basic_dMod_model__, my_dMod_data, myNlmeEst__,
                                   SIMOPT.nauxtimes, SIMOPT.auxtimes = NULL, SIMOPT.cores = 1,
                                   opt.method = "trust") {
  IDs__ <- stats::setNames(nm = names(my_dMod_data))
  parameters <- myNlmeEst__$modelSpec[c("POPvalues0", "POPestimate", "IIVdistribution", "IIVvalues0", "IIVestimate", "LOCmodel", "LOCvalues0", "LOCestimate")]
  IIVpars__ <- dMod_build_IIVpars(parameters = parameters, conditions = IDs__)
  forcings__ <- attr(my_dMod_data, "time_varying_regressors")
  x <- basic_dMod_model__$x
  if (!is.null(forcings__)) {
    x <- lapply(IDs__, function(C__) {
      mycontrols__ <- environment(environment(basic_dMod_model__$x)$mappings[[1]])$controls
      forcings_c__ <- forcings__[forcings__[["ID"]] == C__, ][, c("name", "time", "value")]
      forcings_c__ <- forcings_c__[order(forcings_c__[["time"]]),]
      forcings_c__ <- unique(forcings_c__)
      Xs(odemodel = basic_dMod_model__$model,
         forcings = forcings_c__,
         condition = C__,
         optionsOde = mycontrols__[["optionsOde"]],
         optionsSens = mycontrols__[["optionsSens"]],
         fcontrol = mycontrols__[["fcontrol"]]
      )
    })
    x <- Reduce("+", x)
    modelname(x) <- modelname(basic_dMod_model__$x)
  }
  indiv_objects__ <- dMod_build_indiv_grids(myNlmeEst__, my_dMod_data, basic_dMod_model__)
  fixed.grid__    <- indiv_objects__$fixed.grid
  est.grid__      <- indiv_objects__$est.grid
  est.vec__       <- indiv_objects__$est.vec
  trafo__ <- dMod_build_nlme_trafo(myNlmeEst__, IDs__, basic_dMod_model__)
  p__ <- dMod_build_nlme_p(trafo__)
  prd__ <- basic_dMod_model__[["g"]]*x*p__
  p_indiv__   <- dMod_P_indiv(p = p__, fixed.grid = fixed.grid__, est.grid = est.grid__)
  prd_indiv__ <- dMod_PRD_indiv(prd = prd__, fixed.grid = fixed.grid__, est.grid = est.grid__)
  obj__ <- obj_data__ <- NULL
  if (!is_IQRsysModelEst(myNlmeEst__)){
    obj__ <- obj_data__ <- dMod_normL2_indiv(data         = my_dMod_data,
                                             prd          = prd__,
                                             errmodel     = basic_dMod_model__[["e"]],
                                             fixed.grid   = fixed.grid__,
                                             est.grid     = est.grid__,
                                             SIMOPT.nauxtimes = SIMOPT.nauxtimes,
                                             SIMOPT.cores = SIMOPT.cores,
                                             opt.method   = opt.method)
    nlme.fix__ <- nlme.est__ <- nlme__ <- NULL
    if (length(IIVpars__$omegapars.est) > 0) {
      nlme.est__ <- constraintL2(mu = IIVpars__$ETA.est, sigma = unlist(IIVpars__$omegapars.est, use.names = FALSE), attr.name = "data")
      nlme__ <- nlme.est__
    }
    if (length(IIVpars__$omegapars.fix) > 0) {
      nlme.fix__ <- constraintL2(mu = IIVpars__$ETA.fix, sigma = exp(as.numeric(IIVpars__$omega.fix[unlist(IIVpars__$omegapars.fix, use.names = FALSE)])), attr.name = "data")
      nlme__ <- nlme.fix__
    }
    if (length(IIVpars__$omega.est) > 0 & length(IIVpars__$omega.fix) > 0)
      nlme__ <- nlme.est__ + nlme.fix__
    if (!is.null(nlme__))
      obj__ <- obj_data__ + nlme__
  }
  timerange__ <- suppressWarnings(range(as.data.frame(my_dMod_data)[["time"]]))
  if (!is.finite(timerange__[1])) timerange__[1] <- 0
  if (!is.finite(timerange__[2])) timerange__[2] <- 20
  times__     <- seq(min(0, timerange__[1]), timerange__[2], length.out = 200)
  times__     <- sort(unique(c(times__, SIMOPT.auxtimes)))
  covariateModel__        <- myNlmeEst__$modelSpec$covariateModel
  COVestimate__           <- myNlmeEst__$modelSpec$COVestimate
  covariateModelValues__  <- myNlmeEst__$modelSpec$covariateModelValues
  covNames                <- myNlmeEst__$data$covNames
  covariateMedianValues__ <- myNlmeEst__$data$covariateMedianValues
  covariateMedianValues__[intersect(covNames, names(myNlmeEst__$modelSpec$COVcentering))] <-
    myNlmeEst__$modelSpec$COVcentering[intersect(covNames, names(myNlmeEst__$modelSpec$COVcentering))]
  catNames                <- myNlmeEst__$data$catNames
  covariateCATreference__ <- myNlmeEst__$data$covariateCATreference
  covariateCATreference__[intersect(catNames, names(myNlmeEst__$modelSpec$COVcentering))] <-
    myNlmeEst__$modelSpec$COVcentering[intersect(catNames, names(myNlmeEst__$modelSpec$COVcentering))]
  parameters <- c(parameters, list(
    covariateModel = covariateModel__,
    covariateModelValues = covariateModelValues__,
    COVestimate = COVestimate__,
    COVcentering = c(covariateMedianValues__, covariateCATreference__)
  ))
  out__ <- dMod.frame(hypothesis = date(),
                      g = basic_dMod_model__[["g"]],
                      x = x,
                      p0 = list(p__),
                      p = p_indiv__,
                      data = my_dMod_data,
                      e = basic_dMod_model__[["e"]],
                      model = list(basic_dMod_model__[["model"]]))
  out__ <- appendObj(out__,
                     prd = list(prd_indiv__),
                     obj_data = list(obj_data__),
                     obj = list(obj__),
                     parameters = list(parameters),
                     pars = list(est.vec__),
                     est.grid = list(est.grid__),
                     fixed.grid = list(fixed.grid__),
                     times = list(times__),
                     ETA = list(c(IIVpars__$ETApars.est, IIVpars__$ETApars.fix)),
                     omega = list(IIVpars__$omegapars.est))
  class(out__) <- c("IQRsysModel", class(out__))
  attr(out__, "sysModelEst") <- list(
    model = myNlmeEst__$model,
    dosing = stats::setNames(
      object = lapply(seq_along(myNlmeEst__$dosingInfo[["INPUTnames"]]), function(i__) {
        c(type = myNlmeEst__$dosingInfo[["types"]][[i__]])
      }),
      nm = myNlmeEst__$dosingInfo[["INPUTnames"]]
    ),
    data = stats::setNames(
      object = myNlmeEst__$data[c("datafile", "covNames", "catNames", "regressorNames")],
      nm = c("datafile", "covNames", "catNames", "regressorNames")
    ),
    estSpec = myNlmeEst__$modelSpec,
    modelSpec = myNlmeEst__$modelSpec
  )
  out__ <- partable_IQRsysModel(out__)
  return(out__)
}
#'@export
modelSpec_IQRest <- function (
  POPvalues0,
  POPestimate = NULL,
  IIVdistribution = NULL,
  IIVvalues0 = NULL,
  IIVestimate = NULL,
  IOVvalues0 = NULL,
  IOVestimate = NULL,
  errorModel = NULL,
  covarianceModel = NULL,
  covariateModel = NULL,
  covariateModelValues = NULL,
  COVestimate = NULL,
  COVcentering = NULL,
  PriorVarPOP = NULL,
  PriorVarCovariateModelValues = NULL,
  PriorDFerrorModel = NULL,
  PriorIIV = NULL,
  PriorDFIIV = NULL
) {
  modelSpec <- list()
  modelSpec$POPvalues0 <- POPvalues0
  modelSpec$POPestimate <- POPestimate
  modelSpec$IIVdistribution <- IIVdistribution
  modelSpec$IIVvalues0 <- IIVvalues0
  modelSpec$IIVestimate <- IIVestimate
  modelSpec$IOVvalues0 <- IOVvalues0
  modelSpec$IOVestimate <- IOVestimate
  modelSpec$errorModel <- errorModel
  modelSpec$covarianceModel <- covarianceModel
  modelSpec$covariateModel <- covariateModel
  modelSpec$covariateModelValues <- covariateModelValues
  modelSpec$COVestimate <- COVestimate
  modelSpec$COVcentering <- COVcentering
  modelSpec$PriorVarPOP <- PriorVarPOP
  modelSpec$PriorVarCovariateModelValues <- PriorVarCovariateModelValues
  modelSpec$PriorDFerrorModel <- PriorDFerrorModel
  modelSpec$PriorIIV <- PriorIIV
  modelSpec$PriorDFIIV <- PriorDFIIV
  return(modelSpec)
}
#'@export
data_IQRest <- function (
  datafile,
  covNames = NULL,
  catNames = NULL,
  regressorNames = NULL
) {
  data <- list()
  data$datafile <- datafile
  data$covNames <- covNames
  data$catNames <- catNames
  data$regressorNames <- regressorNames
  return(data)
}
#'@export
dosing_IQRest <- function (...) {
  dots <- list(...)
  return(dots)
}
#'@export
addCovariateToModelSpec_IQRest <- function(
  modelSpec,
  covariateModel,
  covariateModelValues = NULL,
  COVestimate = NULL,
  COVcentering = NULL) {
  message("This function is deprecated. Please use addCov_modelSpec() instead.")
  modelSpec__ <- modelSpec
  if (class(covariateModel) != "list") stopIQR("covariateModel needs to be a list.")
  if (!is.null(COVestimate)) {
    if (class(COVestimate) != "list") stopIQR("COVestimate needs to be a list.")
    if (!setequal(names(COVestimate), names(covariateModel))) stopIQR("If given COVestimate needs contain entries for the same model parameters as covariateModel.")
    for (k__ in names(COVestimate))
      if (!setequal(names(COVestimate[[k__]]), covariateModel[[k__]])) stopIQR("If given COVestimate needs contain same covariate-parameter relationship as covariateModel.")
  }
  if (!is.null(covariateModelValues)) {
    if (class(covariateModelValues) != "list") stopIQR("covariateModelValues needs to be a list.")
    if (!setequal(names(covariateModelValues), names(covariateModel))) stopIQR("If given covariateModelValues needs contain entries for the same model parameters as covariateModel.")
    for (k__ in names(covariateModelValues))
      if (!setequal(names(covariateModelValues[[k__]]), covariateModel[[k__]])) stopIQR("If given covariateModelValues needs contain same covariate-parameter relationship as covariateModel.")
  }
  if ("COVcentering" %in% names(modelSpec__)) {
    check__ <- names(COVcentering) %in% names(modelSpec__$COVcentering)
    if (any(check__)) {
      warningIQR("Reference value for ", names(COVcentering)[check__], "already exists. Value will not be changed")
      COVcentering <- COVcentering[!check__]
    }
  }
  if (is.null(modelSpec__$covariateModel)) modelSpec__$covariateModel <- list()
  if (is.null(modelSpec__$covariateModelValues) & !is.null(covariateModelValues)) {
    modelSpec__$covariateModelValues <- list()
    for (kpar__ in names(modelSpec__$covariateModel)) {
      modelSpec__$covariateModelValues[[kpar__]] <- structure(rep(0.1, length(modelSpec__$covariateModel[[kpar__]])), names = modelSpec__$covariateModel[[kpar__]])
    }
  }
  if (is.null(modelSpec__$COVestimate) & !is.null(COVestimate)) {
    modelSpec__$COVestimate <- list()
    for (kpar__ in names(modelSpec__$covariateModel)) {
      modelSpec__$COVestimate[[kpar__]] <- structure(rep(1, length(modelSpec__$covariateModel[[kpar__]])), names = modelSpec__$covariateModel[[kpar__]])
    }
  }
  if (!is.null(modelSpec__$covariateModelValues) & is.null(covariateModelValues)) {
    covariateModelValues <- list()
    for (kpar__ in names(covariateModel)) {
      covariateModelValues[[kpar__]] <- structure(rep(0.1, length(covariateModel[[kpar__]])), names = covariateModel[[kpar__]])
    }
  }
  if (!is.null(modelSpec__$COVestimate) & is.null(COVestimate)) {
    COVestimate <- list()
    for (kpar__ in names(covariateModel)) {
      COVestimate[[kpar__]] <- structure(rep(1, length(covariateModel[[kpar__]])), names = covariateModel[[kpar__]])
    }
  }
  for (kpar__ in names(covariateModel)) {
    if (!is.null(modelSpec__$covariateModel[[kpar__]])) {
      check__ <- covariateModel[[kpar__]] %in% modelSpec__$covariateModel[[kpar__]]
      if (any(check__)) {
        warningIQR("Covariate relationship for ", covariateModel[[kpar__]][check__], " on ", kpar__, " already exists.")
        covariateModel[[kpar__]] <- covariateModel[[kpar__]][!check__]
        if (!is.null(covariateModelValues))
          covariateModelValues[[kpar__]] <- covariateModelValues[[kpar__]][names(covariateModelValues[[kpar__]])]
        if (!is.null(COVestimate))
          COVestimate[[kpar__]] <- COVestimate[[kpar__]][names(covariateModel[[kpar__]])]
      }
      if (length(covariateModel[[kpar__]]) > 0) {
        modelSpec__$covariateModel[[kpar__]] <- c(modelSpec__$covariateModel[[kpar__]], covariateModel[[kpar__]])
        if (!is.null(covariateModelValues))
          modelSpec__$covariateModelValues[[kpar__]] <- c(modelSpec__$covariateModelValues[[kpar__]], covariateModelValues[[kpar__]])
        if (!is.null(COVestimate))
          modelSpec__$COVestimate[[kpar__]] <- c(modelSpec__$COVestimate[[kpar__]], COVestimate[[kpar__]])
      } else {
        covariateModel[[kpar__]] <- NULL
      }
    } else {
      modelSpec__$covariateModel[[kpar__]] <- covariateModel[[kpar__]]
      if (!is.null(covariateModelValues))
        modelSpec__$covariateModelValues[[kpar__]] <- covariateModelValues[[kpar__]]
      if (!is.null(COVestimate))
        modelSpec__$COVestimate[[kpar__]] <- COVestimate[[kpar__]]
    }
  }
  if (!(is.null(COVcentering) | length(COVcentering) == 0)) {
    if (is.null(modelSpec__$COVcentering)) {
      modelSpec__$COVcentering <- COVcentering
    } else {
      modelSpec__$COVcentering <- c(modelSpec__$COVcentering, COVcentering)
    }
  }
  if (length(covariateModel) > 0) {
    messtext__ <- "\nThe following covariate relationships were added to the model specification:\n"
    for (k__ in seq_along(covariateModel)) {
      messtext__ <- paste0(messtext__,paste0(" - ", covariateModel[[k__]], " on ", names(covariateModel)[[k__]], "\n"))
    }
  } else {
    messtext__ <- "\nAll covariate relationships already in model specification.\n"
  }
  message(messtext__)
  modelSpec__
}
#'@export
addPar_modelSpec <- function(modelSpec, parname, value, estimate = NULL, distribution = NULL, IIVvalue = NULL, IIVestimate = NULL) {
  modelSpec[["POPvalues0"]][parname] <- value
  if (!is.null(estimate)) modelSpec[["POPestimate"]][parname] <- estimate
  if (!is.null(distribution)) modelSpec[["IIVdistribution"]][parname] <- distribution
  if (!is.null(IIVvalue)) modelSpec[["IIVvalues0"]][parname] <- IIVvalue
  if (!is.null(IIVestimate)) modelSpec[["IIVestimate"]][parname] <- IIVestimate
  return(modelSpec)
}
#'@export
addCov_modelSpec <- function(modelSpec, parname, covname, value = NULL, estimate = NULL, center = NULL) {
  if (is.null(modelSpec[["covariateModel"]])) {
    modelSpec[["covariateModel"]] <- structure(list(covname), names = parname)
  } else {
    modelSpec[["covariateModel"]][[parname]] <- union(modelSpec[["covariateModel"]][[parname]], covname)
  }
  if (!is.null(value)) {
    if (is.null(modelSpec[["covariateModelValues"]])) {
      modelSpec[["covariateModelValues"]] <- structure(list(structure(value, names = covname)), names = parname)
    } else {
      modelSpec[["covariateModelValues"]][[parname]][covname] <- value
    }
  }
  if (!is.null(estimate)) {
    if (is.null(modelSpec[["COVestimate"]])) {
      modelSpec[["COVestimate"]] <- structure(list(structure(estimate, names = covname)), names = parname)
    } else {
      modelSpec[["COVestimate"]][[parname]][covname] <- estimate
    }
  }
  if (!is.null(center)) {
    modelSpec[["COVcentering"]][[covname]] <- center
  }
  return(modelSpec)
}
#'@export
removePar_modelSpec <- function(modelSpec, parname) {
  fields__ <- c("POPvalues0", "POPestimate", "IIVdistribution",
                "IIVvalues0", "IIVestimate", "covariateModel",
                "covariateModelValues", "COVestimate", "COVcentering")
  for (n__ in fields__) {
    index__ <- which(names(modelSpec[[n__]]) %in% parname)
    if (length(index__) > 0 & length(modelSpec[[n__]]) > 0)
      modelSpec[[n__]] <- modelSpec[[n__]][-index__]
  }
  return(modelSpec)
}
#'@export
removeCov_modelSpec <- function(modelSpec, parname, covname) {
  newCovModelPar <- setdiff(modelSpec$covariateModel[[parname]], covname)
  if (length(newCovModelPar) == 0) newCovModelPar <- NULL
  modelSpec$covariateModel[[parname]] <- newCovModelPar
  fields__ <- c("covariateModelValues", "COVestimate")
  for (n__ in fields__) {
    newentry__ <- modelSpec[[n__]][[parname]][setdiff(names(modelSpec[[n__]][[parname]]), covname)]
    if (length(newentry__) == 0) newentry__ <- NULL
    modelSpec[[n__]][[parname]] <- newentry__
  }
  all_covariates__ <- Reduce(union, modelSpec[["covariateModel"]])
  if (length(all_covariates__) > 0)
    modelSpec[["COVcentering"]] <- modelSpec[["COVcentering"]][intersect(names(modelSpec[["COVcentering"]]), all_covariates__)]
  else
    modelSpec[["COVcentering"]] <- NULL
  return(modelSpec)
}
#'@export
print_modelSpec <- function(modelSpec, pop = TRUE, cov = TRUE, error = TRUE, prior = FALSE, loc = FALSE) {
  if (prior) warningIQR("Printing of prior information not yet supported.")
  if (loc)   warningIQR("Printing of local parameter information not yet supported.")
  fields.par <- c("POPvalues0", "POPestimate", "IIVdistribution", "IIVvalues0", "IIVestimate")
  fields.cov <- c("covariateModel", "covariateModelValues", "COVestimate")
  if (pop) {
    parameters <- Reduce(union, lapply(fields.par, function(n) names(modelSpec[[n]])))
    table <- lapply(fields.par, function(myfield) {
      structure(tibble(x = parameters, y = "."), names = c("Parameter", myfield))
    })
    table <- Reduce(function(x, y) left_join(x, y, by = "Parameter"), table)
    for (f in intersect(fields.par, names(modelSpec))) {
      values <- modelSpec[[f]][table$Parameter]
      if (is.numeric(values)) values <- signif(values, digits = 3)
      values <- as.character(values)
      values[is.na(values)] <- "."
      if (grepl("estimate", f)) {
        values[values == "0"] <- ""
        values[values == "1"] <- "+"
        values[values == "2"] <- "/"
      }
      table[[f]] <- values
    }
    cat("Parameter set-up:\n\n")
    print(as_IQRtable(table))
    cat("\n\n")
  }
  if (cov) {
    parameters <- Reduce(union, lapply(fields.cov, function(n) names(modelSpec[[n]])))
    covariates <- Reduce(union, lapply(modelSpec$covariateModel, identity))
    if (length(covariates) > 0) {
      table <- lapply(covariates, function(myfield) {
        structure(tibble(x = parameters, y = "."), names = c("Parameter", myfield))
      })
      table <- Reduce(function(x, y) left_join(x, y, by = "Parameter"), table)
      for (f in covariates) {
        if (is.null(modelSpec$covariateModelValues)) {
          values <- structure(rep(".", length(parameters)), names = parameters)
        } else {
          values <- modelSpec$covariateModelValues[intersect(names(modelSpec$covariateModelValues), parameters)]
          values <- structure(sapply(values, function(v) v[f]), names = names(values))
          values <- signif(values, 3)
          values <- sapply(values, as.character)
          values[is.na(values)] <- "."
        }
        if (is.null(modelSpec$COVestimate)) {
          estimate <- structure(rep("", length(parameters)), names = parameters)
        } else {
          estimate <- modelSpec$COVestimate[intersect(names(modelSpec$COVestimate), parameters)]
          estimate <- structure(sapply(estimate, function(v) v[f]), names = names(estimate))
          estimate <- sapply(estimate, as.character)
          estimate[is.na(estimate)] <- "."
          estimate[estimate == "."] <- ""
          estimate[estimate == "1"] <- "(+)"
          estimate[estimate == "0"] <- "(-)"
        }
        valest <- rep(".", length(parameters))
        valest[match(names(values), parameters)] <- values
        valest[match(names(estimate), parameters)] <- paste(valest[match(names(estimate), parameters)], estimate)
        table[[f]] <- valest
        centering <- modelSpec$COVcentering[[f]]
        if (!is.null(centering)) names(table)[match(f, names(table))] <- paste0(f, " (", centering, ")")
      }
      cat("Covariate set-up:\n\n")
      print(as_IQRtable(table))
      cat("\n\n")
    }
  }
  if (error) {
    outputs <- modelSpec$errorModel
    if (length(outputs) > 0) {
      table <- matrix(".", nrow = length(outputs)+1, ncol = 3)
      table[1,] <- c("Output", "abs", "rel")
      for (i in seq_along(outputs)) {
        table[i + 1, 1] <- names(outputs)[i]
        values <- dplyr::case_when(outputs[[i]][1] == "abs" ~ c(outputs[[i]][2], ""),
                                   outputs[[i]][1] == "rel" ~ c("", outputs[[i]][2]),
                                   outputs[[i]][1] == "absrel" ~ outputs[[i]][2:3])
        values[is.na(values)] <- ""
        table[i + 1, 2:3] <- values
      }
      cat("Error model set-up:\n\n")
      print(as_IQRtable(table))
      cat("\n\n")
    }
  }
  invisible()
}
#'@export
create_IQReventTable <- function(dosing, indivData, regression = NULL, abs0inputs=NULL, abs0Tk0param=NULL) {
  if (!is.data.frame(indivData)) stopIQR("indivData argument is not a data.frame")
  eData__ <- create_IQReventData(dosing, indivData)
  if (is.null(regression)) regressNames__ <- setdiff(names(indivData), c("ID", "USUBJID")) else regressNames__ <- regression
  eTable__ <- IQReventTable(
    eData__,
    regression = regressNames__,
    abs0inputs=abs0inputs, abs0Tk0param=abs0Tk0param
  )
  eTable__
}
#'@export
create_IQReventData <- function(dosing, indivData) {
  if (!(is_IQRdosing(dosing))) {
    if ("list" %in% class(dosing)) {
      if (!all(sapply(dosing, is_IQRdosing))) stopIQR("List of dosings does not exclusively contain IQRdosing objects.")
    } else {
      stopIQR("'dosing'is not an IQRdosing object.")
    }
  }
  if (!("ID" %in% names(indivData))) stopIQR("'indivData' needs to contain ID column.")
  indivData$ID <- as.character(indivData$ID)
  IDs <- unique(indivData$ID)
  if ("list" %in% class(dosing)) {
    if (is.null(names(dosing))) {
      if (length(dosing) != length(unique(indivData$ID)))
        stopIQR("Number of dosings in list does not match number of individuals in indivData.")
      warningIQR("List of dosings unnamed. Individuals in indivData are match by order of occurence.")
      names(dosing) <- unique(indivData$ID)
    } else {
      if (!(all(names(dosing) %in% as.character(unique(indivData$ID))) &
            all(as.character(unique(indivData$ID)) %in% names(dosing))))
        stopIQR("Names of dosing list is not equal to IDs in indivData.")
    }
  } else {
    dosing <- plyr::alply(IDs, 1, function(x) dosing)
    names(dosing) <- IDs
  }
  eventData <- plyr::adply(IDs, 1, function(x) {
    dosi__ <- dosing[[x]]
    dati__ <- dplyr::filter(indivData, ID == x)
    dosi__$ID <- x
    bycol__ <- intersect(names(dosi__), names(dati__))
    out__ <- dplyr::full_join(dosi__, dati__, by = bycol__)
    out__ <- dplyr::arrange(out__,TIME)
  }, .id = "ID")
  eventData$ID <- as.numeric(eventData$ID)
  eventData
}
#'@export
extract_IQRdosing <- function(data, FLAGunique = FALSE, TIMEcol = "TIME") {
  data__ <- data
  if (!(TIMEcol %in% names(data__))) stopIQR("Time column not available in data. Check also 'TIMEcol' input.")
  data__$TIME <- data__[[TIMEcol]]
  reqCol__ <- c("ID", "TIME", "EVID", "ADM", "AMT")
  if (!all(reqCol__ %in% names(data__)))  {
    miss__ <- setdiff(reqCol__, names(data__))
    stopIQR(paste0(paste0(miss__, collapse = ", ")," missing in input dataset."))
  }
  data__ <- dplyr::filter(data__, EVID == 1)
  if ("ADDL" %in% names(data__)) {
    data__$ADDL[is.na(data__$ADDL)] <- 0
    flagADDL__ <- data__$ADDL > 0
    idxADDL__ <- which(flagADDL__)
    if (length(idxADDL__) > 0) {
      if (!("II" %in% names(data__))) stopIQR("For some dosing records ADDL is positive, but II column is missing.")
      data__$II[is.na(data__$II)] <- 0
      idxII__ <- which(data__$II > 0)
      if (!all(idxADDL__ %in% idxII__)) stopIQR("For some dosing records ADDL is positive, but II entry not.")
      dataADDL__ <- data__[idxADDL__,]
      dataExp__ <- plyr::adply(dataADDL__, 1, function(x__) {
        out__ <- x__[rep(1,x__$ADDL+1),]
        out__$TIME <- seq(x__$TIME, length.out = x__$ADDL+1, by = x__$II)
        out__$ADDL <- 0
        out__
      })
      data__ <- rbind(data__[!flagADDL__,], dataExp__)
    }
  }
  if ("RATE" %in% names(data__)) {
    data__$RATE[is.na(data__$RATE)] <- 0
    flagRATE__ <- data__$RATE > 0
  } else {
    flagRATE__ <- NULL
  }
  if ("TINF" %in% names(data__)) {
    data__$TINF[is.na(data__$TINF)] <- 0
    flagTINF__ <- data__$TINF > 0
  } else {
    flagTINF__ <- NULL
    data__$TINF <- 0
  }
  if (any(flagTINF__ & flagRATE__)) stopIQR("TINF>0 and RATE>0 defined for dosing record.")
  if (!is.null(flagRATE__)) data__$TINF[flagRATE__] <- data__$AMT[flagRATE__] / data__$RATE[flagRATE__]
  data__$TINF[data__$TINF == 0] <- 1e-4
  data__ <- data__[,c("ID", "TIME", "ADM", "AMT", "TINF")]
  out__ <- plyr::dlply(data__, ~ID, function(x) {
    xx__ <- x
    xx__$ID <- NULL
    class(xx__) <- c("IQRdosing","data.frame")
    xx__
  })
  attr(out__,"split_type") <- NULL
  attr(out__,"split_labels") <- NULL
  if (FLAGunique) {
    idxUNIQUE <- !duplicated(out__)
    out__ <- out__[idxUNIQUE]
    names(out__) <- NULL
  }
  return(out__)
}
hasError <- function(object, FLAGrecursive = TRUE) {
  if(inherits(object, "try-error")) {
    TRUE
  } else if(is.vector(object) && FLAGrecursive) {
    any(sapply(object, inherits, "try-error"))
  } else {
    FALSE
  }
}
getErrorMessage <- function(object, FLAGrecursive = TRUE) {
  if(hasError(object)) {
    if(inherits(object, "try-error")) {
      toString(object)
    } else if(is.vector(object) && FLAGrecursive){
      errorMessages <- ""
      for(o__ in object) {
        if(inherits(o__, "try-error")) {
          errorMessages <- paste0(errorMessages, toString(o__), "\n")
        }
      }
      errorMessages
    } else {
      ""
    }
  }
}
aux_getFileGitRevision <- function (filename,revision,outputfile) {
  if (nchar(revision) < 5) stop
  if (!file.exists(filename)) stopIQR("File chosen with filename does not exist. Note: use relative paths from the current working directory.")
  if (file.exists(outputfile)) stopIQR("Output file exists and will not be overwritten. Delete yourself if desired.")
  command <- paste0("git show ",revision,":./",filename,"> ",outputfile)
  content <- suppressWarnings(shell(command))
  if (file.size(outputfile)==0) {
    unlink(outputfile)
    stopIQR("Chosen filename either did not exist in the selected revision - or revision number is wrong.")
  }
}
#'@export
tempdirIQR <- function(check=FALSE){
  out <- tempdir(check=check)
  out <- gsub("\\","/",out,fixed=TRUE)
  out
}
#'@export
tempfileIQR <- function(pattern = "file", tmpdir = tempdirIQR(), fileext = ""){
  out <- tempfile(pattern = pattern, tmpdir = tmpdir, fileext = fileext)
  out <- gsub("\\","/",out,fixed=TRUE)
  out
}
#'@export
stopIQR <- function(..., call.=FALSE,domain=NULL){
  stop(..., call.=call., domain=domain)
}
#'@export
warningIQR <- function(..., call.=FALSE,domain=NULL){
  warning(..., call.=call., domain=domain)
}
aux_getOS <- function(){
  i <- Sys.info()[["sysname"]]
  if (i=="Windows") return("windows")
  if (i=="Darwin") return("mac")
  if (i=="Linux") return("unix")
  stopIQR("Unknown operating system")
}
aux_closePDFs <- function() {
  tryCatch({
    mydevs <- grDevices::dev.list()
    dummy <- sapply(mydevs[names(mydevs) %in% c("pdf")], function (x) {
      grDevices::dev.off(x)
    })
  },error=function(x){})
  if (1==1) {a=1}
}
aux_closePNGs <- function() {
  if (aux_getOS()=="mac") {
    grDevices::graphics.off()
    return(invisible(NULL))
  }
  tryCatch({
    mydevs <- grDevices::dev.list()
    dummy <- sapply(mydevs[names(mydevs) %in% c("png")], function (x) {
      grDevices::dev.off(x)
    })
  },error=function(x){})
  return(invisible(NULL))
}
aux_closeDevice <- function(device) {
  if (grepl("pdf", device, ignore.case = TRUE)) aux_closePDFs()
  if (grepl("png", device, ignore.case = TRUE)) aux_closePNGs()
}
#'@export
aux_version <- function(pkgName="IQRtools", IQdesktop=NULL, exactVersion=NULL, minVersion=NULL,Rversion=NULL,isRopen=NULL,OS=NULL) {
  curVersion__ <- tryCatch(
    utils::packageVersion(pkgName),
    error=function (err) return ("No version present")
  )
  if (is.null(curVersion__)) return (NULL) 
  if (is.null(minVersion) & is.null(exactVersion) & is.null(IQdesktop)) {
    return(curVersion__)
  }
  if (!is.null(IQdesktop)) {
    if (!isIQdesktop()) stopIQR("You are not using IQdesktop (might not be a problem if everything else is well set up)")
    currentIQdesktopVersion <- getIQdesktopversion()
    if (currentIQdesktopVersion!=IQdesktop) stopIQR(sprintf("IQdesktop version %s required. You are using version %s",IQdesktop,currentIQdesktopVersion))
  }
  if (!is.null(minVersion)) {
    if (curVersion__ < minVersion) {
      stopIQR(sprintf("Package %s version is %s - but at least %s is required", pkgName, curVersion__, minVersion))
    }
  }
  if (!is.null(exactVersion)) {
    if (curVersion__ != exactVersion) {
      stopIQR(sprintf("Package %s version is %s - but version %s is required", pkgName, curVersion__, exactVersion))
    }
  }
  if (!is.null(Rversion)) {
    currentVersion <- paste0(version$major,".",version$minor)
    if (currentVersion != Rversion) {
      stopIQR(paste0("Your current R version (",currentVersion,") does not match the version the analysis was written for (",Rversion,").\n  You can on your own risk remove the 'Rversion' argument in the aux_version() function and run this script.\n  Or use the indicated version. We need to do this in order to ensure 100% reproducibility at all times."))
    }
  }
  if (!is.null(isRopen)) {
    if (!is.logical(isRopen)) stopIQR("isRopen needs to be TRUE or FALSE.")
    instPack <- utils::installed.packages()[,1]
    currentIsRopen <- "MicrosoftR" %in% instPack
    if (isRopen == TRUE & currentIsRopen == FALSE) stopIQR("You are using R, but the script was written for R open.\n  You can on your own risk remove the 'isRopen' argument in the aux_version() function and run this script.\n  Or use R Open instead of R. We need to do this in order to ensure 100% reproducibility at all times.")
    if (isRopen == FALSE & currentIsRopen == TRUE) stopIQR("You are using R open, but the script was written for R.\n  You can on your own risk remove the 'isRopen' argument in the aux_version() function and run this script.\n  Or use R instead of R Open. We need to do this in order to ensure 100% reproducibility at all times.")
  }
  if (!is.null(OS)) {
    if (OS != .Platform$OS.type) {
      stopIQR(paste0("Your current operating system (",.Platform$OS.type,") does not match the OS the analysis was written for (",OS,").\n  You can on your own risk remove the 'OS' argument in the aux_version() function and run this script.\n  Or use the indicated OS. We need to do this in order to ensure 100% reproducibility at all times."))
    }
  }
  return(TRUE)
}
#'@export
gen_aux_version <- function(pkgName="IQRtools", automode = TRUE, exactVersion=TRUE, IQdesktop=TRUE, Rversion=TRUE,isRopen=TRUE,OS=TRUE) {
  args <- list(
    pkgName = pkgName,
    exactVersion = tryCatch(as.character(utils::packageVersion(pkgName)),
                            error = function (err) return ("No version present")),
    IQdesktop = getIQdesktopversion(),
    Rversion = paste0(version$major,".",version$minor),
    isRopen = {
      instPack <- utils::installed.packages()[,1]
      "MicrosoftR" %in% instPack
    },
    OS = .Platform$OS.type
  )
  if (automode) {
    if (isIQdesktop()) {
      IQdesktop <- TRUE
      exactVersion <- Rversion <- isRopen <- OS <- FALSE
    } else {
      IQdesktop <- FALSE
      exactVersion <- Rversion <- isRopen <- OS <- TRUE
    }
  }
  if (!exactVersion) args[["exactVersion"]] <- NULL
  if (!IQdesktop | !isIQdesktop()) args[["IQdesktop"]] <- NULL
  if (!Rversion) args[["Rversion"]] <- NULL
  if (!isRopen) args[["isRopen"]] <- NULL
  if (!OS) args[["OS"]] <- NULL
  args <- paste(names(args), "=", sapply(args, function(a) ifelse(is.character(a), paste0("\"", a, "\""), a)))
  args <- paste(args, collapse = ", ")
  args <- paste0("aux_version(", args, ")")
  args <- sub("pkgName = ", "", args)
  return(args)
}
isIQdesktop <- function () {
  if (!file.exists("/IQDESKTOP/.version")) return (FALSE)
  return(TRUE)
}
getIQdesktopversion <- function () {
  if (!isIQdesktop()) return (NULL)
  aux_fileread("/IQDESKTOP/.version")
}
#'@export
aux_quantilenumber <- function(x, probs=c(0.25,0.5,0.75), na.rm=TRUE) {
  q <- c(min(x,na.rm = TRUE), stats::quantile(x = x,probs = probs,na.rm=TRUE),Inf)
  Qx <- rep(NA,length(x))
  lapply(1:(length(q)-1), function (k__) {
    Qx[x>=q[k__] & x<q[k__+1]] <<- k__
  })
  return(list(quantilenumbers=Qx,quantiles=q))
}
#'@export
aux_unlevel <- function(x) {
  if (!is.null(levels(x))) {
    x <- levels(x)[as.numeric(x)]
  }
  return(x)
}
#'@export
logit <- function (x) {
  if (any(x<=0 | x>=1)) stopIQR("Argument not between 0 and 1")
  log(x/(1-x))
}
#'@export
inv_logit <- function (x) {
  1/(1+exp(-x))
}
#'@export
aux_na_locf <- function(x,T=NULL) {
  v <- !is.na(x)
  y <- c(NA, x[v])[cumsum(v)+1]
  if (!is.null(T)) {
    naTindex <- which(is.na(T))
    T[is.na(T)] <- 999999999+naTindex 
    z <- data.frame(T=T,y=y)
    sZ <- split(z,z$T)
    z2 <- do.call(rbind,lapply(sZ,function (d) {
      d$y <- d$y[nrow(d)]
      d
    }))
    y <- z2$y
  }
  y
}
#'@export
aux_strtrim <- function(input) {
  return(gsub("^\\s+|\\s+$", "", input))
}
#'@export
aux_strrep <- function(origstr,oldsubstr,newsubstr) {
  return(gsub(oldsubstr, newsubstr, origstr, fixed="TRUE"))
}
aux_removeWhiteSpace <- function(inputText) {
  return(gsub("\\s+|\\s+", "", inputText))
}
aux_strmatch <- function(inputVec,searchString) {
  x <- which(inputVec==searchString)
  if (length(x)==0) return (NULL)
  return(unname(x))
}
#'@export
aux_explode <- function(input,separator=",") {
  return(unlist(strsplit(input,separator)))
}
#'@export
aux_explodePC <- function(input,separator=",",group="round") {
  if (group=="round") {
    groupStart__ <- "("
    groupEnd__   <- ")"
  }
  if (group=="square") {
    groupStart__ <- "["
    groupEnd__   <- "]"
  }
  if (group=="curly") {
    groupStart__ <- "{"
    groupEnd__   <- "}"
  }
  if (group!="round" & group!="square" & group!="curly") stopIQR("wrong group definition")
  elements        <- c()
  openParenthesis <- 0
  lastIndex       <- 1
  elementIndex    <- 1
  for (k2 in 1:nchar(input)) {
    if (substr(input,k2,k2) == groupStart__) {
      openParenthesis <- openParenthesis + 1
    } else {
      if (substr(input,k2,k2) == groupEnd__) {
        openParenthesis <- openParenthesis - 1;
      } else {
        if ((substr(input,k2,k2) == separator) & (openParenthesis == 0)) {
          elements[elementIndex] <- aux_strtrim(substr(input,lastIndex,k2-1))
          elementIndex           <- elementIndex + 1
          lastIndex              <- k2+1
        }
      }
    }
  }
  elements[elementIndex] <- aux_strtrim(substr(input,lastIndex,nchar(input)))
  return(elements)
}
#'@export
aux_strFindAll <- function(input,searchString) {
  x      <- gregexpr(pattern=searchString,input,fixed=TRUE)
  start  <- unlist(x)
  end <- start+attr(x[[1]],"match.length")-1
  if (length(start)==1 && start==-1)
    return(list(start=NULL,end=NULL))
  else
    return(list(start=start,end=end))
}
#'@export
aux_fileparts <- function(filename.with.path){
  pathname <- dirname(filename.with.path)
  filename <- basename(filename.with.path)
  fileext <- gsub(".*(\\.[^\\.]*)$","\\1",filename)
  filename <- gsub("(.*)(\\.[^\\.]*)$","\\1",filename)
  for (k in seq_along(filename)) {
    if(fileext[k]==filename[k]) fileext[k] <- ""
  }
  return(list(pathname=pathname,filename=filename,fileext=fileext))
}
#'@export
aux_filewrite <- function(text,filename) {
  if (is.null(filename)) return(0)
  fid <- aux_fopen(filename, mode="w")
  write(text, fid)
  aux_fclose(fid)
}
#'@export
aux_fileread <- function(filename,collapserows=TRUE) {
  fid <- aux_fopen(filename, mode="r")
  text <- readLines(fid)
  aux_fclose(fid)
  if (collapserows) {
    text <- paste(text,collapse="\n")
  }
  return(text)
}
#'@export
aux_mkdir <- function(pathdir) {
  suppressWarnings(if (!file.exists(pathdir)) dir.create(pathdir,recursive='TRUE'))
}
#'@export
aux_rmdir <- function(pathdir) {
  unlink(pathdir,recursive = 'TRUE')
}
aux_fopen <- function(filename,mode="w") {
  if (mode=="w") aux_mkdir(aux_fileparts(filename)$pathname)
  fid <- file(filename,open=mode)
  return(fid)
}
aux_fclose <- function(fid) {
  close(fid)
}
aux_fwrite <- function(fid,text) {
  write(text,fid)
}
aux_isnumericVector <- function(input) {
  return (!(NA %in% suppressWarnings(as.numeric(as.character(input)))))
}
#'@export
aux_preFillChar <- function(value2prefill,lengthString,fillChar) {
  if (nchar(value2prefill) >= lengthString) {
    lengthString <- nchar(value2prefill)
  }
  result <- paste0(c(paste0(rep(fillChar,lengthString-nchar(as.character(value2prefill))),collapse=""),
                     as.character(value2prefill)),collapse="")
  return(result)
}
#'@export
aux_postFillChar <- function(value2postfill,lengthString,fillChar) {
  if (nchar(value2postfill) >= lengthString) {
    lengthString <- nchar(value2postfill)
  }
  result <- tryCatch({
    paste0(c(as.character(value2postfill)),
           paste0(rep(fillChar,lengthString-nchar(as.character(value2postfill))),collapse=""),
           collapse="")
  }, error = function (msg) {
    result
  })
  return(result)
}
#'@export
aux_splitVectorEqualPieces <- function(x,n) {
  if (n>=length(x)) return(list(x))
  n_pieces_same_length <- round(length(x)/n)
  out   <- list()
  for (k in 1:n_pieces_same_length)
    out[[k]] <- x[seq(1+(k-1)*n,n+(k-1)*n)]
  if (n_pieces_same_length*n<length(x)) {
    k <- length(out)+1
    out[[length(out)+1]] <- x[seq(1+(k-1)*n,length(x))]
  }
  return(out)
}
aux_repStringElements <- function(string,old,new) {
  for (k in seq_along(old))
    string <- gsub(x = string,pattern = paste0("\\b",old[k],"\\b"),replacement = new[k])
  return(string)
}
aux_wrapText <- function(text,ncol,npre) {
  textwork__            <- text
  wraptext__            <- ""
  while (nchar(textwork__) > ncol) {
    ixSpace__ <- which(charToRaw(textwork__)==32)
    ixSpaceAbove__ <- which(ixSpace__>ncol)
    if (length(ixSpaceAbove__) > 0) {
      ixSpaceLimit__ <- ixSpace__[ixSpaceAbove__[1]-1]
    } else {
      ixSpaceLimit__ <- ixSpace__[length(ixSpace__)]
    }
    wraptext__ <- sprintf('%s%s\n',wraptext__,substr(textwork__,1,ixSpaceLimit__-1))
    textwork__ <- paste0(paste0(rep(" ",npre),collapse=""), substr(textwork__,ixSpaceLimit__+1,nchar(textwork__)))
  }
  wraptext__ = aux_strtrim(sprintf('%s%s\n',wraptext__,textwork__))
  return(wraptext__)
}
#'@export
aux_getRelPath <- function(fromFolder,toFolder) {
  if (fromFolder==toFolder) return("./")
  toFolder_vec__    <- aux_explode(toFolder,"/")
  fromFolder_vec__ <- aux_explode(fromFolder,"/")
  if (toFolder_vec__[1] != fromFolder_vec__[1])
    stopIQR("Absolute path of fromFolder and toFolder do not share the same root")
  maxLength__ <- min(length(toFolder_vec__),length(fromFolder_vec__))
  foundDIFF__ <- FALSE
  for (k in 1:maxLength__) {
    if (toFolder_vec__[k] != fromFolder_vec__[k]) {
      diff_elements_toFolder__ <- toFolder_vec__[k:length(toFolder_vec__)]
      diff_elements_fromFolder__ <- fromFolder_vec__[k:length(fromFolder_vec__)]
      foundDIFF__ <- TRUE
      break()
    }
  }
  if (!foundDIFF__) {
    diff_elements_toFolder__ <- toFolder_vec__[maxLength__:length(toFolder_vec__)]
    diff_elements_fromFolder__ <- fromFolder_vec__[maxLength__:length(fromFolder_vec__)]
  }
  returnFrom__ <- c(rep("..",length(diff_elements_fromFolder__)))
  gotTo__      <- diff_elements_toFolder__
  fromTo__     <- c(returnFrom__,gotTo__)
  relPath__ <- paste0(fromTo__,collapse="/")
  return(relPath__)
}
#'@export
aux_simplifypath <- function(path) {
  tmp0 <- aux_explode(path,"/")
  tmp0 <- tmp0[grep("^[.]$", tmp0, invert = TRUE)]
  tmp <- tmp0
  idxnodots <- c(grep("..",tmp, fixed = TRUE, invert = TRUE),length(tmp)+1)
  step <- diff(idxnodots)
  while (any(step>1)) {
    idx0 <- which(step>1)[1]
    tmp <- tmp[-(idxnodots[idx0]+c(0,1))]
    idxnodots <- c(grep("..",tmp, fixed = TRUE, invert = TRUE),length(tmp)+1)
    step <- diff(idxnodots)
  }
  paste0(tmp,collapse = "/")
}
#'@export
geomean <- function (x, na.rm = FALSE)
{
  if (!is.vector(x, mode = "numeric") || is.factor(x))
    stopIQR("'x' must be a numeric vector")
  wna <- which(is.na(x))
  if (length(wna)) {
    if (na.rm)
      x <- x[-wna]
    else return(NA)
  }
  if (any(x <= 0)) {
    warningIQR("Non-positive values in 'x'")
    return(NA)
  }
  else return(exp(mean(log(x[x>0]))))
}
#'@export
geosd <- function (x, na.rm = FALSE, sqrt.unbiased = TRUE)
{
  if (!is.vector(x, mode = "numeric") || is.factor(x))
    stopIQR("'x' must be a numeric vector")
  wna <- which(is.na(x))
  if (length(wna)) {
    if (na.rm)
      x <- x[-wna]
    else return(NA)
  }
  if (any(x <= 0)) {
    warningIQR("Non-positive values in 'x'")
    return(NA)
  }
  else {
    sd.log <- sd(log(x))
    if (!sqrt.unbiased) {
      n <- length(x)
      sd.log <- sqrt((n - 1)/n) * sd.log
    }
  }
  exp(sd.log)
}
#'@export
geocv <- function (x, na.rm = FALSE)
{
  sqrt(exp(stats::sd(log(x), na.rm = na.rm)^2) - 1) * 100
}
getSymbols <- function(char, exclude = NULL) {
  char <- char[char!="0"]
  out <- parse(text=char, keep.source = TRUE)
  out <- utils::getParseData(out)
  names <- unique(out$text[out$token == "SYMBOL"])
  if(!is.null(exclude)) names <- names[!names%in%exclude]
  return(names)
}
replaceSymbols <- function(what, by, x) {
  xOrig <- x
  is.not.zero <- which(x!="0")
  x <- x[is.not.zero]
  mynames <- names(x)
  x.parsed <- parse(text = x, keep.source = TRUE)
  data <- utils::getParseData(x.parsed)
  by <- rep(by, length.out=length(what))
  names(by) <- what
  data$text[data$text%in%what] <- by[data$text[data$text%in%what]]
  data <- data[data$token!="expr",]
  breaks <- c(0, which(diff(data$line1) == 1), length(data$line1))
  out <- lapply(1:(length(breaks)-1), function(i) {
    paste(data$text[(breaks[i]+1):(breaks[i+1])], collapse="")
  })
  names(out) <- mynames
  out <- unlist(out)
  xOrig[is.not.zero] <- out
  return(xOrig)
}
#'@export
#'@importFrom stats median
#'@importFrom utils tail
clusterX <- function(x, y = NULL, groupsize = 5, resolution = 0.1, lambda = 1, iterlim = 100, log = FALSE) {
  n_inner <- groupsize
  n_iter <- iterlim
  n_min <- 1 
  alpha <- 0 
  if (log & any(x <= 0))
    stopIQR("When argument log = TRUE, x values must be strictly positive.")
  if (is.null(y)) y <- rep(1, length(x))
  if (!is.null(y) & length(y) != length(x))
    stopIQR("x and y must have the same length.")
  x <- cbind.data.frame(TIME = x, VALUE = y)
  x <- x[order(x[[1]]),]
  is_finite <- Reduce("&", lapply(x, is.finite))
  if (all(!is_finite))
    stopIQR("x contains only NA/NaN/Inf values.")
  if (any(!is_finite)) {
    x <- x[is_finite,]
    warningIQR("x or y contained NA/NaN/Inf values. These were removed befor applying the clustering algorithm.")
  }
  if (log) x[[1]] <- log(x[[1]])
  getMu <- function(x) {
    do.call(rbind, lapply(split(x, x[["block"]]), function(d) {
      c(mu1 = median(d[[1]]),
        mu2 = median(d[[2]]))
    }))
  }
  getB <- function(x) {
    do.call(rbind, lapply(split(x, x[["block"]]), function(d) {
      c(b1 = max(c(sum(abs(d[[1]] - median(d[[1]])))/length(d[[1]]), 1e-16)),
        b2 = max(c(sum(abs(d[[2]] - median(d[[2]])))/length(d[[2]]), 1e-16)))
    }))
  }
  getBlocks <- function(d) {
    d <- d[!is.na(d)]
    q <- structure(as.numeric(names(d)), names = d)
    cl <- list()
    if (length(d) > 0) {
      for (i in 1:length(d)) {
        cl[[i]] <- c(as.numeric(q[1]), as.numeric(d[1]))
        for (j in 1:length(d)) {
          cl_new <- unique(c(cl[[i]], d[names(d) %in% as.character(cl[[i]])], q[names(q) %in% as.character(cl[[i]])]))
          if (length(setdiff(cl_new, cl[[i]])) == 0) break else cl[[i]] <- cl_new
        }
        d <- d[setdiff(names(d), cl[[i]])]
        q <- structure(as.numeric(names(d)), names = d)
        if (length(d) == 0) break
      }
    }
    return(cl)
  }
  x[["block"]] <- ceiling(seq_along(x[[1]])/n_inner)
  counts <- table(x[["block"]])
  if (tail(counts, 1) < n_min) x[["block"]][x[["block"]] == length(counts)] <- length(counts) - 1
  mu <- getMu(x)
  d <- outer(mu[, 1], mu[, 1], function(x, y) abs(x - y)/resolution); diag(d) <- Inf
  d <- apply(d, 1, function(x) which(x <= 1)[1])
  blocks <- getBlocks(d)
  for (b in blocks) x[["block"]][x[["block"]] %in% b] <- b[1]
  blocks <- unique(x[["block"]])
  x[["block"]] <- match(x[["block"]], blocks)
  for (i in 1:n_iter) {
    mu <- getMu(x)
    b <- getB(x)
    d1 <- t(t(outer(x[, 1], mu[, 1], function(x, y)  abs(x - y)))/b[,1] + lambda*log(b[,1]^2))
    d2 <- t(t(outer(x[, 2], mu[, 2], function(x, y)  abs(x - y)))/b[,2] + lambda*log(b[,2]^2))
    d <- cos(alpha)*d1 + sin(alpha)*d2
    block_old <- x[["block"]]
    block_new <- apply(d, 1, which.min)
    counts <- table(block_new)
    x[["block"]] <- block_new
    if (any(counts < n_min)) {
      small_blocks <- counts[counts < n_min]
      k <- 1
      while (k <= length(small_blocks)) {
        block_nr <- names(small_blocks)[k]
        block_counts <- small_blocks[[k]]
        while (block_counts < n_min) {
          block_nr_next <- as.character(as.numeric(block_nr[length(block_nr)]) + 1)
          block_nr <- c(block_nr, block_nr_next)
          block_counts <- block_counts + counts[block_nr_next]
        }
        index <- x[["block"]] %in% block_nr
        x[["block"]][index] <- block_nr[1]
        available <- which(names(small_blocks) %in% as.character(x[["block"]]))
        if (any(available > k)) {
          k <- seq_along(small_blocks)[available[available > k]][1]
        } else break
      }
    }
    x <- x[order(x[["block"]], x[[1]]),]
    blocks <- unique(x[["block"]])
    x[["block"]] <- match(x[["block"]], blocks)
    if (all(block_new == block_old) & all(counts >= n_min)) {
      break
    }
  }
  if (i == n_iter) warningIQR("Algorithm did not converge.")
  if (log) x[[1]] <- exp(x[[1]])
  x[["block"]] <- factor(as.character(x[["block"]]), levels = as.character(sort(unique(x[["block"]]))))
  return(x)
}
#'@export
#'@importFrom stats median quantile
statXY <- function(x, y = NULL, ..., quantiles = c(0.05, 0.95)) {
  clout <- clusterX(x = x, y = y, ...)
  if (any(y <= 0))
    warningIQR("NA values returned for GEOMETRICAL MEAN or SD due to y-values <= 0.")
  out <- do.call(rbind, lapply(split(clout, clout[[3]]), function(d) {
    t <- d[[1]]
    y <- d[[2]]
    group <- as.numeric(as.character(d[[3]]))
    d <- data.frame(
      GROUP = group[1],
      TIME = mean(t, na.rm = TRUE),
      MEAN.VALUE = mean(y, na.rm = TRUE),
      MEDIAN.VALUE = median(y, na.rm = TRUE),
      SD.VALUE = sd(y, na.rm = TRUE),
      SE.VALUE = sd(y, na.rm = TRUE)/sqrt(length(y)),
      GEOMMEAN.VALUE = suppressWarnings(exp(mean(log(y)))),
      GEOMSD.VALUE = suppressWarnings(exp(sd(log(y))))
    )
    quantile_names <- paste0("P", round(100*quantiles), ".VALUE")
    quantile_data <- lapply(quantiles, function(q) as.numeric(stats::quantile(y, probs = q, na.rm = TRUE)))
    names(quantile_data) <- quantile_names
    quantile_data <- as.data.frame(quantile_data)
    cbind(d, quantile_data)
  }))
  attr(out, "clusterOut") <- clout
  return(out)
}
#'@export
ginv <- function(X, tol = sqrt(.Machine$double.eps)) {
  if(length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X)))
    stopIQR("'X' must be a numeric or complex matrix")
  if(!is.matrix(X)) X <- as.matrix(X)
  Xsvd <- svd(X)
  if(is.complex(X)) Xsvd$u <- Conj(Xsvd$u)
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(Positive)) Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
  else if(!any(Positive)) array(0, dim(X)[2L:1L])
  else Xsvd$v[, Positive, drop=FALSE] %*% ((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive, drop=FALSE]))
}
#'@export
mvrnorm <- function(n = 1, mu, Sigma, tol=1e-6, empirical = FALSE) {
  p <- length(mu)
  if(!all(dim(Sigma) == c(p,p))) stopIQR("incompatible arguments")
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if(!all(ev >= -tol*abs(ev[1L]))) stopIQR("'Sigma' is not positive definite")
  X <- matrix(stats::rnorm(p * n), n)
  if(empirical) {
    X <- scale(X, TRUE, FALSE) 
    X <- X %*% svd(X, nu = 0)$v 
    X <- scale(X, FALSE, TRUE) 
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
  nm <- names(mu)
  if(is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if(n == 1) drop(X) else t(X)
}
#'@export
calcAICBIC <- function(OFV,nData,nParam, nSubj = NULL, nParamF = NULL, nParamR = NULL) {
  AIC <- as.numeric(OFV) + 2*as.numeric(nParam)
  BIC <- as.numeric(OFV) + log(as.numeric(nData))*as.numeric(nParam)
  BICc <- NA_real_
  if (!is.null(nSubj) & !is.null(nParamF) & !is.null(nParamR))
    BICc <- as.numeric(OFV) + log(as.numeric(nData))*as.numeric(nParamF) + log(as.numeric(nSubj))*as.numeric(nParamR)
  list(AIC=AIC,BIC=BIC, BICc = BICc)
}
#'@export
format_GUM <- function(value, se = NULL, digits = 2, justify = c("decimal", "left", "right", "centre"), na.string = "--", ...) {
  x <- utils::type.convert(as.character(value))
  if (!all(is.na(x)) && !is.numeric(x))
    stopIQR("value cannot be type-converted to numeric")
  sigma_x <- NULL
  if (!is.null(se)) {
    sigma_x <- utils::type.convert(as.character(se))
    if (!is.numeric(sigma_x)) {
      rel_x <- utils::type.convert(gsub("%", "", as.character(se), fixed = TRUE))
      if (!all(is.na(rel_x)) && !is.numeric(rel_x)) {
        stopIQR("se/rse cannot be type-converted to numeric")
      } else {
        sigma_x <- abs(x * rel_x / 100)
      }
    }
    tol__ <- sqrt(.Machine$double.eps)
    sigma_x <- sign(sigma_x)*sigma_x
    sigma_x[sigma_x < tol__] <- NA
  }
  N <- length(x)
  magnitude.x <- floor(log10(abs(x)))
  justify <- match.arg(justify)
  if (is.null(sigma_x)) {
    y <- sapply(1:N, function(i) {
      if (is.infinite(magnitude.x[i])) return(format(x[i]))
      nsmall <- max(0, -magnitude.x[i] + digits - 1, na.rm = TRUE)
      nsmall <- min(nsmall,20) 
      format(round(x[i], -magnitude.x[i] + digits - 1),
             nsmall = nsmall)
    })
  } else {
    magnitude.sigma_x <- floor(log10(sigma_x))
    y <- sapply(1:N, function(i) {
      if (is.na(sigma_x[i]) | is.infinite(magnitude.sigma_x[i])) {
        z <- format(x[i])
      } else {
        nsmall <- max(0, -magnitude.sigma_x[i] + digits - 1, na.rm = TRUE)
        nsmall <- min(nsmall,20) 
        z <- format(round(x[i], -magnitude.sigma_x[i] + digits - 1),
                    nsmall = nsmall)
      }
      return(z)
    })
  }
  y[grepl("NA", y)] <- na.string
  digits.y <- sapply(strsplit(y, ".", fixed = TRUE), function(v) {
    out <- nchar(v[2])
    if (is.na(out)) out <- -1
    return(out)
  })
  if (justify == "decimal") {
    y <- format(
      x = sapply(1:N, function(i) {
        paste0(y[i], paste(rep(" ", max(digits.y, na.rm = TRUE) - digits.y[i]), collapse = ""))
      }),
      justify = "right", ...
    )
  } else {
    y <- format(y, justify = justify, ...)
  }
  return(y)
}
#'@export
aux_extractObjects <- function(mylist, myclass = "ggplot") {
  sublist <- mylist
  output <- list()
  for (i in 1:5000) {
    is.object <- sapply(sublist, function(element) inherits(element, myclass))
    is.list <- sapply(sublist, is.list)
    if (!any(is.list)) break
    output <- c(output, sublist[is.object])
    sublist <- do.call(c, sublist[!is.object & is.list])
  }
  return(output)
}
alphnumsort <- function(x) {
  data__ <- dplyr::bind_rows(lapply(x, function(myx__) {
    contents__ <- sapply(seq_len(nchar(myx__)), function(i__) is.numeric(utils::type.convert(substr(myx__, i__, i__))))
    change__ <- c(1, abs(diff(contents__)))
    start__ <- which(change__ == 1)
    stop__ <- c((which(change__ == 1) - 1)[-1], nchar(myx__))
    split__ <- as.data.frame(lapply(seq_along(start__), function(i__) substr(myx__, start__[i__], stop__[i__])), stringsAsFactors = FALSE)
    colnames(split__) <- paste0("X", seq_len(ncol(split__)))
    return(split__)
  }))
  datanum__ <- lapply(data__, function(col__) utils::type.convert(col__))
  order__ <- do.call(order, datanum__)
  x[order__]
}
#'@export
run_silent_IQR <- function(expr, filename = tempfileIQR(), report = FALSE) {
  zz <- file(filename, open = "wt")
  sink(zz)
  sink(zz, type = "message")
  out <- suppressWarnings(suppressMessages(expr))
  sink(type = "message")
  sink()
  con <- file(filename)
  message <- readLines(con)
  close.connection(con)
  suppressWarnings(file.remove(filename))
  if (report && length(message) > 0) attr(out, "message") <- message
  return(out)
}
progressBar <- function(percentage, size = 50, number = TRUE) {
  if(percentage < 0) percentage <- 0
  if(percentage > 100) percentage <- 100
  out <- paste("\r|", paste(rep("=", round(size*percentage/100)), collapse=""), paste(rep(" ", size-round(size*percentage/100)), collapse=""), "|", sep="")
  cat(out)
  if(number) cat(format(paste(" ", round(percentage), "%", sep=""), width=5))
}
#'@export
rbind_IQRdosing <- function(...) {
  dots <- list(...)
  x <- do.call(rbind,dots)
  x <- dplyr::arrange(x,TIME,ADM)
  class(x) <- c("IQRdosing", class(x))
  x
}
#'@export
is_IQRdosing <- function(input) {
  methods::is(input,"IQRdosing")
}
#'@export
print.IQRdosing <- function(x, ...) {
  print.data.frame(x)
  cat("\nIQRdosing object")
  cat("\n  Number of inputs: ",numberInputs_IQRdosing(x),sep="")
}
#'@export
IQRdosing <- function(TIME, ADM, AMT,
                      TINF  = NULL,
                      RATE  = NULL,
                      ADDL  = NULL,
                      II    = NULL) {
  if (!is.null(ADDL)) ADDL[is.na(ADDL)] <- 0
  if (!is.null(II)) II[is.na(II)] <- 0
  if (!aux_isnumericVector(TIME) | !aux_isnumericVector(ADM) |
      !aux_isnumericVector(AMT) | !aux_isnumericVector(TINF) |
      !aux_isnumericVector(RATE) | !aux_isnumericVector(ADDL) | !aux_isnumericVector(II))
    stopIQR("All input arguments need to be numeric")
  if (length(ADM)==1) ADM <- rep(ADM,length(TIME))
  if (length(AMT)==1) AMT <- rep(AMT,length(TIME))
  if (!is.null(TINF) & length(TINF)==1) TINF <- rep(TINF,length(TIME))
  if (!is.null(RATE) & length(RATE)==1) RATE <- rep(RATE,length(TIME))
  if (!is.null(ADDL) & length(ADDL)==1) ADDL <- rep(ADDL,length(TIME))
  if (!is.null(II) & length(II)==1)     II   <- rep(II,length(TIME))
  if (length(TIME)!=length(ADM) | length(TIME)!=length(AMT))
    stopIQR("TIME, ADM, and AMT need to have same length")
  if (!is.null(TINF) & !is.null(RATE))
    stopIQR("RATE and TINF not allowed to be defined at the same time")
  if (!is.null(RATE)) {
    if (length(RATE) != length(AMT))
      stopIQR("RATE needs to have same length as TIME, ADM, AMT")
    TINF <- AMT/RATE
    TINF[RATE==0] <- 0
  }
  if (is.null(TINF)) TINF <- rep(0,length(TIME))
  if (length(TINF) != length(TIME))
    stopIQR("TINF needs to have same length as TIME, ADM, AMT")
  TINF[TINF<.Machine$double.eps] <- 0.0001
  if (is.null(ADDL) & !is.null(II))
    stopIQR("If II is defined also ADDL needs to be defined")
  if (!is.null(ADDL) & is.null(II))
    stopIQR("If ADDL is defined also II needs to be defined")
  if (is.null(ADDL) & length(TIME) > 0) ADDL <- rep(0,length(TIME))
  if (is.null(II) & length(TIME) > 0) II <- rep(0,length(TIME))
  dosingTable__ <- data.frame(TIME=as.numeric(TIME),ADM=as.numeric(ADM),
                              AMT=as.numeric(AMT),TINF=as.numeric(TINF),
                              ADDL=ADDL, II=II,
                              stringsAsFactors=FALSE)
  if (nrow(dosingTable__) > 0) {
    dosingTable__ <- do.call(rbind,lapply(1:nrow(dosingTable__), function (x) {
      if (dosingTable__$ADDL[x] == 0) {
        return(dosingTable__[x,])
      } else {
        row__ <- dosingTable__[x,]
        dtx__ <- row__[rep(seq(nrow(row__)), row__$ADDL+1),]
        dtx__$TIME <- seq(dtx__$TIME[1],
                          dtx__$TIME[1]+dtx__$ADDL[1]*dtx__$II[1],
                          dtx__$II[1])
        return(dtx__)
      }
    }))
  }
  dosingTable__$ADDL <- NULL
  dosingTable__$II   <- NULL
  dosingTable__ <- dplyr::arrange(dosingTable__,TIME,ADM)
  attr(dosingTable__,"class") <- c("IQRdosing", "data.frame")
  return(dosingTable__)
}
numberInputs_IQRdosing <- function(x) {
  if (!is_IQRdosing(x))
    stopIQR("Input is not an IQRdosing object")
  return(length(unique(x$ADM)))
}
#'@export
is_IQReventTable <- function(input) {
  methods::is(input,"IQReventTable")
}
#'@export
print.IQReventTable <- function(x, ...) {
  print.data.frame(x)
  cat("\nIQReventTable object")
  cat("\n")
  cat("\n")
  cat(paste0("  Nr subjects: ",length(unique(x$ID))))
  cat("\n")
  cat("\n")
  adm__ <- unique(x$ADM[!is.na(x$ADM)])
  if (length(adm__) > 0) {
    cat("  Dose records present for: ")
    cat(paste0("INPUT",adm__,collapse=", "))
    cat("\n")
    cat("\n")
  }
  regParam__ <- setdiff(names(x),c("ID","TIME","ADM","AMT","TINF"))
  if (length(regParam__) > 0) {
    cat("  Regression parameters: ")
    cat(paste0(regParam__, collapse=", "))
    cat("\n")
    cat("\n")
  }
}
#'@export
#'@importFrom data.table data.table setorderv copy rbindlist :=
IQReventTable <- function(data, regression=NULL, abs0inputs=NULL, abs0Tk0param=NULL,FLAGfixDoseOverlap=FALSE) {
  if (!is.data.frame(data))
    stopIQR("Input argument 'data' is not a data.frame object")
  if (!("TIME" %in% names(data)))
    stopIQR("TIME column not present in the 'data' input argument")
  if (!is.null(abs0Tk0param)) {
    if (!all(abs0Tk0param %in% regression)) {
      stopIQR("Not all parameters in 'abs0Tk0param' provided as 'regression' parameters")
    }
  }
  if (length(abs0inputs) != length(abs0Tk0param)) {
    stopIQR("Number of elements in arguments 'abs0inputs' and 'abs0Tk0param' need to match")
  }
  if (!("ID" %in% names(data)))
    data <- cbind(ID=1,data)
  if (!("ADM" %in% names(data)))
    data <- cbind(ADM=NA,data)
  if (!("AMT" %in% names(data)))
    data <- cbind(AMT=NA,data)
  if (!("ADDL" %in% names(data)))
    data <- cbind(ADDL=0,data)
  if (!("II" %in% names(data)))
    if (all(data$ADDL[data$ADM != 0 & !is.na(data$ADM)] == 0)) {
      data <- cbind(II=0,data)
    } else {
      stopIQR("If additional doses are defined, the dosing intervals needs to be given.")
    }
  if (!(any(c("TINF","RATE") %in% names(data))))
    data <- cbind(data,TINF=0)
  if (!"TINF" %in% names(data)) {
    if ("RATE" %in% names(data)) {
      TINF__ <- data$AMT/data$RATE
      TINF__[data$RATE==0] <- 0
      TINF__[data$RATE==-2] <- 0  
      data$TINF <- TINF__
    }
  }
  if (any(is.na(data$TINF) & !(is.na(data$ADM) | data$ADM == 0))){
    warningIQR("There are NA entries for TINF for dosings. A bolus dose will be assumed.")
    data$TINF[is.na(data$TINF) & !(is.na(data$ADM) | data$ADM == 0)] <- 0
  }
  if (!is.null(regression)) {
    if (!all(regression %in% names(data))) {
      stopIQR("Not all defined regression names are present in the dataset")
    }
    if (any(sapply(regression, function(.r) any(is.na(data[[.r]]))))) {
      check__ <- regression[sapply(regression, function(.r) any(is.na(data[[.r]])))]
      stopIQR(paste0("Regression columns are not allowed to contain NAs.\nCheck ", paste0(check__, collapse = ", "), " for NAs."))
    }
  }
  dataWork__ <- data[,c("ID","TIME","ADM","AMT","TINF","ADDL","II",regression)]
  `:=` <- data.table::`:=`
  if (nrow(dataWork__) > 0) {
    dataWork__$ADDL[is.na(dataWork__$ADDL)] <- 0
    dW__ <- split(dataWork__, dataWork__$ADDL > 0)
    if (!is.null(dW__$`TRUE`)){
      dWADDL__ <- data.table::data.table(dW__$`TRUE`)
      dWADDL__[,ROWID := 1:.N]
      dWADDL2__ <- data.table::copy(dWADDL__)[,list(TIME, ADDL, II, ROWID)]
      dWADDL2__ <- dWADDL2__[,list(TIME = TIME + II * seq(0,ADDL)), by = ROWID]
      dWADDL__[,`:=`(TIME = NULL)]
      dWADDL__ <- merge(dWADDL2__, dWADDL__,all = TRUE)
      dWADDL__[,`:=`(ROWID = NULL)]
      dW__$`TRUE` <- dWADDL__
    }
    dataWork__ <- data.table::rbindlist(dW__, use.names = TRUE)
  }
  dataWork__$ADDL <- NULL
  dataWork__$II   <- NULL
  dataWork__ <- data.table::as.data.table(dataWork__)
  dataWork__[ADM == 0,   `:=`(ADM = NA)]
  dataWork__[is.na(ADM), `:=`(TINF = NA, AMT = NA)]
  data.table::setorderv(dataWork__,c("ID","TIME","ADM"), na.last = TRUE)
  dataWork__ <- data.table::data.table(dataWork__)
  dataWork__[.N > 1, dummy__ := 1, by = c("TIME", "ID") ]
  if (length(regression))
    dataWork__[,
               {if (any(vapply(.SD, function(x) length(unique(x)) > 1, FALSE)))
                 stopIQR("There are regression parameters at same TIME that have different values")},
               by = c("TIME", "ID"), .SDcols = regression]
  dataWork__[, dummy__ := NULL]
  dummy__ <- sapply(seq_along(abs0inputs), function (kx__) {
    input__ <- abs0inputs[kx__]
    dataWork__$TINF[dataWork__$ADM==input__ & !is.na(dataWork__$ADM)] <<- dataWork__[[abs0Tk0param[kx__]]][dataWork__$ADM==input__ & !is.na(dataWork__$ADM)]
  })
  dataWork__$TINF[dataWork__$TINF<.Machine$double.eps] <- 0.0001
  dfCheck__ <- dataWork__
  lagNames <- grep("Tlag[[:digit:]]", names(dfCheck__), value = TRUE)
  lagNo    <- as.numeric(gsub("Tlag","",lagNames))
  if (any(is.na(lagNo))) stopIQR("For ", paste0(lagNames[is.na(lagNo)], collapse = ", "), " the corresponding input (ADM) number could not be determined.\nLag time names must be of the form 'TlagX' with X being the corresponding ADM.")
  dfCheck__$TIMEtinfStart <- dfCheck__$TIME
  for (kk__ in seq_along(lagNo)) {
    ixk__ <- dfCheck__$ADM == lagNo[[kk__]] & !is.na(dfCheck__$ADM)
    dfCheck__$TIMEtinfStart[ixk__] <- dfCheck__$TIME[ixk__] + dfCheck__[[lagNames[kk__]]][ixk__]
  }
  dfCheck__ <- dplyr::mutate(dfCheck__, TIMEtinfEnd = TIMEtinfStart + TINF)
  dfCheck__ <- dplyr::arrange(dfCheck__, ID, ADM, TIMEtinfStart)
  check2 <- check1 <- data.frame()
  for (admk__ in unique(dfCheck__$ADM)) {
    x__ <- filter(dfCheck__, ADM == admk__ & !is.na(ADM) & ADM != 0)
    for (idk__ in unique(x__$ID)) {
      xx__ <- dplyr::filter(x__, ID == idk__)
      ix_dupl__ <- duplicated(xx__$TIME, fromLast = TRUE)
      if (any(ix_dupl__))
        check1 <- rbind(check1,unique(xx__[ix_dupl__, c("ID", "ADM", "TIME")]))
      ix_longInf__ <- c(diff(xx__$TIMEtinfStart) <= xx__$TINF[-length(xx__$TINF)], FALSE)
      if (any(ix_longInf__ & !ix_dupl__))
        check2 <- rbind(check2,
                        unique(xx__[ix_longInf__ & !ix_dupl__, c("ID", "ADM", "TIME", "TINF")]))
    }
  }
  if (nrow(check1)>0 | nrow(check2)>0) {
    msg <- paste0("Dosing event times are duplicated or infusions last longer than next dose given.\n",
                  "(Note that for bolus doses, infusion time of 1e-4 is assumed and potential lag times are taken into account.):\n")
    if (nrow(check1)>0) {
      check1tab <- dplyr::bind_rows(structure(names(check1), names = names(check1)),
                                    dplyr::mutate_all(check1, as.character))
      check1tab <- as.data.frame(lapply(check1tab, format, justify = "right"), stringsAsFactors = FALSE)
      msg <- paste0(msg, "Multiple dosing events at same time points for:\n")
      for (kk in 1:nrow(check1tab)) {
        msg <- paste0(msg, check1tab$ID[kk], " | ", check1tab$ADM[kk], " | ", check1tab$TIME[kk], "\n")
      }
    }
    if (nrow(check2)>0) {
      check2tab <- bind_rows(structure(names(check2), names = names(check2)),
                             dplyr::mutate_all(check2, as.character))
      check2tab <- as.data.frame(lapply(check2tab, format, justify = "right"), stringsAsFactors = FALSE)
      msg <- paste0(msg, "Dosing events for which infusion end times are at or after next dosing time:\n")
      for (kk in 1:nrow(check2tab)) {
        msg <- paste0(msg, check2tab$ID[kk], " | ", check2tab$ADM[kk], " | ", check2tab$TIME[kk], " | ", check2tab$TINF[kk], "\n")
      }
    }
    if (!FLAGfixDoseOverlap) {
      msg <- paste0("!!! Please handle as simulation would fail !!!\n", msg)
      stopIQR(msg)
    } else {
      msg <- paste0("Event table for each ID/ADM instance is manipulated by introducing new non-overlapping dosing events.\n\n", msg)
      warningIQR(msg)
      id_adm_corr <- unique(check2[,c("ID", "ADM")])
      for (k in 1:nrow(id_adm_corr)) {
        et_k <- dplyr::inner_join(dataWork__, id_adm_corr[k,], by = c("ID", "ADM"))
        etnew_k <- remove_input_overlap(et_k)
        dataWork__ <- dplyr::anti_join(dataWork__, id_adm_corr[k,], by = c("ID", "ADM"))
        dataWork__ <- rbind(dataWork__, etnew_k)
      }
      dataWork__ <- data.table::data.table(dataWork__)
      data.table::setorderv(dataWork__,c("ID","TIME","ADM"), na.last = TRUE)
    }
  }
  dataWork__ <- data.table::data.table(dataWork__)
  if (length(regression))
    dataWork__[, c(regression) := lapply(.SD, function(.x) replace(.x, c(1, diff(.x)) == 0, NA)), by = "ID", .SDcols = regression]
  dataWork__ <- as.data.frame(dataWork__)
  dataWork__ <- as.data.frame(dataWork__)
  xx__ <- dataWork__[c("ADM","AMT","TINF",regression)]
  NR_NA__ <- rowSums(is.na(xx__))
  ix_ROWS_NOT_ALL_NA__ <- which(NR_NA__ != length(c("ADM","AMT","TINF",regression)))
  dataWork__ <- dataWork__[ix_ROWS_NOT_ALL_NA__,]
  dataWork__ <- dataWork__[,c("ID","TIME","ADM","AMT","TINF",regression)]
  attr(dataWork__,"class") <- c("IQReventTable", "data.frame")
  return(dataWork__)
}
genSim_IQReventTable <- function (eventTable,model,simtime,parametersSim) {
  if (!is_IQReventTable(eventTable))
    stopIQR("The 'eventTable' argument is not an IQReventTable object")
  regressionParam__ <- setdiff(names(eventTable),c("ID", "TIME", "ADM", "AMT", "TINF"))
  lagTimeParam__ <- names(parametersSim)[grepl("\\<Tlag[0-9]+\\>",names(parametersSim))]
  lagTimeRegre__ <- regressionParam__[grepl("\\<Tlag[0-9]+\\>",regressionParam__)]
  addeventTable__ <- setdiff(lagTimeParam__,lagTimeRegre__)
  dummy__ <- sapply(addeventTable__, function (xxx__) {
    eventTable[[xxx__]] <<- parametersSim[xxx__]
  })
  if (!all(c("ADM", "AMT", "TINF") %in% names(eventTable)))
    stopIQR("Event table needs to contain ADM, AMT, and TINF columns.")
  if (length(setdiff(regressionParam__,names(model$parameters))) > 0)
    stopIQR("eventTable seems to contain column names that are neither parameters in the model or ID, TIME, ADM, AMT, TINF: \n",
            paste0(setdiff(regressionParam__,names(model$parameters)), collapse = ", "))
  eventTable$LAGTIMECOL <- NA
  allADM__ <- unique(eventTable$ADM[!is.na(eventTable$ADM)])
  dummy__ <- sapply(allADM__, function (xxx__) {
    tryCatch({
      eventTable$LAGTIMECOL[eventTable$ADM==xxx__ & !is.na(eventTable$ADM) & eventTable$ADM != 0] <<- aux_na_locf(eventTable[[paste0("Tlag",xxx__)]])[eventTable$ADM==xxx__ & !is.na(eventTable$ADM)  & eventTable$ADM != 0]
    }, error=function(err) {
      eventTable$LAGTIMECOL[eventTable$ADM==xxx__ & !is.na(eventTable$ADM) & eventTable$ADM != 0] <<- 0
    })
    eventTable$LAGTIMECOL[is.na(eventTable$ADM) | eventTable$ADM==0] <<- NA
  })
  ADM_data__ <- unique(eventTable$ADM[!is.na(eventTable$ADM)])
  if (length(ADM_data__) > 0) {
    INPUTS_data__ <- paste0("INPUT",ADM_data__)
    INPUTS_model__ <- names(model$inputs)
    if (length(setdiff(INPUTS_data__,INPUTS_model__)) > 0)
      stopIQR("There are input definitions in the eventTable (ADM) for INPUTs that are not present in the model")
  }
  if (!is.list(simtime)) {
    eventTable__ <- eventTable[eventTable$TIME<=max(simtime),]
  } else {
    eventTable__ <- eventTable
  }
  x__ <- tidyr::gather(eventTable__,"NAME","VALUE", dplyr::one_of("AMT",regressionParam__))
  x__ <- x__[!is.na(x__$VALUE),]
  x__ <- x__[!(is.na(x__$ADM) & x__$NAME %in% c("AMT","TINF","TIME")),]
  x__ <- x__[!(x__$ADM==0 & x__$NAME %in% c("AMT","TINF","TIME")),]
  x__$PARAMNAME[!is.na(x__$ADM)] <- paste0("INPUT",x__$ADM[!is.na(x__$ADM)])
  x__$PARAMNAME[x__$NAME %in% regressionParam__] <- x__$NAME[x__$NAME %in% regressionParam__]
  x__$VALUE[x__$NAME=="AMT"] <- x__$VALUE[x__$NAME=="AMT"]/x__$TINF[x__$NAME=="AMT"]
  x__$LAGTIMECOL[x__$NAME!="AMT"] <- 0
  x__$TIME[!is.na(x__$ADM)] <- x__$TIME[!is.na(x__$ADM)] + x__$LAGTIMECOL[!is.na(x__$ADM)]
  x__$LAGTIMECOL <- NULL
  y__ <- x__[x__$NAME=="AMT",]
  if (nrow(y__) > 0) {
    y__$TIME <- y__$TIME+y__$TINF
    y__$VALUE <- 0
  }
  x__ <- dplyr::arrange(rbind(x__,y__),ID,TIME,PARAMNAME)
  if (length(model$inputs)==0) {
    x__ <- x__[!grepl("INPUT[0-9]+",x__$PARAMNAME),]
  }
  xSplitID__ <- split(x__,x__$ID)
  eventTableICs__ <- list()
  eventTableSim__ <- data.frame(do.call(rbind,lapply(xSplitID__, function (xID__) {
    TIME__ <- unique(xID__$TIME)
    if (!is.list(simtime)) {
      TIMEeventTable__ <- sort(unique(c(min(simtime), TIME__ ,max(simtime))))
    } else {
      TIMEeventTable__ <- sort(unique(c(min(simtime[[as.character(xID__$ID[1])]]), TIME__ ,max(simtime[[as.character(xID__$ID[1])]]))))
    }
    NrowsEventTable__ <- length(TIMEeventTable__)
    eventTableSimID__ <- rbind(parametersSim,
                               matrix(rep(NA,length(parametersSim)*(NrowsEventTable__-1)),ncol=length(parametersSim)))
    dummy__ <- sapply(split(xID__,xID__$PARAMNAME), function (xx__) {
      ix_row__   <- match(TIMEeventTable__,xx__$TIME)
      ix_row__   <- which(!is.na(ix_row__))
      if (length(ix_row__) != nrow(xx__)) {
        stopIQR(paste0("Event table input argument seems to have same type of event happening at an identical time point in ID=",xID__$ID[1]))
      }
      eventTableSimID__[ix_row__,xx__$PARAMNAME[1]] <<- xx__$VALUE
    })
    eventTableSimID__ <- cbind(ID=xID__$ID[1],TIME=TIMEeventTable__,eventTableSimID__)
    rownames(eventTableSimID__) <- NULL
    eventTableICs__ <<- rbind(eventTableICs__,calcNNic(model,eventTableSimID__[1,c(-1,-2)])$ICs)
    eventTableSimID__
  })),stringsAsFactors=FALSE)
  output__ <- list(eventTableSim=eventTableSim__,eventTableICs=eventTableICs__)
}
#'@export
remove_input_overlap <- function(et) {
  if (length(unique(et$ID)) != 1) {
    stopIQR("input needs to be from one individual")
  }
  if (length(unique(et$ADM)) != 1) {
    stopIQR("input needs to be from one input/ADM")
  }
  lagName <- paste0("Tlag", et$ADM[1])
  if (lagName %in% names(et)) {
    et$TIME <- et$TIME + et[[lagName]]
  }
  et$TEND <- et$TIME + et$TINF
  evtT <- sort(c(et$TIME, et$TEND))
  nD <- nrow(et)
  nE <- length(evtT)-1
  mat <- matrix(NA, nrow = nE, ncol = nD)
  for (k in 1:nD) {
    mat[,k] <- evtT[1:nE] >= et$TIME[k] & evtT[1:nE] < et$TEND[k]
  }
  etnew <- data.frame(
    ID   = et$ID[1],
    TIME = evtT[1:nE],
    ADM  = et$ADM[1],
    RATE = mat %*% matrix(et$AMT/et$TINF, ncol = 1),
    TINF = diff(evtT)
  )
  etnew$AMT <- etnew$RATE * etnew$TINF
  etnew$RATE <- NULL
  etnew <- etnew[etnew$AMT != 0,]
  etnew <- dplyr::left_join(etnew,
                            select(et, -AMT, -TINF, -TEND),
                            by = c("ID", "ADM", "TIME"))
  if (lagName %in% names(et)){
    etnew <- dplyr::left_join(etnew,
                              select(et, c("ID", "ADM", "TEND", lagName)) %>% rename(TIME=TEND),
                              by = c("ID", "ADM", "TIME"),
                              suffix = c("", "end"))
    etnew$shift <- ifelse(is.na(etnew[[lagName]]), etnew[[paste0(lagName,"end")]], etnew[[lagName]])
    etnew$TIME <- etnew$TIME - etnew$shift
    etnew$shift <- NULL
    etnew[[paste0(lagName,"end")]] <- NULL
  }
  etnew$TINF[etnew$TINF>1e-9] <- etnew$TINF[etnew$TINF>1e-9] - 1e-9
  etnew
}
handleVectorSyntaxR_IQRmodel <- function(model) {
  for (k in seq_along(model$states)) {
    if (!is.null(aux_strFindAll(model$states[[k]]$ODE,"[")$start)) {
      model$states[[k]]$ODE <- aux_strrep(aux_strrep(aux_removeWhiteSpace(model$states[[k]]$ODE),"[","c("),"]",")")
    }
  }
  for (k in seq_along(model$variables)) {
    if (!is.null(aux_strFindAll(model$variables[[k]]$formula,"[")$start)) {
      model$variables[[k]]$formula <- aux_strrep(aux_strrep(aux_removeWhiteSpace(model$variables[[k]]$formula),"[","c("),"]",")")
    }
  }
  for (k in seq_along(model$reactions)) {
    if (!is.null(aux_strFindAll(model$reactions[[k]]$formula,"[")$start)) {
      model$reactions[[k]]$formula <- aux_strrep(aux_strrep(aux_removeWhiteSpace(model$reactions[[k]]$formula),"[","c("),"]",")")
    }
  }
  return(model)
}
nonNumICsFct_IQRmodel <- function (model__) {
  varInfo__   <- variablesInfo_IQRmodel(model__)
  for (k in seq_along(names(model__$variables))) {
    if (k+1 <= length(names(model__$variables))) {
      for (k2__ in (k+1):length(names(model__$variables)))
        varInfo__$varformulas[k2__] <- gsub(pattern = paste0("\\<",names(model__$variables)[k],"\\>"),
                                            replacement = paste0("(",varInfo__$varformulas[k],")"),
                                            x = varInfo__$varformulas[k2__])
    }
  }
  reacInfo__  <- reactionsInfo_IQRmodel(model__)
  for (k in seq_along(names(model__$reactions))) {
    if (k+1 <= length(names(model__$reactions))) {
      for (k2__ in (k+1):length(names(model__$reactions)))
        reacInfo__$reacformulas[k2__] <- gsub(pattern = paste0("\\<",names(model__$reactions)[k],"\\>"),
                                              replacement = paste0("(",reacInfo__$reacformulas[k],")"),
                                              x = reacInfo__$reacformulas[k2__])
    }
  }
  stateInfo__ <- statesInfo_IQRmodel(model__)
  for (k in seq_along(names(model__$states))) {
    for (k2__ in seq_along(names(model__$reactions))) {
      updatedIC__ <- gsub(pattern = paste0("\\<",names(model__$reactions)[k2__],"\\>"),
                          replacement = paste0("(",reacInfo__$reacformulas[k2__],")"),
                          x = stateInfo__$stateICs[k])
      stateInfo__$stateICs[k] <- updatedIC__
    }
  }
  for (k in seq_along(names(model__$states))) {
    for (k2__ in seq_along(names(model__$variables))) {
      updatedIC__ <- gsub(pattern = paste0("\\<",names(model__$variables)[k2__],"\\>"),
                          replacement = paste0("(",varInfo__$varformulas[k2__],")"),
                          x = stateInfo__$stateICs[k])
      stateInfo__$stateICs[k] <- updatedIC__
    }
  }
  IAinfo__ <- list(
    paramnames = names(model__$initalAssignments),
    formulas = sapply(model__$initalAssignments, function (x) x$formula)
  )
  ix__ <- which(!IAinfo__$paramnames %in% names(model__$states))
  IAinfo__$paramnames <- IAinfo__$paramnames[ix__]
  IAinfo__$formulas <- IAinfo__$formulas[ix__]
  for (k in seq_along(names(model__$initalAssignments))) {
    for (k2__ in seq_along(names(model__$reactions))) {
      updatedIA__ <- gsub(pattern = paste0("\\<",names(model__$reactions)[k2__],"\\>"),
                          replacement = paste0("(",reacInfo__$reacformulas[k2__],")"),
                          x = IAinfo__$formulas[k])
      IAinfo__$formulas[k] <- updatedIA__
    }
  }
  for (k in seq_along(IAinfo__$paramnames)) {
    for (k2__ in seq_along(names(model__$variables))) {
      updatedIA__ <- gsub(pattern = paste0("\\<",names(model__$variables)[k2__],"\\>"),
                          replacement = paste0("(",varInfo__$varformulas[k2__],")"),
                          x = IAinfo__$formulas[k])
      IAinfo__$formulas[k] <- updatedIA__
    }
  }
  time <- 0
  for (k in seq_along(model__$functions)) {
    text <- paste(names(model__$functions)[k]," <- function(",model__$functions[[k]]$arguments,") { ",model__$functions[[k]]$formula," }")
    eval(parse(text=text))
  }
  for (k in seq_along(names(model__$states))) {
    text <- paste(names(model__$states)[k],"= NA")
    eval(parse(text=text))
  }
  paramInfo__ <- parametersInfo_IQRmodel(model__)
  for (k in seq_along(paramInfo__$paramnames)) {
    text <- paste(paramInfo__$paramnames[k],"=",paramInfo__$paramvalues[k])
    eval(parse(text=text))
  }
  for (k in seq_along(names(model__$states))) {
    text <- paste(names(model__$states)[k],"=",stateInfo__$stateICs[k])
    eval(parse(text=text))
  }
  test <- c()
  for (k in seq_along(names(model__$states))) {
    text <- paste("test[k] <- ",names(model__$states)[k])
    eval(parse(text=text))
  }
  if (sum(as.double(is.na(test))) > 0) stopIQR("Non-numerical initial conditions are wrongly defined and non-evaluable.")
  text__ <- "function (parameters,intialConditions,time) {\n"
  for (k in seq_along(model__$functions)) text__ <- paste0(text__,"  ",names(model__$functions)[k]," <- function(",model__$functions[[k]]$arguments,") { ",model__$functions[[k]]$formula," }\n")
  for (k in seq_along(names(model__$states))) text__ <- paste0(text__,"  ",names(model__$states)[k]," <- NA\n")
  for (k in seq_along(paramInfo__$paramnames)) text__ <- paste0(text__,"  ",paramInfo__$paramnames[k]," <- parameters[",k,"]\n")
  for (k in seq_along(names(model__$states))) {
    test__ <- suppressWarnings(as.numeric(stateInfo__$stateICs[k]))
    if (is.na(test__)) {
      text__ <- paste0(text__,"  ",names(model__$states)[k]," <- ",stateInfo__$stateICs[k],"\n")
    } else {
      text__ <- paste0(text__,"  ",names(model__$states)[k]," <- as.numeric(intialConditions[",k,"])\n")
    }
  }
  for (k in seq_along(IAinfo__$paramnames)) text__ <- paste0(text__,"  ",IAinfo__$paramnames[k]," <- ",IAinfo__$formulas[k],"\n")
  text__ <- paste0(text__,"  ICs <- c()\n")
  for (k in seq_along(names(model__$states))) text__ <- paste0(text__,'  ICs["',names(model__$states)[k],'"] <- ',names(model__$states)[k],"\n")
  text__ <- paste0(text__,"  IAs <- c()\n")
  IAparams__ <- intersect(names(model__$initalAssignments),names(model__$parameters))
  for (k in seq_along(IAparams__)) text__ <- paste0(text__,'  IAs["',IAparams__[k],'"] <- ',IAparams__[k],"\n")
  text__ <- paste0(text__,"  if (sum(as.double(is.na(ICs))) > 0) stopIQR('Non-numerical initial conditions are wrongly defined and non-evaluable.')\n")
  text__ <- paste0(text__,"  if (sum(as.double(is.na(IAs))) > 0) stopIQR('Initial parameter assignments are wrongly defined and non-evaluable.')\n")
  text__ <- paste0(text__,"  return(list(ICs=ICs,IAs=IAs))\n")
  text__ <- paste0(text__,"}\n")
  return(eval(parse(text=text__)))
}
nonNumICsSensFct_IQRmodel <- function (model,sensParams) {
  stateInfo__ <- statesInfo_IQRmodel(model)
  paramInfo__ <- parametersInfo_IQRmodel(model)
  varInfo__   <- variablesInfo_IQRmodel(model)
  reacInfo__  <- reactionsInfo_IQRmodel(model)
  if (is.null(sensParams)) sensParams <- paramInfo__$paramnames
  for (k in seq_along(varInfo__$varnames)) {
    if (k+1 <= length(varInfo__$varnames)) {
      for (k2__ in (k+1):length(varInfo__$varnames)) {
        varInfo__$varformulas[k2__] <- gsub(pattern = paste0("\\<",varInfo__$varnames[k],"\\>"),replacement = paste0("(",varInfo__$varformulas[k],")"),x = varInfo__$varformulas[k2__])
      }
    }
  }
  for (k in seq_along(reacInfo__$reacnames)) {
    if (k+1 <= length(reacInfo__$reacnames)) {
      for (k2__ in (k+1):length(reacInfo__$reacnames)) {
        reacInfo__$reacformulas[k2__] <- gsub(pattern = paste0("\\<",reacInfo__$reacnames[k],"\\>"),replacement = paste0("(",reacInfo__$reacformulas[k],")"),x = reacInfo__$reacformulas[k2__])
      }
    }
  }
  for (k in seq_along(stateInfo__$statenames)) {
    for (k2__ in seq_along(reacInfo__$reacnames)) {
      updatedIC__ <- gsub(pattern = paste0("\\<",reacInfo__$reacnames[k2__],"\\>"),replacement = paste0("(",reacInfo__$reacformulas[k2__],")"),x = stateInfo__$stateICs[k])
      stateInfo__$stateICs[k] <- updatedIC__
    }
  }
  for (k in seq_along(stateInfo__$statenames)) {
    for (k2__ in seq_along(varInfo__$varnames)) {
      updatedIC__ <- gsub(pattern = paste0("\\<",varInfo__$varnames[k2__],"\\>"),replacement = paste0("(",varInfo__$varformulas[k2__],")"),x = stateInfo__$stateICs[k])
      stateInfo__$stateICs[k] <- updatedIC__
    }
  }
  allICs__ <- stateInfo__$stateICs
  allDerivICs__ <- matrix(NA,ncol=length(sensParams),nrow=length(allICs__))
  for (kstate__ in seq_along(allICs__)) {
    for (kpar__ in seq_along(sensParams)) {
      x__ <- eval(parse(text=paste0("expression(",aux_removeWhiteSpace(allICs__[kstate__]),")")))
      res__ <- paste0(deparse(stats::D(x__,aux_removeWhiteSpace(sensParams[kpar__]))),collapse="")
      allDerivICs__[kstate__,kpar__] <- res__
    }
  }
  text__ <- "function (parameters,time) {\n"
  for (k in seq_along(model$functions)) text__ <- paste0(text__,"  ",names(model$functions)[k]," <- function(",model$functions[[k]]$arguments,") { ",model$functions[[k]]$formula," }\n")
  for (k in seq_along(stateInfo__$statenames)) text__ <- paste0(text__,"  ",stateInfo__$statenames[k]," <- NA\n")
  for (k in seq_along(paramInfo__$paramnames)) text__ <- paste0(text__,"  ",paramInfo__$paramnames[k]," <- parameters[",k,"]\n")
  for (k in seq_along(stateInfo__$statenames)) text__ <- paste0(text__,"  ",stateInfo__$statenames[k]," <- ",stateInfo__$stateICs[k],"\n")
  for (kstate__ in seq_along(stateInfo__$statenames)) {
    for (kpar__ in seq_along(sensParams)) {
      text__ <- paste0(text__,"  ",stateInfo__$statenames[kstate__],"_",sensParams[kpar__]," <- ",allDerivICs__[kstate__,kpar__],"\n")
    }
  }
  text__ <- paste0(text__,"  out <- c()\n")
  for (kstate__ in seq_along(stateInfo__$statenames)) {
    for (kpar__ in seq_along(sensParams)) {
      text__ <- paste0(text__,'  out["',stateInfo__$statenames[kstate__],"_",sensParams[kpar__],'"] <- ',stateInfo__$statenames[kstate__],"_",sensParams[kpar__],"\n")
    }
  }
  text__ <- paste0(text__,"  if (sum(as.double(is.na(out))) > 0) stopIQR('Non-numerical sensitivity initial conditions are wrongly defined and non-evaluable.')\n")
  text__ <- paste0(text__,"  return(out)\n")
  text__ <- paste0(text__,"}\n")
  return(eval(parse(text=text__)))
}
exchangeInterp0 <- function (text) {
  if (is.null(aux_strFindAll(text,"interp0(")$start)) return(text)
  if (length(aux_strFindAll(text,"interp0(")$start) > 1)
    stopIQR("The interp0 function is only allowed to be present once in each formula.")
  textnew__ <- text
  indexstart__ <- aux_strFindAll(text,"interp0(")$end+1
  pc__ <- 1
  cstart__ <- indexstart__
  cend__ <- cstart__
  while (pc__ != 0) {
    cend__ <- cend__ + 1
    if (substr(textnew__,cend__,cend__) == '(') {
      pc__ <- pc__+1
    } else {
      if (substr(textnew__,cend__,cend__) == ')') {
        pc__ <- pc__-1
      }
    }
  }
  indexend__ <- cend__-1
  indexafter__ <- indexend__+1
  textinside__ <- substr(textnew__,indexstart__,indexend__)
  terms__ <- aux_explodePC(textinside__,separator=",",group="square")
  xtermstring__ <- aux_strtrim(terms__[1])
  ytermstring__ <- aux_strtrim(terms__[2])
  xtermstring__ = substr(xtermstring__,2,nchar(xtermstring__)-1)
  ytermstring__ = substr(ytermstring__,2,nchar(ytermstring__)-1)
  xtermelements__ = aux_explodePC(xtermstring__)
  ytermelements__ = aux_explodePC(ytermstring__)
  if (length(xtermelements__) < 3) {
    stopIQR('The interp0 function requires at least 3 points on the x and y axis.')
  }
  if (length(xtermelements__) != length(ytermelements__)) {
    stopIQR('x and y arguments for interp0 function do not have same number of elements.');
  }
  pwText__ <- paste0(ytermelements__[1],",lt(",terms__[3],",",xtermelements__[1],"),")
  for (k in 2:length(xtermelements__)-1) {
    pwText__ <- paste0(pwText__,"(",ytermelements__[k],")")
    if (k<length(xtermelements__)-1) {
      pwText__ <- paste0(pwText__,",andIQR(lt(",terms__[3],",",xtermelements__[k+1],"),ge(",terms__[3],",",xtermelements__[k],")),")
    }
  }
  pwText__ <- paste0(pwText__,",andIQR(lt(",terms__[3],",",xtermelements__[length(xtermelements__)],"),ge(",terms__[3],",",xtermelements__[length(xtermelements__)-1],")),(",ytermelements__[length(ytermelements__)],")")
  textnew__ <- paste(substr(text,1,indexstart__-1), pwText__, substr(text,indexend__+1,nchar(text)))
  textnew__ <- aux_strrep(textnew__,'interp0','piecewiseIQR')
  return(textnew__)
}
exchangeInterp1 <- function (text) {
  if (is.null(aux_strFindAll(text,"interp1(")$start)) return(text)
  if (length(aux_strFindAll(text,"interp1(")$start) > 1)
    stopIQR("The interp1 function is only allowed to be present once in each formula.")
  textnew__ <- text
  indexstart__ <- aux_strFindAll(text,"interp1(")$end+1
  pc__ <- 1
  cstart__ <- indexstart__
  cend__ <- cstart__
  while (pc__ != 0) {
    cend__ <- cend__ + 1
    if (substr(textnew__,cend__,cend__) == '(') {
      pc__ <- pc__+1
    } else {
      if (substr(textnew__,cend__,cend__) == ')') {
        pc__ <- pc__-1
      }
    }
  }
  indexend__ <- cend__-1
  indexafter__ <- indexend__+1
  textinside__ <- substr(textnew__,indexstart__,indexend__)
  terms__ <- aux_explodePC(textinside__,separator=",",group="square")
  xtermstring__ <- aux_strtrim(terms__[1])
  ytermstring__ <- aux_strtrim(terms__[2])
  xtermstring__ = substr(xtermstring__,2,nchar(xtermstring__)-1)
  ytermstring__ = substr(ytermstring__,2,nchar(ytermstring__)-1)
  xtermelements__ = aux_explodePC(xtermstring__)
  ytermelements__ = aux_explodePC(ytermstring__)
  if (length(xtermelements__) < 3) {
    stopIQR('The interp1 function requires at least 3 points on the x and y axis.')
  }
  if (length(xtermelements__) != length(ytermelements__)) {
    stopIQR('x and y arguments for interp1 function do not have same number of elements.');
  }
  pwText__ <- paste0(ytermelements__[1],",lt(",terms__[3],",",xtermelements__[1],"),")
  for (k in 2:length(xtermelements__)-1) {
    pwText__ <- paste0(pwText__,"(",ytermelements__[k+1],"-(",ytermelements__[k],"))/(",xtermelements__[k+1],"-(",xtermelements__[k],"))*(",terms__[3],"-(",xtermelements__[k],"))+(",ytermelements__[k],")")
    if (k<length(xtermelements__)-1) {
      pwText__ <- paste0(pwText__,",andIQR(lt(",terms__[3],",",xtermelements__[k+1],"),ge(",terms__[3],",",xtermelements__[k],")),")
    }
  }
  pwText__ <- paste0(pwText__,",andIQR(lt(",terms__[3],",",xtermelements__[length(xtermelements__)],"),ge(",terms__[3],",",xtermelements__[length(xtermelements__)-1],")),(",ytermelements__[length(ytermelements__)],")")
  textnew__ <- paste(substr(text,1,indexstart__-1), pwText__, substr(text,indexend__+1,nchar(text)))
  textnew__ <- aux_strrep(textnew__,'interp1','piecewiseIQR')
  return(textnew__)
}
exchangeInterpcs <- function (text) {
  if (is.null(aux_strFindAll(text,"interpcsIQR(")$start)) return(text)
  if (length(aux_strFindAll(text,"interpcsIQR(")$start) > 1)
    stopIQR("The interpcs function is only allowed to be present once in each formula.")
  textnew__ <- text
  indexstart__ <- aux_strFindAll(text,"interpcsIQR(")$end+1
  pc__ <- 1
  cstart__ <- indexstart__
  cend__ <- cstart__
  while (pc__ != 0) {
    cend__ <- cend__ + 1
    if (substr(textnew__,cend__,cend__) == '(') {
      pc__ <- pc__+1
    } else {
      if (substr(textnew__,cend__,cend__) == ')') {
        pc__ <- pc__-1
      }
    }
  }
  indexend__ <- cend__-1
  indexafter__ <- indexend__+1
  textinside__ <- substr(textnew__,indexstart__,indexend__)
  terms__ <- aux_explodePC(textinside__,separator=",",group="square")
  xtermstring__ <- aux_strtrim(terms__[1])
  ytermstring__ <- aux_strtrim(terms__[2])
  xtermstring__ = substr(xtermstring__,2,nchar(xtermstring__)-1)
  ytermstring__ = substr(ytermstring__,2,nchar(ytermstring__)-1)
  xtermelements__ = aux_explodePC(xtermstring__)
  ytermelements__ = aux_explodePC(ytermstring__)
  if (length(xtermelements__) < 3) {
    stopIQR('The interpcs function requires at least 3 points on the x and y axis.')
  }
  if (length(xtermelements__) != length(ytermelements__)) {
    stopIQR('x and y arguments for interpcs function do not have same number of elements.');
  }
  newexpr <- paste0(length(xtermelements__),",",terms__[3])
  newexpr <- paste0(newexpr,',',xtermstring__,',',ytermstring__)
  textnew__ <- paste0(substr(text,1,indexstart__-1), newexpr, substr(text,indexend__+1,nchar(text)))
  return(textnew__)
}
clean_loadedDLLs <- function(basename = "IQRmodel") {
  loadSetupOptions_IQRtools()
  x__ <- unclass(getLoadedDLLs())
  ix_IQRmodel__ <- which(grepl(paste0("^", basename, "_"), names(x__)))
  if (length(ix_IQRmodel__) >= .MAX_NUMBER_IQRMODEL_DLLS_LOADED) {
    y__ <- unclass(x__[[ix_IQRmodel__[1]]])
    DLLpath__ <- y__$path
    dyn.unload(DLLpath__)
  }
}
compile_IQRmodel <- function(model) {
  loadSetupOptions_IQRtools()
  x__ <- unclass(getLoadedDLLs())
  ix_IQRmodel__ <- which(grepl("IQRmodel_",names(x__)))
  if (length(ix_IQRmodel__) >= .MAX_NUMBER_IQRMODEL_DLLS_LOADED) {
    y__ <- unclass(x__[[ix_IQRmodel__[1]]])
    DLLpath__ <- y__$path
    dyn.unload(DLLpath__)
  }
  tempFilePath__   <- tempfileIQR()
  xxx__            <- aux_fileparts(tempFilePath__)
  tempFilePath__   <- paste0(xxx__$pathname,"/","IQRmodel_",xxx__$filename,xxx__$fileext)
  tempFilename__   <- aux_fileparts(tempFilePath__)$filename
  oldpath__        <- getwd()
  setwd(aux_fileparts(tempFilePath__)$pathname)
  exportC_IQRmodel(model,tempFilename__)
  includesPaths__  <- .libPaths()
  if (.LIBPATHS_FIRSTONLY) {
    includesPaths__ <- includesPaths__[1]
  }
  includesLocDef__ <- paste0('PKG_CPPFLAGS =')
  for (k in seq_along(includesPaths__)) includesLocDef__ <- paste0(includesLocDef__,' -I"',includesPaths__[k],'/IQRtools/solver/include/"')
  includesLocDef__ <- paste0(includesLocDef__,' -w')
  includesLocDef__ <- paste0(includesLocDef__,'\nPKG_LIBS = -lm')
  aux_filewrite(text=includesLocDef__,filename="Makevars")
  system(paste0("R CMD SHLIB ",tempFilename__,".c >xxx"))
  tryCatch({
    if (.Platform$OS.type=="unix") {
      dyn.load(paste0(tempFilename__,".so"))
    } else {
      dyn.load(tempFilename__)
    }
  }, error = function(err) {
    setwd(oldpath__)
    stopIQR("Compilation of model led to an error. Please check the above output and correct the model.")
  })
  setwd(oldpath__)
  model_func_ptr__ <- getNativeSymbolInfo("model",PACKAGE=tempFilename__)$address
  attr(model,"DLLpath.IQRmodel") <- tempFilePath__
  attr(model,"DLLname.IQRmodel") <- tempFilename__
  attr(model,"address.IQRmodel") <- model_func_ptr__
  return(model)
}
dealFormulas <- function (formulaArray__) {
  oldElements__ = c('\\bnthroot\\b','\\band\\b','\\bor\\b','\\babs\\b','\\bindexmax\\b','\\bmin\\b','\\bmax\\b','\\bpiecewise\\b','\\binterpcs\\b')
  newElements__ = c('nthrootIQR','andIQR','orIQR','absIQR','indexmaxIQR','minIQR','maxIQR','piecewiseIQR','interpcsIQR')
  for (k in seq_along(oldElements__)) formulaArray__ <- gsub(oldElements__[k],newElements__[k],formulaArray__)
  formulaArray__ <- gsub('\\<power\\(','pow(',formulaArray__)
  for (k in seq_along(formulaArray__)) {
    formula <- exchangeInterp0(formulaArray__[k])
    formula <- exchangeInterp1(formula)
    formula <- exchangeInterpcs(formula)
    formula <- convertPowerOperator(formula)
    formula <- gsub("(\\b[0-9.]+)","\\1.0",formula)
    formula <- gsub("\\.0\\.",".",formula)
    formula <- gsub("(\\.[0-9]+)\\.0","\\1",formula)
    formula <- gsub("(E\\.0)","E",formula)
    formula <- gsub("([0-9]E-\\d*)\\.0","\\1",formula)
    formula <- gsub("([0-9]E\\+\\d*)\\.0","\\1",formula)
    formula <- gsub("([0-9]E\\d*)\\.0","\\1",formula)
    formula <- gsub("(e\\.0)","e",formula)
    formula <- gsub("([0-9]e-\\d*)\\.0","\\1",formula)
    formula <- gsub("([0-9]e\\+\\d*)\\.0","\\1",formula)
    formula <- gsub("([0-9]e\\d*)\\.0","\\1",formula)
    fixElements = c('\\<indexmaxIQR\\>','\\<minIQR\\>','\\<maxIQR\\>','\\<andIQR\\>','\\<orIQR\\>','\\<piecewiseIQR\\>','\\<interpcsIQR\\>')
    for (k1__ in 1:length(fixElements)) {
      index__ <- unlist(gregexpr(pattern=fixElements[k1__],formula))
      for (k2__ in seq_along(index__)) {
        if (index__[k2__] != -1) {
          ixStart <- index__[k2__]+nchar(fixElements[k1__])-4
          ixEnd <- ixStart
          parOpen <- 1
          while (parOpen != 0) {
            ixEnd <- ixEnd + 1
            if (substr(formula,ixEnd,ixEnd) == '(') {
              parOpen <- parOpen + 1
            } else {
              if (substr(formula,ixEnd,ixEnd) == ')') {
                parOpen <- parOpen - 1
              }
            }
          }
          command <- fixElements[k1__]
          oldarguments <- substr(formula,ixStart+1,ixEnd-1)
          oldargumentsReplace <- oldarguments
          if (command %in% c("\\<interpcsIQR\\>")) {
            oldarguments <- sub("([0-9]+).0","\\1",oldarguments)
          }
          newargstring <- paste0(length(aux_explodePC(oldarguments)),",",oldarguments)
          oldrep  <- paste0(substr(command,3,nchar(command)-2), '(', oldargumentsReplace, ')')
          newrep <- paste0(substr(command,3,nchar(command)-2), '(', newargstring, ')')
          formula <- aux_strrep(formula,oldrep,newrep)
          index__ <- index__ + nchar(newargstring)-nchar(oldarguments)
        }
      }
    }
    formulaArray__[k] <- formula
  }
  return(formulaArray__)
}
outputDeclarationData <- function(ALLCONTENT__,data) {
  if (length(data)==0) return(ALLCONTENT__)
  nrperrow__ <- 0
  text__ <- ""
  for (k in seq_along(data)) {
    if (nrperrow__==0) text__ <- paste0(text__,'    double ')
    if (k<length(data) & nrperrow__<20-1) {
      text__ <- paste0(text__,data[k],",")
    } else {
      text__ <- paste0(text__,data[k],";")
    }
    nrperrow__ <- nrperrow__ + 1
    if (nrperrow__ == 20) {
      text__ <- paste0(text__,"\n")
      nrperrow__ <- 0
    }
  }
  ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  return(ALLCONTENT__)
}
gettriggerPiecewise <- function(input) {
  trigger__ <- c()
  index__ <- aux_strFindAll(input,'piecewise')$start
  if (is.null(index__)) return(trigger__)
  for (k in seq_along(index__)) {
    work__       <- input
    work__       <- substr(work__,index__[k]+nchar('piecewise')+1,nchar(work__))
    popen      <- 1
    offset     <- 1
    while (popen != 0) {
      if (substr(work__,offset,offset) == '(') popen <- popen + 1
      if (substr(work__,offset,offset) == ')') popen <- popen - 1
      offset   <- offset + 1
    }
    work__ <- substr(work__,1,offset-2)
    terms__ <- aux_explodePC(work__)
    trigger__ <- c(trigger__, terms__[seq(2,length(terms__),2)])
  }
  return(unique(trigger__))
}
convertPowerOperator <- function (formula) {
  formula <- aux_removeWhiteSpace(formula)
  indices__ <- aux_strFindAll(formula,'^')$start
  while (!is.null(indices__)) {
    index__ <- indices__[1]
    formula1 <- substr(formula,1,index__-1)
    formula2 <- substr(formula,index__+1,nchar(formula))
    pc__ <- 0
    cend__ <- nchar(formula1)
    cstart__ <- cend__
    run <- TRUE
    while (run) {
      if (pc__==0) {
        if (substr(formula1,cstart__,cstart__) %in% c("+","-","*","/")) { cstart__ <- cstart__+1; break }
        if (substr(formula1,cstart__,cstart__) == "(") {
          if (cstart__ == 1) { cstart__ <- cstart__+1; break }
          if (substr(formula1,cstart__-1,cstart__-1) %in% c("+","-","*","/","(")) { cstart__ <- cstart__+1; break }
        }
      }
      if (substr(formula1,cstart__,cstart__) == ')') pc__ <- pc__+1
      if (substr(formula1,cstart__,cstart__) == '(') pc__ <- pc__-1
      if (cstart__<=1) break
      cstart__ <- cstart__ - 1
    }
    firstargument <- substr(formula1,cstart__,cend__)
    cendfirst <- cstart__
    pc__ <- 0
    cstart__ <- 1
    cend__ <- cstart__
    run <- TRUE
    while (run) {
      if (cend__ > 1 & pc__==0 & substr(formula2,cend__,cend__) %in% c("+","-","*","/",")","^")) { cend__ <- cend__-1; break }
      if (cend__>=nchar(formula2)) break
      if (substr(formula2,cend__,cend__) == ')') pc__ <- pc__+1
      if (substr(formula2,cend__,cend__) == '(') pc__ <- pc__-1
      cend__ <- cend__ + 1
    }
    secondargument <- substr(formula2,cstart__,cend__)
    cstartsecond <- cend__
    powerexp <- paste0("pow(",firstargument,",",secondargument,")")
    formula <- paste0(substr(formula1,1,cendfirst-1), powerexp, substr(formula2,cstartsecond+1,nchar(formula2)))
    indices__ <- aux_strFindAll(formula,'^')$start
  }
  return(formula)
}
addPiecewiseEvents_IQRmodel <- function(model) {
  triggers__ <- c()
  for (k in seq_along(model$states))
    triggers__    <- c(triggers__, gettriggerPiecewise(model$states[[k]]$ODE))
  for (k in seq_along(model$variables))
    triggers__    <- c(triggers__, gettriggerPiecewise(model$variables[[k]]$formula))
  for (k in seq_along(model$reactions))
    triggers__    <- c(triggers__, gettriggerPiecewise(model$reactions[[k]]$formula))
  triggers__      <- unique(triggers__)
  triggers__ <- handlePulsePiecewiseExpressions(triggers__,'and<ge<time,(.*)>,lt<time,(.*)>>')
  triggers__ <- handlePulsePiecewiseExpressions(triggers__,'and<ge<time,(.*)>,le<time,(.*)>>')
  triggers__ <- handlePulsePiecewiseExpressions(triggers__,'and<gt<time,(.*)>,lt<time,(.*)>>')
  triggers__ <- handlePulsePiecewiseExpressions(triggers__,'and<gt<time,(.*)>,le<time,(.*)>>')
  triggers__ <- handlePulsePiecewiseExpressions(triggers__,'and<lt<time,(.*)>,ge<time,(.*)>>')
  triggers__ <- handlePulsePiecewiseExpressions(triggers__,'and<lt<time,(.*)>,gt<time,(.*)>>')
  triggers__ <- handlePulsePiecewiseExpressions(triggers__,'and<le<time,(.*)>,ge<time,(.*)>>')
  triggers__ <- handlePulsePiecewiseExpressions(triggers__,'and<le<time,(.*)>,gt<time,(.*)>>')
  presentModelEvents__ <- eventsInfo_IQRmodel(model)$evetriggers
  presentModelEvents2__ <- presentModelEvents__
  presentModelEvents2__ <- gsub(x=presentModelEvents2__,pattern="\\<gt\\(","ge(")
  presentModelEvents2__ <- gsub(x=presentModelEvents2__,pattern="\\<lt\\(","le(")
  presentModelEvents3__ <- presentModelEvents__
  presentModelEvents3__ <- gsub(x=presentModelEvents3__,pattern="\\<ge\\(","gt(")
  presentModelEvents3__ <- gsub(x=presentModelEvents3__,pattern="\\<le\\(","lt(")
  triggers__ <- setdiff(triggers__,c(presentModelEvents__,presentModelEvents2__,presentModelEvents3__))
  if (length(triggers__) > 0) {
    sn <- names(model$states)[1]
    for (k in seq_along(triggers__)) {
      eventName <- paste0("piecewise_event_",k)
      model <- addEvent_IQRmodel(model,name=eventName,trigger=triggers__[k],notes='Just a dummy assignment for correct piecewise timing')
      model <- addEventAssignment_IQRmodel(model,eventname=eventName,variable=sn,formula=sn)
    }
  }
  return(model)
}
handlePulsePiecewiseExpressions <- function(triggers__,syntax) {
  usetriggers__ <- c()
  for (k in seq_along(triggers__)) {
    trigger__ <- aux_removeWhiteSpace(triggers__[k])
    x <- aux_strrep(trigger__,'(','<')
    x <- aux_strrep(x,')','>')
    y <- regexpr(text=x,pattern=syntax,perl=TRUE)
    if (y[1] != -1) {
      found_start__  <- attr(y,"capture.start")
      found_length__ <- attr(y,"capture.length")
      trigger1 <- substr(trigger__,found_start__[1],found_start__[1]+found_length__[1]-1)
      trigger2 <- substr(trigger__,found_start__[2],found_start__[2]+found_length__[2]-1)
      usetriggers__ <- c(usetriggers__, paste0("ge(time,",trigger1,")"))
      usetriggers__ <- c(usetriggers__, paste0("ge(time,",trigger2,")"))
    } else {
      usetriggers__ <- c(usetriggers__, trigger__)
    }
  }
  triggers__ <- usetriggers__
  return(triggers__)
}
#'@export
export_IQRmodel <- function (model, filename=NULL, FLAGbc=FALSE) {
  if (FLAGbc) {
    message("For export in BC notation the underlying assumptions are:")
    message("  * All reaction rates are defined in amount/time units")
    message("  * Species ideally in amount and ODEs defined by sum of reaction rates")
    message("    * Stoichiometric factors can be numeric or parameters and need to be define in front of reaction names")
    message("      Example: d/dt(speciesAmount) = 2*reaction1 - reaction2 - 1*reaction3")
  }
  if (!is_IQRmodel(model)) stopIQR("Input argument is not an IQRmodel")
  if (!is.null(filename)) filename <- paste0(aux_strrep(filename,".txt",""), ".txt")
  stoichiometry         <- stoichiometry_IQRmodel(model,raw=FALSE)
  stoichiometricMatrix  <- stoichiometry$N
  stateNamesBiochemical <- stoichiometry$statenames
  stateNamesAll__       <- names(model$states)
  stateNamesODE__       <- setdiff(stateNamesAll__,stateNamesBiochemical)
  ALLCONTENT__ <- ""
  ALLCONTENT__ <- paste0(ALLCONTENT__,"********** MODEL NAME\n\n",model$name,"\n\n")
  ALLCONTENT__ <- paste0(ALLCONTENT__,"********** MODEL NOTES\n\n",model$notes,"\n\n")
  if (!FLAGbc) {
    ALLCONTENT__ <- addStatesODE_IQRmodel(ALLCONTENT__,model)
  } else {
    ALLCONTENT__ <- addStatesBC_IQRmodel(ALLCONTENT__,model,stateNamesODE__)
  }
  ALLCONTENT__ <- addParameters_IQRmodel(ALLCONTENT__,model)
  ALLCONTENT__ <- addVariables_IQRmodel(ALLCONTENT__,model)
  if (!FLAGbc) {
    ALLCONTENT__ <- addReactionsODE_IQRmodel(ALLCONTENT__,model)
  } else {
    ALLCONTENT__ <- addReactionsBC_IQRmodel(ALLCONTENT__,model,stateNamesBiochemical,stoichiometricMatrix)
  }
  ALLCONTENT__ <- addFunctions_IQRmodel(ALLCONTENT__,model)
  ALLCONTENT__ <- addEvents_IQRmodel(ALLCONTENT__,model)
  ALLCONTENT__ <- addCfunctions_IQRmodel(ALLCONTENT__,model)
  ALLCONTENT__ <- addInitialAssignments_IQRmodel(ALLCONTENT__,model)
  aux_filewrite(ALLCONTENT__,filename)
  if (is.null(filename)) return(ALLCONTENT__)
}
addStatesODE_IQRmodel <- function (ALLCONTENT__,model__) {
  ALLCONTENT__ <- paste0(ALLCONTENT__,"********** MODEL STATES\n\n")
  for (k in seq_along(model__$states)) {
    text__ <- paste0("d/dt(",names(model__$states)[k],") = ",model__$states[[k]]$ODE)
    text__ <- addSBMLinfo_IQRmodel(text__,model__$states,k)
    text__ <- addNotes_IQRmodel(text__,model__$states,k)
    ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  }
  ALLCONTENT__ <- paste0(ALLCONTENT__," \n")
  for (k in seq_along(model__$states)) {
    ALLCONTENT__ <- paste0(ALLCONTENT__,names(model__$states)[k],"(0) = ",model__$states[[k]]$IC,"\n")
  }
  ALLCONTENT__ <- paste0(ALLCONTENT__," \n")
}
addStatesBC_IQRmodel <- function (ALLCONTENT__,model__,stateNamesODE__) {
  ALLCONTENT__ <- paste0(ALLCONTENT__,"********** MODEL STATE INFORMATION\n\n")
  for (k in seq_along(stateNamesODE__)) {
    index        <- aux_strmatch(names(model__$states),stateNamesODE__[k])
    text__       <- paste0("d/dt(",names(model__$states)[index],") = ",model__$states[[index]]$ODE)
    text__       <- addSBMLinfo_IQRmodel(text__,model__$states,index)
    text__       <- addNotes_IQRmodel(text__,model__$states,index)
    ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  }
  ALLCONTENT__   <- paste0(ALLCONTENT__," \n")
  for (k in seq_along(model__$states)) {
    text__ <- paste0(names(model__$states)[k],"(0) = ",model__$states[[k]]$IC)
    if (!(names(model__$states)[k] %in% stateNamesODE__)) {
      text__       <- addSBMLinfo_IQRmodel(text__,model__$states,k)
      text__       <- addNotes_IQRmodel(text__,model__$states,k)
    }
    ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  }
  ALLCONTENT__ <- paste0(ALLCONTENT__," \n")
}
addParameters_IQRmodel <- function (ALLCONTENT__,model__) {
  ALLCONTENT__   <- paste0(ALLCONTENT__,"********** MODEL PARAMETERS\n\n")
  for (k in seq_along(model__$parameters)) {
    text__       <- paste0(names(model__$parameters)[k]," = ",model__$parameters[[k]]$value)
    text__       <- addSBMLinfo_IQRmodel(text__,model__$parameters,k)
    text__       <- addNotes_IQRmodel(text__,model__$parameters,k)
    ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  }
  ALLCONTENT__   <- paste0(ALLCONTENT__," \n")
  return(ALLCONTENT__)
}
addVariables_IQRmodel <- function (ALLCONTENT__,model__) {
  ALLCONTENT__   <- paste0(ALLCONTENT__,"********** MODEL VARIABLES\n\n")
  for (k in seq_along(model__$variables)) {
    text__       <- paste0(names(model__$variables)[k]," = ",model__$variables[[k]]$formula)
    text__       <- addSBMLinfo_IQRmodel(text__,model__$variables,k)
    text__       <- addNotes_IQRmodel(text__,model__$variables,k)
    ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  }
  ALLCONTENT__   <- paste0(ALLCONTENT__," \n")
  return(ALLCONTENT__)
}
addReactionsODE_IQRmodel <- function (ALLCONTENT__,model__) {
  ALLCONTENT__   <- paste0(ALLCONTENT__,"********** MODEL REACTIONS\n\n")
  for (k in seq_along(model__$reactions)) {
    text__       <- paste0(names(model__$reactions)[k]," = ",model__$reactions[[k]]$formula)
    if (model__$reactions[[k]]$reversible) text__ <- paste(text__,"{reversible}")
    if (model__$reactions[[k]]$fast) text__ <- paste(text__,"{fast}")
    text__       <- addNotes_IQRmodel(text__,model__$reactions,k)
    ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  }
  ALLCONTENT__   <- paste0(ALLCONTENT__," \n")
  return(ALLCONTENT__)
}
addReactionsBC_IQRmodel <- function (ALLCONTENT__,model__,stateNamesBiochemical,stoichiometricMatrix) {
  ALLCONTENT__ <- paste0(ALLCONTENT__,"********** MODEL REACTIONS\n\n")
  reacInfo__                     <- reactionsInfo_IQRmodel(model__)
  if (!is.null(stoichiometricMatrix)) {
    for (k1 in 1:dim(stoichiometricMatrix)[2]) {
      Ncol                       <- stoichiometricMatrix[,k1]
      substrateIndices           <- unname(which(Ncol < 0))
      productIndices             <- unname(which(Ncol > 0))
      substrateNames__           <- stateNamesBiochemical[substrateIndices]
      productNames__             <- stateNamesBiochemical[productIndices]
      substrateStoichiometries__ <- abs(Ncol[substrateIndices])
      productStoichiometries__   <- abs(Ncol[productIndices])
      if (reacInfo__$reacreversible[k1]) {
        irreversibleRates__      <- aux_explodePC(reacInfo__$reacformulas[k1],'-')
        if (length(irreversibleRates__) != 2) {
          reacInfo__$reacreversible[k1]      <- FALSE
        } else {
          reactionForward        <- irreversibleRates__[1]
          reactionReverse        <- irreversibleRates__[2]
        }
      }
      text__ <- ""
      if (length(substrateNames__) > 0) {
        if (substrateStoichiometries__[1] != 1) {
          text__             <- paste0(text__,substrateStoichiometries__[1],"*",substrateNames__[1])
        } else {
          text__             <- paste0(text__,substrateNames__[1])
        }
      }
      if (length(substrateNames__) > 1) {
        for (k2 in 2:length(substrateNames__)) {
          if (substrateStoichiometries__[k2] != 1) {
            text__           <- paste0(text__,"+",substrateStoichiometries__[k2],"*",substrateNames__[k2])
          } else {
            text__           <- paste0(text__,"+",substrateNames__[k2])
          }
        }
      }
      if (reacInfo__$reacreversible[k1]) {
        text__               <- paste0(text__," <=> ")
      } else {
        text__               <- paste0(text__," => ")
      }
      if (length(productNames__) > 0) {
        if (productStoichiometries__[1] != 1) {
          text__             <- paste0(text__,productStoichiometries__[1],"*",productNames__[1])
        } else {
          text__             <- paste0(text__,productNames__[1])
        }
      }
      if (length(productNames__) > 1) {
        for (k2 in 2:length(productNames__)) {
          if (productStoichiometries__[k2] != 1) {
            text__           <- paste0(text__,"+",productStoichiometries__[k2],"*",productNames__[k2])
          } else {
            text__           <- paste0(text__,"+",productNames__[k2])
          }
        }
      }
      text__                 <- paste0(text__," : ",reacInfo__$reacnames[k1])
      if (reacInfo__$reacfast[k1]) text__ <- paste0(text__," {fast}")
      if (!is.null(model__$reactions[[k1]]$notes)) text__ <- paste0(text__," # ",model__$reactions[[k1]]$notes)
      text__                 <- paste0(text__,"\n")
      if (!reacInfo__$reacreversible[k1]) {
        text__               <- paste0(text__,"\tvf = ",reacInfo__$reacformulas[k1],"\n")
      } else {
        text__               <- paste0(text__,"\tvf = ",reactionForward,"\n\tvr = ",reactionReverse,"\n")
      }
      ALLCONTENT__               <- paste0(ALLCONTENT__,text__,"\n")
    }
  } else {
    ALLCONTENT__ <- paste0(ALLCONTENT__," \n")
  }
  return(ALLCONTENT__)
}
addFunctions_IQRmodel <- function(ALLCONTENT__,model__) {
  ALLCONTENT__   <- paste0(ALLCONTENT__,"********** MODEL FUNCTIONS\n\n")
  for (k in seq_along(model__$functions)) {
    text__       <- paste0(names(model__$functions)[k],"(",model__$functions[[k]]$arguments,") = ",model__$functions[[k]]$formula)
    text__       <- addNotes_IQRmodel(text__,model__$functions,k)
    ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  }
  ALLCONTENT__   <- paste0(ALLCONTENT__," \n")
  return(ALLCONTENT__)
}
addEvents_IQRmodel <- function(ALLCONTENT__,model__) {
  ALLCONTENT__   <- paste0(ALLCONTENT__,"********** MODEL EVENTS\n\n")
  for (k in seq_along(model__$events)) {
    text__       <- paste0(names(model__$events)[k]," = ",model__$events[[k]]$trigger)
    for (k2 in seq_along(model__$events[[k]]$assignment)) {
      text__ <- paste0(text__,",",model__$events[[k]]$assignment[[k2]]$variable,",",model__$events[[k]]$assignment[[k2]]$formula)
    }
    text__       <- addNotes_IQRmodel(text__,model__$events,k)
    ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  }
  ALLCONTENT__ <- paste0(ALLCONTENT__," \n")
  return(ALLCONTENT__)
}
addCfunctions_IQRmodel <- function(ALLCONTENT__,model__) {
  if (!is.null(model__$Cfunctions)) {
    if (nchar(aux_strtrim(model__$Cfunctions)) > 0) {
      ALLCONTENT__ <- paste0(ALLCONTENT__,"********** MODEL C FUNCTIONS\n\n")
      ALLCONTENT__ <- paste0(ALLCONTENT__,model__$Cfunctions,"\n ")
    }
  }
  return(ALLCONTENT__)
}
addInitialAssignments_IQRmodel <- function(ALLCONTENT__,model__) {
  ALLCONTENT__   <- paste0(ALLCONTENT__,"********** MODEL INITIAL ASSIGNMENTS\n\n")
  for (k in seq_along(model__$initalAssignments)) {
    text__       <- paste0(names(model__$initalAssignments)[k]," = ",model__$initalAssignments[[k]]$formula)
    text__       <- addNotes_IQRmodel(text__,model__$initalAssignments,k)
    ALLCONTENT__ <- paste0(ALLCONTENT__,text__,"\n")
  }
  ALLCONTENT__   <- paste0(ALLCONTENT__," \n")
  return(ALLCONTENT__)
}
addSBMLinfo_IQRmodel <- function(text__,modelElement__,k) {
  type        <- modelElement__[[k]]$type
  compartment <- modelElement__[[k]]$compartment
  unittype    <- modelElement__[[k]]$unittype
  SBMLinfo__ <- ""
  if (!is.null(type) | !is.null(compartment) | !is.null(unittype)) {
    if (type=="isSpecie" & !is.null(compartment) && (unittype=="amount" | unittype=="concentration"))
      SBMLinfo__ <- paste0(" {",type,":",compartment,":",unittype,"}")
    if (type=="isParameter" & is.null(compartment) & is.null(unittype))
      SBMLinfo__ <- paste0(" {",type,"}")
    if (type=="isCompartment" & is.null(unittype))
      SBMLinfo__ <- paste0(" {",type,":",compartment,"}")
    if (SBMLinfo__=="") {
      stopIQR(paste0("Type information for ",names(modelElement__)[k]," seems to be wrong."))
    }
  }
  text__ <- aux_strtrim(paste0(text__,SBMLinfo__))
  return(text__)
}
addNotes_IQRmodel <- function(text__,modelElement__,k) {
  if (!is.null(modelElement__[[k]]$notes)) {
    text__ <- aux_strtrim(paste(text__,"#",modelElement__[[k]]$notes,sep=" "))
  }
  return(text__)
}
import_IQRmodel <- function(input,FLAGtextIQRmodel=FALSE) {
  if (FLAGtextIQRmodel) {
    modelText__ <- input
  } else {
    filename__ <- input
    suppressWarnings(modelText__ <- aux_fileread(filename__,collapserows=FALSE))
  }
  modelText__ <- gsub("^\\s*%.*$", "", modelText__)
  modelText__ <- gsub("^\\s*#.*$", "", modelText__)
  modelText__ <- gsub("^\\s+$", "", modelText__)
  modelText__ <- modelText__[modelText__!=""]
  if (!any(grepl("********** MODEL NAME",modelText__,fixed = TRUE))) stopIQR("The '********** MODEL NAME' identifier is not present in the model")
  if (!any(grepl("********** MODEL NOTES",modelText__,fixed = TRUE))) stopIQR("The '********** MODEL NOTES' identifier is not present in the model")
  if (!any(grepl("********** MODEL PARAMETERS",modelText__,fixed = TRUE))) stopIQR("The '********** MODEL PARAMETERS' identifier is not present in the model")
  if (!any(grepl("********** MODEL VARIABLES",modelText__,fixed = TRUE))) stopIQR("The '********** MODEL VARIABLES' identifier is not present in the model")
  if (!any(grepl("********** MODEL REACTIONS",modelText__,fixed = TRUE))) stopIQR("The '********** MODEL REACTIONS' identifier is not present in the model")
  if (!any(grepl("********** MODEL FUNCTIONS",modelText__,fixed = TRUE))) stopIQR("The '********** MODEL FUNCTIONS' identifier is not present in the model")
  if (!any(grepl("********** MODEL EVENTS",modelText__,fixed = TRUE))) stopIQR("The '********** MODEL EVENTS' identifier is not present in the model")
  if (!any(grepl("********** MODEL STATES",modelText__,fixed = TRUE))
      &
      !any(grepl("********** MODEL STATE INFORMATION",modelText__,fixed = TRUE)))
    stopIQR("Neither the '********** MODEL STATES' nor the '********** MODEL STATE INFORMATION' identifier is present in the model (one of them is needed)")
  if (any(grepl("********** MODEL STATES",modelText__,fixed = TRUE))
      &
      any(grepl("********** MODEL STATE INFORMATION",modelText__,fixed = TRUE)))
    stopIQR("Both the '********** MODEL STATES' and the '********** MODEL STATE INFORMATION' identifier is present in the model (only one is allowed)")
  if (any(grepl("********** MODEL MATLAB FUNCTIONS",modelText__,fixed = TRUE)))
    stopIQR("'MODEL MATLAB FUNCTIONS' identifier present in the model.\nThis is not allowed - please take it out")
  sectionStarts__ <- which(grepl("********** MODEL",modelText__,fixed = TRUE))
  sectionLimits__ <- c(sectionStarts__, length(modelText__)+1)
  sections__ <- lapply(1:(length(sectionLimits__)-1), function (k__) {
    modelText__[sectionLimits__[k__]:(sectionLimits__[k__+1]-1)]
  })
  namesSections__ <- sapply(sections__,function (s__) s__[1])
  names(sections__) <- namesSections__
  model_name__        <- sections__$`********** MODEL NAME`
  model_notes__       <- sections__$`********** MODEL NOTES`
  if ("********** MODEL STATES" %in% namesSections__) {
    model_states__    <- sections__$`********** MODEL STATES`
    FLAGodeModel      <- TRUE
  } else {
    model_states__    <- sections__$`********** MODEL STATE INFORMATION`
    FLAGodeModel      <- FALSE
  }
  model_parameters__  <- sections__$`********** MODEL PARAMETERS`
  model_variables__   <- sections__$`********** MODEL VARIABLES`
  model_reactions__   <- sections__$`********** MODEL REACTIONS`
  model_functions__   <- sections__$`********** MODEL FUNCTIONS`
  model_events__      <- sections__$`********** MODEL EVENTS`
  if ("********** MODEL C FUNCTIONS" %in% namesSections__) {
    model_Cfunctions__ <- sections__$`********** MODEL C FUNCTIONS`
  } else {
    model_Cfunctions__ <- "********** MODEL C FUNCTIONS"
  }
  if ("********** MODEL INITIAL ASSIGNMENTS" %in% namesSections__) {
    model_initialassignments__ <- sections__$`********** MODEL INITIAL ASSIGNMENTS`
  } else {
    model_initialassignments__ <- "********** MODEL INITIAL ASSIGNMENTS"
  }
  if (length(model_name__) == 1) {
    model_name__ <- "Please provide a model name in one line"
  } else {
    model_name__ <- model_name__[2:length(model_name__)]
  }
  if (length(model_notes__) == 1) {
    model_notes__ <- "Please provide some model information\nMultiple lines possible"
  } else {
    model_notes__ <- model_notes__[2:length(model_notes__)]
  }
  if (length(model_states__) == 1) {
    model_states__ <- NULL
  } else {
    model_states__ <- model_states__[2:length(model_states__)]
  }
  if (length(model_parameters__) == 1) {
    model_parameters__ <- NULL
  } else {
    model_parameters__ <- model_parameters__[2:length(model_parameters__)]
  }
  if (length(model_variables__) == 1) {
    model_variables__ <- NULL
  } else {
    model_variables__ <- model_variables__[2:length(model_variables__)]
  }
  if (length(model_reactions__) == 1) {
    model_reactions__ <- NULL
  } else {
    model_reactions__ <- model_reactions__[2:length(model_reactions__)]
  }
  if (length(model_functions__) == 1) {
    model_functions__ <- NULL
  } else {
    model_functions__ <- model_functions__[2:length(model_functions__)]
  }
  if (length(model_events__) == 1) {
    model_events__ <- NULL
  } else {
    model_events__ <- model_events__[2:length(model_events__)]
  }
  if (length(model_Cfunctions__) == 1) {
    model_Cfunctions__ <- NULL
  } else {
    model_Cfunctions__ <- model_Cfunctions__[2:length(model_Cfunctions__)]
    model_Cfunctions__ <- paste0(aux_strtrim(model_Cfunctions__),collapse="\n")
  }
  if (length(model_initialassignments__) == 1) {
    model_initialassignments__ <- NULL
  } else {
    model_initialassignments__ <- model_initialassignments__[2:length(model_initialassignments__)]
  }
  model       <- init_IQRmodel()
  model$name  <- paste0(aux_strtrim(model_name__),collapse=" ")
  model$notes <- paste0(aux_strtrim(model_notes__),collapse="\n")
  model       <- parseParameters_IQRmodelText(model,model_parameters__)
  model       <- parseVariables_IQRmodelText(model,model_variables__)
  model       <- parseFunctions_IQRmodelText(model,model_functions__)
  model       <- parseEvents_IQRmodelText(model,model_events__)
  if (FLAGodeModel) {
    model              <- parseStatesODE_IQRmodelText(model,model_states__)
    model              <- parseReactionsODE_IQRmodelText(model,model_reactions__)
  } else {
    info__             <- parseReactionsBC_IQRmodelText(model,model_reactions__)
    model              <- info__$model
    model_states__     <- parseStatesBC_IQRmodelText(info__,model_states__)
    model              <- parseStatesODE_IQRmodelText(model,model_states__)
  }
  model       <- parseOutputs_IQRmodelText(model)
  model       <- parseInputs_IQRmodelText(model)
  model$Cfunctions <- model_Cfunctions__
  model       <- parseInitialAssigments_IQRmodelText(model,model_initialassignments__)
  return(model)
}
parseParameters_IQRmodelText <- function(model,model_parameters__) {
  for (k in seq_along(model_parameters__)) {
    parameterString   <- aux_strtrim(model_parameters__[k])
    commentInfo__     <- parseNotes_IQRmodelText(parameterString)
    parameterString   <- commentInfo__$main
    notesk__          <- commentInfo__$comment
    SBMLinfo__        <- parseSBMLinfo_IQRmodelText(parameterString,"parameter")
    typek__           <- SBMLinfo__$type
    compartmentk__    <- SBMLinfo__$compartment
    unittypek__       <- SBMLinfo__$unittype
    parameterString   <- SBMLinfo__$textString__
    temp__            <- regexpr("=", parameterString,fixed=TRUE)
    test__            <- aux_strtrim(substr(parameterString,1,(temp__[1]-1)))
    if (nchar(test__) == 0) stopIQR("At least one parameter name not given:\n", parameterString)
    namek__           <- aux_removeWhiteSpace(test__)
    valuek__          <- aux_strtrim(substr(parameterString,(temp__+1),nchar(parameterString)))
    if (nchar(valuek__) == 0) stopIQR("At least one parameter definition not given:\n", parameterString)
    model             <- addParameter_IQRmodel(model,
                                               name=namek__,
                                               value=as.numeric(valuek__),
                                               notes=notesk__,
                                               type=typek__,
                                               compartment=compartmentk__,
                                               unittype=unittypek__)
  }
  return(model)
}
parseVariables_IQRmodelText <- function(model,model_variables__) {
  for (k in seq_along(model_variables__)) {
    variableString    <- aux_strtrim(model_variables__[k])
    commentInfo__     <- parseNotes_IQRmodelText(variableString)
    variableString    <- commentInfo__$main
    notesk__          <- commentInfo__$comment
    SBMLinfo__        <- parseSBMLinfo_IQRmodelText(variableString,"variable")
    typek__           <- SBMLinfo__$type
    compartmentk__    <- SBMLinfo__$compartment
    unittypek__       <- SBMLinfo__$unittype
    variableString    <- SBMLinfo__$textString__
    temp__            <- regexpr("=", variableString,fixed=TRUE)
    test__            <- aux_strtrim(substr(variableString,1,(temp__[1]-1)))
    if (nchar(test__) == 0) stopIQR("At least one variable name not given")
    namek__           <- aux_removeWhiteSpace(test__)
    formulak__        <- aux_strtrim(substr(variableString,(temp__+1),nchar(variableString)))
    if (nchar(formulak__) == 0) stopIQR("At least one variable definition not given")
    model             <- addVariable_IQRmodel(model,
                                              name=namek__,
                                              formula=formulak__,
                                              notes=notesk__,
                                              type=typek__,
                                              compartment=compartmentk__,
                                              unittype=unittypek__)
  }
  return(model)
}
parseInitialAssigments_IQRmodelText <- function(model,model_initialassignments__) {
  for (k in seq_along(model_initialassignments__)) {
    variableString    <- aux_strtrim(model_initialassignments__[k])
    commentInfo__     <- parseNotes_IQRmodelText(variableString)
    elementString     <- commentInfo__$main
    notesk__          <- commentInfo__$comment
    temp__            <- regexpr("=", elementString,fixed=TRUE)
    test__            <- aux_strtrim(substr(elementString,1,(temp__[1]-1)))
    if (nchar(test__) == 0) stopIQR("At least one element name for initial assignments not given")
    namek__           <- aux_removeWhiteSpace(test__)
    formulak__        <- aux_strtrim(substr(elementString,(temp__+1),nchar(variableString)))
    if (nchar(formulak__) == 0) stopIQR("At least one element definition for initial assignments not given")
    model             <- addInitialAssignment_IQRmodel(model,
                                                       name=namek__,
                                                       formula=formulak__,
                                                       notes=notesk__)
  }
  return(model)
}
parseFunctions_IQRmodelText <- function(model,model_functions__) {
  for (k in seq_along(model_functions__)) {
    functionString    <- aux_strtrim(model_functions__[k])
    commentInfo__     <- parseNotes_IQRmodelText(functionString)
    functionString    <- commentInfo__$main
    notesk__          <- commentInfo__$comment
    temp__            <- regexpr("(", functionString,fixed=TRUE)
    namek__           <- aux_strtrim(substr(functionString,1,(temp__[1]-1)))
    if (nchar(namek__) == 0) stopIQR("Check if all function names provided")
    namek__           <- aux_removeWhiteSpace(namek__)
    temp2__           <- regexpr(")", functionString,fixed=TRUE)
    test__            <- aux_strtrim(substr(functionString,(temp__[1]+1),(temp2__[1]-1)))
    if (nchar(test__) == 0)
      stopIQR("Check if all functions have arguments")
    argumentsk__      <- aux_removeWhiteSpace(test__)
    temp3__           <- regexpr("=", functionString)
    formulak__        <- aux_strtrim(substr(functionString,(temp3__[1]+1),nchar(functionString)))
    if(nchar(formulak__) == 0) stopIQR("Check if all functions have formulas")
    model             <- addFunction_IQRmodel(model,
                                              name=namek__,
                                              arguments=argumentsk__,
                                              formula=formulak__,
                                              notes=notesk__)
  }
  return(model)
}
parseEvents_IQRmodelText <- function(model,model_events__) {
  for (k in seq_along(model_events__)) {
    eventString__ <- aux_strtrim(model_events__[k])
    commentInfo__ <- parseNotes_IQRmodelText(eventString__)
    eventString__ <- commentInfo__$main
    notesk__      <- commentInfo__$comment
    temp__        <- regexpr("=", eventString__,fixed=TRUE)
    namek__       <- aux_strtrim(substr(eventString__,1,(temp__[1]-1)))
    if (nchar(namek__) == 0) stopIQR("At least one event name not given")
    eventRHS      <- aux_strtrim(substr(eventString__,temp__[1]+1,nchar(eventString__)))
    elementsRHS__ <- aux_explodePC(eventRHS)
    if ((length(elementsRHS__) < 3) | ((length(elementsRHS__) %% 2) == 0)) stopIQR("At least one event has no full information given")
    triggerk__    <- aux_removeWhiteSpace(elementsRHS__[1])
    model         <- addEvent_IQRmodel(model,
                                       name=namek__,
                                       trigger=aux_removeWhiteSpace(triggerk__),
                                       notes=notesk__)
    for (k2 in seq(2,length(elementsRHS__),2)) model <- addEventAssignment_IQRmodel(model,
                                                                                    eventname=namek__,
                                                                                    variable=aux_removeWhiteSpace(elementsRHS__[k2]),
                                                                                    formula=aux_removeWhiteSpace(elementsRHS__[k2+1]))
  }
  return(model)
}
parseStatesODE_IQRmodelText <- function(model,model_states__) {
  ODEtest__ <- grep("d/dt(", model_states__, fixed=TRUE)
  ICtest__ <- grep("(0)", model_states__, fixed=TRUE)
  if (length(ODEtest__)==0) stopIQR('The model does not contain any states')
  if (length(ICtest__) != 0) {
    if (max(ODEtest__) > min(ICtest__)) stopIQR('Initial conditions have to be defined after the definition of the ODEs')
  }
  for (k__ in seq_along(ODEtest__)) {
    stateString__  <- aux_strtrim(model_states__[ODEtest__[k__]])
    commentInfo__  <- parseNotes_IQRmodelText(stateString__)
    notesk__       <- commentInfo__$comment
    stateString__  <- commentInfo__$main
    SBMLinfo__     <- parseSBMLinfo_IQRmodelText(stateString__,"state")
    compartmentk__ <- SBMLinfo__$compartment
    typek__        <- SBMLinfo__$type
    unittypek__    <- SBMLinfo__$unittype
    stateString__  <- SBMLinfo__$textString__
    temp__         <- regexpr(")", stateString__, fixed=TRUE)
    test__         <- substr(stateString__,6,(temp__[1]-1))
    if (nchar(test__) == 0) stopIQR("At least on state name in ODE definition is not given")
    namek__        <- aux_removeWhiteSpace(test__)
    temp__         <- regexpr("=", stateString__, fixed=TRUE)
    test__         <- substr(stateString__,temp__[1]+1,nchar(stateString__))
    if (nchar(test__) == 0) stopIQR("At least one RHS of an ODE is not given")
    ODEk__         <- aux_strtrim(test__)
    if (substr(ODEk__,1,1) == "+") {
      ODEk__ <- aux_strtrim(substr(ODEk__,2,nchar(ODEk__)))
    }
    model          <- addState_IQRmodel(model,
                                        name=namek__,
                                        IC=0,
                                        ODE=ODEk__,
                                        type=typek__,
                                        compartment=compartmentk__,
                                        unittype=unittypek__,
                                        notes=notesk__)
  }
  for (k__ in seq_along(ICtest__)) {
    ICString__     <- aux_removeWhiteSpace(model_states__[ICtest__[k__]])
    temp__         <- regexpr("(0)", ICString__,fixed=TRUE)
    stateName__    <- aux_strtrim(substr(ICString__,1,temp__[1]-1))
    temp__         <- regexpr("=", ICString__,fixed=TRUE)
    stateIC__      <- aux_strtrim(substr(ICString__,temp__[1]+1,nchar(ICString__)))
    found__        <- FALSE
    ix__           <- unname(which(statesInfo_IQRmodel(model)$statenames==stateName__))
    if (length(ix__) != 0) {
      model$states[[stateName__]]$IC <- stateIC__
      found__     <- TRUE
    }
    if (!found__) stopIQR("An initial condition ( ", ICString__," ) is defined for a state that is not present in the model ( ", stateName__, " )")
  }
  return(model)
}
parseReactionsODE_IQRmodelText <- function(model,model_reactions__) {
  for (k in seq_along(model_reactions__)) {
    reactionString__ <- aux_strtrim(model_reactions__[k])
    commentInfo__    <- parseNotes_IQRmodelText(reactionString__)
    reactionString__ <- commentInfo__$main
    notesk__         <- commentInfo__$comment
    temp__           <- regexpr("=", reactionString__,fixed=TRUE)
    test__           <- aux_strtrim(substr(reactionString__,1,(temp__[1]-1)))
    if (nchar(test__) == 0) stopIQR("At least one reaction name not given")
    namek__          <- aux_removeWhiteSpace(test__)
    formulak__       <- aux_strtrim(substr(reactionString__,(temp__+1),nchar(reactionString__)))
    flagInfo__       <- parseReactionFlags_IQRmodelText(formulak__,"{fast}")
    formulak__       <- flagInfo__$textString__
    fastFlag         <- flagInfo__$flagPresent__
    flagInfo__       <- parseReactionFlags_IQRmodelText(formulak__,"{reversible}")
    formulak__       <- flagInfo__$textString__
    reversibleFlag   <- flagInfo__$flagPresent__
    if (nchar(formulak__) == 0) stopIQR("At least one reaction definition not given")
    model <- addReaction_IQRmodel(model,
                                  name=namek__,
                                  formula=formulak__,
                                  notes=notesk__,
                                  reversible=reversibleFlag,
                                  fast=fastFlag)
  }
  return(model)
}
parseReactionsBC_IQRmodelText <- function(model,model_reactions__) {
  reactionsList__   <- list()
  allSpecies__      <- c()
  k__               <- 1
  nReac__           <- 0
  while (k__ < length(model_reactions__)) {
    rowText__ = model_reactions__[k__]
    if (!is.null(aux_strFindAll(rowText__,":")$start)) {
      nReac__       <- nReac__+1
      reacInfo__    <- parseNotes_IQRmodelText(rowText__)
      reacDef__     <- aux_removeWhiteSpace(reacInfo__$main)
      reacNotes__   <- reacInfo__$comment
      flagCheck__   <- parseReactionFlags_IQRmodelText(reacDef__,"{fast}")
      reacFast      <- flagCheck__$flagPresent__
      reacDef__     <- flagCheck__$textString__
      if (!is.null(aux_strFindAll(reacDef__,"<=>")$start)) {
        reacReversible__   <- TRUE
        reacDef__          <- aux_strrep(reacDef__,"<=>","xyxyxyxy")
      } else {
        if (!is.null(aux_strFindAll(reacDef__,"=>")$start)) {
          reacReversible__ <- FALSE
          reacDef__        <- aux_strrep(reacDef__,"=>","xyxyxyxy")
        } else {
          stopIQR("Wrong reaction expression")
        }
      }
      terms              <- aux_explode(reacDef__,":")
      reacName__         <- terms[2]
      reacDef__          <- terms[1]
      terms              <- aux_explode(paste0(" ",reacDef__," "),"xyxyxyxy")
      reacLHS__          <- aux_strtrim(terms[1])
      reacRHS__          <- aux_strtrim(terms[2])
      k__                <- k__+1
      if (k__>length(model_reactions__)) stopIQR("Error in reaction expression")
      rowKineticsVF__    <- aux_removeWhiteSpace(model_reactions__[k__])
      if (is.null(aux_strFindAll(rowKineticsVF__,"vf=")$start)) stopIQR("Error in reaction expression")
      rowKineticsVF__    <- aux_strrep(rowKineticsVF__,"vf=","")
      if (reacReversible__) {
        k__              <- k__+1
        if (k__>length(model_reactions__)) stopIQR("error in reaction expression")
        rowKineticsVR__  <- aux_removeWhiteSpace(model_reactions__[k__])
        addParentheses__ <- FALSE
        termsplus__ <- aux_explodePC(rowKineticsVR__,"+")
        termsminus__ <- aux_explodePC(rowKineticsVR__,"-")
        if (length(termsplus__) > 1) addParentheses__ <- TRUE
        if (length(termsminus__) > 1) addParentheses__ <- TRUE
        if (addParentheses__) rowKineticsVR__ <- paste0("(",rowKineticsVR__,")")
        if (is.null(aux_strFindAll(rowKineticsVR__,"vr=")$start)) stopIQR("error in reaction expression")
        rowKineticsVR__  <- aux_strrep(rowKineticsVR__,"vr=","")
        reacFormula__    <- paste0(rowKineticsVF__,"-",rowKineticsVR__)
      } else {
        rowKineticsVR__  <- NULL
        reacFormula__    <- rowKineticsVF__
      }
    }
    model <- addReaction_IQRmodel(model,
                                  name=reacName__,
                                  formula=reacFormula__,
                                  notes=reacNotes__,
                                  reversible=reacReversible__,
                                  fast=reacFast)
    substrateInfo__      <- parseReactionTermsBC_IQRmodelText(reacLHS__)
    productInfo__        <- parseReactionTermsBC_IQRmodelText(reacRHS__)
    if (!is.null(substrateInfo__$names)) allSpecies__ <- unique(c(allSpecies__, substrateInfo__$names))
    if (!is.null(productInfo__$names))   allSpecies__ <- unique(c(allSpecies__, productInfo__$names))
    reactionsList__[[nReac__]] <- list(name=reacName__,
                                       substrateNames=substrateInfo__$names,
                                       substrateFactors=substrateInfo__$factors,
                                       productNames=productInfo__$names,
                                       productFactors=productInfo__$factors)
    k__ <- k__+1
  }
  return(list(model=model,allSpecies__=allSpecies__,reactionsList__=reactionsList__))
}
parseStatesBC_IQRmodelText <- function(info__,model_states__) {
  model                <- info__$model
  allSpecies__         <- info__$allSpecies__
  reactionsList__      <- info__$reactionsList__
  allSpeciesStates     <- c()
  allParNames          <- parametersInfo_IQRmodel(model)$paramnames
  allVarNames          <- variablesInfo_IQRmodel(model)$varnames
  for (k in seq_along(allSpecies__)) {
    parameterIndex     <- aux_strmatch(allSpecies__[k],allParNames)
    variableIndex      <- aux_strmatch(allSpecies__[k],allVarNames)
    if (is.null(parameterIndex) & is.null(variableIndex)) allSpeciesStates <- c(allSpeciesStates, allSpecies__[k])
  }
  for (k__ in seq_along(allSpeciesStates)) {
    stateName__        <- allSpeciesStates[k__]
    stateODE           <- ""
    for (k2__ in seq_along(reactionsList__)) {
      reacName__       <- reactionsList__[[k2__]]$name
      ix__             <- aux_strmatch(stateName__,reactionsList__[[k2__]]$substrateNames)
      if (!is.null(ix__))
        stateODE       <- paste0(stateODE,"-",reactionsList__[[k2__]]$substrateFactors[ix__],"*",reacName__)
      ix__ <- aux_strmatch(stateName__,reactionsList__[[k2__]]$productNames)
      if (!is.null(ix__))
        stateODE       <- paste0(stateODE,"+",reactionsList__[[k2__]]$productFactors[ix__],"*",reacName__)
    }
    ODEtext            <- paste0("d/dt(",stateName__,") = ",stateODE)
    model_states__     <- c(ODEtext,model_states__)
  }
  ix_ICs__             <- which(grepl("(0)",model_states__,fixed=TRUE))
  for (k__ in seq_along(ix_ICs__)) {
    xx__ <- model_states__[ix_ICs__[k__]]
    ixCB__ <- aux_strFindAll(xx__,"{")$start
    ixC1__ <- aux_strFindAll(xx__,"%")$start
    ixC2__ <- aux_strFindAll(xx__,"#")$start
    xxxx__ <- c(ixCB__,ixC1__,ixC2__)
    if (!is.null(xxxx__)) {
      annotation__ <- aux_strtrim(substr(xx__,min(xxxx__),nchar(xx__)))
      new_ICtext__ <- aux_strtrim(aux_strrep(xx__,annotation__,""))
      ix0__  <- aux_strFindAll(xx__,"(0)")$start
      stateName__ <- aux_removeWhiteSpace(substr(xx__,1,ix0__-1))
      model_states__[ix_ICs__[k__]] <- new_ICtext__
      ixState__ <- which(grepl(paste0("d/dt(",stateName__,")"),aux_removeWhiteSpace(model_states__),fixed=TRUE))
      model_states__[ixState__] <- paste0(model_states__[ixState__]," ",annotation__)
    }
  }
  return(model_states__)
}
parseNotes_IQRmodelText <- function(textString__) {
  startNotesMATLAB__ <- regexpr("%",textString__)
  startNotesR__      <- regexpr("#",textString__)
  if (startNotesMATLAB__ != -1 & startNotesR__ != -1)
    stopIQR("Both MATLAB and R-type comment signs used in the comment of a model element.\nPlease note that 'hashtag' and % are not allowed to be used in comments.")
  startNotes__       <- startNotesMATLAB__
  if (startNotesR__ != -1)
    startNotes__     <- startNotesR__
  notesk__           <- NULL
  if (startNotes__[1] != -1) {
    notesk__         <- aux_strtrim(substr(textString__,startNotes__[1]+1,nchar(textString__)))
    textString__     <- aux_strtrim(substr(textString__,1,startNotes__[1]-1))
    if (nchar(notesk__)==0)
      notesk__       <- NULL
  }
  return(list(main=textString__,comment=notesk__))
}
parseSBMLinfo_IQRmodelText <- function(textString__,elementname) {
  infoStart__         <- aux_strFindAll(textString__,"{")
  infoEnd__           <- aux_strFindAll(textString__,"}")
  informationText__   <- ""
  if ((length(infoStart__$start)+length(infoEnd__$start))>2)
    stopIQR(paste0("To many curly parentheses in a ",elementname," definition"))
  if (length(infoStart__$start) != length(infoEnd__$start))
    stopIQR(paste0("At least one ",elementname," information not properly defined"))
  if (!is.null(infoStart__$start)) {
    informationText__ <- aux_strtrim(substr(textString__,infoStart__$start+1,infoEnd__$start-1))
    textString__      <- aux_strtrim(substr(textString__,1,infoStart__$start-1))
  }
  type                <- NULL
  compartment         <- NULL
  unittype            <- NULL
  if(nchar(informationText__)>0) {
    terms             <- aux_explode(informationText__,':')
    found__           <- FALSE
    if (tolower(aux_strtrim(terms[1]))=="isparameter") {
      type            <- "isParameter"
      found__         <- TRUE
    }
    if (tolower(aux_strtrim(terms[1]))=="iscompartment") {
      type            <- "isCompartment"
      if (length(terms)==1) terms[2] = ""
      compartment     <- aux_strtrim(terms[2])
      found__         <- TRUE
    }
    if (tolower(aux_strtrim(terms[1]))=="isspecie") {
      type            <- "isSpecie"
      if (length(terms)!=3)
        stopIQR(paste0("Error in ",elementname," isSpecie SBML information"))
      compartment     <- aux_strtrim(terms[2])
      unittype        <- aux_strtrim(terms[3])
      found__         <- TRUE
    }
    if (!found__)
      stopIQR(paste0("Error in ",elementname," SBML information"))
  }
  return(list(textString__=textString__,type=type,compartment=compartment,unittype=unittype))
}
parseReactionFlags_IQRmodelText <- function(textString__,flagString) {
  if (!is.null(aux_strFindAll(textString__,flagString)$start)) {
    textString__  <- aux_strtrim(aux_strrep(textString__,flagString,""))
    flagPresent__ <- TRUE
  } else {
    flagPresent__ <- FALSE
  }
  return(list(textString__=textString__,flagPresent__=flagPresent__))
}
parseReactionTermsBC_IQRmodelText <- function(reactionEquation) {
  if (nchar(reactionEquation)==0) return(list(names=NULL, factors=NULL))
  allTerms__             <- aux_explode(reactionEquation,"\\+")
  termsNames__           <- c()
  termsFactors__         <- c()
  for (k__ in seq_along(allTerms__)) {
    checkTerms__         <- aux_explode(allTerms__[k__],"\\*")
    if (length(checkTerms__) == 1) {
      termsNames__       <- c(termsNames__, checkTerms__[1])
      termsFactors__     <- c(termsFactors__, 1)
    } else {
      if (length(checkTerms__) == 2) {
        termsNames__     <- c(termsNames__, checkTerms__[2])
        termsFactors__   <- c(termsFactors__, checkTerms__[1])
      } else {
        stopIQR("Error in a reaction equation")
      }
    }
  }
  return(list(names=termsNames__, factors=termsFactors__))
}
parseOutputs_IQRmodelText <- function(model) {
  if (length(grep("OUTPUT",names(model$states)))>0) stopIQR("'OUTPUT' present in a state name")
  if (length(grep("OUTPUT",names(model$parameters)))>0) stopIQR("'OUTPUT' present in a parameter name")
  if (length(grep("OUTPUT",names(model$reactions)))>0) stopIQR("'OUTPUT' present in a reaction name")
  Noutputs__ <- length(grep("OUTPUT",names(model$variables)))
  if (Noutputs__==0) return(model)
  for (k__ in 1:Noutputs__) {
    name__     <- paste0("OUTPUT",k__)
    varIndex__ <- grep(paste0("\\<",name__,"\\>"),names(model$variables))
    if (length(varIndex__)==0) stopIQR("OUTPUTs need to be defined in order 1,2,3,...N")
    formula__  <- model$variables[[varIndex__]]$formula
    notes__    <- model$variables[[varIndex__]]$notes
    outputInfo__ <- list(formula=formula__, notes=notes__, varindex=varIndex__)
    model$outputs[[name__]] <- outputInfo__
    test__ <- cbind(unname(which(formula__==names(model$states))),unname(which(formula__==names(model$variables))))
    if (length(test__)==0) warningIQR("An OUTPUT is defined by an expression. It is better to assign a variable or a state to an output (for MONOLIX export)")
  }
  return(model)
}
parseInputs_IQRmodelText <- function(model) {
  reacNamesInput__ <- names(model$reactions)[grep("INPUT",reactionsInfo_IQRmodel(model)$reacformulas)]
  for (k in seq_along(reacNamesInput__)) {
    reacName__    <- reacNamesInput__[k]
    reacFormula__ <- model$reactions[[reacName__]]$formula
    if (!is.null(aux_strFindAll(reacFormula__,"(")$start))  stopIQR("Model contains reaction with INPUT and parentheses in formula. Parentheses not allowed in this case!")
    if (!is.null(aux_strFindAll(reacFormula__,"+")$start)) stopIQR("Model contains a reaction with INPUT and a '+' sign. The '+' is not allowed. Reaction needs to be defined only be the input")
    if (!is.null(aux_strFindAll(reacFormula__,"-")$start)) stopIQR("Model contains a reaction with INPUT and a '-' sign. The '-' is not allowed. Reaction needs to be defined only be the input")
    for (k2 in seq_along(model$states))
      model$states[[k2]]$ODE <- gsub(pattern=paste0("\\<",reacName__,"\\>"),replacement=reacFormula__,x=model$states[[k2]]$ODE)
    for (k2 in 1:length(model$reactions))
      if (length(grep(paste0("\\b",reacName__,"\\b"),model$reactions[[k2]]$formula[k2]))>0)
        stopIQR("A reaction with an INPUT is present on the RHS of another reaction. This is not allowed!")
    model$reactions[[reacName__]] <- NULL
  }
  if (length(grep("\\bINPUT[0-9]+\\b",names(model$variables)))>0) stopIQR("Model variable name contains INPUT")
  if (length(grep("\\bINPUT[0-9]+\\b",names(model$reactions)))>0) stopIQR("Model reaction name contains INPUT")
  if (length(grep("\\bINPUT[0-9]+\\b",variablesInfo_IQRmodel(model)$varformulas))>0) stopIQR("Model variable formula contains INPUT")
  if (length(grep("\\bINPUT[0-9]+\\b",reactionsInfo_IQRmodel(model)$reacformulas))>0) stopIQR("Model variable formula contains INPUT")
  m         <- gregexpr("INPUT[0-9]+",statesInfo_IQRmodel(model)$stateODEs,perl=TRUE)
  y         <- as.numeric(aux_strrep(unique(unlist(regmatches(statesInfo_IQRmodel(model)$stateODEs,m))),"INPUT",""))
  NINPUTS__ <- length(y)
  if (NINPUTS__==0) return(model)
  if (min(y) != 1 | max(y) != NINPUTS__) stopIQR("INPUTs need to be ordered sequntially, starting from 1 until N=number of INPUTs")
  states__ <- statesInfo_IQRmodel(model)
  for (k in 1:NINPUTS__) {
    inputName__ <- paste0("INPUT",k)
    inputStateindex__ <- grep(paste0("\\b",inputName__,"\\b"),states__$stateODEs)
    parameters__ <- parametersInfo_IQRmodel(model)
    inputParindex__ <- grep(paste0("\\b",inputName__,"\\b"),parameters__$paramnames)
    if (length(inputParindex__)==0) {
      model <- addParameter_IQRmodel(model,name=inputName__,value=0)
      inputParindex__ <- length(model$parameters)
    }
    TlaginputParindex__ <- grep(paste0("\\bTlag",k,"\\b"),parameters__$paramnames)
    if (length(TlaginputParindex__)==0) {
      model <- addParameter_IQRmodel(model,name=paste0("Tlag",k),value=0)
    }
    inputFactors__ <- c()
    inputTerms__ <- c()
    for (k2 in inputStateindex__) {
      ODE__          <- aux_removeWhiteSpace(states__$stateODEs[[k2]])
      results__      <- parseInputFactorsTerms_IQRmodel(model,inputName__,ODE__)
      inputFactors__ <- cbind(inputFactors__,results__$inputFactor__)
      inputTerms__   <- cbind(inputTerms__,results__$inputTerm__)
    }
    model$inputs[[inputName__]] <- list(name=inputName__,
                                        factors=inputFactors__,
                                        terms=inputTerms__,
                                        stateindex=inputStateindex__,
                                        parindex=inputParindex__)
  }
  return(model)
}
parseInputFactorsTerms_IQRmodel <- function(model,inputName__,ODE__) {
  ix__ <- aux_strFindAll(ODE__,inputName__)
  if (length(ix__$start)>1) stopIQR("Same INPUT more than once on an ODE")
  if (ix__$start==1) {
    ODEpre__ <- ""
  } else {
    ODEpre__ <- aux_strtrim(substr(ODE__,1,ix__$start-1))
  }
  ODEpost__ <- aux_strtrim(substr(ODE__,ix__$end+1,nchar(ODE__)))
  if (nchar(ODEpost__) > 0) {
    if (substr(ODEpost__,1,1) != "+" & substr(ODEpost__,1,1) != "-") {
      stopIQR("The INPUT* identifier must be the last element in the input term")
    }
  }
  npo__ <- length(aux_strFindAll(ODEpost__,'(')$start)
  npc__ <- length(aux_strFindAll(ODEpost__,')')$start)
  if (npo__ != npc__) stopIQR("The INPUT* identifier is not allowed to be inside a parentheses")
  if (nchar(ODEpre__) > 0) {
    if (substr(ODEpre__,nchar(ODEpre__),nchar(ODEpre__)) != '+' &
        substr(ODEpre__,nchar(ODEpre__),nchar(ODEpre__)) != '-' &
        substr(ODEpre__,nchar(ODEpre__),nchar(ODEpre__)) != '*') {
      stopIQR("The INPUT term can have a multiplicative pre-term and the whole term needs to be additive")
    }
  }
  inputFactor__ <- NULL
  inputTerm__   <- NULL
  if (nchar(ODEpre__)==0) {
    inputFactor__ <- "1"
    inputTerm__ <- inputName__
    return(list(inputFactor__=inputFactor__, inputTerm__=inputTerm__))
  }
  if (substr(ODEpre__,nchar(ODEpre__),nchar(ODEpre__)) == "+") {
    inputFactor__ <- "1"
    inputTerm__ <- paste0("+",inputName__)
    return(list(inputFactor__=inputFactor__, inputTerm__=inputTerm__))
  }
  if (substr(ODEpre__,nchar(ODEpre__),nchar(ODEpre__)) == "-") {
    inputFactor__ <- "-1"
    inputTerm__ <- paste0("-",inputName__)
    return(list(inputFactor__=inputFactor__, inputTerm__=inputTerm__))
  }
  po <- 0
  for (k in nchar(ODEpre__):1) {
    if (substr(ODEpre__,k,k) == '(') po <- po+1
    if (substr(ODEpre__,k,k) == ')') po <- po-1
    if (substr(ODEpre__,k,k) == "+" & po==0) break
    if (substr(ODEpre__,k,k) == "-" & po==0) break
  }
  inputFactor__ <- substr(ODEpre__,k,nchar(ODEpre__)-1)
  inputTerm__ <- paste0(inputFactor__,"*",inputName__)
  testNames__ <- c(names(model$states),names(model$variables),names(model$reactions))
  for (k in seq_along(testNames__)) {
    test__ <- grep(paste0("\\<",testNames__[k],"\\>"),inputFactor__)
    if (length(test__)!=0)
      warningIQR("IMPORTANT (Will not work for NONMEM conversion!): Pre-factors of INPUT* terms are not allowed to depend on states, variables, and reactions")
  }
  return(list(inputFactor__=inputFactor__, inputTerm__=inputTerm__))
}
#'@export
new_IQRmodel <- function(filename="newmodel.txt",FLAGbc=FALSE) {
  if (is.null(filename)) stopIQR("Please provide a filename")
  filename <- paste0(aux_strrep(filename,".txt",""),".txt")
  model__ <- IQRmodel()
  if (FLAGbc) {
    export_IQRmodel(model__,filename=filename,FLAGbc=TRUE)
  } else {
    export_IQRmodel(model__,filename=filename)
  }
  file.edit(filename)
}
#'@export
is_IQRmodel <- function(input) {
  methods::is(input,"IQRmodel")
}
#'@export
print.IQRmodel <- function(x, ...) {
  cat("\tIQRmodel\n\t========\n")
  cat("\tName:                      ", x$name,"\n")
  cat("\tNumber States:             ", length(x$states),"\n")
  cat("\tNumber Parameters:         ", length(x$parameters),"\n")
  cat("\tNumber Variables:          ", length(x$variables),"\n")
  cat("\tNumber Reactions:          ", length(x$reactions),"\n")
  cat("\tNumber Functions:          ", length(x$functions),"\n")
  cat("\tNumber Events:             ", length(x$events),"\n")
  cat("\tNumber Inputs:             ", length(x$inputs),"\n")
  cat("\tNumber Outputs:            ", length(x$outputs),"\n")
  if (!hasOnlyNumericICs_IQRmodel(x)) {
    cat("\tNon-numeric initial conditions are present in the model.\n")
  }
  if (hasCcodeFunctions_IQRmodel(x)) {
    cat("\tC-code functions are present in the model.\n")
  }
  if (hasInitialAssignments_IQRmodel(x)) {
    cat("\tInitial assignments are present in the model.\n")
  }
}
#'@export
IQRmodel <- function (input=NULL,FLAGsim=TRUE,FLAGsym=FALSE,sensParams=NULL) {
  if (is.null(input)) return(init_IQRmodel())
  if (!file.exists(input)) stopIQR("Provided input argument does not point to a file on the filesystem.")
  if (aux_fileparts(input)$fileext==".xml") {
    model__ <- importSBML_IQRmodel(input)
  } else {
    model__ <- import_IQRmodel(input,FLAGtextIQRmodel=FALSE)
    checkNames_IQRmodel(model__)
  }
  model__ <- regenerate_IQRmodel(model = model__,FLAGsim=FLAGsim,FLAGsym=FLAGsym,sensParams=sensParams)
  checkOrderVarReacDefinition(model__)
  class(model__) <- "IQRmodel"
  return(model__)
}
hasOnlyNumericICs_IQRmodel <- function(model__) {
  test <- suppressWarnings(as.numeric(sapply(model__$states, function (x) { x$IC })))
  return (all(!is.na(test)))
}
hasFastReactions_IQRmodel <- function(model__) {
  if (!is_IQRmodel(model__))
    stopIQR("Input argument is not an IQRmodel")
  return(any(sapply(model__$reactions,function (x) x$fast)==TRUE))
}
#'@export
exportSBML_IQRmodel <- function(model,filename) {
  message("For export in SBML the following should be considered")
  message("  * All reaction rates are defined in amount/time units")
  message("  * Species ideally in amount and ODEs defined by sum of reaction rates")
  message("    * Stoichiometric factors can be numeric or parameters and need to be define in front of reaction names")
  message("      Example: d/dt(speciesAmount) = 2*reaction1 - reaction2 - 1*reaction3")
  message("  * Species can also be defined in concentration units. In this case the following needs to be done:")
  message("    * The volume of the compartment in which the species is located needs to be added to the ODE as follows:")
  message("      Example: d/dt(speciesConcentration) = (2*reaction1 - reaction2 - 1*reaction3)/volumeOfCompartment")
  loadSetupOptions_IQRtools()
  if (!file.exists(.PATH_IQRsbmlOut))
    stopIQR("The IQRsbmlOut executable has not been installed. Currently it is only available on IQdesktop (https://iqdesktop.intiquan.com)")
  if (!is_IQRmodel(model))
    stopIQR("model argument is not an IQRmodel")
  pathModel <- paste0(tempfileIQR(),".txt")
  export_IQRmodel(model = model,filename = pathModel)
  xxx__ <- aux_fileparts(pathModel)
  oldpath__ <- getwd()
  setwd(xxx__$pathname)
  absModelPath__ <- paste0(getwd(),"/",xxx__$filename,xxx__$fileext)
  setwd(oldpath__)
  xxx__ <- aux_fileparts(filename)
  oldpath__ <- getwd()
  setwd(xxx__$pathname)
  absOutputPath__ <- paste0(getwd(),"/",xxx__$filename,xxx__$fileext)
  setwd(oldpath__)
  aux_mkdir(xxx__$pathname)
  syscall__ <- paste0(.PATH_IQRsbmlOut, " \"",absModelPath__, "\" \"",absOutputPath__,"\"")
  system(syscall__)
  return(invisible(NULL))
}
#'@export
importSBML_IQRmodel <- function(input,FLAGexportBC=FALSE,FLAGcheckComplete=FALSE,FLAGuseSBMLnames=FALSE,FLAGremoveSBMLinfo=FALSE,filename=NULL,FLAGchangeINPUT=FALSE) {
  loadSetupOptions_IQRtools()
  if (!file.exists(.PATH_IQRsbml))
    stopIQR("The IQRsbml executable has not been installed. Please obtain it from http://www.intiquan.com/iqrsbml for Windows or use IQdesktop (https://iqdesktop.intiquan.com)")
  if (!file.exists(input))
    stopIQR("SBML file not found")
  tempfile <- input
  if (FLAGchangeINPUT) {
    content <- aux_fileread(input)
    content <- gsub("\\<IN_PUT([1-9]+)\\>", "INPUT\\1", content)
    tempfile <- paste0(tempfileIQR(),".xml")
    aux_filewrite(content,filename = tempfile)
  }
  xxx__ <- aux_fileparts(tempfile)
  oldpath__ <- getwd()
  setwd(xxx__$pathname)
  absModelPath__ <- paste0(getwd(),"/",xxx__$filename,xxx__$fileext)
  setwd(oldpath__)
  outPath <- tempfileIQR()
  FLAGexportBC <- as.numeric(FLAGexportBC)
  FLAGcheckComplete <- as.numeric(FLAGcheckComplete)
  FLAGuseSBMLnames <- as.numeric(FLAGuseSBMLnames)
  FLAGremoveSBMLinfo <- as.numeric(FLAGremoveSBMLinfo)
  syscall__ <- paste0(.PATH_IQRsbml, " \"",absModelPath__, "\" \"",outPath,"\""," ",FLAGexportBC," ",FLAGcheckComplete," ",FLAGuseSBMLnames," ",FLAGremoveSBMLinfo)
  system(syscall__)
  content <- aux_fileread(paste0(outPath,".txt"),collapserows = FALSE)
  suppressWarnings(
    if (any(grepl("default =|\\(default\\)",content))) {
      warning("'default' present as component name in SBML model. All occurrence of 'default' changed to 'def_ault'")
      content <- gsub("\\<default\\>","def_ault",content)
    }
  )
  if (is.null(filename)) {
    model__ <- suppressWarnings(import_IQRmodel(content,FLAGtextIQRmodel=TRUE))
    return(model__)
  } else {
    aux_filewrite(content,paste0(gsub(".txt","",filename),".txt"))
  }
  return(invisible(NULL))
}
init_IQRmodel <- function () {
  model__        <- list()
  model__$name   <- "Provide Model Name"
  model__$notes  <- "Provide Model Information"
  class(model__) <- "IQRmodel"
  return(model__)
}
checkNames_IQRmodel <- function(model) {
  loadSetupOptions_IQRtools()
  stateNames__ <- toupper(statesInfo_IQRmodel(model)$statenames)
  paramNames__ <- toupper(parametersInfo_IQRmodel(model)$paramnames)
  varNames__   <- toupper(variablesInfo_IQRmodel(model)$varnames)
  reacNames__  <- toupper(reactionsInfo_IQRmodel(model)$reacnames)
  if (!.ALLOW_UNDERSCORES_IN_PARAMETER_NAMES) {
    if (any(grepl("_",paramNames__)))
      stopIQR("At least one parameter name contains an '_'.\nThis is not allowed mainly for NLME model and covariate purposes.\nThis check can be switched off in the setup_options_IQRtools.R file. Run setup_IQRtools() if you want to change the file.")
  }
  if (length(which(nchar(stateNames__) > .MODEL_MAX_DESIRED_STATENAME_LENGTH)) > 0)
    warningIQR(paste0("Some state names have more than ",.MODEL_MAX_DESIRED_STATENAME_LENGTH," characters.\nIt makes sense to not have them too long (for good old NONMEM)\nYou can set warning thresholds in the setup_options_IQRtools.R file. Run setup_IQRtools() if you want to change the file."))
  if (length(which(nchar(paramNames__) > .MODEL_MAX_DESIRED_PARAMETERNAME_LENGTH)) > 0)
    warningIQR(paste0("Some parameter names have more than ",.MODEL_MAX_DESIRED_PARAMETERNAME_LENGTH," characters.\nIt makes sense to use a max length of 8 (for good old requirements in XPT files for the FDA)\nYou can set warning thresholds in the setup_options_IQRtools.R file. Run setup_IQRtools() if you want to change the file."))
  if (length(which(nchar(varNames__) > .MODEL_MAX_DESIRED_VARIABLENAME_LENGTH)) > 0)
    warningIQR(paste0("Some variable names have more than ",.MODEL_MAX_DESIRED_VARIABLENAME_LENGTH," characters.\nIt makes sense to not have them too long (for good old NONMEM)\nYou can set warning thresholds in the setup_options_IQRtools.R file. Run setup_IQRtools() if you want to change the file."))
  if (length(which(nchar(reacNames__) > .MODEL_MAX_DESIRED_REACTIONNAME_LENGTH)) > 0)
    warningIQR(paste0("Some reaction names have more than ",.MODEL_MAX_DESIRED_REACTIONNAME_LENGTH," characters.\nIt makes sense to not have them too long (for good old NONMEM)\nYou can set warning thresholds in the setup_options_IQRtools.R file. Run setup_IQRtools() if you want to change the file."))
  reservedWords__ <- toupper(.RESERVED_WORD_IQRMODELS)
  if (length(intersect(stateNames__,reservedWords__)) > 0) {
    text <- paste0("Model contains the following states that are reserved words: ",intersect(stateNames__,reservedWords__))
    text <- paste0(text,"\nYou can set the reserved words in the setup_options_IQRtools.R file. Run setup_IQRtools() if you want to change the file.")
    stopIQR(text)
  }
  if (length(intersect(paramNames__,reservedWords__)) > 0) {
    text <- paste0("Model contains the following parameters that are reserved words: ",intersect(paramNames__,reservedWords__))
    text <- paste0(text,"\nYou can set the reserved words in the setup_options_IQRtools.R file. Run setup_IQRtools() if you want to change the file.")
    stopIQR(text)
  }
  if (length(intersect(varNames__,reservedWords__)) > 0) {
    text <- paste0("Model contains the following variables that are reserved words: ",intersect(varNames__,reservedWords__))
    text <- paste0(text,"\nYou can set the reserved words in the setup_options_IQRtools.R file. Run setup_IQRtools() if you want to change the file.")
    stopIQR(text)
  }
  if (length(intersect(reacNames__,reservedWords__)) > 0) {
    text <- paste0("Model contains the following reactions that are reserved words: ",intersect(reacNames__,reservedWords__))
    text <- paste0(text,"\nYou can set the reserved words in the setup_options_IQRtools.R file. Run setup_IQRtools() if you want to change the file.")
    stopIQR(text)
  }
  if (any(grepl("\\<TLAG[0-9]*\\>",stateNames__)))
    stopIQR("A state name contains Tlag* (case insensitive). This is not allowed")
  if (any(grepl("\\<TLAG[0-9]*\\>",varNames__)))
    stopIQR("A variable name contains Tlag* (case insensitive). This is not allowed")
  if (any(grepl("\\<TLAG[0-9]*\\>",reacNames__)))
    stopIQR("A reaction name contains Tlag* (case insensitive). This is not allowed")
  inputindices <- unique(as.numeric(aux_strrep(paramNames__[grepl("\\<TLAG[0-9]*\\>",paramNames__)],"TLAG","")))
  if (length(inputindices) != length(model$inputs))
    stopIQR("Model contains a non match between Tlag* parameters and INPUT* definitions")
  if (length(inputindices) > 0) {
    if (min(inputindices) != 1 & max(inputindices) != length(model$inputs))
      stopIQR("Model contains a non match between Tlag* parameters and INPUT* definitions")
  }
}
#'@export
regenerate_IQRmodel <- function(model,FLAGsim=TRUE,FLAGsym=TRUE,sensParams=NULL) {
  regenmodel__                           <- model
  if (!is.null(sensParams)) {
    if (length(setdiff(sensParams,parametersInfo_IQRmodel(regenmodel__)$paramnames)) > 0)
      stopIQR("Provided sensitivity parameters not all available in the model")
  } else {
    sensParams <- parametersInfo_IQRmodel(regenmodel__)$paramnames
    sensParams <- setdiff(sensParams,sensParams[grep(x=sensParams,pattern = "\\<INPUT[0-9]+\\>")])
  }
  attr(regenmodel__,"sensParams.IQRmodel") <- sensParams
  IA__ <- regenmodel__$initalAssignments
  IAstates__ <- intersect(names(IA__),names(regenmodel__$states))
  adummy__ <- sapply(IAstates__, function (ias__) {
    regenmodel__$states[[ias__]]$IC <<- IA__[[ias__]]$formula
  })
  modelVectorSyntaxR <- handleVectorSyntaxR_IQRmodel(regenmodel__)
  attr(regenmodel__,"nonNumICs.IQRmodel") <- nonNumICsFct_IQRmodel(modelVectorSyntaxR)
  if (FLAGsym) {
    attr(regenmodel__,"nonNumICsSens.IQRmodel") <- nonNumICsSensFct_IQRmodel(modelVectorSyntaxR,sensParams)
    attr(regenmodel__,"dfdx.IQRmodel") <- symStateJacobian_IQRmodel(regenmodel__)
    attr(regenmodel__,"dfdp.IQRmodel") <- symStateGradient_IQRmodel(regenmodel__,paramnames = sensParams)
    if (length(regenmodel__$outputs) > 0) {
      attr(regenmodel__,"dgdx.IQRmodel") <- symOutputJacobian_IQRmodel(regenmodel__)
      attr(regenmodel__,"dgdp.IQRmodel") <- symOutputGradient_IQRmodel(regenmodel__,paramnames = sensParams)
    } else {
      attr(regenmodel__,"dgdx.IQRmodel") <- NULL
      attr(regenmodel__,"dgdp.IQRmodel") <- NULL
    }
  }
  if (length(regenmodel__$states) < 50) {
    attr(regenmodel__,"dfdx.IQRmodel") <- tryCatch({symStateJacobian_IQRmodel(regenmodel__)}, error=function(err) NULL)
  }
  if (FLAGsim) regenmodel__ <- compile_IQRmodel(model = regenmodel__)
  return(regenmodel__)
}
dynLoadModelDLL_IQRmodel <- function(model__) {
  if (!is_IQRmodel(model__)) stopIQR("Please provide a model as input argument")
  DLLfile__ <- attr(model__,"DLLpath.IQRmodel")
  if (.Platform$OS.type=="unix") {
    DLLfile__ <- paste0(DLLfile__,".so")
  }
  dyn.load(DLLfile__)
}
#'@export
reload_DLLIQRmodel <- function(model){
  .so__ <- switch(Sys.info()["sysname"], "Linux" = ".so", "Windows" = "")
  dyn.load(paste0(attr(model, "DLLpath.IQRmodel"), .so__))
  ptr__ <- getNativeSymbolInfo("model",PACKAGE=attr(model,"DLLname.IQRmodel"))$address
  attr(model,"address.IQRmodel") <- ptr__
  model
}
#'@export
strReplace_IQRmodel <- function(model,old,new,regenerateModel=FALSE) {
  text__ <- export_IQRmodel(model)
  for (k in seq_along(old)) {
    text__ <- gsub(x = text__,pattern = paste0("\\b",old[k],"\\b"),replacement = new[k])
  }
  modelnew <- import_IQRmodel(input=aux_explode(text__,"\n"),FLAGtextIQRmodel=TRUE)
  if (regenerateModel) modelnew <- regenerate_IQRmodel(modelnew,FLAGsim = TRUE,FLAGsym = FALSE,sensParams = NULL)
  return(modelnew)
}
#'@export
hasCcodeFunctions_IQRmodel <- function(model) {
  if (!is_IQRmodel(model))
    stopIQR("input argument is not an IQRmodel")
  if (is.null(model$Cfunctions)) return(FALSE)
  if (nchar(model$Cfunctions)==0) return(FALSE)
  return(TRUE)
}
#'@export
hasInitialAssignments_IQRmodel <- function(model) {
  if (!is_IQRmodel(model))
    stopIQR("input argument is not an IQRmodel")
  if (is.null(model$initalAssignments)) return(FALSE)
  return(TRUE)
}
addState_IQRmodel <- function(model,
                              name,
                              IC,
                              ODE,
                              type = NULL,
                              compartment = NULL,
                              unittype = NULL,
                              notes = NULL) {
  if (name %in% names(model$states))
    stopIQR("Provided state name already exists in the model")
  if (!is.null(type)) {
    if (!(type %in% c("isSpecie", "isCompartment", "isParameter"))) {
      stopIQR("Wrong definition of 'type'. Needs to be 'isSpecie', 'isCompartment', or 'isParameter'")
    }
  }
  if (!is.null(unittype)) {
    if (!(unittype %in% c("amount", "concentration"))) {
      stopIQR("Wrong definition of 'unittype'. Needs to be 'amount' or 'concentration'")
    }
  }
  model$states[[name]] <- list(IC          = IC,
                               ODE         = ODE,
                               type        = type,
                               compartment = compartment,
                               unittype    = unittype,
                               notes       = notes)
  return(model)
}
addParameter_IQRmodel <- function(model,
                                  name,
                                  value,
                                  type = NULL,
                                  compartment = NULL,
                                  unittype = NULL,
                                  notes = NULL) {
  if (name %in% names(model$parameters))
    stopIQR(paste0("Provided parameter name (", name, ") already exists in the model"))
  if (!is.null(type)) {
    if (!(type %in% c("isSpecie", "isCompartment", "isParameter"))) {
      stopIQR("Wrong definition of 'type'. Needs to be 'isSpecie', 'isCompartment', or 'isParameter'")
    }
  }
  if (!is.null(unittype)) {
    if (!(unittype %in% c("amount", "concentration"))) {
      stopIQR("Wrong definition of 'unittype'. Needs to be 'amount' or 'concentration'")
    }
  }
  model$parameters[[name]] <- list(value       = value,
                                   type        = type,
                                   compartment = compartment,
                                   unittype    = unittype,
                                   notes       = notes)
  return(model)
}
#'@export
setParameters_IQRmodel <- function(model,parameters) {
  if (!is_IQRmodel(model))
    stopIQR("model input argument is not an IQRmodel")
  if (is.null(names(parameters)))
    stopIQR("parameters input argument needs to be a vector with named elements")
  if (any(!(names(parameters) %in% names(model$parameters))))
    stopIQR("At least one provided parameter is not available in the model: ",
            paste0(setdiff(names(model$parameters), names(parameters)), collapse = ", "))
  if (!is.numeric(parameters))
    stopIQR("Parameters are only allowed to be numeric values")
  sapply(names(parameters), function (pname) {
    model$parameters[[pname]]$value <<- unname(parameters[pname])
  })
  return(model)
}
addVariable_IQRmodel <- function(model,
                                 name,
                                 formula,
                                 type = NULL,
                                 compartment = NULL,
                                 unittype = NULL,
                                 notes = NULL) {
  if (name %in% names(model$variables))
    stopIQR("Provided variable name already exists in the model: ", name)
  if (!is.null(type)) {
    if (!(type %in% c("isSpecie", "isCompartment", "isParameter"))) {
      stopIQR("In variable ", name, ", type ", type,":\n",
              "Wrong definition of 'type'. Needs to be 'isSpecie', 'isCompartment', or 'isParameter'")
    }
  }
  if (!is.null(unittype)) {
    if (!(unittype %in% c("amount", "concentration"))) {
      stopIQR("In variable ", name, ", type ", unittype,":\n",
              "Wrong definition of 'unittype'. Needs to be 'amount' or 'concentration'")
    }
  }
  model$variables[[name]] <- list(formula     = formula,
                                  type        = type,
                                  compartment = compartment,
                                  unittype    = unittype,
                                  notes       = notes)
  return(model)
}
addInitialAssignment_IQRmodel <- function(model,
                                          name,
                                          formula,
                                          notes = NULL) {
  if (name %in% names(model$initalAssignments))
    stopIQR("Provided initial assignment element name already exists in the model: ", name)
  model$initalAssignments[[name]] <- list(formula     = formula,
                                          notes       = notes)
  return(model)
}
addReaction_IQRmodel <- function(model,
                                 name,
                                 formula,
                                 notes = NULL,
                                 reversible = FALSE,
                                 fast = FALSE) {
  if (name %in% names(model$reactions))
    stopIQR("Provided reaction name already exists in the model: ", name)
  model$reactions[[name]] <- list(formula    = formula,
                                  notes      = notes,
                                  reversible = reversible,
                                  fast       = fast)
  return(model)
}
addFunction_IQRmodel <- function(model,
                                 name,
                                 arguments,
                                 formula,
                                 notes = NULL) {
  if (name %in% names(model$functions))
    stopIQR("Provided function name already exists in the model: ", name)
  model$functions[[name]] <- list(arguments = arguments,
                                  formula   = formula,
                                  notes     = notes)
  return(model)
}
addEvent_IQRmodel <- function(model,
                              name,
                              trigger,
                              notes = NULL) {
  if (name %in% names(model$events))
    stopIQR("Provided event name already exists in the model: ", name)
  model$events[[name]] <- list(name       = name,
                               trigger    = trigger,
                               assignment = NULL,
                               notes      = notes)
  return(model)
}
addEventAssignment_IQRmodel <- function(model,
                                        eventname,
                                        variable,
                                        formula) {
  model$events[[eventname]]$assignment[[length(model$events[[eventname]]$assignment)+1]] <- list(variable=variable, formula=formula)
  return(model)
}
delInput_IQRmodel <- function(model, inputname) {
  si <- model$inputs[[inputname]]$stateindex
  for (k in seq_along(si)) {
    model$states[[si[k]]]$ODE <- aux_strrep(aux_removeWhiteSpace(model$states[[si[k]]]$ODE),model$inputs[[inputname]]$terms[[k]],"")
  }
  model$inputs[[inputname]] <- NULL
  model$parameters[[inputname]] <- NULL
  return(model)
}
#'@export
gt <- function (x,y) {
  return(as.numeric(x>y))
}
#'@export
ge <- function (x,y) {
  return(as.numeric(x>=y))
}
#'@export
lt <- function (x,y) {
  return(as.numeric(x<y))
}
#'@export
le <- function (x,y) {
  return(as.numeric(x<=y))
}
#'@export
mod <- function (x,y) {
  return(x %% y)
}
#'@export
and <- function (...) {
  varargin <- unlist(list(...))
  return(as.numeric(all(varargin==1)))
}
#'@export
or <- function (...) {
  varargin <- unlist(list(...))
  return(as.numeric(any(varargin==1)))
}
#'@export
piecewise <- function (...) {
  varargin <- unlist(list(...))
  nargin <- length(varargin)
  result <- NULL
  oddnumber = mod(nargin,2)
  for (k in seq(1,nargin-oddnumber,2)) {
    if (varargin[k+1] != 0) {
      result <- varargin[k]
      return(result)
    }
  }
  if (is.null(result)) {
    if (oddnumber==1) {
      result <- varargin[nargin]
    } else {
      stopIQR('Piecewise statement is wrongly defined - missing (but needed) default value');
    }
  }
  return(result)
}
#'@export
interp0 <- function (x,y,xi) {
  if (xi >= x[length(x)]) return(y[length(y)])
  if (xi <= x[1]) return(y[1])
  return(stats::approx(x,y,xi,method="const")$y)
}
#'@export
interp1 <- function (x,y,xi) {
  if (xi >= x[length(x)]) return(y[length(y)])
  if (xi <= x[1]) return(y[1])
  return(stats::approx(x,y,xi,method="linear")$y)
}
#'@export
interpcs <- function (x,y,xi) {
  if (xi >= x[length(x)]) return(y[length(y)])
  if (xi <= x[1]) return(y[1])
  return(stats::spline(x=x,y=y,xout=xi,method="natural")$y)
}
statesInfo_IQRmodel <- function(model__) {
  if (length(model__$states)==0)
    return(list(statenames=NULL,stateICs=NULL,stateODEs=NULL))
  statenames__ <- names(model__$states)
  stateICs__   <- c()
  stateODEs__  <- c()
  dummy__ <- sapply(model__$states, function (x) {
    stateICs__ <<- c(stateICs__, x$IC)
    stateODEs__ <<- c(stateODEs__, x$ODE)
  })
  names(statenames__) <- statenames__
  names(stateICs__) <- statenames__
  names(stateODEs__) <- statenames__
  return(list(statenames=statenames__,stateICs=stateICs__,stateODEs=stateODEs__))
}
parametersInfo_IQRmodel <- function(model__) {
  if (length(model__$parameters)==0)
    return(list(paramnames=NULL,paramvalues=NULL))
  paramnames__ <- names(model__$parameters)
  paramvalues__ <- sapply(model__$parameters, function (x) {
    x$value
  })
  names(paramnames__) <- paramnames__
  return(list(paramnames=paramnames__,paramvalues=paramvalues__))
}
variablesInfo_IQRmodel <- function(model__) {
  if (length(model__$variables)==0)
    return(list(varnames=NULL,varformulas=NULL))
  varnames__ <- names(model__$variables)
  varformulas__ <- sapply(model__$variables, function (x) {
    x$formula
  })
  names(varnames__) <- varnames__
  return(list(varnames=varnames__,varformulas=varformulas__))
}
reactionsInfo_IQRmodel <- function(model__) {
  if (length(model__$reactions)==0)
    return(list(reacnames=NULL,reacformulas=NULL,reacfast=NULL,reacreversible=NULL))
  reacnames__ <- names(model__$reactions)
  reacformulas__ <- c()
  reacfast__ <- c()
  reacreversible__ <- c()
  dummy__ <- sapply(model__$reactions, function (x) {
    reacformulas__ <<- c(reacformulas__, x$formula)
    reacfast__ <<- c(reacfast__, x$fast)
    reacreversible__ <<- c(reacreversible__, x$reversible)
  })
  names(reacnames__) <- reacnames__
  names(reacformulas__) <- reacnames__
  names(reacfast__) <- reacnames__
  names(reacreversible__) <- reacnames__
  return(list(reacnames=reacnames__,reacformulas=reacformulas__,reacfast=reacfast__,reacreversible=reacreversible__))
}
functionsInfo_IQRmodel <- function(model__) {
  if (length(model__$functions)==0)
    return(list(funcnames=NULL,funcformulas=NULL,funcarguments=NULL))
  funcnames__ <- names(model__$functions)
  funcformulas__ <- c()
  funcarguments__ <- c()
  dummy__ <- sapply(model__$functions, function (x) {
    funcformulas__ <<- c(funcformulas__, x$formula)
    funcarguments__ <<- c(funcarguments__, x$arguments)
  })
  names(funcnames__) <- funcnames__
  names(funcformulas__) <- funcnames__
  names(funcarguments__) <- funcnames__
  return(list(funcnames=funcnames__,funcformulas=funcformulas__,funcarguments=funcarguments__))
}
eventsInfo_IQRmodel <- function(model__) {
  if (length(model__$events)==0)
    return(list(evenames=NULL,evetriggers=NULL,evevariables=NULL,eveformulas=NULL))
  evenames__ <- names(model__$events)
  evetriggers__ <- c()
  evevariables__ <- c()
  eveformulas__ <- c()
  dummy__ <- sapply(model__$events, function (x) {
    evetriggers__ <<- c(evetriggers__, x$trigger)
    assignmentInfo__ <- unname(unlist(x$assignment))
    evevariables__ <<- c(evevariables__, list(assignmentInfo__[seq(1,length(assignmentInfo__),2)]))
    eveformulas__ <<- c(eveformulas__, list(assignmentInfo__[seq(2,length(assignmentInfo__),2)]))
  })
  names(evenames__) <- evenames__
  names(evetriggers__) <- evenames__
  names(evevariables__) <- evenames__
  names(eveformulas__) <- evenames__
  return(list(evenames=evenames__,evetriggers=evetriggers__,evevariables=evevariables__,eveformulas=eveformulas__))
}
outputInfo_IQRmodel <- function(model__) {
  if (length(model__$outputs)==0)
    return(list(outnames=NULL,outformulas=NULL,outvarindices=NULL))
  outnames__      <- names(model__$outputs)
  outformulas__   <- c()
  outvarindices__ <- c()
  dummy__ <- sapply(model__$outputs, function (x) {
    outformulas__ <<- c(outformulas__, x$formula)
    outvarindices__ <<- c(outvarindices__, x$varindex)
  })
  names(outnames__)      <- outnames__
  names(outformulas__)   <- outnames__
  names(outvarindices__) <- outnames__
  return(list(outnames=outnames__,outformulas=outformulas__,outvarindices=outvarindices__))
}
checkOrderVarReacDefinition <- function (model__) {
  X <- tryCatch({
    param__ <- sapply(model__$parameters, function (x__) x__$value)
    ICs__ <- sapply(model__$states, function (x__) x__$IC)
    states__ <- attributes(model__)$nonNumICs.IQRmodel(param__,ICs__,0)$ICs
    vars__ <- sapply(model__$variables, function (x__) x__$formula)
    reac__ <- sapply(model__$reactions, function (x__) x__$formula)
    time <- 0
    if (length(model__$variables) > 0) eval(parse(text=paste0(names(vars__),"<-NA")))
    if (length(model__$reactions) > 0) eval(parse(text=paste0(names(reac__),"<-NA")))
    eval(parse(text=paste0(names(states__),"<-",states__)))
    eval(parse(text=paste0(names(param__),"<-",param__)))
    if (length(model__$variables) > 0) eval(parse(text=paste0(names(vars__),"<-",vars__)))
    if (length(model__$reactions) > 0) eval(parse(text=paste0(names(reac__),"<-",reac__)))
    if (length(model__$variables) > 0) testVars__ <- sapply(names(vars__), function (x__) eval(parse(text=paste0("test<-",x__))))
    if (length(model__$reactions) > 0) testReac__ <- sapply(names(reac__), function (x__) eval(parse(text=paste0("test<-",x__))))
    "OK"
  }, error = function(err) {
    NULL
  })
  if (!is.null(X)) {
    if (length(model__$variables) > 0)  {
      ix__ <- which(is.na(testVars__))
      if (length(ix__) > 0) {
        stopIQR("The following variables are wrongly defined\nThe reason could be incorrect order of assignment or a divison by zero:\n",
                paste0(names(ix__),collapse=", "))
      }
    }
    if (length(model__$reactions) > 0) {
      ix__ <- which(is.na(testReac__))
      if (length(ix__) > 0) {
        stopIQR("The following reactions are wrongly defined\nThe reason could be incorrect order of assignment or a divison by zero:\n",
                paste0(names(ix__),collapse=", "))
      }
    }
  }
}
obfuscate_IQRmodel <- function(model, filename = NULL) {
  foldername <- tempdirIQR()
  modelname <- model
  if (is_IQRmodel(model)) {
    modelname <- file.path(foldername, "tmp.txt")
    export_IQRmodel(model, filename = modelname)
  }
  con <- file(modelname)
  script <- readLines(con)
  close.connection(con)
  is_eq <- sapply(script, function(s) !inherits(try(parse(text = s), silent = TRUE), "try-error") & grepl("=", s, fixed = TRUE))
  script[is_eq] <- sub("d/dt", "diffOp__", script[is_eq])
  symbols <- getSymbols(script[is_eq], exclude = "diffOp__")
  symbols <- symbols[!grepl("^INPUT[1-9]", symbols) & !grepl("^OUTPUT[1-9]", symbols)]
  nsymbols <- log10(length(symbols))
  newsymbols <- paste0("x", formatC(1:length(symbols), flag = "0", digits = ceiling(nsymbols)))
  script[is_eq] <- replaceSymbols(symbols, newsymbols, script[is_eq])
  script[is_eq] <- sub("diffOp__", "d/dt", script[is_eq])
  con <- file(file.path(foldername, "tmp.txt"))
  writeLines(script, con, sep = "\n")
  close.connection(con)
  m <- IQRmodel(file.path(foldername, "tmp.txt"), FLAGsim = FALSE)
  m$parameters <- lapply(m$parameters, function(mypar) {
    mypar$value <- signif(mypar$value*exp(stats::rnorm(1, 0, 1)), 3)
    return(mypar)
  })
  getNumbers <- function (char, exclude = NULL) {
    if (is.null(char))
      return(NULL)
    char <- char[char != "0"]
    out <- parse(text = char, keep.source = TRUE)
    out <- utils::getParseData(out)
    names <- unique(out$text[out$token == "NUM_CONST"])
    if (!is.null(exclude))
      names <- names[!names %in% exclude]
    return(names)
  }
  m$states <- lapply(m$states, function(mypar) {
    numbers <- getNumbers(mypar$IC)
    newnumbers <- signif(as.numeric(numbers)*exp(stats::rnorm(length(numbers), 0, 1)), 3)
    mypar$IC <- replaceSymbols(numbers, newnumbers, mypar$IC)
    return(mypar)
  })
  m$notes <- ""
  elements <- c("parameters", "variables", "states", "reactions", "outputs", "inputs", "initalAssignments")
  for (el in elements) {
    m[[el]] <- lapply(m[[el]], function(myel) {
      myel$notes <- NULL
      return(myel)
    })
  }
  m$name <- "Model"
  if (is.null(filename)) return(m)
  export_IQRmodel(m, filename = filename)
}
#'@export
is_IQRsimres <- function(input) {
  methods::is(input,"IQRsimres")
}
#'@export
print.IQRsimres <- function(x, ..., digits=3) {
  print.data.frame(x,digits=digits)
  cat("\nIQRsimres object")
  if (containsSensitivityInfoIQRsimres(x))
    cat('\n  Simulation results contain sensitivity information')
  if (hasIDcolumnIQRsimres(x)) cat('\n  Number of individual simulations: ',getNumberOfIndividualsIQRsimres(x),sep='')
}
hasIDcolumnIQRsimres <- function(x) {
  return("ID" %in% names(x))
}
hasCONDITIONcolumnIQRsimres <- function(x) {
  return("CONDITION" %in% names(x))
}
getNumberOfIndividualsIQRsimres <- function(x) {
  if (!is_IQRsimres(x))
    stopIQR("Input argument is not an IQRsimres object")
  if (!"ID" %in% names(x)) return(1)
  return(length(unique(x$ID)))
}
containsSensitivityInfoIQRsimres <- function(x) {
  if (!is_IQRsimres(x))
    stopIQR("Input argument is not an IQRsimres object")
  return(!is.null(attr(x,"sensitivity")))
}
#'@export
plot.IQRsimres <- function(x, ..., scales="free", facet="wrap", legend=TRUE) {
  xxx__ <- list(...)
  if (length(xxx__) > 0) {
    addIQMsimresIX__ <- which(unlist(lapply(xxx__, function(x) "IQRsimres" %in% class(x)))==TRUE)
  } else {
    addIQMsimresIX__ <- NULL
  }
  simNames__ <- setdiff(names(x),c("TIME","ID","CONDITION"))
  min__ <- match(simNames__[1],names(x))
  max__ <- match(simNames__[length(simNames__)],names(x))
  x.long__        <- tidyr::gather(x, "NAME", "VALUE", dplyr::one_of(simNames__))
  x.long.nar__    <- x.long__[!is.na(x.long__$VALUE),]
  if (!"ID" %in% names(x.long.nar__)) x.long.nar__$ID <- 1
  x.long.nar__$ID <- as.factor(x.long.nar__$ID)
  if (!"CONDITION" %in% names(x.long.nar__)) {
    x.long.nar__$CONDITION <- 1
    legend <- FALSE 
  }
  x.long.nar__$CONDITION <- as.factor(x.long.nar__$CONDITION)
  if (facet == "wrap") {
    p__ <- IQRggplot(x.long.nar__, aes_string(x = "TIME", y = "VALUE", group="ID", color="CONDITION")) +
      facet_wrap(~NAME, scales = scales) +
      scale_color_IQRtools() +
      geom_line(size=1)
  } else {
    p__ <- IQRggplot(x.long.nar__, aes_string(x = "TIME", y = "VALUE", color="ID", group="ID")) +
      facet_grid(NAME ~ CONDITION, scales = scales) +
      scale_color_IQRtools() +
      geom_line(size=1)
  }
  if (!legend) {
    p__ <- p__ + theme(legend.position="none")
  } else {
    p__ <- p__ + theme(legend.position="bottom",
                       legend.background=element_rect(linetype="solid",color="black"))
  }
  print(p__)
  p__
}
#'@export
is_IQRsimresMulti <- function(input) {
  methods::is(input,"IQRsimresMulti")
}
#'@export
print.IQRsimresMulti <- function(x, ..., digits=3) {
  print.data.frame(x,digits=digits)
  cat("\nIQRsimresMulti object")
  cat('\n  Number of individual simulations: ',length(unique(x$ID)),sep='')
  if (!is.null(attr(x,"sensitivity")))
    cat('\n  Simulation results contain sensitivity information')
}
#'@export
plot.IQRsimresMulti <- function(x, ..., type="quantiles",quantiles=c(0.05, 0.5, 0.95)) {
  if (type=="quantiles") {
    x$ID <- NULL
    data <- do.call(rbind,lapply(quantiles, function(qkkk) {
      a <- as.data.frame(do.call(rbind,lapply(split(x,x$TIME), function (yyy) {
        apply(yyy,2,stats::quantile,qkkk)
      })))
      a$QUANTILE <- paste0(qkkk*100,"th Quantile")
      a
    }))
    simNames__ <- setdiff(names(data),c("TIME","ID","CONDITION"))
    gather_cols <- setdiff(simNames__,"QUANTILE")
    x.long__        <- tidyr::gather(data, "NAME", "VALUE", dplyr::one_of(gather_cols))
    x.long.nar__    <- x.long__[!is.na(x.long__$VALUE),]
    x.long.nar__$QUANTILE <- as.factor(x.long.nar__$QUANTILE)
    p__ <- IQRggplot(x.long.nar__, aes_string(x = "TIME", y = "VALUE", group="NAME", color="QUANTILE")) +
      facet_wrap(~NAME, scales="free") +
      scale_color_IQRtools() +
      geom_line(size=1, aes_string(group="QUANTILE")) +
      theme(legend.position="bottom",
            legend.background=element_rect(linetype="solid",color="black"))
    print(p__)
  }
  if (type=="individual") {
    simNames__ <- setdiff(names(x),c("TIME","ID","CONDITION"))
    x.long__        <- tidyr::gather(x, "NAME", "VALUE", dplyr::one_of(simNames__))
    x.long.nar__    <- x.long__[!is.na(x.long__$VALUE),]
    x.long.nar__$ID <- as.factor(x.long.nar__$ID)
    p__ <- IQRggplot(x.long.nar__, aes_string(x = "TIME", y = "VALUE", group="NAME")) +
      facet_wrap(~NAME, scales="free") +
      scale_color_IQRtools() +
      geom_line(size=0.5,aes_string(group="ID"),alpha=0.1)
    print(p__)
  }
}
#'@export
#'@import numDeriv
fit_EmaxModel <- function(data, Emax = NULL, EC50 = NULL, Hill = NULL, y0 = 0, errmodel = "abs", log10Data = "") {
  stopifnot(
    is.null(Emax) || (is.numeric(Emax) && Emax > 0),
    is.null(EC50) || (is.numeric(EC50) && EC50 > 0),
    is.null(Hill) || (is.numeric(Hill) && Hill > 0),
    is.null(y0) || (is.numeric(y0)),
    is.character(errmodel),
    is.character(log10Data)
  )
  errmodel <- match.arg(errmodel, c("abs", "rel", "absrel"))
  if (log10Data != "") log10Data <- match.arg(log10Data, c("x", "y", "xy"))
  x <- data[, 1]
  y <- data[, 2]
  if (grepl("x", log10Data)) x <- 10^x
  natpars <- c("y0", "sigma_abs", "sigma_rel")
  logpars <- c("EC50", "Emax", "Hill")
  to_natScale <- function(p) {
    logpars <- setdiff(names(p), natpars)
    natpars <- intersect(names(p), natpars)
    if (length(logpars) > 0) p[logpars] <- exp(p[logpars])
    p
  }
  D_to_natScale <- function(p) {
    natpars <- intersect(names(p), natpars)
    logpars <- setdiff(names(p), natpars)
    if (length(natpars) > 0) p[natpars] <- 1
    if (length(logpars) > 0) p[logpars] <- exp(p[logpars])
    p
  }
  to_logScale <- function(p) {
    logpars <- setdiff(names(p), natpars)
    if (length(logpars) > 0) p[logpars] <- log(p[logpars])
    p
  }
  p.fixed <- to_logScale(
    c(Emax = Emax,
      EC50 = EC50,
      Hill = Hill,
      y0 = y0,
      sigma_abs = switch(errmodel, "abs" = NULL, "rel" = 0, "absrel" = NULL),
      sigma_rel = switch(errmodel, "abs" = 0, "rel" = NULL, "absrel" = NULL))
  )
  parameters <- c(logpars, natpars)
  parameters.fixed <- names(p.fixed)
  parameters.est <- setdiff(parameters, parameters.fixed)
  p.est.0 <- to_logScale(
    c(Emax = max(y),
      EC50 = 0.5*max(x),
      Hill = 1,
      y0 = min(y),
      sigma_abs = 0.05*max(y),
      sigma_rel = 0.05)
  )[parameters.est]
  prd <- function(p) {
    p["y0"] + p["Emax"]*x^p["Hill"]/(p["EC50"]^p["Hill"] + x^p["Hill"])
  }
  var.prd <- function(p) {
    p["sigma_abs"]^2 + p["sigma_rel"]^2 * prd(p)^2
  }
  ll <- function(p) {
    p <- to_natScale(p)
    sum(
      (prd(p) - y)^2/var.prd(p) + log(2*pi*var.prd(p))
    )
  }
  obj <- function(p) {
    p.full <- c(p, p.fixed)
    idx <- match(parameters.est, names(p.full))
    list(
      value = ll(p.full),
      gradient = structure(
        numDeriv::jacobian(ll, p.full, method = "complex")[1, idx],
        names = parameters.est),
      hessian = structure(
        numDeriv::hessian(ll, p.full, method = "complex")[idx, idx, drop = FALSE],
        dimnames = list(parameters.est, parameters.est))
    )
  }
  myfit <- trust(obj, p.est.0, rinit = 1, rmax = 10)
  myfit.pars <- c(myfit$argument, p.fixed)[parameters]
  myfit.cov <- structure(MASS::ginv(myfit$hessian), dimnames = list(parameters.est, parameters.est))
  eigen__ <- eigen(myfit$hessian)
  tol__ <- 1e-8
  V__ <- eigen__$vectors[, abs(eigen__$values) < tol__, drop = FALSE]
  identifiable__ <- apply(V__, 1, function(v) all(abs(v) < tol__))
  diag(myfit.cov)[!identifiable__] <- Inf
  myfit.se <- c(sqrt(diag(myfit.cov)),
                structure(rep(NA, length(p.fixed)), names = parameters.fixed))[parameters]
  myparameters <- data.frame(
    name = parameters,
    value = abs(to_natScale(myfit.pars)),
    se = abs(myfit.se*D_to_natScale(myfit.pars)),
    fixed = parameters %in% parameters.fixed,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  myprd <- function(x) {
    if (grepl("x", log10Data)) x <- 10^x
    p <- to_natScale(myfit.pars)
    y <- p["y0"] + p["Emax"]*x^p["Hill"]/(p["EC50"]^p["Hill"] + x^p["Hill"])
    sigma <- sqrt(p["sigma_abs"]^2 + p["sigma_rel"]^2 * y^2)
    out <- data.frame(x = x, y = y, lower = y - sigma, upper = y + sigma)
    if (grepl("x", log10Data)) out[["x"]] <- log10(out[["x"]])
    if (grepl("y", log10Data)) for (n in c("y", "lower", "upper")) out[[n]] <- log10(out[[n]])
    names(out)[1:2] <- names(data)[1:2]
    out
  }
  myprediction <- myprd(
    if (grepl("x", log10Data))
      log10(x)
    else
      x
  )
  myprediction.long <- myprd(
    if (grepl("x", log10Data))
      seq(min(log10(x)), max(log10(x)), length.out = 100)
    else
      seq(min(x), max(x), length.out = 100)
  )
  myplot <- IQRggplot(myprediction.long, aes_string(x = names(myprediction)[1],
                                                    names(myprediction)[2])) +
    geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = .3) +
    geom_line() +
    geom_point(data = data)
  mytable <- IQRtable(myparameters,
                      stat = statSE(value = value, se = se),
                      Name = "name",
                      Value = "value",
                      SE = "se",
                      RSE  = "rse%",
                      Fixed = "fixed")
  list(
    objval = myfit[["value"]],
    converged = myfit[["converged"]],
    iterations = myfit[["iterations"]],
    parameters = myparameters,
    prd = myprd,
    prediction = myprediction,
    plot = function() myplot,
    table = mytable
  )
}
calcNNic <- function(model,parametersSim__,time) {
  if (hasOnlyNumericICs_IQRmodel(model) & !hasInitialAssignments_IQRmodel(model)) {
    return(list(ICs=statesInfo_IQRmodel(model)$stateICs,params=parametersSim__))
  }
  ICs__ <- sapply(model$states, function (x__) x__$IC)
  res__ <- attr(model,"nonNumICs.IQRmodel")(parametersSim__,ICs__,time)
  calcIC__ <- res__$ICs
  IAs__ <- res__$IAs
  parametersSim__[names(IAs__)] <- IAs__
  return(list(ICs=calcIC__,param=parametersSim__))
}
#'@export
stoichiometry_IQRmodel <- function (model,raw=TRUE) {
  if (!is_IQRmodel(model)) stopIQR("Model argument is not an IQRmodel")
  if (length(model$reactions)==0) return(list(N=NULL, statenames=NULL, reacnames=NULL, reacreversible=NULL))
  N              <- c()
  statenames     <- c()
  for (k in seq_along(model$states)) {
    Nrow__       <- parseStoichiometryODE_IQRmodel(aux_removeWhiteSpace(model$states[[k]]$ODE),model,raw)
    if (!is.null(Nrow__) & !(NA %in% Nrow__)) {
      N          <- rbind(N,Nrow__)
      statenames <- c(statenames,names(model$states)[k])
    }
  }
  return(list(N=N,
              statenames=statenames,
              reacnames=names(model$reactions),
              reacreversible=reactionsInfo_IQRmodel(model)$reacreversible))
}
parseStoichiometryODE_IQRmodel <- function(ODE,model,raw) {
  paramInfo__ <- parametersInfo_IQRmodel(model)
  reacInfo__  <- reactionsInfo_IQRmodel(model)
  nrOpenParentheses__   <- length(aux_strFindAll(ODE,"(")$start)
  nrClosedParentheses__ <- length(aux_strFindAll(ODE,")")$start)
  compartmentSize__     <- 1
  if (nrOpenParentheses__ != nrClosedParentheses__) stopIQR("Parentheses not in pairs")
  if (nrOpenParentheses__ > 1) return(NULL) 
  if (nrOpenParentheses__==1) {
    if (substr(ODE,1,1) != "(") return(NULL) 
    closePar__          <- aux_strFindAll(ODE,")")
    ODEinpar__          <- substr(ODE,2,closePar__$start-1)
    rest__              <- substr(ODE,closePar__$start+1,nchar(ODE))
    if (substr(rest__,1,1) != "/") return(NULL) 
    rest__              <- substr(rest__,2,nchar(rest__))
    index__             <- aux_strmatch(paramInfo__$paramnames,rest__)
    if (is.null(index__)) return(NULL) 
    compartmentSize__ = paramInfo__$paramvalues[index__]
    ODE <- ODEinpar__
  }
  ODE <- paste0(ODE,"+")
  terms__               <- c()
  termIndex__           <- 1
  if (substr(ODE,1,1)=="-") {
    signCurrent__ <- -1
    lastIndex__ <- 2
  } else {
    if (substr(ODE,1,1)=="+") {
      signCurrent__ <- +1
      lastIndex__ <- 2
    } else {
      signCurrent__ = +1
      lastIndex__ = 1
    }
  }
  Nrow__ <- as.vector(matrix(0,1,length(reacInfo__$reacnames)))
  startk__ <- lastIndex__
  for (k in startk__:nchar(ODE)) {
    if (substr(ODE,k,k) == '+' | substr(ODE,k,k) == '-') {
      element__ <- substr(ODE,lastIndex__,k-1)
      multIndex__ <- aux_strFindAll(element__,"*")$start
      if (is.null(multIndex__)) {
        stoichiometry__ <- signCurrent__*1
        reactionterm__ <- element__
      } else {
        if (length(multIndex__) > 1) return(NULL) 
        suppressWarnings(absStoichiometry <- as.numeric(substr(element__,1,multIndex__-1)))
        stoichiometry__ <- signCurrent__*absStoichiometry
        reactionterm__ <- substr(element__,multIndex__+1,nchar(element__))
      }
      indexReaction__ <- aux_strmatch(reacInfo__$reacnames,reactionterm__)
      if (is.null(indexReaction__)) return(NULL)
      if(is.na(stoichiometry__)) return(NULL)
      Nrow__[indexReaction__] <- Nrow__[indexReaction__] + stoichiometry__
      termIndex__ <- termIndex__ + 1
      lastIndex__ <- k+1
      if (substr(ODE,k,k) == "+")
        signCurrent__ <- +1
      else
        signCurrent__ <- -1
    }
  }
  if (!raw) Nrow__ <- Nrow__ / compartmentSize__
  return(Nrow__)
}
symStateGradient_IQRmodel <- function (model,paramnames=NULL) {
  if (!is_IQRmodel(model))
    stopIQR("model argument is not an IQRmodel")
  result__    <- prepareIQRmodelODEs4symbolic(model)
  stateInfo__ <- result__$stateInfo
  varInfo__   <- result__$varInfo
  paramInfo__ <- parametersInfo_IQRmodel(model)
  if (is.null(paramInfo__$paramnames)) {
    warningIQR("No parameters in model. No gradient derived.")
    return(NULL)
  }
  if (!is_differentiable_expression(model,stateInfo__$stateODEs)) return(NULL)
  if (is.null(paramnames)) {
    paramnames <- paramInfo__$paramnames
    isNotINPUTparam__ <- unlist(gregexpr(pattern="\\<INPUT[0-9]+",text=paramnames)) == -1
    paramnames <- paramnames[isNotINPUTparam__]
  }
  test__ <- setdiff(paramnames,paramInfo__$paramnames)
  if (length(test__) > 0)
    stopIQR("One or more of the provided paramnames are not present in the IQRmodel.")
  gradient__ <- matrix(nrow=length(stateInfo__$statenames),ncol=length(paramnames))
  for (k in seq_along(stateInfo__$statenames)) {
    for (k2 in seq_along(paramnames)) {
      x <- eval(parse(text=paste0("expression(",aux_removeWhiteSpace(stateInfo__$stateODEs[k]),")")))
      res__ <- paste0(deparse(stats::D(x,aux_removeWhiteSpace(paramnames[k2]))),collapse="")
      gradient__[k,k2] <- res__
    }
  }
  rownames(gradient__) <- stateInfo__$statenames
  colnames(gradient__) <- paramnames
  output <- list(gradient=gradient__, statenames=stateInfo__$statenames, paramnames=paramnames)
  return(output)
}
symOutputGradient_IQRmodel <- function (model,paramnames=NULL) {
  if (!is_IQRmodel(model))
    stopIQR("model argument is not an IQRmodel")
  result__    <- prepareIQRmodelODEs4symbolic(model)
  stateInfo__ <- result__$stateInfo
  varInfo__   <- result__$varInfo
  paramInfo__ <- parametersInfo_IQRmodel(model)
  if (is.null(paramInfo__$paramnames))
    stopIQR("No parameters in model. Derivation of gradient does not make sense.")
  outInfo__   <- outputInfo_IQRmodel(model)
  outInfo__$outformulas <- varInfo__$varformulas[outInfo__$outvarindices]
  if (!is_differentiable_expression(model,stateInfo__$stateODEs)) return(NULL)
  if (!is_differentiable_expression(model,outInfo__$outformulas)) return(NULL)
  if (is.null(paramnames)) {
    paramnames <- paramInfo__$paramnames
    isNotINPUTparam__ <- unlist(gregexpr(pattern="\\<INPUT[0-9]+",text=paramnames)) == -1
    paramnames <- paramnames[isNotINPUTparam__]
  }
  test__ <- setdiff(paramnames,paramInfo__$paramnames)
  if (length(test__) > 0)
    stopIQR("One or more of the provided paramnames are not present in the IQRmodel.")
  dgdp__ <- matrix(nrow=length(outInfo__$outnames),ncol=length(paramnames))
  for (k in seq_along(outInfo__$outnames)) {
    for (k2 in seq_along(paramnames)) {
      x <- eval(parse(text=paste0("expression(",aux_removeWhiteSpace(outInfo__$outformulas[k]),")")))
      res__ <- paste0(deparse(stats::D(x,aux_removeWhiteSpace(paramnames[k2]))),collapse="")
      dgdp__[k,k2] <- res__
    }
  }
  rownames(dgdp__) <- outInfo__$outnames
  colnames(dgdp__) <- paramnames
  output <- list(dgdp=dgdp__, outnames=outInfo__$outnames, outformulas=outInfo__$outformulas, paramnames=paramnames)
  return(output)
}
symStateJacobian_IQRmodel <- function (model) {
  if (!is_IQRmodel(model))
    stopIQR("model argument is not an IQRmodel")
  result__    <- prepareIQRmodelODEs4symbolic(model)
  stateInfo__ <- result__$stateInfo
  varInfo__   <- result__$varInfo
  paramInfo__ <- parametersInfo_IQRmodel(model)
  if (!is_differentiable_expression(model,stateInfo__$stateODEs)) return(NULL)
  jacobian__ <- matrix(nrow=length(stateInfo__$statenames),ncol=length(stateInfo__$statenames))
  for (k in seq_along(stateInfo__$statenames)) {
    for (k2 in seq_along(stateInfo__$statenames)) {
      x <- eval(parse(text=paste0("expression(",aux_removeWhiteSpace(stateInfo__$stateODEs[k]),")")))
      res__ <- paste0(deparse(stats::D(x,aux_removeWhiteSpace(stateInfo__$statenames[k2]))),collapse="")
      jacobian__[k,k2] <- res__
    }
  }
  rownames(jacobian__) <- stateInfo__$statenames
  colnames(jacobian__) <- stateInfo__$statenames
  output <- list(jacobian=jacobian__, statenames=stateInfo__$statenames)
  return(output)
}
symOutputJacobian_IQRmodel <- function (model) {
  if (!is_IQRmodel(model))
    stopIQR("model argument is not an IQRmodel")
  result__    <- prepareIQRmodelODEs4symbolic(model)
  stateInfo__ <- result__$stateInfo
  varInfo__   <- result__$varInfo
  paramInfo__ <- parametersInfo_IQRmodel(model)
  outInfo__   <- outputInfo_IQRmodel(model)
  outInfo__$outformulas <- varInfo__$varformulas[outInfo__$outvarindices]
  if (!is_differentiable_expression(model,stateInfo__$stateODEs)) return(NULL)
  if (!is_differentiable_expression(model,outInfo__$outformulas)) return(NULL)
  dgdx__ <- matrix(nrow=length(outInfo__$outnames),ncol=length(stateInfo__$statenames))
  for (k in seq_along(outInfo__$outnames)) {
    for (k2 in seq_along(stateInfo__$statenames)) {
      x <- eval(parse(text=paste0("expression(",aux_removeWhiteSpace(outInfo__$outformulas[k]),")")))
      res__ <- paste0(deparse(stats::D(x,aux_removeWhiteSpace(stateInfo__$statenames[k2]))),collapse="")
      dgdx__[k,k2] <- res__
    }
  }
  rownames(dgdx__) <- outInfo__$outnames
  colnames(dgdx__) <- stateInfo__$statenames
  output <- list(dgdx=dgdx__, outnames=outInfo__$outnames, outformulas=outInfo__$outformulas, statenames=stateInfo__$statenames)
  return(output)
}
prepareIQRmodelODEs4symbolic <- function(model) {
  stateInfo__ <- statesInfo_IQRmodel(model)
  paramInfo__ <- parametersInfo_IQRmodel(model)
  varInfo__   <- variablesInfo_IQRmodel(model)
  reacInfo__  <- reactionsInfo_IQRmodel(model)
  if (length(varInfo__$varnames) > 0) {
    for (k in 1:length(varInfo__$varnames)) {
      if (k+1 <= length(varInfo__$varnames)) {
        for (k2 in (k+1):length(varInfo__$varnames)) {
          varInfo__$varformulas[k2] <- gsub(pattern = paste0("\\<",varInfo__$varnames[k],"\\>"),replacement = paste0("(",varInfo__$varformulas[k],")"),x = varInfo__$varformulas[k2])
        }
      }
    }
  }
  if (length(reacInfo__$reacnames) > 0) {
    for (k in 1:length(reacInfo__$reacnames)) {
      if (k+1 <= length(reacInfo__$reacnames)) {
        for (k2 in (k+1):length(reacInfo__$reacnames)) {
          reacInfo__$reacformulas[k2] <- gsub(pattern = paste0("\\<",reacInfo__$reacnames[k],"\\>"),replacement = paste0("(",reacInfo__$reacformulas[k],")"),x = reacInfo__$reacformulas[k2])
        }
      }
    }
  }
  if (length(stateInfo__$statenames) > 0) {
    for (k in 1:length(stateInfo__$statenames)) {
      if (length(reacInfo__$reacnames) > 0) {
        for (k2 in 1:length(reacInfo__$reacnames)) {
          updatedODE__ <- gsub(pattern = paste0("\\<",reacInfo__$reacnames[k2],"\\>"),replacement = paste0("(",reacInfo__$reacformulas[k2],")"),x = stateInfo__$stateODEs[k])
          stateInfo__$stateODEs[k] <- updatedODE__
        }
      }
    }
  }
  if (length(stateInfo__$statenames) > 0) {
    for (k in 1:length(stateInfo__$statenames)) {
      if (length(varInfo__$varnames) > 0) {
        for (k2 in 1:length(varInfo__$varnames)) {
          updatedODE__ <- gsub(pattern = paste0("\\<",varInfo__$varnames[k2],"\\>"),replacement = paste0("(",varInfo__$varformulas[k2],")"),x = stateInfo__$stateODEs[k])
          stateInfo__$stateODEs[k] <- updatedODE__
        }
      }
    }
  }
  return(list(stateInfo=stateInfo__,varInfo=varInfo__))
}
is_differentiable_expression <- function(model,checkTextVector) {
  if (hasCcodeFunctions_IQRmodel(model)) {
    return(FALSE)
  }
  if (length(model$functions)>0) {
    return(FALSE)
  }
  if (hasFastReactions_IQRmodel(model)) {
    return(FALSE)
  }
  for (k in seq_along(checkTextVector)) {
    if (regexpr(pattern="\\<piecewise",text=checkTextVector[k]) != -1) {
      return(FALSE)
    }
  }
  for (k in seq_along(checkTextVector)) {
    if (regexpr(pattern="\\<interp",text=checkTextVector[k]) != -1) {
      return(FALSE)
    }
  }
  for (k in seq_along(checkTextVector)) {
    if (regexpr(pattern="\\<min\\(",text=checkTextVector[k]) != -1) {
      return(FALSE)
    }
    if (regexpr(pattern="\\<max\\(",text=checkTextVector[k]) != -1) {
      return(FALSE)
    }
  }
  return(TRUE)
}
#'@export
geom_uperrorbar <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomUperrorbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
GeomUperrorbar <- ggproto("GeomUperrorbar", Geom,
                          default_aes = aes(colour = "black", size = 0.5, linetype = 1, width = 0.5,
                                            alpha = NA),
                          draw_key = draw_key_path,
                          required_aes = c("x", "y", "ymax"),
                          setup_data = function(data, params) {
                            data$width <- data$width %II%
                              params$width %II% (resolution(data$x, FALSE) * 0.9)
                            transform(data,
                                      xmin = x - width / 2, xmax = x + width / 2, width = NULL
                            )
                          },
                          draw_panel = function(data, panel_scales, coord, width = NULL) {
                            GeomPath$draw_panel(data.frame(
                              x = as.vector(rbind(data$xmin, data$xmax, NA, data$x,   data$x)),
                              y = as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$y)),
                              colour = rep(data$colour, each = 5),
                              alpha = rep(data$alpha, each = 5),
                              size = rep(data$size, each = 5),
                              linetype = rep(data$linetype, each = 5),
                              group = rep(1:(nrow(data)), each = 5),
                              stringsAsFactors = FALSE,
                              row.names = 1:(nrow(data) * 5)
                            ), panel_scales, coord)
                          }
)
"%II%" <- function(a, b) {
  if (!is.null(a)) a else b
}
mybreaks <- function(n_major, n_minor, allowAddMinor = TRUE, allowReduceMajor = TRUE) {
  function(limits) {
    upper <- log10(limits[2])
    lower <- log10(limits[1])
    n <- ceiling(upper) - floor(lower)
    step <- 1
    logbreaks <- seq(floor(lower), ceiling(upper), step)
    nombreaks <- 10^logbreaks
    nombreaks.inrange <- nombreaks[nombreaks > limits[1] & nombreaks < limits[2]]
    if (allowAddMinor & length(nombreaks.inrange) < n_major & n_minor > 0) {
      minbreaks <- do.call("c", lapply(seq_len(length(logbreaks) - 1), function(i) {
        allminbreaks <- seq(10^logbreaks[i], 10^logbreaks[i+1], length.out = 10)
        allminbreaks[seq(1, 10, length.out = n_minor + 2)][-c(1, n_minor + 2)]
      }))
      minbreaks.inrange <- unique(minbreaks[minbreaks > limits[1] & minbreaks < limits[2]])
      nombreaks <- sort(union(nombreaks, minbreaks.inrange))
    }
    if (allowReduceMajor & length(nombreaks.inrange) > n_major) {
      nombreaks <- nombreaks.inrange[seq(1, length(nombreaks.inrange), by = floor(length(nombreaks.inrange)/n_major))]
    }
    nombreaks
  }
}
myminorbreaks <- function(n_major, n_minor) {
  n_minor <- 10
  function(limits) {
    logbreaks <- log10(mybreaks(n_major, n_minor, allowAddMinor = FALSE, allowReduceMajor = FALSE)(limits))
    minbreaks <- do.call("c", lapply(seq_len(length(logbreaks) - 1), function(i) {
      seq(10^logbreaks[i], 10^logbreaks[i+1], length.out = 10)
    }))
    return(minbreaks)
  }
}
mylabels <- function(labeltype = "natural") {
  function(breaks) {
    if (labeltype == "standard") {
      labels <- format(breaks)
    }
    if (labeltype == "scientific") {
      logbreaks <- log10(breaks)
      labels <- parse(text = paste0("10^", logbreaks))
    }
    if (labeltype == "natural") {
      labels <- format(breaks, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)
    }
    if (labeltype == "comma") {
      labels <- format(breaks, scientific = FALSE, drop0trailing = TRUE, trim = TRUE, big.mark = ",")
    }
    return(labels)
  }
}
#'@export
scale_y_log10_IQRtools <- function(labeltype = c("natural", "comma", "scientific", "standard"), n_major = 4, n_minor = 0, ...) {
  labeltype <- match.arg(labeltype)
  args <- list(...)
  if (n_major < 1) stopIQR("scale_y_log10_IQRtools() requires n_major to be > 0.")
  if (n_minor < 0) stopIQR("scale_y_log10_IQRtools() requires n_minor to be >= 0.")
  if (length(args) > 0 & is.null(names(args)) | any(names(args) %in% ""))
    stopIQR("scale_y_log10_IQRtools() requires that arguments passed via ... are named.")
  args <- c(args, list(breaks = mybreaks(n_major, n_minor),
                       minor_breaks = myminorbreaks(n_major, n_minor),
                       labels = mylabels(labeltype = labeltype)))
  args <- args[!duplicated(names(args))]
  do.call(scale_y_log10, args)
}
#'@export
scale_x_log10_IQRtools <- function(labeltype = c("natural", "comma", "scientific", "standard"), n_major = 4, n_minor = 0, ...) {
  labeltype <- match.arg(labeltype)
  args <- list(...)
  if (n_major < 1) stopIQR("scale_x_log10_IQRtools() requires n_major to be > 0.")
  if (n_minor < 0) stopIQR("scale_x_log10_IQRtools() requires n_minor to be >= 0.")
  if (length(args) > 0  & is.null(names(args)) | any(names(args) %in% ""))
    stopIQR("scale_x_log10_IQRtools() requires that arguments passed via ... are named.")
  args <- c(args, list(breaks = mybreaks(n_major, n_minor),
                       minor_breaks = myminorbreaks(n_major, n_minor),
                       labels = mylabels(labeltype = labeltype)))
  args <- args[!duplicated(names(args))]
  do.call(scale_x_log10, args)
}
#'@export
printGrid <- function (plotList,nrow=4,ncol=4) {
  if (length(plotList)==0) return(invisible(NULL))
  x <- which(!sapply(plotList, is.null))
  plotList <- plotList[x]
  pieces__ <- aux_splitVectorEqualPieces(x = 1:length(plotList), nrow*ncol)
  for (k__ in seq_along(pieces__)) {
    pO__ <- cowplot::plot_grid(plotlist=plotList[pieces__[[k__]]],nrow=nrow,ncol=ncol)
    print(pO__)
  }
}
plot_slides <- function(plotlist, nrow = 3, ncol = 3, legend = NULL, legend.width = 0.2) {
  plots__ <- plotlist
  attr_out__ <- attr(plotlist, "plotData")
  plotsPerSlide__ <- nrow*ncol
  nPlots__ <- length(plots__)
  nSlides__ <- ceiling(nPlots__/plotsPerSlide__)
  slides__ <- lapply(seq_len(nSlides__), function(i__) {
    range__ <- plotsPerSlide__*(i__ - 1) + 1:plotsPerSlide__
    myplots__ <- vector("list", plotsPerSlide__)
    for (j__ in seq_len(plotsPerSlide__)) {
      if (range__[j__] > length(plots__)) {
        myplots__[[j__]] <- ggplot() + theme_classic()
      } else {
        myplots__[[j__]] <- plots__[[range__[j__]]]
      }
    }
    myplots__
  })
  output__ <- lapply(slides__, function(myslide__) {
    out__ <- suppressWarnings(cowplot::plot_grid(plotlist = myslide__, nrow = nrow, ncol = ncol)) 
    if (!is.null(legend))
      out__ <- cowplot::plot_grid(out__, legend, ncol = 2, rel_widths = c(1-legend.width, legend.width))
    return(out__)
  })
  attr(output__, "plotData") <- attr_out__
  class(output__) <- "IQRslideplot"
  return(output__)
}
#'@export
print.IQRslideplot <- function(x, ...) {
  if (length(x) > 1) cat(length(x), "page output has been printed.\n")
  for (n__ in x) print(n__)
}
plotListObjects_ggplot <- function (myList) {
  plots <- aux_extractObjects(myList, "ggplot")
  for (p in plots) print(p)
}
get_timelabelIQR <- function(data) paste0("Time (",tolower(unique(data$TIMEUNIT)),")")
get_obslabelIQR <- function(data) paste0(unique(data$NAME), " (", unique(data$UNIT), ")")
scale_color_IQRblloq <- scale_color_manual("BLOQ", values = c("yes" = '#F15A60', "no" = '#737373'), drop = FALSE)
scale_shape_IQRblloq <- scale_shape_manual("BLOQ", values = c("yes" = 4, "no" = 19), drop = FALSE)
handle_obsScalesIQR <- function(scale, varNames, default = "log"){
  sc__ <- scale
  vn__ <- varNames
  if (is.null(sc__)) sc__ <- default
  if (!all(sc__ %in% c("log","linear")))
    stopIQR("Unknown scale defined. Needs to be 'linear' or 'log'.")
  if ( is.null(names(sc__)) ){
    if ( length(sc__) == 1 ){
      out__ <- data.frame(
        NAME = vn__,
        scale = sc__,
        stringsAsFactors = FALSE
      )
    } else {
      stopIQR("If given scale unnamed, provide only 'linear' or 'log' or name by observation.")
    }
  } else {
    if ( all(names(sc__) %in% vn__) ) {
      miss.scale__ <- setdiff(vn__,names(sc__))
      for (missk in miss.scale__) sc__ <- eval(parse(text=paste0("c(sc__, \"",missk,"\" = \"",default,"\")")))
      out__ <- data.frame(
        NAME = names(sc__),
        scale = sc__,
        stringsAsFactors = FALSE
      )
    } else {
      stopIQR("Scale defined for non-existing observation.")
    }
  }
  return(out__)
}
scale_y_apply <- function(scaletype, ...){
  if (scaletype == "log") {
    scale_y_log10(...)
  } else {
    scale_y_continuous(...)
  }
}
scale_x_apply <- function(scaletype, ...){
  if (scaletype == "log") {
    scale_x_log10(...)
  } else {
    scale_x_continuous(...)
  }
}
median90range <- function(x) {
  return(
    data.frame(
      y    = stats::median(x, na.rm=TRUE),
      ymin = stats::quantile(x, 0.05, na.rm=TRUE),
      ymax = stats::quantile(x, 0.95, na.rm=TRUE)
    )
  )
}
samplesize  <- function(x) {
  N__ = sum(!is.na(x))
  return(c(y=N__,label=N__))
}
handle_stratificationIQR <- function(x, stratify) {
  data__ <- x
  strat__ <- stratify
  if (!is.null(attr(data__,"covInfo")))
    covInfo0__ <- subset(attr(data__,"covInfo"), !TIME.VARYING)
  else
    covInfo0__ <- data.frame()
  if (!is.null(attr(data__,"catInfo")))
    catInfo0__ <- subset(attr(data__,"catInfo"), !TIME.VARYING)
  else
    catInfo0__ <- data.frame()
  missingstrat__ <- setdiff(stratify, c(covInfo0__$COLNAME, catInfo0__$COLNAME))
  if (length(missingstrat__) != 0){
    warningIQR('Following stratifier(s) are no time-independent covariates in the dataset: ',paste0(missingstrat__, collapse = ", "))
    strat__ = intersect(strat__,c(covInfo0__$COLNAME, catInfo0__$COLNAME))
  }
  stratcont__ <- intersect(strat__, covInfo0__$COLNAME)
  for (k__ in seq_along(stratcont__)) {
    strk__ <- stratcont__[k__]
    strmed__ <- stats::median(unique(as.data.frame(data__)[,c("USUBJID",strk__)])[[strk__]], na.rm = TRUE)
    strknew__ <- paste0(strk__,"CAT")
    data__[[strknew__]] <- (data__[[strk__]] > strmed__)+1
    covInfo0k__ <- subset(covInfo0__, COLNAME == strk__)
    catInfo0__ <- rbind(catInfo0__,
                        data.frame(COLNAME = strknew__, NAME = covInfo0k__$NAME, UNIT = covInfo0k__$UNIT, VALUES = "1,2",
                                   VALUETXT = paste0("< ",strmed__,covInfo0k__$UNIT,",",">= ",strmed__,covInfo0k__$UNIT),
                                   TIME.VARYING = FALSE)
    )
    strat__[strat__ == strk__] <- strknew__
  }
  return(list(data__, strat__, catInfo0__))
}
get_xposDosing <- function(xx__, tmin, tmax) {
  nd__ <- length(unique(xx__$NAME))
  if (nd__ == 1) xx__$TIMEdos <- xx__$TIME else {
    xx__$DNo <- as.numeric(factor(xx__$NAME))
    mar <- 0.005 * (tmax-tmin)  
    a = 2*mar / (nd__-1)
    b = - a - mar
    xx__$TIMEdos <- xx__$TIME + a*xx__$DNo+b
  }
  out <- xx__
  out
}
get_yposDosing <- function(xx__, ymin, ymax, sc__) {
  if (ymin == ymax) {
    if (sc__ == "linear"){
      ymin <- ymin-0.5
      ymax <- ymax
    } else {
      ymin <- 10^(log10(ymin)-0.5)
      ymax <- ymax
    }
  }
  dmax <- max(xx__$VALUE, na.rm = TRUE)
  xx__$ystart <- ymin
  if (dmax == 0) { 
    xx__$yend = xx__$ystart
  } else {
    if (sc__ == "linear"){
      xx__$yend   <- ymin + 0.5*(ymax-ymin)*xx__$VALUE/dmax
    } else {
      xx__$yend   <- exp(log(ymin)+0.5*(log(ymax)-log(ymin))*xx__$VALUE/dmax)
    }
  }
  out <- xx__
  out
}
get_labelDosing <- function(xx__) {
  xx__$labelDos <- ifelse(
    xx__$ADDL == 0 | is.na(xx__$ADDL),
    xx__$VALUE,
    paste0(xx__$ADDL+1, "x ", xx__$VALUE, " every ", round(xx__$II)," ",  xx__$TIMEUNIT)
  )
  if (dim(xx__)[1] > 1) {
    xx__ <- xx__[order(xx__$NAME,xx__$TIME),]
    for (k in seq(dim(xx__)[1],2,-1) )
      if (xx__$ADDL[k] == 0 | is.na(xx__$ADDL[k])) {
        xx__$labelDos[k] <- ifelse(
          xx__$labelDos[k] == xx__$labelDos[k-1] & xx__$NAME[k] == xx__$NAME[k-1],
          "",
          xx__$labelDos[k]
        )
      }
  }
  out <- xx__
  out
}
add_LayerDosingSingle <- function(plobj__, dInfo__, ymin, ymax, tmin, tmax, sc__) {
  dInfo__ <- get_yposDosing(dInfo__, ymin, ymax, sc__)
  dInfo__ <- get_xposDosing(dInfo__, tmin, tmax)
  dInfo__ <- get_labelDosing(dInfo__)
  out__ <- plobj__ +
    geom_segment(data=dInfo__, mapping = aes_string(x="TIMEdos", xend = "TIMEdos", y = "ystart", yend = "yend", color = "NAME"), size = 0.5, linetype = 2) +
    geom_text(data=dInfo__, mapping = aes_string(x="TIMEdos", y="yend", label = "labelDos", color = "NAME"), angle = 90, size = 2.5, hjust = 0, show.legend = FALSE) +
    scale_color_manual("", values=IQRtoolsColors[2:20])
  out__
}
add_LayerDosingMulti <- function(plobj__, dInfo__, ymin, ymax, tmin, tmax, sc__) {
  dInfoS__ <- subset(dInfo__, ADDL == 0)
  if ("NT" %in% names(dInfo__)) {
    dInfoS__ <- dInfoS__[order(dInfoS__$NT),]
  } else {
    dInfoS__ <- dInfoS__[order(dInfoS__$TIME),]
  }
  diffT <- diff(dInfoS__$TIME)
  medianDiffT <- stats::median(diffT)
  diffdiffT <- abs(diff(diffT))
  idxBreak1 <- c(TRUE, FALSE, diffdiffT/diffT[1:(length(diffT)-1)] > 0.3)
  idxBreak2 <- c(TRUE, diff(dInfoS__$VALUE) > 0)
  idxBreak <- c(which(idxBreak1 | idxBreak2), dim(dInfoS__)[1]+1)
  dInfoSM__ <- data.frame(NAME = rep(dInfoS__$NAME[1], length(idxBreak)-1), TIME=NA,VALUE=NA,ADDL=NA,II=NA)
  for (k in 1:(length(idxBreak)-1)) {
    dInfoSM__$TIME[k]  <- dInfoS__$TIME[idxBreak[k]]
    dInfoSM__$VALUE[k] <- dInfoS__$VALUE[idxBreak[k]]
    dInfoSM__$ADDL[k]  <- idxBreak[k+1]-idxBreak[k]-1
    if (dInfoSM__$ADDL[k] == 0) {
      dInfoSM__$II[k]    <- medianDiffT
    } else {
      dInfoSM__$II[k]    <- mean(c(diffT,diffT[length(diffT)])[idxBreak[k]:(idxBreak[k+1]-2)])
    }
  }
  dInfoM__ <- subset(dInfo__, ADDL >  0)
  dInfoAll__ <- rbind(
    within(dInfoSM__, {FLAGautoMerge = TRUE}),
    within(dInfoM__[,c("NAME","TIME","VALUE","ADDL","II")], {FLAGautoMerge = FALSE})
  )
  dInfoAll__ <- get_yposDosing(dInfoAll__, ymin, ymax, sc__)
  dInfoAll__ <- get_labelDosing(dInfoAll__)
  out__ <- plobj__ +
    geom_rect(data=dInfoAll__,
              mapping = aes_string(
                xmin="TIME", xmax = "TIME+(ADDL+1)*II",
                ymin = "ystart", ymax = "yend",
                fill="NAME"),
              color = "transparent", alpha=0.5) +
    geom_rect(data=subset(dInfoAll__, FLAGautoMerge),
              mapping = aes_string(
                xmin="TIME", xmax = "TIME+(ADDL+1)*II",
                ymin = "ystart", ymax = "yend"),
              color = "grey20", fill = NA, size = 0.5, linetype = 3) +
    geom_text(data=dInfoAll__, mapping = aes_string(x="TIME", y="yend", label = "labelDos", color="NAME"), angle = 90, size = 2.5, hjust = 0, show.legend = FALSE) +
    scale_color_manual("", values=IQRtoolsColors[2:20]) +
    scale_fill_manual("", values=IQRtoolsColors[2:20])
  out__
}
handle_MDVBLQplot <- function(x, FLAGremoveMDV = TRUE) {
  out__ <- x[!is.na(x$VALUE),]
  if (FLAGremoveMDV) {
    out__ <- x[x$MDV==0,]
  } else {
    out__$MDVann <- factor(out__$MDV, levels = c(0,1), labels = c("no", "yes"))
  }
  BLOQ <- as.numeric(out__$VALUE < out__$LLOQ)
  BLOQ[is.na(BLOQ)] <- 0
  out__$BLOQ <- BLOQ
  out__$BLOQ <- factor(out__$BLOQ, levels = c(0,1), labels = c("no", "yes"))
  out__
}
handle_duplicatedLevels <- function(catInfo__) {
  for (k in seq_along(catInfo__$COLNAME)) {
    vals__ = aux_explode(catInfo__$VALUES[k])
    txts__ = aux_explode(catInfo__$VALUETXT[k])
    if (any(duplicated(txts__))) {
      warningIQR("Non-unique text values in", catInfo__$COLNAME[k])
      utxts__ <- unique(txts__)
      for (txtk__ in utxts__) {
        idx__ <- txts__ %in% txtk__
        if (sum(idx__) > 1) {
          txts__[idx__] <- paste0(txts__[idx__],".",vals__[idx__])
        }
      }
      catInfo__$VALUETXT[k] <- paste0(txts__, collapse = ",")
    }
  }
  catInfo__
}
#'@export
aux_plotlayout <- function(nplots, nrow = NULL, ncol = NULL, npage = NULL) {
  if (is.null(nrow) & is.null(ncol)) {
    if (is.null(npage)) npage <- nplots
    nPerPage <- ceiling(nplots / npage)
    nrow <- floor(sqrt(4/3*nPerPage))
    ncol <- ceiling(nPerPage/nrow)
    nPerPage <- ncol*nrow
  } else {
    if (is.null(ncol)){
      if (is.null(npage)) {
        ncol <- 1
        npage <- ceiling(nplots/nrow)
      } else {
        nPerPage <- ceiling(nplots / npage)
        ncol <- ceiling(nPerPage/nrow)
      }
    }
    if (is.null(nrow)){
      if (is.null(npage)) {
        nrow <- 1
        npage <- ceiling(nplots/ncol)
      } else {
        nPerPage <- ceiling(nplots / npage)
        nrow <- ceiling(nPerPage/ncol)
      }
    }
  }
  nPerPage <- nrow*ncol
  npage <-  ceiling(nplots / nPerPage)
  return(list(nrow=nrow, ncol=ncol, npage=npage))
}
is_plot_object <- function(x) {
  ggplot2::is.ggplot(x) | gtable::is.gtable(x)
}
remove_legend <- function(x) {
  if (ggplot2::is.ggplot(x)) x <- x + theme(legend.position = "none")
  return(x)
}
#'@export
createPages_IQRoutputFigure <- function(x,
                                        nrow = NULL, ncol = NULL, npage = NULL,
                                        legend.option = c("as.is", "remove", "common"),
                                        legend.object = NULL, legend.position = "right", legend.relsize = 0.2,
                                        title.relheight = 0.05, subtitle.relheight = 0.05, footer.relheight = 0.05) {
  draft <- x$draft
  if (!is.null(x$opt.layout)) {
    for (opt in names(x$opt.layout)) assign(opt, x$opt.layout[[opt]])
  }
  inputoptions <- as.list(match.call(expand.dots = TRUE))
  inputoptions <- inputoptions[!names(inputoptions) %in% c("", "x")]
  for (opt in names(inputoptions)) assign(opt, inputoptions[[opt]])
  if (!is.character(legend.option)) stopIQR("legend.option needs to be character ('as.is', 'remove', or 'common')")
  legend.option <- match.arg(legend.option)
  x <- purrr::compact(x)
  if (is_plot_object(x$content)) x$content <- list(x$content)
  if (!legend.position %in% c("right","left","bottom","top")) stopIQR("Legend position needs to be either right, left, bottom, or top.")
  nplots <- length(x$content)
  layout  <- aux_plotlayout(nplots, nrow, ncol, npage)
  nrow__  <- layout$nrow
  ncol__  <- layout$ncol
  npages__ <- layout$npage
  nPerPage <- nrow__ * ncol__
  pageIndex__ <- rep(1:npages__, each=nPerPage)[1:nplots]
  if (legend.option == "as.is") {
    legend__ <- list(NULL)[rep(1,npages__)]
    if (!is.null(legend.object)) warningIQR("Provided legend object ignored.\nIt is only considered as common legend when setting legend.option = 'common'. ")
  } else if (legend.option == "remove"){
    x$content <- lapply(x$content, remove_legend)
    legend__ <- list(NULL)[rep(1,npages__)]
  } else if (legend.option == "common") {
    x$content <- lapply(x$content, remove_legend)
    if (!is.null(legend.object)) {
      legend__ <- list(legend.object)[rep(1, npages__)]
    } else {
      legend__ <- lapply(which(!duplicated(pageIndex__)), function(ii) {
        ll__ <- tryCatch(cowplot::get_legend(x$content[[ii]] + theme(legend.position=legend.position)), error = function(e) cat("First figure of page has no legend."))
        if ("try-error" %in% class(ll__)) ll__ <- NULL
        ll__
      })
    }
    relsizes__ <- c(1-legend.relsize, legend.relsize)
    addlegendfun__ <- switch(legend.position,
                             top    = function(pc, legend) cowplot::plot_grid(legend, pc, ncol = 1, rel_heights = rev(relsizes__)),
                             bottom = function(pc, legend) cowplot::plot_grid(pc, legend, ncol = 1, rel_heights = relsizes__),
                             left   = function(pc, legend) cowplot::plot_grid(legend, pc, nrow = 1, rel_widths  = rev(relsizes__)),
                             right  = function(pc, legend) cowplot::plot_grid(pc, legend, nrow = 1, rel_widths  = relsizes__)
    )
  }
  plot_content <- lapply(1:npages__, function(p__) {
    oo__ <- cowplot::plot_grid(plotlist = x$content[pageIndex__ == p__],
                               nrow = nrow__, ncol = ncol__)
    if (legend.option == "common" & !is.null(legend__[[p__]])) {
      oo__ <- addlegendfun__(oo__, legend__[[p__]])
    }
    oo__
  })
  relheights__ <- 1
  plot_subtitle <- NULL
  if ("subtitle" %in% names(x)){
    plot_subtitle <- cowplot::ggdraw() + cowplot::draw_label(x$subtitle, fontface = "plain", x=0, hjust=0)
    relheights__ <- c(subtitle.relheight,relheights__)
  }
  plot_title <- NULL
  if ("title" %in% names(x)) {
    plot_title <- cowplot::ggdraw() + cowplot::draw_label(x$title, fontface = "bold", x=0, hjust=0)
    relheights__ <- c(title.relheight,relheights__)
  }
  plot_footer <- NULL
  if ("footer" %in% names(x)) {
    plot_footer <- cowplot::ggdraw() + cowplot::draw_label(x$footer, fontface = "plain", x=0, hjust=0, size = 10)
    relheights__ <- c(relheights__, footer.relheight)
  }
  relheights__[relheights__==1] <- 2-sum(relheights__)
  plot_pages__ <- lapply(1:npages__, function(p__) {
    plist <- c(list(plot_title, plot_subtitle), plot_content[p__], list(plot_footer))
    oo__ <- cowplot::plot_grid(
      plotlist = plist[!sapply(plist, is.null)],
      ncol = 1,
      rel_heights = relheights__
    )
    if (draft) {
      file__ <- system.file("images", "Preliminary.png", package = "IQRtools")
      oo__ <- oo__ + cowplot::draw_image(file__,
                                         hjust = 0.5, vjust = 0.5, y = 0, x = 0, scale = 0.8)
    }
    oo__
  })
  if (length(plot_pages__) == 1) plot_pages__ <- plot_pages__[[1]]
  return(plot_pages__)
}
#'@export
themeIQRtools <- function(base_size = 12, base_family = "") {
  colors <- list(
    medium = c(gray = '#737373', red = '#F15A60', green = '#7AC36A', blue = '#5A9BD4', orange = '#FAA75B', purple = '#9E67AB', maroon = '#CE7058', magenta = '#D77FB4'),
    dark = c(black = '#010202', red = '#EE2E2F', green = '#008C48', blue = '#185AA9', orange = '#F47D23', purple = '#662C91', maroon = '#A21D21', magenta = '#B43894'),
    light = c(gray = '#CCCCCC', red = '#F2AFAD', green = '#D9E4AA', blue = '#B8D2EC', orange = '#F3D1B0', purple = '#D5B2D4', maroon = '#DDB9A9', magenta = '#EBC0DA')
  )
  gray <- colors$medium["gray"]
  black <- colors$dark["black"]
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = "black"),
          rect = element_rect(fill = "white", colour = NA),
          text = element_text(colour = "black"),
          axis.text = element_text(size = rel(1.0), colour = "black"),
          axis.text.x = element_text(margin=unit(c(4, 4, 0, 4), "mm")),
          axis.text.y = element_text(margin=unit(c(4, 4, 4, 0), "mm")),
          axis.ticks = element_line(colour = "black"),
          axis.ticks.length = unit(-2, "mm"),
          legend.key = element_rect(colour = NA),
          panel.border = element_rect(colour = "black"),
          strip.background = element_rect(fill = "white", colour = NA),
          strip.text = element_text(size = rel(1.0)))
}
#'@export
IQRtoolsColors <- rep_len(c("#000000", "#C5000B", "#0084D1",
                            "#579D1C", "#FF950E", "#4B1F6F",
                            "#1B9E77", "#D95F02", "#7570B3"), length.out = 5000)
#'@export
scale_color_IQRtools <- function(...) {
  scale_color_manual(..., values = IQRtoolsColors)
}
#'@export
scale_fill_IQRtools <- function(...) {
  scale_fill_manual(..., values = IQRtoolsColors)
}
#'@export
IQRggplot <- function(..., fontsize = 12) {
  p__ <- ggplot(...) + themeIQRtools(base_size = fontsize)
  return(p__)
}
nearPD <- function (x, corr = FALSE)
{
  X__ <- 0.5*(x+t(x))
  e__ <- eigen(X__)
  eV__ <- e__$values
  if (min(eV__) > 0) return(X__)
  if (min(eV__) < -1e-4) {
    warningIQR("could not adjust matrix to be positive semidefinite")
    return(X__)
  }
  eV__[eV__<0] <- 0
  Xout__ <- e__$vectors %*% diag(eV__) %*% ginv(e__$vectors)
  if (corr) diag(Xout__) <- 1
  return(Xout__)
}
#'@export
print.IQRtteProjectMulti <- function(x, ...) {
  cat("IQRtteProjectMulti object\n")
  cat("\n")
  cat(sprintf("  The object contains N=%d IQRtteProjects\n",length(x)))
  cat(sprintf("  %d out of these %d have been run already",sum(sapply(x,function(xxx__) hasrun_IQRnlmeProject(xxx__))),length(x)))
}
is_IQRtteProjectMulti <- function (input) {
  methods::is(input,"IQRtteProjectMulti")
}
#'@export
print.IQRtteProject <- function(x, ...) {
  if (file.exists(paste0(x,"/RESULTS/project_results.txt"))) {
    text__ <- aux_fileread(paste0(x,"/RESULTS/project_results.txt"))
    cat(text__)
    cat(paste0("\nIQRtteProject: ",x,"\n\n"))
  } else {
    cat(paste0("IQRtteProject: ",x,"\n"))
    cat("Project has not been run yet (most likely)\n\n")
  }
}
#'@export
is_IQRtteProject <- function (projectPath) {
  if (!file.exists(paste0(projectPath,'/','project.mlxtran'))) return(FALSE)
  x <- parseMONOLIXprojectHeader(projectPath)
  if (is.null(x$ERRORMODELS)) return(TRUE)
  return(FALSE)
}
#'@export
is_GPF <- function(x) {
  inherits(x, "GPF")
}
#'@export
load_GPF <- function(x) {
  if (is_GPF(x))
    return(x)
  if (is.data.frame(x)){
    GPF <- GPF(filename = NULL, estimates = x, uncertainty_correlation = data.frame())
    return(GPF)
  }
  if (is.character(x)) {
    if(endsWith(x, ".xlsx") || endsWith(x, ".xls")) {
      GPF <- read_GPFFromXLS(x)
      return(GPF)
    } else if(endsWith(x, ".csv")) {
      GPF <- read_GPFFromCSV(x)
      return(GPF)
    } else if (is_IQRnlmeProject(x)){
      GPF <- generate_GPFFromIQRnlmeProject(x, filename = NULL)
      return(GPF)
    } else {
      stopIQR(paste0(
        "load_GPF:: Unrecognized filetype ",
        "(only .csv, .xlsx and .xls files and IQRnlmeProject directories are supported"))
    }
  }
  stopIQR(class(x), " not supported or the filename does not point to an xls a csv or a NLME/SYS-project.")
}
#'@importFrom writexl write_xlsx
read_GPFFromCSV <- function(filename) {
  if(!is.character(filename) &&
     file.exists(filename)  && endsWith(filename, ".csv") ) {
    stopIQR(
      paste0("Argument filename should denote an .csv file. ",
             "Check that this file (", filename, ") exists."))
  }
  estimates <- IQRloadCSVdata(filename = filename)
  uncertainty_cor <- data.frame()
  GPF(filename = filename,
      estimates = estimates,
      uncertainty_correlation = uncertainty_cor)
}
#'@importFrom data.table data.table rbindlist
#'@importFrom data.table data.table as.data.table setnames copy data.table rbindlist
#'@importFrom data.table data.table as.data.table setnames copy data.table rbindlist
getNamesAllParameters <- function(obj) {
  pgpf <- parse_GPF(obj)
  c(getNamesModelParameters(pgpf),
    getNamesIIVParameters(pgpf),
    getNamesIIVCorrParameters(pgpf),
    getNamesCovariateParameters(pgpf),
    getNamesErrorParameters(pgpf))
}
getNamesModelParameters <- function(obj) {
  pgpf <- parse_GPF(obj)
  if(length(pgpf$p.MODEL) > 0) {
    names(pgpf$p.MODEL)
  } else {
    character(0L)
  }
}
getNamesCovariateParameters <- function(obj) {
  pgpf <- parse_GPF(obj)
  if(length(pgpf$p.COVRT) > 0) {
    names(pgpf$p.COVRT)
  } else {
    character(0L)
  }
}
getNamesCovariates <- function(obj, FLAGunique = TRUE, FLAGparamNames = !FLAGunique) {
  pgpf <- parse_GPF(obj)
  res <- pgpf$p.COVRT.COVRTNAME
  if(FLAGunique) {
    res <- unique(res)
  } else if(FLAGparamNames) {
    names(res) <- pgpf$p.COVRT.PARAMNAME
  }
  res
}
getNamesIIVParameters <- function(obj) {
  pgpf <- parse_GPF(obj)
  if(length(pgpf$p.MODEL.IIV) > 0) {
    paste0("omega(", names(pgpf$p.MODEL.IIV), ")")
  } else {
    character(0)
  }
}
getNamesIIVCorrParameters <- function(obj) {
  pgpf <- parse_GPF(obj)
  if(length(pgpf$p.MODEL.IIVcor) > 0) {
    names(pgpf$p.MODEL.IIVcor)
  } else {
    character(0L)
  }
}
getNamesErrorParameters <- function(obj) {
  pgpf <- parse_GPF(obj)
  if(length(pgpf$p.ERR) > 0) {
    names(pgpf$p.ERR)
  } else {
    character(0L)
  }
}
getCovMatrixUncertainty <- function(obj) {
  pgpf <- parse_GPF(obj)
  pgpf$p.UNCERT.COV
}
getCorMatrixUncertainty <- function(obj) {
  pgpf <- parse_GPF(obj)
  pgpf$p.UNCERT.COR
}
GPFprojectInfo <- function(projectPath,
                           indPars_select = NULL, 
                           popPars_select = NULL, 
                           cov_select     = NULL  
) {
  list(projectPath     = projectPath,
       indPars_select  = indPars_select,
       popPars_select  = popPars_select,
       cov_select      = cov_select)
}
#'@importFrom data.table rbindlist as.data.table
#'@export
generate_GPFFromProjectInfoList <- function(projectInfoList, filename = NULL, FLAGdropErrParams = TRUE) {
  GPFlist <- lapply(projectInfoList, function(p) generate_GPFFromProjectInfo(projectInfo = p, filename = NULL, FLAGdropErrParams = FLAGdropErrParams))
  combineGPF(GPFlist = GPFlist, filename = filename)
}
#'@importFrom data.table data.table
imputeTYPEcol_GPF <- function(estimates) {
  estimates <- data.table::data.table(estimates)
  estimates[,`:=`(TYPE = "MODEL PARAMETER")]
  estimates[grep("^corr", PARAMETER),`:=`(TYPE = "IIV CORRELATION")]
  estimates[grep("^beta_.*\\([^_]+\\)", PARAMETER),`:=`(TYPE = "CONTINUOUS COVARIATE")]
  estimates[grep("^beta_.*\\(.+_.+\\)", PARAMETER),`:=`(TYPE = "CATEGORICAL COVARIATE")]
  estimates[grep("error_PROP", PARAMETER),`:=`(TYPE = "RELATIVE ERROR")]
  estimates[grep("error_ADD", PARAMETER),`:=`(TYPE = "ADDITIVE ERROR")]
  estimates[grep("error_EXP", PARAMETER),`:=`(TYPE = "EXPONENTIAL ERROR")]
  as.data.frame(estimates)
}
specifyParamSampling <- function(obj,
                                 Nsamples    = if(is_IQRnlmeParamSpec(obj)) obj$Nsamples else 1L,
                                 Npop        = if(is_IQRnlmeParamSpec(obj)) obj$Npop else if(FLAG_SAMPLE==2) Nsamples else 1L,
                                 FLAG_SAMPLE = 0,
                                 data        = NULL) {
  if(is.character(obj) || is_GPF(obj)) {
    obj <- parse_GPF(obj)
  }
  force(Nsamples)
  force(Npop)
  if(is_GPF(obj)) {
    class(obj) <- unique(c("IQRnlmeParamSpec", class(obj)))
    obj$Nsamples       <- Nsamples
    obj$Npop           <- Npop
    obj$FLAG_SAMPLE    <- FLAG_SAMPLE
    obj$data           <- data
    obj
  } else {
    stopIQR(
      paste0(
        "Argument obj should be an .xls or .xlsx file or a GPF object, or ",
        "an IQRnlmeParamSpec object, but is of class ",
        toString(class(obj))
      )
    )
  }
}
#'@export
is_IQRnlmeParamSpec <- function(x) {
  inherits(x, "IQRnlmeParamSpec")
}
#'@importFrom stats plogis qlogis
#'@importFrom stats rnorm
sampleIDs <- function(data, Nsamples) {
  if(!is.data.frame(data)) {
    stopIQR("Argument data must be a data.frame.")
  }
  if (nrow(data) < Nsamples) {
    sampData <- data[sample(nrow(data), Nsamples, replace=TRUE), , drop=FALSE]
  } else if (nrow(data)==Nsamples){
    sampData <- data
  } else {
    sampData <- data[sample(nrow(data), Nsamples, replace=FALSE), , drop=FALSE]
  }
  if (is.matrix(sampData)) {
    sampData <- as.data.frame(sampData)
  }
  if ("ID" %in% names(sampData)) {
    if("ID0" %in% names(sampData)) {
      stopIQR(paste0(
        "data already has columns ID and ID0. Not overwriting column ID0. ",
        "To avoid this error, please rename column ID0 in the original data."))
    }
    sampData$ID0 <- sampData$ID
  }
  rownames(sampData) <- NULL
  sampData$ID <- seq_len(Nsamples)
  sampData
}
calcTypicalIndParamValues <- function(spec,
                                      referencePopParamValues,
                                      data,
                                      doCartesian = TRUE) {
  if(!is.data.frame(referencePopParamValues)) {
    stopIQR("Argument referencePopParamValues should be a data.frame.")
  }
  if(!is.data.frame(data)) {
    stopIQR("Argument data should be a data.frame.")
  }
  if(!is.logical(doCartesian)) {
    stopIQR("Argument doCartesian should be a logical (TRUE or FALSE).")
  }
  if(doCartesian == FALSE && nrow(data) != nrow(referencePopParamValues)) {
    stopIQR("When doCartesian is set to FALSE datareferencePopParamValues should have the same number of rows.")
  }
  Nsamples <- nrow(data)
  Npop <- nrow(referencePopParamValues)
  spec <- specifyParamSampling(obj = spec, Nsamples = Nsamples)
  dtData <- data.table::as.data.table(data)
  dtRefParValues <- data.table::as.data.table(referencePopParamValues)
  dtTypicalParValues <- if(doCartesian) {
    data.table::rbindlist(
      lapply(
        seq_len(nrow(dtRefParValues)),
        function(iPop) {
          cbind(dtRefParValues[iPop,], dtData)
        })
    )
  } else {
    cbind(dtRefParValues, dtData)
  }
  for (k in seq_along(spec$p.COVRT)) {
    if (spec$p.COVRT.COVRTNAME[k] %in% names(data)) {
      if (spec$p.COVRT.CONTINUOUS[k]) {
        idx_Modify <- rep(TRUE, nrow(dtTypicalParValues))
      } else {
        idx_Modify <- dtTypicalParValues[[ spec$p.COVRT.COVRTNAME[k] ]] == spec$p.COVRT.CATVALUE[k]
      }
      .formulaTextToApply <- spec$p.COVRT.FORM[k] 
      dtTypicalParValues[idx_Modify, eval(parse(text=.formulaTextToApply))]
    } else {
      warningIQR(
        paste0(
          "The covariate '", spec$p.COVRT.COVRTNAME[k],
          "' is not defined in the argument 'data', therefore, it will be ignored.")
      )
    }
  }
  as.data.frame(dtTypicalParValues[, c("ID.POP", "ID", getNamesModelParameters(spec)), with = FALSE])
}
calcIndParamValues <- function(spec,
                               typicalIndParamValues,
                               randomEffects) {
  spec <- specifyParamSampling(obj = spec)
  if(!is.data.frame(typicalIndParamValues)) {
    stopIQR("Argument typicalIndParamValues should be a data.frame.")
  }
  if(!is.data.frame(randomEffects)) {
    stopIQR("Argument randomEffects should be a data.frame.")
  }
  if(!setequal(names(typicalIndParamValues), names(randomEffects))) {
    stopIQR(
      paste0("The data.frame arguments typicalIndParamValues and randomEffects",
             "should have the same columns."))
  }
  if(nrow(typicalIndParamValues) != nrow(randomEffects)) {
    stopIQR(
      paste0("Tahe data.frame arguments typicalIndParamValues and randomEffects",
             "should have the same numbers of rows.")
    )
  }
  indParValuesNormal <- transformParamToNormal(spec, typicalIndParamValues)
  for(name in setdiff(names(indParValuesNormal), c("ID.POP", "ID"))) {
    indParValuesNormal[[name]] <- indParValuesNormal[[name]] + randomEffects[[name]]
  }
  untransformParamFromNormal(spec, indParValuesNormal)
}
transformParamToNormal <- function(spec,
                                   values,
                                   columnNames = setdiff(names(values), c("ID.POP", "ID"))) {
  spec <- specifyParamSampling(obj = spec)
  if(!is.data.frame(values)) {
    stopIQR(paste0("Argument values should be a data.frame."))
  }
  TRANSF_LETTER <- getParamTransformationTypes(spec)
  TRANSF_FUN <- list("N" = identity,
                     "L" = log,
                     "G" = qlogis
  )
  if(length(setdiff(columnNames, names(TRANSF_LETTER)))) {
    stopIQR(
      paste0(
        "All columns in values should be among ",
        toString(names(TRANSF_LETTER)), " but the following are not: ",
        toString(setdiff(columnNames, names(TRANSF_LETTER)))))
  }
  for(name in columnNames) {
    values[[name]] <- TRANSF_FUN[[TRANSF_LETTER[name]]](values[[name]])
  }
  values
}
untransformParamFromNormal <- function(spec,
                                       values,
                                       columnNames = setdiff(names(values), c("ID.POP", "ID"))) {
  spec <- specifyParamSampling(obj = spec)
  if(!is.data.frame(values)) {
    stopIQR(paste0("Argument values should be a data.frame."))
  }
  TRANSF_LETTER <- getParamTransformationTypes(spec)
  UNTRANSF_FUN <- list("N" = identity,
                       "L" = exp,
                       "G" = plogis
  )
  if(length(setdiff(columnNames, names(TRANSF_LETTER)))) {
    stopIQR(
      paste0(
        "All columns in values should be among ",
        toString(names(TRANSF_LETTER)), " but the following are not: ",
        toString(setdiff(columnNames, names(TRANSF_LETTER)))))
  }
  for(name in columnNames) {
    values[[name]] <- UNTRANSF_FUN[[TRANSF_LETTER[name]]](values[[name]])
  }
  values
}
#'@importFrom fpc bhattacharyya.dist
#'@importFrom fpc bhattacharyya.dist
#'@importFrom utils combn
#'@importFrom stats cov t.test
adjustMatrixToPDMatrix <- function(x,
                                   corr = FALSE) {
  X__ <- x
  e__ <- eigen(X__)
  eV__ <- e__$values
  if (min(eV__) > 0) return(X__)
  if (min(eV__) < -1e-3) {
    return("failed")
  }
  eV__[eV__<0] <- 0
  Xout__ <- e__$vectors %*% diag(eV__) %*% ginv(e__$vectors)
  if (corr) diag(Xout__) <- 1
  return(Xout__)
}
extractParamNamesFromIIVCorrParamName <- function(corrString) {
  namesSepComma <- gsub("corr(", "", gsub(")$", "", corrString), fixed = TRUE)
  parnames      <- aux_explode(namesSepComma)
  parnames
}
fill_names <- function(nm) {
  if (!length(names(nm))) nm <- setNames(nm = nm)
  names(nm)[nchar(names(nm)) == 0] <- nm[nchar(names(nm)) == 0]
  nm
}
#'@importFrom data.table data.table rbindlist
#'@importFrom stats cov2cor
blockMatrixDiagonal <- function(...){
  matrixList<-list(...)
  if (!all(vapply(matrixList, function(x) dim(x)[1] == dim(x)[2], TRUE)))
    stopIQR("Not all matrices supplied are square-matrices")
  rownm_exist <- vapply(matrixList, function(x) is.null(rownames(x)), FALSE)
  colnm_exist <- vapply(matrixList, function(x) is.null(colnames(x)), FALSE)
  all_or_none <- function(x) all(x) | !any(x)
  if (!all_or_none(rownm_exist)) stopIQR("Either all matrices or no matrix can have rownames")
  if (!all_or_none(colnm_exist)) stopIQR("Either all matrices or no matrix can have colnames")
  dimensions <- vapply(matrixList,FUN=function(x) dim(x)[1], 1)
  finalDimension<-sum(dimensions)
  finalMatrix<-matrix(0,nrow=finalDimension,ncol=finalDimension)
  index<-1
  for(k in 1:length(dimensions)){
    finalMatrix[index:(index+dimensions[k]-1),index:(index+dimensions[k]-1)]<-matrixList[[k]]
    index<-index+dimensions[k]
  }
  finalMatrix <- `rownames<-`(finalMatrix, do.call(c, lapply(matrixList, rownames)))
  finalMatrix <- `colnames<-`(finalMatrix, do.call(c, lapply(matrixList, colnames)))
  finalMatrix
}
#'@export
print.IQRsensitivity <- function (x,...) {
  print(x$plot)
}
#'@export
simbio_CSV2namedVector <- function (filename) {
  if (!file.exists(filename)) stopIQR("File to load does not exist")
  d <- IQRloadCSVdata(filename)
  if (ncol(d)<2) stopIQR("File does not contain at least 2 columns")
  if (any(!is.numeric(d[[2]]))) stopIQR("Second element in file need to be numeric")
  values <- d[[2]]
  names(values) <- d[[1]]
  return(values)
}
#'@export
"+.IQRrmd" <- function (a,b) {
  out__ <- paste0(a,b)
  class(out__) <- "IQRrmd"
  return(out__)
}
#'@export
print.IQRrmd <- function (x, ...) {
  cat(x)
  cat("\n\nIQRrmd object")
}
#'@export
export_IQRrmd <- function (rmdDoc, filename) {
  filename <- paste0(aux_strrep(filename,".rmd",""),".rmd")
  aux_filewrite(paste0(rmdDoc),filename)
}
#'@export
rmdEMPTY <- function() {
  out__ <- ""
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdLINEBREAK <- function() {
  out__ <- "\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdPARAGRAPH <- function() {
  out__ <- "\n\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdEMPTY <- function() {
  out__ <- ""
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdTITLE <- function(template="DefaultStyle.rmdt",
                     title="Default Title",
                     subtitle="Default Subtitle",
                     date=format(Sys.Date(),"%d-%b-%Y")) {
  out__ <-              "=====\n"
  out__ <- paste0(out__,"TITLE:    ",title,"\n")
  out__ <- paste0(out__,"SUBTITLE: ",subtitle,"\n")
  out__ <- paste0(out__,"DATE:     ",date,"\n")
  out__ <- paste0(out__,"TEMPLATE: ",template,"\n")
  out__ <- paste0(out__,"=====\n\n")
  class(out__) <- "IQRrmd"
  return(out__)
}
#'@export
rmdNEWPAGE <- function() {
  out__ <- "\n!NEWPAGE\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdAPPROVALS <- function() {
  out__ <- "\n!APPROVALS\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdNOINTRO <- function() {
  out__ <- "\n!NOINTRO\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdLANDSCAPE <- function() {
  out__ <- "\n!LANDSCAPE\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdPORTRAIT <- function() {
  out__ <- "\n!PORTRAIT\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdSECTION <- function(title="Section Title",label=NULL,level=1,numbered=TRUE) {
  out__ <- paste0("\n\n",paste0(rep("#",level),collapse=""))
  if (!numbered)
    out__ <- paste0(out__,"*")
  out__ <- paste0(out__," ",title)
  if (!is.null(label))
    out__ <- paste0(out__," {#",label,"}")
  out__ <- paste0(out__,"\n")
  class(out__) <- "IQRrmd"
  return(out__)
}
#'@export
rmdURL <- function(url="http://www.intiquan.com",caption=NULL) {
  out__ <- "!URL"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",url,")")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdCOMMENT <- function(comment="some comment",author=NULL) {
  out__ <- "!COM"
  if (!is.null(author))
    out__ <- paste0(out__,"[author:",author,"]")
  out__ <- paste0(out__,"{",comment,"}")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdTXT <- function(file,caption=NULL,fontsize=10,style="CODE",label=NULL) {
  out__ <- "\n!TXT"
  if (!is.null(c(caption,style,label))) {
    opttext__ <- ""
    if (!is.null(caption))
      opttext__ <- paste0(opttext__,"caption:",caption,",")
    if (!is.null(style))
      opttext__ <- paste0(opttext__,"style:",style,"(",fontsize,"),")
    if (!is.null(label))
      opttext__ <- paste0(opttext__,"label:",label,",")
    if (substr(opttext__,nchar(opttext__),nchar(opttext__))==",") opttext__ <- substr(opttext__,1,nchar(opttext__)-1)
    out__ <- paste0(out__,"[",opttext__,"]")
  }
  out__ <- paste0(out__,"(",file,")\n")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdTABLE <- function(file,caption=NULL,fontsize=10,label=NULL,
                     ignoreCaption=FALSE,
                     valueTable=FALSE) {
  out__ <- "\n!TAB"
  if (!is.null(c(caption,fontsize,label,ignoreCaption,valueTable))) {
    opttext__ <- ""
    if (!is.null(caption))
      opttext__ <- paste0(opttext__,"caption:",caption,",")
    if (!is.null(fontsize))
      opttext__ <- paste0(opttext__,"size:",fontsize,",")
    if (!is.null(label))
      opttext__ <- paste0(opttext__,"label:",label,",")
    if (ignoreCaption) {
      opttext__ <- paste0(opttext__,"ignoreCaption:true,")
    } else {
      opttext__ <- paste0(opttext__,"ignoreCaption:false,")
    }
    if (valueTable) {
      opttext__ <- paste0(opttext__,"valueTable:true,")
    } else {
      opttext__ <- paste0(opttext__,"valueTable:false,")
    }
    if (substr(opttext__,nchar(opttext__),nchar(opttext__))==",") opttext__ <- substr(opttext__,1,nchar(opttext__)-1)
    out__ <- paste0(out__,"[",opttext__,"]")
  }
  out__ <- paste0(out__,"(",file,")\n")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdTABLEDF <- function(df,caption=NULL,fontsize=10,label=NULL,ignoreCaption=FALSE,valueTable=FALSE,footertext=NULL) {
  out__ <- "\n!TABINLINE["
  if (!is.null(caption))
    out__ <- paste0(out__,"caption:",caption,",")
  if (!is.null(fontsize))
    out__ <- paste0(out__,"size:",fontsize,",")
  if (!is.null(label))
    out__ <- paste0(out__,"label:",label,",")
  if (ignoreCaption) {
    out__ <- paste0(out__,"ignoreCaption:true,")
  } else {
    out__ <- paste0(out__,"ignoreCaption:false,")
  }
  if (valueTable) {
    out__ <- paste0(out__,"valueTable:true")
  } else {
    out__ <- paste0(out__,"valueTable:false")
  }
  out__ <- paste0(out__,"]")
  out__
  headerNames__ <- names(df)
  dfchar__ <- data.frame(
    lapply(df, function (x__) {
      out__ <- as.character(x__)
      out__[is.na(out__)] <- "NA"
      out__ <- gsub(pattern="\n",replacement="",x=out__)
      out__ <- gsub(pattern="\r",replacement="",x=out__)
      out__
    })
    ,stringsAsFactors=FALSE
  )
  ncharCol__ <- sapply(1:ncol(dfchar__), function (y__) max(nchar(headerNames__[y__]),max(nchar(dfchar__[[y__]]))))
  headerExp__ <- sapply(seq_along(headerNames__), function(k__) aux_postFillChar(value2postfill=headerNames__[k__],lengthString=ncharCol__[k__],fillChar=" ") )
  headerLine__ <- sapply(headerExp__, function (x__) paste0(rep("-",nchar(x__)),collapse=""))
  headerOut__ <- paste0("| ",
                        paste(headerExp__,collapse = " | "),
                        " |\n",
                        "| ",
                        paste(headerLine__,collapse = " | "),
                        " |\n")
  textContent__ <- paste0(sapply(1:nrow(dfchar__), function (nrow__) {
    textExp__ <- sapply(seq_along(headerNames__), function(k__) aux_postFillChar(value2postfill=dfchar__[nrow__,k__],lengthString=ncharCol__[k__],fillChar=" ") )
    textOut__ <- paste0("| ",
                        paste(textExp__,collapse = " | "),
                        " |\n")
  }),collapse="")
  tableOut__ <- paste0("\n",headerOut__,textContent__)
  if (!is.null(footertext)) {
    footertext <- gsub("\n","<br>",footertext)
    tableOut__ <- paste0(tableOut__,"{",footertext,"}")
  }
  out__ <- paste0(out__,tableOut__)
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdFIGURE <- function(file,
                      caption=NULL,label=NULL,
                      legend=NULL,
                      pages=NULL,
                      scale=100, crop=TRUE,
                      repeatLegend=TRUE,
                      ignoreCaption=FALSE) {
  out__ <- "\n!FIG"
  if (!is.null(c(caption,label,legend,pages,scale,crop,repeatLegend,ignoreCaption))) {
    opttext__ <- ""
    if (!is.null(caption))
      opttext__ <- paste0(opttext__,"caption:",caption,",")
    if (!is.null(label))
      opttext__ <- paste0(opttext__,"label:",label,",")
    if (!is.null(pages))
      opttext__ <- paste0(opttext__,"pages:",pages,",")
    if (!is.null(scale))
      opttext__ <- paste0(opttext__,"scale:",scale,",")
    if (crop) {
      opttext__ <- paste0(opttext__,"crop:true,")
    } else {
      opttext__ <- paste0(opttext__,"crop:false,")
    }
    if (ignoreCaption) {
      opttext__ <- paste0(opttext__,"ignoreCaption:true,")
    } else {
      opttext__ <- paste0(opttext__,"ignoreCaption:false,")
    }
    if (repeatLegend) {
      opttext__ <- paste0(opttext__,"repeatLegend:true,")
    } else {
      opttext__ <- paste0(opttext__,"repeatLegend:false,")
    }
    if (substr(opttext__,nchar(opttext__),nchar(opttext__))==",") opttext__ <- substr(opttext__,1,nchar(opttext__)-1)
    out__ <- paste0(out__,"[",opttext__,"]")
  }
  out__ <- paste0(out__,"(",file,")")
  if (!is.null(legend))
    out__ <- paste0(out__,"\n{\n",gsub("\n","  \n",legend),"\n}")
  out__ <- paste0(out__,"\n")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdPDF <- function(file,pages=NULL,scale=100,crop=TRUE) {
  out__ <- "\n!PDF"
  if (!is.null(c(pages,scale,crop))) {
    opttext__ <- ""
    if (!is.null(pages))
      opttext__ <- paste0(opttext__,"pages:",pages,",")
    if (!is.null(scale))
      opttext__ <- paste0(opttext__,"scale:",scale,",")
    if (crop) {
      opttext__ <- paste0(opttext__,"crop:true,")
    } else {
      opttext__ <- paste0(opttext__,"crop:false,")
    }
    if (substr(opttext__,nchar(opttext__),nchar(opttext__))==",") opttext__ <- substr(opttext__,1,nchar(opttext__)-1)
    out__ <- paste0(out__,"[",opttext__,"]")
  }
  out__ <- paste0(out__,"(",file,")")
  out__ <- paste0(out__,"\n")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdSECref <- function(label,caption=NULL) {
  out__ <- " @SEC"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",label,")")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdTXTref <- function(label,caption=NULL) {
  out__ <- " @TXT"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",label,")")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdFIGref <- function(label,caption=NULL) {
  out__ <- " @FIG"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",label,")")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdTABref <- function(label,caption=NULL) {
  out__ <- " @TAB"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",label,") ")
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdVALref <- function(labelTable,row=NULL,col=NULL,labelValue=NULL) {
  if (!is.null(labelValue)) {
    out__ <- paste0(" @VAL[",labelValue,"](",labelTable,") ")
  } else {
    out__ <- paste0(" @VAL[",row,",",col,"](",labelTable,") ")
  }
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
rmdTEXTSTYLE <- function(text,color=NULL,size=NULL,style=NULL) {
  if (is.null(color) & is.null(size) & is.null(style)) {
    out__ <- text
  } else {
    out__ <- "!TEXTSTYLE["
    x1__ <- NULL
    x2__ <- NULL
    x3__ <- NULL
    if (!is.null(color)) x1__ <- paste0("color=",color)
    if (!is.null(size)) x2__ <- paste0("size=",size)
    if (!is.null(style)) x3__ <- paste0("style=",style)
    out__ <- paste0(out__,paste0(c(x1__,x2__,x3__),collapse = ","),"](",text,")")
  }
  class(out__) <- "IQRrmd"
  return (out__)
}
#'@export
IQReport <- function(RMDfilePath){
  if (!has_IQReport_executable())
    stopIQR("The IQReport executable has not been installed.\nYou can obtain IQReport from here: http://www.intiquan.com/iqreport-the-reporting-solution/")
  RMDfilePath <- paste0(aux_strrep(RMDfilePath,".rmd",""),".rmd")
  if (!file.exists(RMDfilePath))
    stopIQR("The provided RMDfilePath argument does not point to an '.rmd' file (case sensitive)")
  if (grepl("~",RMDfilePath,fixed = TRUE))
    stopIQR("The use of '~' in path names is not allowed. Please use a relative path instead!")
  if (grepl(":",RMDfilePath,fixed = TRUE))
    stopIQR("The use of ':' in path names is not allowed. Please use a relative path instead!")
  if (.Platform$OS.type=="windows") {
    oldpath <- getwd()
    fp      <- aux_fileparts(RMDfilePath)
    setwd(fp$pathname)
    RMDfilePathAbsolute <- paste0(getwd(),"/",fp$filename,fp$fileext)
    setwd(oldpath)
    callIQReport <- paste0(.PATH_IQRreport,' "',RMDfilePathAbsolute,'"')
    oldpath <- getwd()
    setwd(aux_fileparts(.PATH_IQRreport)$pathname)
    system(callIQReport)
    setwd(oldpath)
  } else {
    system(paste0("IQReport.sh ",RMDfilePath))
  }
  aux_rmdir("tmp")
  unlink("*.rmd.bak")
}
#'@export
has_IQReport_executable <- function(){
  loadSetupOptions_IQRtools()
  if (file.exists(.PATH_IQRreport)) return(TRUE)
  return(FALSE)
}
#'@export
IQRcoxRegression <- function (
  data,
  TIMEcol,
  CENScol,
  PREDcol         = NULL,
  IDcol           = "ID",
  PREDinteraction = NULL,
  PREDval         = NULL,
  RHSformula      = NULL,
  ci.level        = 0.95,
  xlab            = NULL,
  ylab            = NULL,
  pathname        = NULL,
  FLAGreport      = TRUE,
  SIGNIF          = 4
) {
  data__ <- data
  if (!is.null(RHSformula)) {
    if (!("formula" %in% class(RHSformula))) {
      if (strtrim(RHSformula,1) != "~") RHSformula <- paste0("~", RHSformula)
      RHSformula <- stats::as.formula(RHSformula)
    }
    if (length(RHSformula) > 2) stopIQR("Only RHS of formula should given.")
    PREDcol <- all.vars(RHSformula[[length(RHSformula)]])
    PREDinteractionTerms__ <- setdiff(attr(stats::terms(RHSformula), "term.labels"), PREDcol)
  }
  if (!TIMEcol %in% names(data__)) stopIQR("The TIMEcol column is not present in the data")
  if (!CENScol %in% names(data__)) stopIQR("The CENScol column is not present in the data")
  if (!all(sort(unique(data__[[CENScol]])) == c(0,1))) stopIQR("The CENScol column does not contain only 0s and 1s")
  idxNAvar <- is.na(data__[[TIMEcol]]) | is.na(data__[[CENScol]])
  if (any(idxNAvar)) { 
    warningIQR(paste0(sum(idxNAvar)," subjects have NA values for the event time or type."))
  }
  if (!all(PREDcol %in% names(data__))) stopIQR("One or more PREDcol columns are not present in the data")
  if ("Intercept" %in% c(PREDcol,TIMEcol, CENScol)) stopIQR("'Intercept' is not allowed to be used as predictor, time, or censoring name.")
  PREDcolCAT <- PREDcol[sapply(data__[,PREDcol, drop = FALSE], class) %in% c("factor","character")]
  CATval__ <- do.call(rbind,lapply(PREDcolCAT, function(xx__) {
    out__ <- data.frame(
      PREDcol = xx__,
      category = levels(factor(data__[[xx__]])),
      stringsAsFactors = FALSE
    )
    out__ <- within(out__, {
      label0 <- paste0(PREDcol,category)
      label1 <- paste0(PREDcol,"=",category,"")
    })
    out__
  }))
  if (is.null(RHSformula)) {
    if (!is.null(PREDinteraction)) {
      PREDinteraction <- lapply(as.list(PREDinteraction), function(x) trimws(aux_explode(x, separator = ",")))
      if (!all(unlist(PREDinteraction) %in% PREDcol)) stopIQR("Interaction given for unknown predictors.")
      PREDinteractionTerms__ <- c(); lfd <- 0
      for (k__ in seq_along(PREDinteraction)) {
        nk__ <- length(PREDinteraction[[k__]])
        for (kk__ in 1:(nk__-1)) {
          for (kkk__ in (kk__+1):nk__) {
            lfd <- lfd+1
            PREDinteractionTerms__[lfd] <- paste0(PREDinteraction[[k__]][kk__], ":" , PREDinteraction[[k__]][kkk__])
          }
        }
      }
    } else {
      PREDinteractionTerms__ <- NULL
    }
  }
  if (!IDcol %in% names(data__)) {
    data__$ID <- 1:nrow(data__)
    IDcol <- "ID"
  }
  if (!is.null(PREDval)) {
    names(PREDval) <- gsub(",",":",names(PREDval), fixed = TRUE)
    idxInvalid__ <- !(names(PREDval) %in% c(PREDcol, PREDinteractionTerms__, "Intercept"))
    if (any(idxInvalid__)) {
      warningIQR(paste0("'",names(PREDval)[idxInvalid__], "'", collapse = ", "), " are no valid predictor terms. Input will be ignored.\nIn case values for interaction terms should be fixed, they need to be given in the same order as in PREDinteraction or in the given formula.")
      PREDval <- PREDval[!idxInvalid__]
    }
  }
  if (!is.null(xlab)) {
    if (length(xlab) != length(PREDcol))
      stopIQR("Number of given x-axis labels need to correspond to number of predictor variables.")
    names(xlab) <- PREDcol
  } else {
    xlab <- structure(PREDcol, names = PREDcol)
  }
  if (is.null(ylab)) ylab <- paste0("Survival")
  if (ci.level <= 0 | ci.level >= 1) { stopIQR("Confidence levels needs to be between 0 and 1.")
  } else {
    CIlabel <- paste0("CI",c("lb", "ub"),ci.level*100)
    CIrangeLabel <- paste0(ci.level*100, "% CI")
    CIfact <- stats::qnorm(1-(1-ci.level)/2)
  }
  if (!is.numeric(SIGNIF)) {warningIQR("Given number of siginificant digits (SIGNIF) not numerical. Default will be used (4)"); SIGNIF = 4}
  if (is.null(RHSformula) | !is.null(PREDval)) {
    RHSterms0__ <- c(PREDcol, PREDinteractionTerms__)
    RHSterms__ <- RHSterms0__
    if (!is.null(PREDval)){
      for (kfix in names(PREDval)) {
        if (kfix == "Intercept") {
          RHSterms__ <- c(RHSterms__, paste0("offset(", PREDval[[kfix]], "*Intercept)"))
        } else {
          idxFixed <- RHSterms__ == kfix
          RHSterms__[idxFixed] <- paste0("offset(", PREDval[[kfix]], "*", gsub(":","*",RHSterms__[idxFixed], fixed = TRUE), ")")
        }
      }
    }
    RHS0__ <- paste0(RHSterms0__, collapse = "+")
    RHS__ <- paste0(RHSterms__, collapse = "+")
  } else {
    RHS0__ <- gsub("~","",deparse(RHSformula))
    RHS__ <- RHS0__
  }
  if (RHS__ == "") stopIQR("No predictor variable.")
  formula0__ <- paste0("COX~",RHS0__)
  formula__ <- paste0("COX~",RHS__)
  data__$COX <- survival::Surv(time = data__[[TIMEcol]], event = 1-data__[[CENScol]])
  funCall__ <- paste0("fit <- survival::coxph(",formula__,", data=data__, x=TRUE, y=TRUE)")
  eval(parse(text=funCall__))
  fitsummary__ <- summary(fit, conf.int = ci.level)
  phtest__ <- survival::cox.zph(fit,transform = "km",global=TRUE)
  if (nrow(phtest__$table) == 1) {
    phtest__$table <- phtest__$table[c(1,1),]
    rownames(phtest__$table) <- c(rownames(phtest__$table)[1], "GLOBAL")
  }
  estimates__ <- data.frame(
    Parameter = names(fit$coefficients),
    Value     = exp(fit$coefficients),
    CI.lower = fitsummary__$conf.int[,3],
    CI.upper = fitsummary__$conf.int[,4],
    Pvalue   = fitsummary__$coefficients[,5],
    stringsAsFactors = FALSE
  )
  if (!(is.null(PREDval)|length(PREDval)==0)) {
    fixed__ <- data.frame(
      Parameter = names(PREDval),
      Value     = PREDval,
      CI.lower = NA,
      CI.upper = NA,
      Pvalue   = NA,
      stringsAsFactors = FALSE
    )
    estimates__ <- rbind(estimates__, fixed__)
  }
  objval__ <- data.frame(Parameter = c("GLOBAL"), OBJ = -2*logLik(fit), DF = fitsummary__$logtest[["df"]], DeltaOBJ = -2*diff(fit$loglik), stringsAsFactors = FALSE)
  phtestres__ <- data.frame(Parameter = rownames(phtest__$table), "p_PH" = phtest__$table[,"p"], stringsAsFactors = FALSE)
  estimates__ <- dplyr::left_join(estimates__, phtestres__, by = "Parameter")
  objval__ <- dplyr::left_join(objval__, phtestres__, by = "Parameter")
  for (k in seq_along(CATval__$label0)) {
    estimates__$Parameter <- gsub(CATval__$label0[k],CATval__$label1[k],estimates__$Parameter)
  }
  names(estimates__) <- c("Parameter", "Value", CIlabel, "P_value", "P_value_PH")
  names(objval__) <- c("Parameter", "OBJ", "DF", "DeltaOBJ", "P_value_PH")
  plotList__ <- plot_IQRcoxRegression(
    fit,
    xlab = xlab,
    ylab = ylab,
    ci.level = ci.level,
    filename = NULL
  )
  esttable__ <- estimates__ 
  esttable__ <- dplyr::mutate_if(esttable__, is.numeric, function(x) signif(x, SIGNIF))
  esttable__ <- format(data.frame(lapply(esttable__,function (x) as.character(x)),stringsAsFactors = FALSE),justify="left")
  esttable__ <- data.frame(lapply(esttable__, function(x) {x[grepl("^NA$", trimws(x))] <- "-"; x} ))
  colLB__ <- grep("CIlb", names(esttable__))
  colUB__ <- grep("CIub", names(esttable__))
  CIrange <- paste0("[", esttable__[[colLB__]],", ", esttable__[[colUB__]], "]")
  CIrange[esttable__[[colLB__]] == "-"] <- "-"
  esttable__[[colLB__]] <- CIrange
  names(esttable__)[colLB__] <- CIrangeLabel
  esttable__[[colUB__]] <- NULL
  objvalADD__ <- objval__
  objvalADD__ <- dplyr::mutate_if(objvalADD__, is.numeric, function(x) signif(x, SIGNIF))
  objvalADD__ <- dplyr::mutate(objvalADD__, Parameter = "")
  esttable__ <- rbind(esttable__, rep("",ncol(esttable__)))
  esttable__ <- rbind(esttable__, gsub("Parameter","**Metrics**",names(objval__)))
  esttable__ <- rbind(esttable__, unlist(objvalADD__))
  xtitle__ <- paste0("Estimates for Cox proportional hazards regression model")
  xfooter__ <- paste0("Model: ",formula0__, "\nImplemented as: ", funCall__)
  for (k in PREDcolCAT) {
    refCatk__ <- levels(factor(data__[[k]]))[1]
    xfooter__ <- paste0(xfooter__, paste0("\nReference category for ", k, ": ", refCatk__))
  }
  xfooter__ <- paste0(xfooter__, "\nValue: Hazard ratio estimate for predictor. OBJ: -2*log-likelihood, DeltaOBJ: OBJ difference to null model. P_value: statistical significance of predictor variable. P-value_PH: p-value for testing proportional hazard assumption (valid assumption if >0.05, value in OBJ row is global value).")
  xfooter__ <- paste0(xfooter__, "\nValues rounded to ", SIGNIF, " significant digits.")
  table__ <- IQRoutputTable(esttable__, xtitle = xtitle__, xfooter = xfooter__, filename = NULL, report = FLAGreport)
  out__ <- table__
  attr(out__, "estimates") <- estimates__
  attr(out__, "metrics") <- objval__
  attr(out__, "fit") <- fit
  attr(out__, "formula") <- formula0__
  attr(out__, "plots") <- plotList__
  class(out__) <- c("IQRcoxRegression", class(out__))
  if (!is.null(pathname)) {
    export_IQRcoxRegression(out__, pathname, FLAGreport = FLAGreport)
  }
  out__
}
#'@export
summary_IQRcoxRegression <- function(x,
                                     basemodel   = NULL,
                                     FLAGorder   = FALSE,
                                     filename    = NULL,
                                     SIGNIF      = 4,
                                     FLAGreport  =TRUE,
                                     FLAGformula = FALSE,
                                     title       = NULL,
                                     footerAdd   = NULL) {
  idxFolder__ <- !sapply(lapply(x, class), function(yy) "IQRcoxRegression" %in% yy)
  if (any(idxFolder__)) {
    idxFolder__ <- which(idxFolder__)
    for (k in idxFolder__) {
      if (!is.character(x[[k]]) | length(x[[k]]) != 1) stopIQR("For elements not being an IQRcoxRegression object, path to a project folder needs to be given.")
      x[[k]] <- import_IQRcoxRegression(x[[k]])
    }
  }
  modelNames__ <- names(x)
  if (is.null(modelNames__)) {modelNames__ <- paste0("Model_", sprintf("%.2d",seq_along(x))); names(x) <- modelNames__}
  esttables__ <- lapply(x, function(x__) attr(x__, "estimates"))
  estobj__    <- lapply(x, function(x__) attr(x__, "metrics"))
  estsumm__   <- lapply(x, function(x__) summary(attr(x__, "fit")))
  ci.level <- sapply(x, function(x__) gsub("% CI","",grep("CI", names(x__$xtable), value = TRUE), fixed = TRUE))
  if (length(unique(ci.level)) != 1) {
    warningIQR("Confidence level for models are not same.")
    ci.level <- NA
  } else {
    ci.level <- ci.level[1]
  }
  reformat <- function(x__) {
    val__ <- paste0(signif(x__[,"Value"], SIGNIF), " (",
                    signif(x__[,grep("^CIlb",names(x__))], SIGNIF), ",",
                    signif(x__[,grep("^CIub",names(x__))], SIGNIF),")")
    val__ <- gsub(" (NA,NA)","",val__, fixed = TRUE)
    pval__ <- x__[,"P_value"]; pval__[is.na(pval__)] <- 1
    pars__ <- x__[,"Parameter"]
    idxSIG1 <- pval__ < 0.05; val__ <- ifelse(idxSIG1, paste0(val__, "*"), val__)
    idxSIG2 <- pval__ < 0.01; val__ <- ifelse(idxSIG2, paste0(val__, "*"), val__)
    idxSIG3 <- pval__ < 0.001; val__ <- ifelse(idxSIG3, paste0(val__, "*"), val__)
    y__ <- data.frame(t(val__), stringsAsFactors = FALSE); names(y__) <- pars__
    y__
  }
  table__ <- do.call(dplyr::bind_rows, lapply(esttables__, reformat))
  table__$Model <- modelNames__
  table__[is.na(table__)] <- "-"
  metrics__ <- dplyr::bind_rows(estobj__, .id = "Model")
  metrics__ <- dplyr::mutate(metrics__, Parameter = NULL, )
  table__   <- dplyr::left_join(table__, metrics__, by = "Model")
  table__   <- dplyr::mutate(table__,
                             OBJ = signif(OBJ, SIGNIF), DeltaOBJ = signif(DeltaOBJ, SIGNIF), P_value_PH = signif(P_value_PH, SIGNIF))
  FLAGbase <- FALSE
  if (!is.null(basemodel)) {
    if (is.numeric(basemodel)) {
      idxbase__ <- basemodel
      namebase__ <- table__$Model[idxbase__]    } else {
        namebase__ <- basemodel
        idxbase__ <- which(table__$Model == namebase__)
      }
    if (length(idxbase__) == 0) warningIQR("Base model could not be identified by given name.")
    else {
      FLAGbase <- TRUE
      objall__ <- sapply(estsumm__, function(xx__) -2*xx__$loglik[2])
      OBJbase__ <- objall__[idxbase__]
      DFbase__ <- table__$DF[idxbase__]
      table__$DeltaOBJ <- signif(objall__ - OBJbase__, SIGNIF)
      table__$DeltaDF  <- table__$DF  - DFbase__
      table__$DeltaOBJ[idxbase__] <- "(base)"
      table__$DeltaDF[idxbase__] <- "(base)"
    }
  }
  if (FLAGorder) {
    table__ <- table__[order(table__[["OBJ"]]),]
  }
  idxModel__ <- which(names(table__) == "Model")
  idxOBJ__ <- which(names(table__) %in% c("OBJ", "DF","DeltaOBJ", "DeltaDF", "P_value_PH"))
  idxCore__ <- which(!names(table__) %in% c("Model","OBJ", "DF","DeltaOBJ", "DeltaDF", "P_value_PH"))
  table__ <- table__[,c(idxModel__, idxOBJ__, idxCore__)]
  if (FLAGformula)
    table__$Formula <- sapply(x, function(x__) attr(x__,"formula"))
  if (is.null(title))
    title__ <- "Comparison of Cov proportional hazard regression models"
  footer__ <- paste0("Estimates with ",ci.level,"%-confidence intervals\nSignificance: * <0.05, ** <0.01, *** <0.001\n",
                     "Values rounded to ",SIGNIF, " digits.\nOBJ: -2*log-likelihood; DF: Degrees of freedom; P_value_PH: global test for proportional hazards (valid assumption if >0.05); ")
  if (FLAGbase) {
    footer__ <- paste0(footer__, "DeltaOBJ: OBJ difference to base model; DeltaDF: DF difference to base model.")
  } else {
    footer__ <- paste0(footer__, "DeltaOBJ: OBJ difference to respective null model.")
  }
  if (FLAGorder)
    footer__ <- paste0(footer__, "\nModels ordered by objective function value.")
  if (!is.null(footerAdd))
    footer__ <- paste0(footer__, "\n", footerAdd)
  if (!is.null(filename)) {
    filename <- paste0(aux_strrep(filename,".txt",""),".txt")
    IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=title__,report=FLAGreport,filename=filename)
    return(invisible(NULL))
  }
  output__ <- IQRoutputTable(xtable=table__,xfooter=footer__,xtitle=title__,report=FLAGreport)
  output__
}
#'@export
export_IQRcoxRegression <- function(obj, pathname, FLAGreport = TRUE) {
  if (!("IQRcoxRegression" %in% class(obj))) stopIQR("Input object is no IQRcoxRegression object.")
  filename__ <- file.path(pathname, "parameters_table.txt")
  table__ <- IQRoutputTable(obj$xtable, xtitle = obj$xtitle, xfooter = obj$xfooter, filename = filename__, report = FLAGreport, na.string = "-")
  filename__ <- file.path(pathname, "estimates.csv")
  IQRoutputCSV(attr(obj,"estimates"), filename = filename__)
  filename__ <- file.path(pathname, "DiagnosticPlots.pdf")
  IQRoutputPDF(attr(obj,"plots"), filename = filename__)
  filename__ <- file.path(pathname, "fittedModel.rds")
  saveRDS(attr(obj,"fit"), file = filename__)
  filename__ <- file.path(pathname, "fittedProject.rds")
  saveRDS(obj, file = filename__)
  return(invisible(NULL))
}
#'@export
import_IQRcoxRegression <- function(pathname) {
  filename__ <- file.path(pathname, "fittedProject.rds")
  if (!file.exists(filename__)) stopIQR("Given folder does not seem to contain IQRcoxRegression object.")
  obj__ <- readRDS(filename__)
  if (!("IQRcoxRegression" %in% class(obj__))) stopIQR("Object loaded is no IQRcoxRegression object.")
  return(obj__)
}
#'@export
plot_IQRcoxRegression <- function(
  fit,
  xlab = NULL,
  ylab = NULL,
  ci.level = 0.95,
  filename = NULL
) {
  inclass__ <- class(fit)
  if (!any(c("coxph", "IQRcoxRegression") %in% inclass__)) stopIQR("Input needs to be IQRcoxRegression or coxph object.")
  if ("IQRcoxRegression" %in% inclass__) {
    fit__ <- attr(fit, "fit")
  } else {
    fit__ <- fit
  }
  if (ci.level <= 0 | ci.level >= 1) { stopIQR("Confidence levels needs to be between 0 and 1.")
  } else {
    CIlabel <- paste0(ci.level*100, "%-CI")
  }
  if (is.null(xlab)) xlab = "Time"
  if (is.null(ylab)) ylab = "Survival"
  kmObs <- survival::survfit(fit$y~1)
  dataObs <- with(kmObs, data.frame(TIME = time, SURV = surv, LB = lower, UB = upper, which = "observed") )
  kmFit <- survival::survfit(fit)
  dataFit <- with(kmFit, data.frame(TIME = time, SURV = surv, LB = lower, UB = upper, which = "predicted") )
  dataPlot <- rbind(dataObs, dataFit)
  dataPlot <- plyr::ddply(dataPlot, ~which, function(dat) {
    datAdd <- dat[1:(nrow(dat)-1),]
    datAdd$TIME <- dat$TIME[2:nrow(dat)]
    out <- rbind(dat, datAdd)
    out <- dplyr::arrange(out, TIME, desc(SURV))
    out
  })
  dataPlotObs <- dplyr::filter(dataPlot, which == "observed")
  dataPlotObs <- tidyr::gather(dataPlotObs, key = "what", value = "value", SURV, LB, UB)
  dataPlotFit <- dplyr::filter(dataPlot, which == "predicted")
  pObsPred <- IQRggplot(dataPlotObs, aes(TIME)) +
    geom_line(aes(y=value, linetype=what)) +
    geom_line(data=dataPlotFit, aes(y=SURV), color = "firebrick", linetype = 4) +
    geom_ribbon(data=dataPlotFit, aes(ymin=LB,ymax=UB), alpha = 0.3, fill = "firebrick") +
    scale_linetype_manual(values=c("SURV"=1,"LB"=2,"UB"=2)) +
    theme(legend.position = "none") +
    labs(x=xlab, y=ylab, title = "Predicted and observed survival",
         caption = paste0("Black lines: KM curve of observations with ", CIlabel, "\nRed line and area: Predicted median and ", CIlabel))
  residuals__ <- data.frame(Deviance = stats::residuals(fit))
  pDevHist <- IQRggplot(residuals__, aes(Deviance)) + geom_histogram()
  plotList__ <- list(ObsPred = pObsPred, DevianceHistogramm = pDevHist)
  if (!is.null(filename)) {
    IQRoutputPDF(plotList__, filename = filename, onefile = TRUE)
    return(invisible(NULL))
  }
  plotList__
}
#'@export
IQRtable <- function(data, stat, ...) {
  stat.char__ <- deparse(substitute(stat))
  stat.fn__ <- with(data, eval(parse(text = stat.char__)))
  keywords.stat__ <- attr(stat.fn__, "keywords")
  keywords.data__ <- setdiff(names(data), keywords.stat__)
  keywords__ <- c(keywords.data__, keywords.stat__)
  keywords__ <- keywords__[order(nchar(keywords__), decreasing = TRUE)]
  output__ <- lapply(list(...), function(o) rep(o, nrow(data)))
  output__ <- lapply(output__, function(output.column__) {
    used__ <- sapply(keywords__, function(k) grepl(k, output.column__[1]))
    if (!any(used__)) return(output.column__)
    for (k in keywords__[used__]) {
      replacement__ <- NULL
      if (k %in% keywords.stat__) replacement__ <- stat.fn__(k)
      if (k %in% keywords.data__) replacement__ <- data[[k]]
      if (length(replacement__) > 0)
        output.column__ <- sapply(seq_along(output.column__), function(i) gsub(k, replacement__[i], output.column__[i]))
    }
    return(output.column__)
  })
  header__ <- matrix(names(output__), nrow = 1)
  body__ <- do.call(cbind, output__)
  table__ <- rbind(header__, body__)
  as_IQRtable(table__)
}
#'@export
as_IQRtable <- function(x) {
  UseMethod("as_IQRtable", x)
}
#'@export
as_IQRtable.IQRtable <- function(x) x
#'@export
as_IQRtable.matrix <- function(x) {
  dims__ <- dim(x)
  out__ <- matrix(as.character(x), dims__[1], dims__[2])
  colnames(out__) <- rownames(out__) <- NULL
  class(out__) <- c("IQRtable", "matrix")
  return(out__)
}
#'@export
as_IQRtable.data.frame <- function(x) {
  out__ <- rbind(
    names(x),
    as.matrix(as.data.frame(lapply(x, as.character)))
  )
  rownames(out__) <- colnames(out__) <- NULL
  class(out__) <- c("IQRtable", "matrix")
  return(out__)
}
#'@export
as_IQRtable.character <- function(x) {
  out__ <- as.matrix(x, nrow = 1)
  as_IQRtable.matrix(out__)
}
#'@export
as_IQRtable.numeric <- function(x) {
  as_IQRtable.character(as.character(x))
}
#'@export
is_IQRtable <- function(x) {
  inherits(x, "IQRtable")
}
#'@export
"[.IQRtable" <- function(x, i, j, drop = FALSE) {
  x__ <- unclass(x)
  x__[i, j, drop = drop]
}
#'@export
print.IQRtable <- function(x, ...) {
  tmp__ <- as.data.frame(x)
  outputTable__ <- IQRoutputTable(tmp__, report = FALSE)
  cat(text_IQRoutputTable(outputTable__))
}
#'@export
as.data.frame.IQRtable <- function(x, row.names = NULL, optional = FALSE, ..., header = TRUE) {
  x__ <- as.matrix(unclass(x))
  if (header) {
    myheader__ <- x__[1,]
    x__ <- x__[-1, , drop = FALSE]
  } else {
    myheader__ <- rep("", ncol(x__))
  }
  out__ <- as.data.frame(x__, row.names = row.names, optional = optional, ...)
  colnames(out__) <- myheader__
  out__
}
#'@export
compose_IQRtable <- function(..., pattern, just = "tl") {
  tables__ <- list(...)
  if (length(tables__) == 1 & is.list(tables__[[1]])) tables__ <- tables__[[1]]
  tables__ <- lapply(tables__, as_IQRtable)
  rows__ <- strsplit(pattern, "\n")[[1]]
  nrows__ <- length(rows__)
  ncols__ <- unique(sapply(rows__, function(myrow__) length(strsplit(myrow__, "|", fixed = TRUE)[[1]])))
  if (length(ncols__) > 1)
    stopIQR("The number of columns is not uniquely defined. Check missing white space.")
  cells__ <- lapply(rows__, function(myrow__) {
    cells__ <- trimws(strsplit(myrow__, "|", fixed = TRUE)[[1]])
    lapply(cells__, function(mycell__) {
      if (mycell__ == "") return(matrix("", nrow = 1, ncol = 1))
      if (mycell__ %in% names(tables__)) return(tables__[[mycell__]])
      if (any(sapply(names(tables__), function(X) grepl(X, mycell__)))) return(with(tables__, eval(parse(text = mycell__))))
      return(matrix(mycell__, nrow = 1, ncol = 1))
    })
  })
  cells__ <- lapply(seq_len(nrows__), function(i) {
    myrow__ <- cells__[[i]]
    height__ <- max(sapply(myrow__, nrow))
    lapply(myrow__, function(mycell__) {
      grow(mycell__, height__, ncol(mycell__), just)
    })
  })
  cells__ <- lapply(seq_len(ncols__), function(j) {
    mycol__ <- lapply(cells__, function(myrow__) myrow__[[j]])
    width__ <- max(sapply(mycol__, ncol))
    lapply(mycol__, function(mycell__) {
      grow(mycell__, nrow(mycell__), width__, just)
    })
  })
  out__ <- Reduce("cbind", lapply(cells__, function(mycol__) Reduce("rbind", mycol__)))
  as_IQRtable(out__)
}
#'@export
composeByRule_IQRtable <- function(..., rule, just = "tl") {
  args__ <- list(...)
  if (length(args__) == 1 & is.list(args__[[1]])) args__ <- args__[[1]]
  out__ <- args__[[1]]
  if (length(args__) > 1) {
    for (i in 2:length(args__)) {
      x <- out__
      y <- args__[[i]]
      out__ <- compose_IQRtable(x = x, y = y, pattern = rule, just = just)
    }
  }
  as_IQRtable(out__)
}
#'@export
cjoin_IQRtable <- function(..., na.char = "--") {
  args__ <- list(...)
  if (length(args__) == 1 & is.list(args__[[1]])) args__ <- args__[[1]]
  args__ <- lapply(args__, unclass)
  out__ <- as.data.frame(args__[[1]], stringsAsFactors = FALSE)
  if (length(args__) > 1) {
    for (i in 2:length(args__)) {
      x__ <- out__
      y__ <- as.data.frame(args__[[i]], stringsAsFactors = FALSE)
      empty1__ <- which(x__[[1]] %in% "")
      empty2__ <- which(y__[[1]] %in% "")
      if (length(empty1__) > 0)
        x__[[1]][empty1__] <- paste0(".fillempty", 1:length(empty1__))
      if (length(empty2__) > 0)
        y__[[1]][empty2__] <- paste0(".fillempty", 1:length(empty2__))
      out__ <- dplyr::full_join(x__, y__, by = "V1")
    }
  }
  out__ <- as.matrix(out__)
  out__[grepl(".fillempty", out__)] <- ""
  out__[is.na(out__)] <- na.char
  as_IQRtable(out__)
}
#'@export
rjoin_IQRtable <- function(..., na.char = "--") {
  args__ <- list(...)
  if (length(args__) == 1 & is.list(args__[[1]])) args__ <- args__[[1]]
  args__ <- lapply(args__, unclass)
  args__ <- lapply(args__, t)
  args__[["na.char"]] <- na.char
  out__ <- do.call(cjoin_IQRtable, args__)
  t(out__)
}
#'@export
statSE <- function(value, se = 10, digits = NA) {
  na.chars__ <- c(".", "-")
  tmp__ <- data.frame(value, se)
  tmp__ <- lapply(tmp__, function(mycol__) {
    for (myna.char in na.chars__) mycol__[mycol__ %in% myna.char] <- NA
    utils::type.convert(as.character(mycol__))
  })
  value__ <- tmp__[[1]]
  se__ <- tmp__[[2]]
  rse__ <- abs(se__/value__)
  se_rse__ <- rse__^2
  myformat__ <- function(x, ...) {
    if (length(digits) != length(x)) digits <- rep_len(digits, length(x))
    ifelse(is.na(digits), format_GUM(x, ...), sapply(seq_along(x), function(i__) {
      myval__ <- x[i__]
      if (is.na(myval__)) return("--")
      format(x[i__], digits = digits[i__])
    }))
  }
  outfn <- function(char) {
    if (char == "value") {
      return(trimws(myformat__(value__, se__)))
    }
    if (char == "se") {
      return(trimws(myformat__(se__)))
    }
    if (char == "rse") {
      return(trimws(myformat__(rse__, se_rse__)))
    }
    if (char == "rse%") {
      return(paste0(trimws(myformat__(100*rse__, 100*se_rse__)), "%"))
    }
    if (any(char == paste0("lower.", 1:99))) {
      x <- utils::type.convert(strsplit(char, ".", fixed = TRUE)[[1]][2])
      lower__ <- stats::qnorm(0.5 - x/200, mean = value__, sd = se__)
      return(trimws(myformat__(lower__, se__)))
    }
    if (any(char == paste0("upper.", 1:99))) {
      x <- utils::type.convert(strsplit(char, ".", fixed = TRUE)[[1]][2])
      upper__ <- stats::qnorm(0.5 + x/200, mean = value__, sd = se__)
      return(trimws(myformat__(upper__, se__)))
    }
    stopIQR("Keyword not recognized")
  }
  keywords__ <-  c("value", "se",  "rse", "rse%", paste0("lower.", 1:99), paste0("upper.", 1:99))
  attr(outfn, "keywords") <- keywords__
  return(outfn)
}
#'@export
statCI <- function(value, lower, upper, CL = 0.95, digits = NA) {
  na.chars__ <- c(".", "-")
  tmp__ <- data.frame(value, lower, upper)
  tmp__ <- lapply(tmp__, function(mycol__) {
    for (myna.char in na.chars__) mycol__[mycol__ %in% myna.char] <- NA
    utils::type.convert(as.character(mycol__))
  })
  value__ <- tmp__[[1]]
  lower <- tmp__[[2]]
  upper <- tmp__[[3]]
  se.right__ <- sapply(seq_along(value__), function(i) {
    q.right__ <- function(sd) upper[i] - stats::qnorm(0.5 + CL/2, mean = value__[i], sd = sd)
    tol__ <- min(1e-3*abs(value__[i]), .Machine$double.eps^0.25)
    myroot__ <- try(stats::uniroot(q.right__, lower = .Machine$double.eps, upper = .Machine$double.xmax/2, tol = tol__)[["root"]], silent = TRUE)
    if (inherits(myroot__, "try-error"))
      return(NA)
    myroot__
  })
  se.left__ <- sapply(seq_along(value__), function(i) {
    q.left__ <- function(sd) lower[i] - stats::qnorm(0.5 - CL/2, mean = value__[i], sd = sd)
    tol__ <- min(1e-3*abs(value__[i]), .Machine$double.eps^0.25)
    myroot__ <- try(stats::uniroot(q.left__, lower = .Machine$double.eps, upper = .Machine$double.xmax/2, tol = tol__)[["root"]], silent = TRUE)
    if (inherits(myroot__, "try-error"))
      return(NA)
    myroot__
  })
  se__ <- apply(cbind(se.left__, se.right__), 1, mean)
  rse__ <- abs(apply(cbind(se.left__, se.right__), 1, mean)/value__)
  se_rse__ <- rse__^2
  myformat__ <- function(x, ...) {
    if (length(digits) != length(x)) digits <- rep_len(digits, length(x))
    ifelse(is.na(digits), format_GUM(x, ...), sapply(seq_along(x), function(i__) {
      myval__ <- x[i__]
      if (is.na(myval__)) return("--")
      format(x[i__], digits = digits[i__])
    }))
  }
  outfn <- function(char) {
    if (char == "value") {
      return(trimws(myformat__(value__, pmin(se.left__, se.right__, na.rm = TRUE))))
    }
    if (char == "se") {
      return(trimws(myformat__(se__)))
    }
    if (char == "rse") {
      return(trimws(myformat__(rse__, se_rse__)))
    }
    if (char == "rse%") {
      rse__ <- abs(se__/value__)
      se_rse__ <- rse__^2
      return(paste0(trimws(myformat__(100*rse__, 100*se_rse__)), "%"))
    }
    if (any(char == paste0("lower.", 1:99))) {
      x <- utils::type.convert(strsplit(char, ".", fixed = TRUE)[[1]][2])
      lower__ <- stats::qnorm(0.5 - x/200, mean = value__, sd = se.left__)
      return(trimws(myformat__(lower__, se.left__)))
    }
    if (any(char == paste0("upper.", 1:99))) {
      x <- utils::type.convert(strsplit(char, ".", fixed = TRUE)[[1]][2])
      upper__ <- stats::qnorm(0.5 + x/200, mean = value__, sd = se.right__)
      return(trimws(myformat__(upper__, se.right__)))
    }
    stopIQR("keyword not recognized")
  }
  keywords__ <-  c("value", "se",  "rse", "rse%", paste0("lower.", 1:99), paste0("upper.", 1:99))
  attr(outfn, "keywords") <- keywords__
  return(outfn)
}
grow <- function(M, nrow, ncol, just = "tl", fillWith = "") {
  hjust__ <- ifelse(grepl("r", just), "r", "l")
  vjust__ <- ifelse(grepl("b", just), "b", "t")
  n__ <- nrow(M)
  m__ <- ncol(M)
  nrow__ <- max(nrow, n__)
  ncol__ <- max(ncol, m__)
  out__ <- switch(hjust__,
                  l = cbind(M, matrix(fillWith, nrow = n__, ncol = ncol__ - m__)),
                  r = cbind(matrix(fillWith, nrow = n__, ncol = ncol__ - m__), M))
  out__ <- switch(vjust__,
                  t = rbind(out__, matrix(fillWith, nrow = nrow__ - n__, ncol = ncol(out__))),
                  b = rbind(matrix(fillWith, nrow = nrow__ - n__, ncol = ncol(out__)), out__))
  out__
}
#'@import ggplot2
#'@useDynLib IQRtools, .registration = TRUE
NULL
#'@export
loadSetupOptions_IQRtools <- function() {
  .ALLOW_USER_SETTINGS_FILE = F
  IQRTOOLS_OVERRIDE_SETTING_COMPLIANCE_MODE = F
  if (.ALLOW_USER_SETTINGS_FILE) {
    home__ <- Sys.getenv("HOME")
    file__ <- paste0(home__,"/setup_options_IQRtools.R")
    if (file.exists(file__)) {
      source(file__)
    }
  }
}
.onAttach <- function(libname, pkgname) {
  showStartupMessage()
}
.onLoad <- function(libname, pkgname) {
}
.onUnload <- function(libpath) {
  if ("IQRtools" %in% names(getLoadedDLLs())) {
    library.dynam.unload("IQRtools", libpath)
  }
}
globalVariables(c("exportpath","allowMONOLIXgui","atrcontents","x","k__","IQRTOOLS_OVERRIDE_SETTING_COMPLIANCE_MODE",
                  "var", "VALUE",
                  "ITERATION", "TYPE", "NAME", "XNAME", "XVALUE", "YNAME", "YVALUE",  "ID", "X",
                  "High.Correlation", "CORR_INFO", "minX", "textY",  "ST", "Y", "SHOW_CORR", "DV",
                  "CENS", "Stratification", "loess",  "TIME", "XPRED", "slope", "int", "DV.IPRED.PRED",
                  ".PATH_SYSTEM_NONMEM",  ".PATH_SYSTEM_NONMEM_PARALLEL", "sd", "se", "nDataXbinwidth",
                  "INPUTS",  ".COMPLIANCE_MODE", "k", "is.zero", "TIME_DOSE_EFFECT_START",
                  "TINF", "CONDITION", "CONVERGED", "COVNAME", "COVVAL",  "INDEX",
                  "ITERATIONS", "OFV", "PARAMETERS", "PRED", "Var1", "Var2",  "WRES", "value",
                  "par", "AMT", "INPUT", "ADM", "proflist", "data",  "catInfo",
                  ".MODEL_MAX_DESIRED_PARAMETERNAME_LENGTH", ".MODEL_MAX_DESIRED_REACTIONNAME_LENGTH",
                  ".MODEL_MAX_DESIRED_STATENAME_LENGTH", ".MODEL_MAX_DESIRED_VARIABLENAME_LENGTH",
                  ".PATH_IQRreport", ".MAX_NUMBER_IQRMODEL_DLLS_LOADED", ".NLME_ORDER_CRITERION",
                  ".PATH_IQRsbml", ".ALLOW_USER_SETTINGS_FILE","meanX", "meanY", "IXGDF", "IGNORE",
                  "USUBJID",  "CENTER",
                  "SUBJECT", "INDNAME", "IND", "COMPOUND", "STUDY", "STUDYN",  "STUDYDES",
                  "PART", "EXTENS", "TRTNAME", "TRT", "TRTNAMER", "TRTR",  "VISIT", "VISNAME",
                  "BASE", "SCREEN", "TIMEUNIT", "DATEDAY",  "DATETIME", "NT", "DURATION",
                  "TYPENAME", "VALUETXT", "UNIT",  "ULOQ", "LLOQ", "ROUTE", "II", "ADDL",
                  "AE", "AEGRADE", "AESER", "AEDRGREL", "COMMENT", "TIMEPOS",
                  "EVID", "MDV", "RATE",  "YTYPE", "TAD", "DOSE", "doseNAMES", "obsNAMES",
                  "covInfo", "imputeInfo",  "methodBLLOQ",
                  ".ALLOW_UNDERSCORES_IN_PARAMETER_NAMES", "DVcolName", "IPREDcolName",
                  "IWREScolName", ".PATH_SYSTEM_MONOLIX",
                  "QUANTILE", ".RESERVED_WORD_IQRMODELS", "TIME2", "XPREDcolName", "XWREScolName",
                  "obsNames", "startTime","COLOR","MAX","MIN",
                  "MEDIAN","Q05","Q95","PARAMETER","NORM_SENS","ID_PARAMETERS",
                  "TIME.VARYING","CNAME","CVALUE","CNAME.x","CVALUE.x","CNAME.y","CVALUE.y",
                  "pos.x","pos.y","NAME.x","UNIT.x","corr","p.value","timeMin__","valueMax__","N","COLNAME",
                  "TIMEdos", "doseRecords", "yend", "ystart", "CORR","PARAMNAME","values",
                  ".VERSION_SUPPORTED_NLMIXR",
                  "LNo.x", "LNo.y", "labelDos", "lower", "p.value.small", "upper","pvalResult",
                  "BLOQ","regressorNames",
                  "resFit", "fit", "Pvalue", "Fraction", "Freq", "category",
                  "Residuals", "Pred", "fitted.values",
                  "forcings", "obj", "constr_fit", "startpars", "opt.iterlim", "lower_omega", "parframes",
                  "bestfit", "hypothesis", "bestfit", "lower_omega", "fixed_on_lower", "parametername",
                  "parametervalue", "spread", "sigma", "bloq", "parametertask",
                  "time", "condition", "belongs_to", "base_par", "name",
                  "estimated", "lloq", "lower95_parametervalue_linear", "parametervalue_linear",
                  "pred_indiv", "pred_pop", "printname", "res_indiv", "res_pop", "sigma_linear",
                  "upper95_parametervalue_linear", "wres_indiv", "wres_pop", "Condition", "pars",
                  "fixed_on_upper", "index", "optimum", "optimum, -2LL", "parameter",
                  "parvalue", "converged", "trafo",
                  "kkk__",
                  "Statistic", "GROUP", "DU", "DVari", "DVgeo", "VALUEari", "VALUEgeo",
                  "LAMZ", "PROFTIME", "slopePoints", "LABEL", "isBLQ", "b0",
                  "block", "TPRED", "TPRED0", "IPRED", "Value", "Percentile", "intmax", "intmin",
                  "maxTIME", "maxValue", "START", "Trial", "PERIODGRP",
                  "failed", "all_failed", "n_failed", "partial_failed",
                  "Index", "Value", "Parameter", "Obj", "Task", "Iteration",
                  ".OUTPUTFOLDER_SLIDES",
                  "Parameters",
                  "opt.layout", "opt.page","content","title","subtitle","footer","filename","plotdata","width","height",
                  "logLik","ROWID",".N",".SD","scaleWidth","scaleHeight","res","scaleWidth","scaleHeight",
                  "plotGrid",".RDS_FILES_OUTPUT","file.edit","val",
                  "STRAT","IQRoutputPlot","SURV","LB","UB","what","residuals","Deviance","DOSING.",
                  "paramNominal","paramPerturbed","DOSING","Sensitivity","Sign","cor","OBJ","DeltaOBJ",
                  "P_value_PH","MEDIAN.VALUE","LOW","HIGH",".LIBPATHS_FIRSTONLY",".PATH_LICENSE_FILE",
                  "TIMEtinfStart",
                  "atrPath","COVPAR","defineInfo","destination","note","path","rmdPath","setkey",
                  "xptInfo","xptPath",
                  ".PATH_IQRsbmlOut","OBSERVATION.NAME","TOTAL.OBSERVATIONS",".ALLOW_SPACES_IN_PATH",
                  "FREQPLOT","FREQPLOTCI025","FREQPLOTCI975","Naesubjects","ORDERTAB","PLOTSTRAT","PLOTX","Path","Rank",
                  "Reference OFV","Run failed","Signif","Target","dOFV","dDF","Target dOFV","included","prm","p-value",
                  "Model", "Incl. in stage","Nae","TEND"
))
