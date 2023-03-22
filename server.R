# load in modular scripts-----------------------------------------------------------------------------------------
source("docxtractr.R")
resetWorkers(plan(multisession, workers = 2)) # to enable multiple concurrent users

### Function added to improve ggplot render quality/anti-aliasing-------------------------------------------------
trace(grDevices:::tiff, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-tiff"
    antialias <- "subpixel"
  }
}), print = FALSE)
###
### backtick function from package qwraps2------------------------------------------------------------------------
backtick <- function(x, dequote = FALSE) {
  x <- deparse(substitute(x))
  if (dequote) {
    x <- sub("^(\"|')", "", x)
    x <- sub("(\"|')$", "", x)
  }
  sprintf("`%s`", x)
}
### function to check if any columns are missing from the dataset--------------------------------------------------
check_columns <- function(df, col_names, id = "dataset") {
  df_cols <- names(df)
  missing_cols <- col_names[!col_names %in% df_cols]
  
  if (length(missing_cols) > 0) {
    err_msg <- paste0("The following required columns are missing from the ",id,":\n", paste0("'", missing_cols, "'",collapse = ",\n"))
    message(err_msg)
    showNotification(ui = err_msg,
                     duration = NULL,
                     closeButton = T,
                     id = paste0("err_",id %>% str_replace_all(" ","_")),
                     type = "error")
    return(FALSE)
  } else {
    message("All ",id, " columns are present.")
    return(TRUE)
  }
}
###
### Apply default theme for all plots--------------------------------------------
ggplot2::theme_set(
  ggplot2::theme_bw()
  # + theme(panel.background =
  #           element_rect(color = "white")
  #   )
)
### ensure req is not masked by mrgsolve
req <- shiny::req
###
# Function to make sure no other devices are running-----------------------------------------------
graphics.off()
###

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2, shiny.fullstacktrace = TRUE, future.globals.maxSize = 1000 * 1024^2) # Max upload size at 1 GB
  
  ## Update various fileInput progress bars and username if it exists and choose download format
  runjs('$(".progress-bar").addClass("progress-bar-striped progress-bar-animated");
         let svg = $("#format_choice button:nth-child(2)");
         let png = $("#format_choice button:nth-child(1)");
         Shiny.setInputValue("format_png", true);
         Shiny.setInputValue("format_svg", false);
         let toggleChild = function(){
           let formatName = $(this).children()[0].innerText.trim().replace(".","");
           if ($(this).hasClass("active")){
            $(this).removeClass("active")
            $(this).children().children().replaceWith(\'<i class="far fa-square checking-align" role="presentation" aria-label="chart icon" style="margin-right: 4px;vertical-align: middle;"></i>\')
            Shiny.setInputValue("format_" + formatName, false);
           }
           else {
            $(this).addClass("active")
            $(this).children().children().replaceWith(\'<i class="fa fa-check-square checking-align" role="presentation" aria-label="chart icon" style="margin-right: 4px;vertical-align: middle;"></i>\')
            Shiny.setInputValue("format_" + formatName, true);
           } 
         };
         svg.click(toggleChild)
         png.click(toggleChild)
        ')
  runjs('let cookieval = document.cookie.split("; ")[0];
                  let cookiename = cookieval.split("=")[1];
                  if (cookieval.length) {
                    Shiny.setInputValue("username", cookiename);
                  }')
  #add option to filter jQuery w/ regex (from https://blog.mastykarz.nl/jquery-regex-filter/)
  runjs("
        jQuery.extend(
            jQuery.expr[':'], {
                regex: function(a, i, m, r) {
                    var r = new RegExp(m[3], 'i');
                    return r.test(jQuery(a).text());
                }
            }
        );
        ")
  ### Load the dataset------------------------------------------------------------------
  file <- reactive({
    input$file_path
  })
  
  path <- reactive({
    file()$datapath
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(file()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  mydata <- eventReactive(input$file_path, { # Load the data
    req(path())
    ext <- file_ext(path())
    if (ext == "csv") {
      mydata <- data.table::fread(path(), stringsAsFactors = FALSE)
    } else if (ext == "sas7bdat") {
      mydata <- read_sas(path())
    } else {
      showNotification("File type isn't .csv or .sas7bdat; please try a different file", duration = NULL, type = "error")
    }
    message("file uploaded")
    # check if the data is compliant with the required dataset format
    ## and return an error message/notification if it isn't
    musthave_cols <- c("USUBJID","COMPOUND","TRTNAME","TIMEUNIT","NT","TIME","NAME","VALUE","VALUETXT","UNIT","ROUTE")
    if(!check_columns(df = mydata, col_names = musthave_cols)){
      return(NULL)
    } else {
      # make sure that the error message is removed if the user fixed the error
      removeNotification(id = "err_dataset")
    }
    return(mydata)
  })
  
  ### Choose variables from dataset-----------------------------------------------------------
  output$DoseNames <- renderUI({ # Choose dosing records, attempt to select automatically
    if (is.null(input$file_path) | is.null(mydata())) {
      return(NULL)
    }
    
    opts <- mydata() %>%
      filter(TYPENAME != "Adverse Event") %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE) %>%
      sort()
    
    auto_opts <-
      mydata()$NAME %>%
      unique() %>%
      str_detect(., "(?i)Dose") %>%
      unique(mydata()$NAME)[.]
    
    selectInput("doseIn",
                multiple = TRUE,
                label = "Select dosing records:",
                choices = opts,
                selected = auto_opts
    )
  })
  
  output$ObsNames <- renderUI({ # Choose observation records, ""
    if (is.null(input$file_path) | is.null(mydata())) {
      return(NULL)
    }
    opts <- mydata() %>%
      filter(TYPENAME != "Adverse Event") %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE) %>%
      sort()
    
    auto_opts <-
      mydata() %>% 
      filter(str_detect(TYPENAME, "PK")) %>% 
      select(NAME) %>% 
      unique() %>% 
      unlist(F,F)
    pd_opts <- mydata() %>%
      filter(str_detect(TYPENAME, "PD")) %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE)
    auto_opts <- append(auto_opts, pd_opts)
    
    selectInput("obsIn",
                multiple = TRUE,
                "Select observation records:",
                choices = opts,
                selected = auto_opts
    )
  })
  
  output$ConCovNames <- renderUI({ # Select continuous covariates
    if (is.null(input$file_path) | is.null(mydata())) {
      return(NULL)
    }
    opts <- mydata() %>%
      select(NAME, TYPENAME) %>%
      filter(TYPENAME != "Adverse Event") %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE)
    auto_opts <- mydata() %>%
      filter(str_detect(TYPENAME, "(?i)continuous") | str_detect(NAME, "((?i)^Age|(?i)^Weight|(?i)^BMI|(?i)^Height)")) %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE)
    base_pd <- mydata() %>%
      filter(str_detect(TYPENAME, "PD")) %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE) %>%
      paste0("Baseline ", .)
    auto_opts <- append(auto_opts, base_pd)
    opts <- append(opts, base_pd)
    concovIn <- selectInput("concovIn",
                            multiple = TRUE,
                            "Select continuous covariates:",
                            choices = opts,
                            selected = auto_opts
    )
  })
  
  output$CatCovNames <- renderUI({ # Select categorical covariates
    if (is.null(input$file_path) | is.null(mydata())) {
      return(NULL)
    }
    opts <- mydata() %>%
      select(NAME, TYPENAME) %>%
      filter(TYPENAME != "Adverse Event") %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE)
    auto_opts <- mydata() %>%
      filter(str_detect(TYPENAME, "(?i)categorical") | str_detect(NAME, "((?i)^Sex|(?i)^Race|(?i)^Gender|(?i)^Ethnic|(?i)^Health status$)")) %>%
      filter(str_detect(TYPENAME, "(?i)Time-varying", negate = TRUE)) %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE)
    
    catcovIn <- selectInput("catcovIn",
                            multiple = TRUE,
                            "Select categorical covariates:",
                            choices = opts,
                            selected = opts[opts %in% auto_opts]
    )
  })
  
  output$depConCovNames <- renderUI({ # select time-dependent continuous covariates
    if (is.null(input$file_path) | is.null(mydata())) {
      return(NULL)
    }
    opts <- mydata() %>%
      select(NAME, TYPENAME) %>%
      filter(TYPENAME != "Adverse Event") %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE)
    
    auto_opts <-
      mydata() %>%
      filter(str_detect(TYPENAME, "(?i)Time-varying Continuous")) %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE)
    
    depConCovIn <- selectInput("depconcovIn",
                               multiple = TRUE,
                               "Select time-dependent continuous covariates:",
                               choices = opts,
                               selected = opts[opts %in% auto_opts]
    )
  })
  
  output$depCatCovNames <- renderUI({ # select time-dependent categorical covariates
    if (is.null(input$file_path) | is.null(mydata())) {
      return(NULL)
    }
    opts <- mydata() %>%
      select(NAME, TYPENAME) %>%
      filter(TYPENAME != "Adverse Event") %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE)
    
    auto_opts <-
      mydata() %>%
      filter(str_detect(TYPENAME, "(?i)Time-varying Categorical")) %>%
      select(NAME) %>%
      unique() %>%
      unlist(use.names = FALSE)
    
    depCatCovIn <- selectInput("depcatcovIn",
                               multiple = TRUE,
                               "Select time-dependent categorical covariates:",
                               choices = opts,
                               selected = opts[opts %in% auto_opts]
    )
  })
  
  output$status_flag <- renderUI({ # option to mark plots with "Draft"
    if (is.null(input$file_path) | is.null(mydata())) {
      return(NULL)
    }
    
    checkboxInput(
      inputId = "status_on",
      label = "Mark plots as draft",
      value = TRUE
    )
  })
  
  
  ## function Add status to plot if the option is selected ------------------------------
  mark_draft <- function(plot) {
    plot <- plot +
      annotate("text",
               label = "DRAFT",
               size = 8,
               color = "grey",
               family = "Courier",
               x = Inf, y = Inf,
               hjust = 1.2, vjust = 1.2,
               na.rm = F
      )
    return(plot)
  }
  
  observeEvent(
    {
      input$file_path
    }, # Resets the data when choosing a new dataset
    {
      reset("mydata")
      reset("dataset")
      reset("plot_type")
      reset("obsIn")
      reset("doseIn")
    },
    ignoreInit = TRUE
  ) # Works only after choosing the second dataset onwards
  
  # notify the user that the dataset is loading
  observeEvent(
    {
      mydata()
    },
    once = TRUE,
    {
      if (is.null(input$file_path) | is.null(mydata())) {
        return(NULL)
      }
      showNotification("Loading...",
                       duration = 1,
                       closeButton = FALSE,
                       type = "message"
      )
    }
  )
  # debounce inputs- only start loading 2 seconds after input to allow for changes
  concov <- reactive(input$concovIn)
  concov_d <- concov %>% debounce(2000)
  impute_flag <- reactive(impute_flag)
  impute_flag_d <- impute_flag %>% debounce(1000)
  catcov <- reactive(input$catcovIn)
  catcov_d <- catcov %>% debounce(2000)
  obsin <- reactive(input$obsIn)
  obsin_d <- obsin %>% debounce(2000)
  dosein <- reactive(input$doseIn)
  dosein_d <- dosein %>% debounce(2000)
  depconcov <- reactive(input$depconcovIn)
  depconcov_d <- depconcov %>% debounce(2000)
  depcatcov <- reactive(input$depcatcovIn)
  depcatcov_d <- depcatcov %>% debounce(2000)
  obs_select <- reactive(input$obs_select)
  obs_select_d <- obs_select %>% debounce(2000)
  
  input_list <- reactive(
    list(
      "concov" = input$concovIn,
      "impute_flag" = input$impute_flag,
      "catcov" = input$catcovIn,
      "obsin" = input$obsIn,
      "dosein" = input$doseIn,
      "depconcov" = input$depconcovIn,
      "depcatcov" = input$depactcovIn,
      "clean_flag" = input$clean_flag
    )
  )
  
  input_list_d <- input_list %>% debounce(2000)
  
  
  status <- reactiveVal("")
  
  # to store variables created inside a reactive expression when generating outputs
  output_holdover <- reactiveValues()
  
  rv_dat <- reactiveValues(last_id = 0)
  rv_plot <- reactiveValues(plot   = NULL)
  running <- reactiveVal(FALSE)
  ### Load the dataset into IQRtools, impute and clean if requested------------------------------
  observeEvent(
    {
      input_list_d()
    },
    ignoreInit = TRUE,
    { # Load chosen paramaters into model and clean the data (if the option is selected)
      if (is.null(input$file_path) | is.null(mydata())) {
        return(NULL)
      }
      
      disable("generateReport")
      
      if (is.null(input_list_d()$dosein)) {
        showNotification("Please choose a dose variable.",
                         type = "error"
        )
        
        return(NULL)
      }
      
      if (is.null(input_list_d()$obsin)) {
        showNotification("Please choose an observation variable.",
                         type = "error",
                         duration = 10
        )
        return(NULL)
      }
      
      ext <- file_ext(path())
      if (ext == "sas7bdat") {
        source <- as.data.frame(mydata())
      }
      else {
        source <- path()
      }
      
      status("notyet")
      
      cov_vec <- c(input_list_d()$concov)
      
      if (!is.null(cov_vec)) {
        names_cov_vec <- str_replace_all(cov_vec, "[-| ()]", "_")
        cov_vec <- str_remove(cov_vec, "Baseline ")
        names(cov_vec) <- names_cov_vec
      }
      
      dep_con_cov <- c(input_list_d()$depconcov)
      if (!is.null(dep_con_cov)) {
        names(dep_con_cov) <- str_replace_all(dep_con_cov, "[-| ()]", "_")
      }
      
      dep_cat_cov <- c(input_list_d()$depcatcov)
      if (!is.null(dep_cat_cov)) {
        names(dep_cat_cov) <- str_replace_all(dep_cat_cov, "[-| ()]", "_")
      }
      
      cat_vec <- c(input_list_d()$catcov)
      if (!is.null(cat_vec)) {
        names(cat_vec) <- str_replace_all(cat_vec, "[-| ()]", "_")
      }
      rv_dat$last_id <- rv_dat$last_id + 1
      last_id_dat <- rv_dat$last_id
      dosein <- input_list_d()$dosein
      obsin <- input_list_d()$obsin
      clean_flag <- input_list_d()$clean_flag
      impute_flag <- input_list_d()$impute_flag
      names_of_cov_vec <- names(cov_vec)
      IQRdataGENERAL <- IQRdataGENERAL
      clean_IQRdataGENERAL <- clean_IQRdataGENERAL
      progress <- ipc::AsyncProgress$new(session,
                                         min = 0, max = 1, millis = 250,
                                         value = 0.1,
                                         message = "Processing dataset... \n",
                                         detail = "Setting up..."
      )
      running(TRUE)
      runjs("$('.shiny-notification-close').remove();")
      runjs("$('.progress-bar').addClass('progress-bar-striped progress-bar-animated');")
      fut <- future(
        globals = c("progress","source","dosein", "obsin", "cov_vec", "cat_vec", "dep_con_cov", "dep_cat_cov",
                    "impute_flag", "names_of_cov_vec", "clean_flag", "last_id_dat"),
        packages = c("ipc","magrittr","stringr","ggplot2"),
        {
          progress$inc(1 / 22, detail = "Loading dataset into IQRtools...")
          source("www/IQRcode.R")
          # get rid of any irrelevant functions to improve performance
          remove(list = ls() %>% .[str_detect(.,"(?i)model|(?i)mod[^e]|(?i)nlme|(?i)sys|(?i)dataER|(?i)simres|(?i)project|(?i)coxR")])
          dataIQR <-
            withCallingHandlers(
              IQRdataGENERAL(
                input = source, # load data as IQRtools object
                doseNAMES = dosein,
                obsNAMES = obsin,
                cov0 = cov_vec,
                cat0 = cat_vec,
                covT = dep_con_cov,
                catT = dep_cat_cov
              ),
              message = function(m) {
                progress$inc(1 / 22, detail = glue::glue("{str_replace(str_trim(m$message),'JID,','JID, \n')}..."))
              },
              error = function(e) {
                progress$inc(20/22, detail = paste0(str_sub(e$message,end = 40),"..."), message = "Error! \n")
                Sys.sleep(5)
                progress$inc(1/22, detail = "The QC tab should still work.", message = "Note: \n")
                Sys.sleep(5)
                progress$close()
              }
            )
          impute_rule <-
            if (impute_flag == TRUE) {
              progress$inc(1 / 22, detail = "Imputing covariates...")
              c(names_of_cov_vec)
            }
          else {
            progress$inc(1 / 22, detail = NULL)
            NULL
          }
          orig_names <- colnames(dataIQR)
          colnames(dataIQR) <-
            colnames(dataIQR) %>%
            str_replace_all("[-|/ :()]", "_")
          
          impute_rule <- impute_rule %>%
            str_replace_all("[-|/ :()]", "_")
          
          imputeIQR <- covImpute_IQRdataGENERAL(
            data = dataIQR,
            continuousCovs = impute_rule
          )
          
          if (clean_flag == TRUE) {
            # clean the data
            progress$inc(1 / 22, detail = "Cleaning dataset...")
            cleanIQR <- clean_IQRdataGENERAL(dataIQR,
                                             FLAGrmIGNOREDrecords = TRUE,
                                             methodBLLOQ = "M1",
                                             continuousCovs = impute_rule
            )
            imputeIQR <- cleanIQR
          }
          else {
            progress$inc(1 / 22, detail = "Finalizing...")
          }
          names(imputeIQR) <- orig_names
          
          progress$close()
          list(
            id = last_id_dat,
            imputeIQR = imputeIQR,
            dataIQR = dataIQR,
            impute_rule = impute_rule
          )
        }, seed = T) %...>% (function(result) {
          enable("generateReport")
          cli::cat_rule(
            sprintf("Back from %s", result$id)
          )
          output_holdover[["pre_cleaned_data"]] <- result$dataIQR
          output_holdover[["impute_rule"]] <- result$impute_rule
          rv_dat$imputeIQR <- result$imputeIQR
        }) %...!%
        (function(e) {
          warning(e)
        })
      cli::cat_rule(
        sprintf("%s sent", rv_dat$last_id)
      )
    }
  )
  
  dataset <- reactive({
    req(rv_dat$imputeIQR)
    rv_dat$imputeIQR
  })
  
  observeEvent(
    {
      dataset()
    },
    ({
      enable(id = "plot_type")
      rv_samp$sched <- NULL
      rv_dose$dose <- NULL
      status("ok")
    })
  )
  output$dataset_uploaded <- reactive({
    return(nrow(dataset()) > 0)
  })
  outputOptions(output, "dataset_uploaded", suspendWhenHidden = FALSE)
  
  observeEvent({
    pkpd_data()
  },{
    choices <- pkpd_data()$ID %>% unique()
    if (input$select_subj %in% choices) {
      selected <- input$select_subj
    }
    else {
      selected <- NULL
    }
    updateSelectInput(session = getDefaultReactiveDomain(),
                      inputId = "select_subj",
                      choices = choices, selected = selected)
  })
  ### generate all the typical outputs for QC-----------------------------------------------------
  output$report_button <- renderUI({
    if (is.null(input$file_path)) {
      return(NULL)
    }
    req(dataset())
    tagList(
      tags$button(
        id = "generateReportPrecursor",
        class = "btn btn-default action-button",
        style = "margin-top: 8px;",
        tags$i(class = "far fa-images", style = "margin-right: 3px;"),
        "Generate all graphs and tables"
      ),
      tags$a(
        id = "generateReport",
        class = "btn btn-default shiny-download-link",
        href = "",
        target = "_blank",
        download = NA,
        "Hidden Button"
      ) %>% tagAppendAttributes(style = "visibility: hidden; height: 0; width: 0; padding: 0;")
    )
  })
  
  # Add log creation function to mimic InitIQRcompliance
  source("createShinylog.R")
  
  # When "Generate Report" is clicked, generate plots in chosen directory
  
  rv <- reactiveValues(last_id = 0)
  
  onclick("generateReportPrecursor", function() {
    rv$click <- rnorm(1)
  })
  
  observeEvent(
    {
      rv$click
    },
    {
      rv$last_id <- rv$last_id + 1
      last_id <- rv$last_id
      x <- rv_dat$imputeIQR
      clean_flag <- input_list_d()$clean_flag
      pre_clean <- output_holdover[["pre_cleaned_data"]]
      impute_r <- output_holdover[["impute_rule"]]
      
      if (input$username != "") {
        user <- input$username
      }
      else {
        user <- "App User"
      }
      
      progress <- ipc::AsyncProgress$new(session,
                                         min = 0, max = 1, millis = 250,
                                         value = 0,
                                         message = "Generating outputs... \n", detail = "Setting up..."
      )
      
      # Run js to improve progress bar
      runjs("$('.shiny-notification-close').remove();")
      runjs("$('.progress-bar').addClass('progress-bar-striped progress-bar-animated');")
      
      future({
        progress$inc(1 / 12, detail = "Creating folder...")
        # Create Temp folder to output files into
        folder <- paste("outputs", Sys.Date(), as.character(rdunif(1,100)), sep = "_")
        dir <- tempdir()
        cc <- 1
        while (dir.exists(file.path(dir, folder))) {
          folder <- str_replace(folder,"_.{1,3}$",paste0("_r",cc))
          cc <- cc + 1
        }
        dir.create(file.path(dir, folder))
        # Create the various types of plots and tables
        progress$inc(1 / 12, detail = "Generating spaghetti plots...")
        plotSpaghetti_IQRdataGENERAL(
          data = x,
          filename = paste0(dir, "/", folder, "/figures/FIG01_Spaghetti_Plot")
        )
        createShinylog(paste0(dir, "/", folder, "/figures/FIG01_Spaghetti_Plot"), user)
        
        progress$inc(1 / 12, detail = "Generating individual plots...")
        plotIndiv_IQRdataGENERAL(x,
                                 filename = paste0(dir, "/", folder, "/figures/FIG02_Individual_Plots")
        )
        createShinylog(paste0(dir, "/", folder, "/figures/FIG02_Individual_Plots"), user)
        
        progress$inc(1 / 12, detail = "Generating covariate distribution plots...")
        plotCovDistribution_IQRdataGENERAL(x,
                                           filename = paste0(dir, "/", folder, "/figures/FIG03_Covariate_Distributions")
        )
        createShinylog(paste0(dir, "/", folder, "/figures/FIG03_Covariate_Distributions"), user)
        
        progress$inc(1 / 12, detail = "Generating continuous correlation plots...")
        plotCorCov_IQRdataGENERAL(x,
                                  filename = paste0(dir, "/", folder, "/figures/FIG04_Continuous_Correlations")
        )
        createShinylog(paste0(dir, "/", folder, "/figures/FIG04_Continuous_Correlations"), user)
        
        progress$inc(1 / 12, detail = "Generating categorical correlation plots...")
        plotCorCat_IQRdataGENERAL(x,
                                  filename = paste0(dir, "/", folder, "/figures/FIG05_Categorical_Correlations")
        )
        createShinylog(paste0(dir, "/", folder, "/figures/FIG05_Categorical_Correlations"), user)
        
        progress$inc(1 / 12, detail = "Generating combined correlation plots...")
        plotCorCovCat_IQRdataGENERAL(x,
                                     filename = paste0(dir, "/", folder, "/figures/FIG06_Combined_Covariate_Correlations")
        )
        createShinylog(paste0(dir, "/", folder, "/figures/FIG06_Combined_Covariate_Correlations"), user)
        
        progress$inc(1 / 12, detail = "Generating dosing schedule plots...")
        gd_ob <- plotDoseSchedule_IQRdataGENERAL(x,
                                                 filename = tempfile(),
                                                 FLAGreturnObject = T
        ) %>%  
          lapply(
            function(x){
              x$labels$title %<>% 
                str_replace("page","page ") %>%
                str_replace("\\s\\)","\\)")
              x$layers[[2]]$aes_params$size <- 5
              return(x)
            }
          )
        IQRoutputPDF(gd_ob, filename = paste0(dir, "/", folder, "/figures/FIG07_Dosing_Schedule"), height = 10)
        createShinylog(paste0(dir, "/", folder, "/figures/FIG07_Dosing_Schedule"), user)
        
        progress$inc(1 / 12, detail = "Generating summary range plots...")
        plotRange_IQRdataGENERAL(x,
                                 filename = paste0(dir, "/", folder, "/figures/FIG08_Range_Plots")
        )
        createShinylog(paste0(dir, "/", folder, "/figures/FIG08_Range_Plots"), user)
        
        progress$inc(1 / 12, detail = "Generating sampling schedule plots...")
        gd_obj <- plotSampleSchedule_IQRdataGENERAL(x,
                                                    NperPage = 5,
                                                    filename = tempfile(),
                                                    FLAGreturnObject = T
        ) %>%  
          lapply(
            function(x){
              x$labels$title %<>%
                str_replace("\\s\\)","\\)")
              x$layers[[1]]$aes_params$size <- 3
              return(x)
            }
          )
        IQRoutputPDF(gd_obj, filename = paste0(dir, "/", folder, "/figures/FIG09_Sampling_Schedule"), height = 10)
        createShinylog(paste0(dir, "/", folder, "/figures/FIG09_Sampling_Schedule"), user)
        
        progress$inc(1 / 12, detail = "Generating summary tables...")
        
        list(
          id = last_id,
          dir = dir,
          user = user,
          folder = folder
        )
      }, seed = T) %...>%
        (function(result) {
          cli::cat_rule(
            sprintf("Back from %s", result$id)
          )
          if (result$id == rv$last_id) {
            message(file.path(result$dir, result$folder))
            # Send .zip archive for download
            rv$dir <- result$dir
            rv$folder <- result$folder
            summaryCov_IQRdataGENERAL(rv_dat$imputeIQR,
                                      FLAGtotal = TRUE,
                                      filename = paste0(rv$dir, "/", rv$folder, "/summary_tables/TAB01_Continuous_Covariate_Summary.txt")
            )
            createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/TAB01_Continuous_Covariate_Summary"), result$user)
            
            summaryCat_IQRdataGENERAL(rv_dat$imputeIQR,
                                      FLAGtotal = TRUE,
                                      filename = paste0(rv$dir, "/", rv$folder, "/summary_tables/TAB02_Categorical_Covariate_Summary.txt")
            )
            createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/TAB02_Categorical_Covariate_Summary"), result$user)
            
            summaryObservations_IQRdataGENERAL(rv_dat$imputeIQR,
                                               filename = paste0(rv$dir, "/", rv$folder, "/summary_tables/TAB03_Observations_Summary.txt")
            )
            createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/TAB03_Observations_Summary"), result$user)
            # Compress all files into zip for download
            progress$inc(1 / 12, detail = "Zipping files...")
            if (clean_flag) {
              # clean the data
              clean_IQRdataGENERAL(pre_clean,
                                   FLAGrmIGNOREDrecords = TRUE,
                                   methodBLLOQ = "M1",
                                   continuousCovs = impute_r,
                                   pathname = paste0(rv$dir, "/", rv$folder, "/summary_tables")
              )
              
              createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/01_Manually_Selected_Records"), result$user)
              createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/02_Missing_TIME_Observation_Records"), result$user)
              createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/03_Missing_DV_Observation_Records"), result$user)
              createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/04_Manually_Selected_Subjects"), result$user)
              createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/05_Non_Dose_Observation_Records"), result$user)
              createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/07_No_Observations_Subjects"), result$user)
              createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/08_Zero_Amount_Dose_Records"), result$user)
              createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/09_IGNORED_records"), result$user)
              createShinylog(paste0(rv$dir, "/", rv$folder, "/summary_tables/10_Covariate_Imputations"), result$user)
            }
            # Erase Temp folder
            progress$close()
            
            shinyjs::runjs("document.getElementById('generateReport').click();")
          }
        }) %...!%
        (function(e) {
          warning(e)
        })
      cli::cat_rule(
        sprintf("%s sent", rv$last_id)
      )
    }
  )
  
  output$generateReport <-
    downloadHandler( # Download the outputs as a .zip file
      filename = function() {
        paste0("outputs", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(rv$dir)
        req(rv$folder)
        target <- zip::zip(file, list.files(), root = file.path(rv$dir, rv$folder))
        unlink(file.path(rv$dir, rv$folder))
        return(target)
      }
    )
  ### QC dataset and compare with original data specifications -----------------------------------------------
  spec_path <- reactive({
    input$spec_file$datapath
  })
  
  output$switch_to_value <- renderUI({
    req(spec_path())
    checkboxInput(
      inputId = "event_table_switch",
      label = "Switch to event table",
      value = FALSE,
      width = "195px"
    )
  })
  
  output$opt_filter_flag <- renderUI({
    req(spec_path())
    req(input$event_table_switch != TRUE)
    checkboxInput(
      inputId = "opt_flag",
      label = "Filter optional variables",
      value = FALSE,
      width = "195px"
    )
  })
  
  output$pmxian_filter_flag <- renderUI({
    req(spec_path())
    req(input$event_table_switch != TRUE)
    checkboxInput(
      inputId = "pmxian_flag",
      label = "Filter variables requiring input from the pharmacometrician",
      value = FALSE,
      width = "195px"
    )
  })
  
  # read the information from the specification file -------------------------------------------
  specs <- reactive({
    req(spec_path())
    
    removeNotification(id = "not_enough_tables")
    removeNotification(id = "not_any_tables")
    removeNotification(id = "err_specification_file")
    removeNotification(id = "err_event_table")
    
    if (is.null(input$opt_flag) | is.null(input$pmxian_flag)){
      return(NULL)
    }
    
    if (file_ext(spec_path()) %>% str_detect("docx")) {
      specs <- read_docx(spec_path(), track_changes = "accept")
      tables <- docx_extract_all_tbls(specs)
      if (length(tables) == 0)
      {
        showNotification("No tables detected in specification file", 
                         type = "error",
                         id = "not_any_tables",
                         duration = NULL)
        return(NULL)
      }
      names(tables[[1]]) <- names(tables[[1]]) %>% 
        str_replace_all("\\.{3}"," / ") %>% 
        str_replace_all("\\."," ")
    }
    else if (file_ext(spec_path()) %>% str_detect("xlsx")) {
      tables <- tryCatch(lapply(1:2, function (x) readxl::read_excel(spec_path(), sheet = x) %>% drop_na(any_of("NAME"))),
                         error = function(e) {
                           err_msg <- e$message
                           if (err_msg == "Can't retrieve sheet in position 2, only 1 sheet(s) found."){
                             err_msg %<>% paste0(.,"\nMake sure you have two tables, general and event, each in seperate sheets.") 
                           }
                           showNotification(paste0("Error in loading the table:\n", err_msg),
                                            type = "error",
                                            id   = "not_any_tables",
                                            duration = NULL)
                         }
      )
    }
    
    if (length(tables) != 2){
      showNotification("Your specification files must contain 2 tables: general and event.", 
                       type = "error",
                       id = "not_enough_tables",
                       duration = NULL)
      return(NULL)
    } else {
      removeNotification(id = "not_enough_tables")
    }
    
    # check if all columns are present in the spec file
    req_col_tables <- c("Name","Type","Label", "Required / Optional","PMXian input","Comments")
    req_val_tables <- c("NAME","VALUE","VALUETEXT","UNIT","TYPENAME","LLOQ")
    
    column_specs <- tables[[1]]
    # ensure uniform column names, even if they're spelled differently than expected
    names(column_specs)[str_detect(names(column_specs),"^(?i)NAME$|^(?i)TYPE$|^(?i)LABEL$")] %<>% str_to_title()
    names(column_specs)[str_detect(names(column_specs),"^(?i)Req|(?i)Req.*/.*Opt")] <- "Required / Optional"
    names(column_specs)[str_detect(names(column_specs),"(?i)Pharmaco.*input")] <- "PMXian input"
    names(column_specs)[str_detect(names(column_specs),"(?i)Comments")] <- "Comments"
    value_specs <- tables[[2]]
    
    
    if (!check_columns(column_specs, req_col_tables, "specification file") | !check_columns(value_specs, req_val_tables, "event table")){
      return(NULL)
    }
    else {
      removeNotification(id = "err_specification_file")
      removeNotification(id = "err_event_table")
    }
    
    if (input$opt_flag & input$pmxian_flag) {
      column_specs <- column_specs %>% 
        filter(if_any(contains("Req"), ~ str_detect(.,"(?i)Req")) & if_any(contains("PMXian.input"), ~ . != "No"))
    }
    else if (input$opt_flag == TRUE) {
      column_specs <- column_specs %>% filter(if_any(contains("Req"), ~ str_detect(.,"(?i)Req")))
    }
    else if (input$pmxian_flag == TRUE) {
      column_specs <- column_specs %>% filter(if_any(contains("PMXian.input"), ~ . != "No"))
    }
    
    if (input$event_table_switch == TRUE) {
      return(value_specs)
    }
    
    
    colnames(column_specs)[5] <- str_to_title(colnames(column_specs)[5])
    
    
    return(column_specs)
  })
  
  output$column_choose <- renderUI({
    req(!is.null(specs()))
    choice_list <- specs() %>% select(matches("^(?i)Name$")) %>% unlist(F,F)
    selectInput(
      inputId = "col_choose",
      label = "Choose column(s) to examine:",
      choices = choice_list,
      multiple = TRUE
    )
  })
  
  col_choose <- reactive({input$col_choose})
  col_choose_d <- col_choose %>% debounce(800)
  
  #clear the column selector when switching tables
  observeEvent(
    {
      input$event_table_switch
    },
    {
      updateSelectInput(session = getDefaultReactiveDomain(), inputId = "col_choose", selected = "")
    }
  )
  
  observeEvent({col_choose_d()},
               {
                 runjs(
                   HTML(
                     glue::glue('// change the color of items that are not present in the dataset to red and in italics
                        let unpresent_cols = "{{
                        col_choose_d()[!col_choose_d() %in% c(names(mydata()), unique(mydata()$NAME))] %>%
                        lapply(function(x) paste("^",x,"$", sep = "")) %>%  
                        paste(collapse = "|")
                        }}"
                        if (unpresent_cols)
                        {
                          let items = $("#col_choose +").children().children(`div.item:regex(${unpresent_cols})`);
                          items.css("color", "red").css("font-style","italic");
                        }
                        ', 
                                .open = "{{",
                                .close = "}}"
                     ),
                   )
                 )
               })
  
  #### align columns with each other ----------------------------------------
  output$align_columns <- renderUI({
    req(spec_path())
    req(length(input$data_cols_rows_selected) > 1)
    req(input$event_table_switch == FALSE)
    tags$div(
      class = "button-container", style = "padding-bottom:10px;",
      actionButton(
        inputId = "col_align",
        label = "Compare and align variables",
        width = "100%"
      ) %>% 
        bs_embed_tooltip(title = "Check selected variables to see if they corrsepond as expected (e.g., NAME and UNIT)",
                         placement = "right", 
                         trigger = "focus")
    )
  })
  
  observeEvent(
    {
      input$col_align
    },
    {
      req(!is.null(specs()))
      runjs('$("#col_align").tooltip("hide")')
      selected_rows <- input$data_cols_rows_selected
      showModal(
        modalDialog(
          title = "Compare variables",
          size = "m",
          easyClose = T,
          renderDataTable(
            mydata() %>%
              select(data_cols_table()$Variable[selected_rows]) %>%
              unique.array(),
            options = list(
              scrollX = FALSE,
              deferRender = TRUE,
              scroller = TRUE,
              dom = "tp",
              buttons = "excel",
              columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            rownames = FALSE
          )
        )
      )
    }
  )
  
  output$add_selected <- renderUI({
    req(!is.null(specs()))
    req(length(input$data_cols_rows_selected) > 0)
    tags$div(
      class = "button-container", style = "padding-bottom:10px;",
      actionButton(
        inputId = "add_column_report",
        label = "Add selected variables to QC report",
        width = "100%",
        style = "border-radius:50px;"
      )
    )
  })
  
  output$review_nas_button <- renderUI({
    req(!is.null(specs()))
    req(length(input$data_cols_rows_selected) > 0)
    actionButton(
      inputId = "review_nas",
      label = "Review NA rows",
      width = "100%",
      style = "margin-bottom: 10px;"
    ) %>% 
      bs_embed_tooltip(title = "Check selected variables to see in which rows they have NA values",
                       placement = "right", 
                       trigger = "focus")
  })
  
  observeEvent(
    {
      input$review_nas
    },
    {
      req(!is.null(specs()))
      selected_rows <- input$data_cols_rows_selected
      runjs('$("#review_nas").tooltip("hide")')
      # compensate for selected columns that aren't in the dataset (make the index correct)
      col_vars <- data_cols_table()$Variable[selected_rows]
      if (input$event_table_switch) {
        show_rows <-
          mydata() %>%
          filter(NAME %in% col_vars & (is.na(VALUE) | is.na(VALUETXT)))
      }
      else {
        show_rows <-
          mydata() %>%
          filter(across(col_vars,is.na))
      }
      
      showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          renderDataTable(show_rows, 
                          extensions = "KeyTable",
                          options = list(
                            scrollX = TRUE,
                            deferRender = TRUE,
                            scroller = TRUE,
                            keys = TRUE,
                            dom = "tp",
                            buttons = "excel",
                            columnDefs = list(list(className = "dt-center", targets = "_all"))
                          ),
                          rownames = FALSE
          )
        )
      )
    }
  )
  
  report_columns <- reactiveValues(
    reporto = c("Variable", "Comments", "Status") %>% purrr::map_dfc(setNames, object = list(character())),
    reporta = c("Subject", "Observation", "Treatment Arm", "Comments") %>% purrr::map_dfc(setNames, object = list(character()))
  )
  
  observeEvent(
    {
      input$add_column_report
    },
    {
      req(!is.null(specs()))
      closeAlert(session, "alert_report")
      
      selected_rows <- input$data_cols_rows_selected
      col_vars <- data_cols_table()$Variable[selected_rows]
      
      if (anyDuplicated(c(report_columns$reporto$Variable, col_vars)) == 0) {
        report_columns$reporto <<- add_row(report_columns$reporto,
                                           Variable = col_vars,
                                           Comments = "",
                                           Status = ""
        )
      }
      else {
        report_columns$reporto <<- add_row(report_columns$reporto,
                                           Variable = col_vars[!col_vars %in% report_columns$reporto$Variable],
                                           Comments = "",
                                           Status = ""
        )
      }
      
      createAlert(session,
                  "report_alert", "alert_report",
                  title = "QC review variables:",
                  content = report_columns$reporto$Variable %>%
                    str_c(collapse = "; \n")
      )
    }
  )
  
  
  output$cur_title <- renderText({
    if (is.null(report_columns$reporto)) {
      return("")
    }
    else {
      return("Reported QC variables:")
    }
  })
  output$current_report <-
    renderDataTable(
      report_columns$reporto,
      options = list(
        scrollX = FALSE,
        deferRender = TRUE,
        scroller = TRUE,
        dom = "tp",
        buttons = "excel",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      editable = "cell"
    )
  
  # Show reported columns --------------------------------------------------------
  observeEvent(
    {
      input$review_report
    },
    {
      toggleModal(session, modalId = "report_modal")
    }
  )
  
  observeEvent(
    {
      input$current_report_cell_edit
    },
    {
      # Compensate for col. number error
      insert <- input$current_report_cell_edit
      insert$col <- insert$col + 1
      report_columns$reporto <<-
        editData(
          report_columns$reporto,
          insert,
          "current_report"
        )
      report_columns[["comments"]] <<- report_columns$reporto[["Comments"]]
    }
  )
  
  output$report_dl_bt <- renderUI({
    req(input$anoms_flag == TRUE || input$specs_flag == TRUE || input$anomggs_flag == TRUE)
    conditionalPanel(
      condition = "input.username.trim().length",
      tags$a(
        id = "submit_report",
        class = "btn btn-default shiny-download-link",
        href = "",
        target = "_blank",
        download = NA,
        tags$i(class = "fa fa-file-download", style = "margin-right: 10px;"),
        "Save report"
      )
    )
  })
  
  output$check_specs <- renderUI({
    req(length(report_columns$reporto) > 0)
    checkboxInput(
      inputId = "specs_flag",
      label = "Include specs table in report",
      value = TRUE
    )
  })
  
  output$check_anoms <- renderUI({
    req(length(report_columns$reporta) > 0)
    checkboxInput(
      inputId = "anoms_flag",
      label = "Include flagged subject table in report",
      value = TRUE
    )
  })
  
  output$check_anomggs <- renderUI({
    req(length(report_columns$reporta) > 0)
    checkboxInput(
      inputId = "anomggs_flag",
      label = "Include flagged subject plots in report",
      value = TRUE
    )
  })
  # show the data from the dataset and provide relevant info (duplicated values, NAs, etc.)-------------------------
  data_cols_table <- reactive({
    req(specs())
    req(col_choose_d())
    if (class(try(mydata(), silent = T))[1] == "try-error"){
      showNotification("No dataset loaded yet - showing only specs", duration = NULL, id = "missing_data_err", type = "error")
      req(mydata())
    } else {
      removeNotification(id = "missing_data_err")
    }
    
    if (!is.null(spec_path()) & !is.null(col_choose_d())) {
      if (input$event_table_switch) { # for variables derived from the NAME-VALUE-VALUETXT columns
        table <- c()
        for (i in req(col_choose_d())) {
          i_type <- mydata() %>%
            filter(NAME %in% i) %>%
            select(TYPENAME) %>%
            unique() %>%
            unlist(F, F)
          if (is_empty(i_type)){
            next
          }
          if (str_detect(i_type, "(?i)categorical|(?i)dose") |
              mydata() %>% filter(NAME == i) %>% filter(!is.na(VALUETXT) && !is.null(VALUETXT) && VALUETXT != "" && VALUETXT != ".") %>% dim(.) %>% is_greater_than(0) %>% all() 
          ) {
            if (str_detect(i_type, "(?i)dose")) {
              param <- "VALUE"
              sepr <- ""
            }
            else {
              param <- "VALUETXT"
              sepr <- "|"
            }
            current <-
              mydata() %>%
              filter(NAME == i) %>%
              select(c(param, UNIT)) %>%
              summarize(
                Variable = i,
                `Number of NAs` = sum(is.na(param)),
                `Number of Blanks` = sum(param == ""),
                Unit = ifelse(
                  str_detect(i_type, "(?i)dose"),
                  unique(UNIT),
                  ""
                )
              ) %>%
              mutate(
                `Values` =
                  paste(
                    mydata() %>%
                      filter(NAME == i) %>%
                      select(VALUETXT) %>%
                      unique() %>%
                      as.character(),
                    sepr,
                    mydata() %>%
                      filter(NAME == i) %>%
                      select(VALUE) %>%
                      unique() %>%
                      as.character()
                  )
              )
          }
          else {
            current <-
              mydata() %>%
              filter(NAME %in% i) %>%
              select(VALUE, UNIT) %>%
              summarize(
                Variable = i,
                `Values` = paste0(min(VALUE, na.rm = T), "-", max(VALUE, na.rm = T)),
                `Number of NAs` = sum(is.na(VALUE)),
                `Number of Blanks` = sum(VALUE == ""),
                Unit = unique(UNIT)
              ) %>%
              mutate(`Values` = `Values` %>% as.character())
          }
          current <- current %>% select(Variable, Values, Unit, `Number of NAs`, `Number of Blanks`)
          table <- rbind(table, current)
        }
        table
      }
      else {
        #get cols from dataset
        cols_tab <- col_choose_d()[col_choose_d() %in% names(mydata())]
        if (!is_empty(cols_tab)) {
          try(
            mydata() %>%
              select(any_of(cols_tab)) %>%
              summarize(
                `Variable` = cols_tab,
                `Unique Values` =
                  lapply(cols_tab, select,
                         .data = mydata() %>% as.data.frame()
                  ) %>%
                  lapply(unique) %>%
                  unlist(recursive = FALSE),
                `Number of Unique Values` = sapply(`Unique Values`, length),
                Type = sapply(`Unique Values`, class) %>% str_to_sentence(),
                `Number of NAs` = sapply(X = cols_tab, FUN = function(x) sum(is.na(mydata()[[x]]))),
                `Number of Blanks` = sapply(X = cols_tab, FUN = function(x) sum(mydata()[[x]] == ""))
              ) %>%
              mutate(`Unique Values` = `Unique Values` %>% as.character()) %>%
              mutate(
                `Unique Values` =
                  ifelse(
                    # if the vector is longer than 100 and contains commas
                    sapply(`Unique Values`, function(x) str_length(x) >= 100 && !is.na(str_locate(x, ","))),
                    # find the comma that's nearest to 100 and shorten the vector to that length
                    str_sub(`Unique Values`, 1, str_locate_all(`Unique Values`, ",") %>% lapply(function(y) y[y <= 100] %>% max())) %>%
                      # then add an ellipsis (...) to it for good measure
                      paste(., "...)"),
                    `Unique Values`
                  )
              ),
            silent = TRUE
          ) %>% suppressWarnings()}
      }
    }
  })
  
  # Show chosen columns from dataset-------------------------------------------------
  output$data_cols <- renderDataTable(
    req(data_cols_table()),
    options = list(
      scrollX = FALSE,
      deferRender = TRUE,
      scroller = TRUE,
      dom = "Bfrtp",
      buttons = "excel",
      paging = T,
      pageLength = min(length(col_choose_d()), nrow(data_cols_table())),
      #scrollY = 10 + (50 * min(length(col_choose_d()), nrow(data_cols_table()))),
      rowHeight = 30,
      # function to mark duplicated rows in red (e.g., if they have different units for the same variable)
      ## and mark rows with multiple NA/Blank values in yellow
      createdRow = JS(
        glue::glue('function(row, data, index) {
                        let conditions = [{{
                        data_cols_table() %>%
                        group_by(Variable) %>%
                        summarise(n=n()) %>%
                        filter(n>1) %>%
                        select("Variable") %>%
                        unlist(F,F) %>%
                        lapply(function (x) paste0("\'",x,"\'")) %>%
                        paste(collapse = ",") 
                        }}]
                        
                        let na_num_col = {{names(data_cols_table()) %>% str_which("Number of NAs")}} - 1
                        let blank_num_col = {{names(data_cols_table()) %>% str_which("Number of Blanks")}} - 1 
                        
                        if (conditions.some(el => data[0].includes(el))) {
                          $(row).css("background-color", "#ee232373");
                        }
                        else if ((data[na_num_col] > 0 & data[na_num_col] < 100) | (data[blank_num_col] > 0 & data[blank_num_col] < 100)) {
                          $(row).css("background-color", "#ddc51854")
                        }
                      }',
                   .open = "{{",
                   .close = "}}"
        )
      )
    ),
    rownames = FALSE
  )
  
  # Show columns from specification file
  output$spec_cols <- renderDataTable(
    if (!is.null(input$event_table_switch) & isTruthy(col_choose_d()) & !is.null(specs())) {
      specs() %>% filter(if_any(contains("Name"), ~ . %in% col_choose_d()))
    }
    else {
      NULL
    },
    options = list(
      scrollX = TRUE,
      deferRender = TRUE,
      scroller = TRUE,
      keys = TRUE,
      dom = "Brtp",
      buttons = "excel",
      scrollY = 300,
      rowHeight = 30
    ),
    extensions = "KeyTable",
    rownames = FALSE
  )
  
  sumobv <- reactive({ # Prepare obseration summary table
    if (!is.null(dataset())) {
      summaryObservations_IQRdataGENERAL(dataset())$xtable
    }
  })
  
  duplv <- reactive({ # Prepare duplicate time table
    if (!is.null(dataset())) {
      mydata() %>%
        filter(TYPENAME != "Adverse Event") %>%
        filter(duplicated(paste(USUBJID,TIME,NAME))) %>% 
        dplyr::select(USUBJID, TIME, VALUE)
    }
  })
  ### Change the tooltip text to fit the different plot types -------
  observeEvent({
    input$plot_type
  },
  {
    req(!is.null(mydata()))
    # select the appropriate tip to display according to the plot type
    tooltip_title <- 
      switch(input$plot_type,
             "Check this tooltip for info on each plot.",
             "Spaghetti" = "Things to look for:\\nIndividuals that seem to clearly follow a different trajectory than others in the same group.\\nUse the \\'focus on individual profiles\\' option to find and add them to the QC report",
             "Median range over time" = "Things to look for:\\nMissing values, note points in the data where variability is low",
             "Individual plots" = "Things to look for:\\nMissing or abnormal values/outliers",
             "Covariate distribution" = "Things to look for:\\nWhether the distribution is normal for each covariate, and the values of the central tendencies",
             "Covariate correlation" = "Things to look for:\\nWhether related values are indeed correlated (e.g., Weight and BMI) and note any other significant correlations",
             "Nominal vs actual time" = "Things to look for:\\nEnsure that actual and nominal time don\\'t differ too much from each other\\n(i.e., high correlation, majority of differences centered around 0)",
             "Dosing schedule" = "Things to look for:\\nEnsure that individuals were dosed correctly per their assigned treatment",
             "Sampling schedule" = "Things to look for:\\nWhether subjects were sampled as detailed in the study protocol(s)"
      )
    # update the tooltip
    runjs(glue("$('#iconic_par').attr('title','{tooltip_title}').tooltip('fixTitle')"))
  })
  ### Show sampling schedule------------------------------------------
  rv_samp <- reactiveValues(last_id = 0)
  observeEvent(
    {
      dataset()
      input$plot_type == "Sampling schedule"
    },
    {
      req(!is.null(dataset()))
      if (is.null(rv_samp$sched) && input$plot_type == "Sampling schedule") {
        req(dataset())
        rv_samp$last_id <- rv_samp$last_id + 1
        last_id <- rv_samp$last_id
        data <- dataset()
        progress <- ipc::AsyncProgress$new(session,
                                           min = 0, max = 1, millis = 250,
                                           value = 0.5,
                                           message = "Generating sampling schedule... \n",
                                           detail = "Setting up..."
        )
        runjs("$('.shiny-notification-close').remove();")
        runjs("$('.progress-bar').addClass('progress-bar-striped progress-bar-animated');")
        fut <- future({
          progress$inc(1 / 4, detail = "Creating plot...")
          sched <- plotSampleSchedule_IQRdataGENERAL(data,
                                                     NperPage = 5,
                                                     FLAGreturnObject = TRUE,
                                                     filename = NULL) %>%  
            lapply(
              function(x){
                x$labels$title %<>%
                  str_replace("\\s\\)","\\)")
                x$layers[[1]]$aes_params$size <- 3
                return(x)
              }
            )
          progress$inc(1 / 4, detail = "Finishing up...")
          progress$close()
          list(id = last_id, sched = sched)
        }, seed = T) %...>% ({
          function(result) {
            cli::cat_rule(
              sprintf("Back from %s", result$id)
            )
            rv_samp$sched <- result$sched
          }
        }) %...!%
          (function(e) {
            warning(e)
          })
        cli::cat_rule(
          sprintf("%s sent", rv_samp$last_id)
        )
      }
    }
  )
  
  plot_obj <- reactive({
    req(dataset())
    rv_samp$sched
  })
  
  ### Show dosing schedule----------------------------------------------------
  rv_dose <- reactiveValues(last_id = 0)
  observeEvent(
    {
      dataset()
      input$plot_type == "Dosing schedule"
    },
    {
      req(!is.null(dataset()))
      if (is.null(rv_dose$dose) && input$plot_type == "Dosing schedule") {
        rv_dose$last_id <- rv_dose$last_id + 1
        last_id <- rv_dose$last_id
        data <- dataset()
        progress <- ipc::AsyncProgress$new(session,
                                           min = 0, max = 1, millis = 250,
                                           value = 0.5,
                                           message = "Generating Dosing Schedule... \n",
                                           detail = "Setting up..."
        )
        runjs("$('.shiny-notification-close').remove();")
        runjs("$('.progress-bar').addClass('progress-bar-striped progress-bar-animated');")
        fut <- future({
          progress$inc(1 / 4, detail = "Creating plot...")
          dose <- plotDoseSchedule_IQRdataGENERAL(data,
                                                  NperPage = 5,
                                                  FLAGreturnObject = TRUE,
                                                  filename = NULL) %>%
            lapply(
              function(x){
                x$labels$title %<>% 
                  str_replace("page","page ") %>%
                  str_replace("\\s\\)","\\)")
                x$layers[[2]]$aes_params$size <- 5
                return(x)
              }
            )
          progress$inc(1 / 4, detail = "Finishing up...")
          progress$close()
          list(id = last_id, dose = dose)
        }, seed = T) %...>% ({
          function(result) {
            cli::cat_rule(
              sprintf("Back from %s", result$id)
            )
            rv_dose$dose <- result$dose
          }
        }) %...!%
          (function(e) {
            warning(e)
          })
        cli::cat_rule(
          sprintf("%s sent", rv_dose$last_id)
        )
      }
    }
  )
  
  dose_sched_obj <- reactive({
    # Prepare dosing schedule plots
    req(dataset())
    rv_dose$dose
  })
  
  ### Show covariate distribution plots -----------------------------------------------------------------------
  dist_obj <- eventReactive(
    {
      dataset()
    },
    { 
      req(!is.null(dataset()))
      # Prepare covariate distribution according to type
      covnames <- append(covInfo(dataset())$COLNAME, catInfo(dataset())$COLNAME)
      covnames <- covnames[!str_detect(covnames, "Baseline_")]
      covnames <- str_replace_all(covnames, "[-| ()]", "_")
      
      
      pdf(tempfile())
      gg <- plotCovDistribution_IQRdataGENERAL(dataset(),
                                               covNames = covnames,
                                               FLAGreturnObject = TRUE
      )
      dev.off()
      # Sort Categorical variables by considering numeric + alphabetical order
      if (!is.null(gg[[1]])) {
        gg[[1]] <-
          gg[[1]] +
          facet_wrap(~COVlabel, as.table = TRUE, ncol = 3, scales = "free") +
          theme(axis.text = element_text(size = 8))
      }
      
      if (!is.null(gg[[2]])) {
        gg[[2]] <- gg[[2]] +
          scale_x_discrete(labels = function(string) str_sort(string, numeric = TRUE)) +
          facet_wrap(~COVlabel, as.table = TRUE, ncol = 1, scales = "free") +
          theme(axis.text = element_text(size = 15))
      }
      
      return(gg)
    }
  )
  ### Show analysis dataset in a table---------------------------------------------------------------------
  observeEvent(
    {
      dataset()
    },
    ({
      req(!is.null(dataset()))
      output$clean_contents <- renderDataTable( # Render a table showing the cleaned data
        dataset(), # A.k.a the Analysis Dataset
        options = list(
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          dom = "Bfrtip",
          buttons = "excel",
          autoWidth = TRUE,
          width = "100%",
          pageLength = 5,
          scrollY = 325
        ),
        rownames = FALSE
      ) # end of output$clean_contents
      # Render a table showing duplicate times -------------------------------------------------------
      output$duplicated_check <- renderDataTable(
        duplv(),
        options = list(
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          dom = "Btp",
          buttons = "excel",
          scrollY = 338,
          pageLength = 3000
        ),
        rownames = FALSE
      ) # end of output$duplicated_check
      # show summary of categorical covs ---------------------------------------------------------------
      output$cat_summary <- renderDataTable(
        summaryCat_IQRdataGENERAL(dataset())$xtable,
        options = list(
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          dom = "t",
          buttons = "excel",
          width = "100%",
          pageLength = 20,
          ordering = FALSE
        ),
        rownames = FALSE
      )
      # show a summary of continuous covs -------------------------------------------------------------
      sumcov <- reactive({
        req(!is.null(dataset()))
        xtable <- summaryCov_IQRdataGENERAL(dataset())$xtable
        unique_lengths <-
          sapply(
            xtable$Characteristic,
            function(x) {
              y <- x == xtable$Characteristic
              as.numeric(y) %>% sum()
            }
          )
        repeated_covs <- xtable$Characteristic[unique_lengths > 1] %>% unique()
        xtable$Characteristic[xtable$Characteristic == repeated_covs][1:(length(repeated_covs))] %<>%
          paste("Baseline", .)
        return(xtable)
      })
      
      output$cov_summary <- renderDataTable(
        sumcov(),
        options = list(
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          dom = "t",
          buttons = "excel",
          width = "100%",
          pageLength = 20,
          ordering = FALSE
        ),
        rownames = FALSE
      )
      ### observation summary table ---------------------------------------------------
      output$contents <- renderDataTable( # Render a table showing observation summary
        sumobv(),
        options = list(
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          dom = "Brt",
          buttons = "excel",
          width = "100%"
        ),
        rownames = FALSE
      ) # end of output$contents
    })
  ) # end of observeEvent
  
  ### selection inputs for general plots-----------------------------------------------
  output$PagechoiceDist <- renderUI({ # Add a covariate type selection input
    req(input$file_path)
    selectInput(
      inputId = "page_choiceDist",
      label = "Variable type:",
      choices = c(
        "Choose variables to display" = "",
        "Continuous" = "continuous",
        "Categorical" = "categorical"
      ),
      width = "25%"
    )
  }) # end of PagechoiceDist
  outputOptions(output, "PagechoiceDist", suspendWhenHidden = FALSE)
  
  output$CorchoiceDist <- renderUI({ # Add a covariate type selection input
    req(input$plot_type == "Covariate correlation")
    selectInput(
      inputId = "cor_choiceDist",
      label = "Variable type:",
      choices = c(
        "Choose variables to display" = "",
        "Continuous covariate correlation" = "continuous",
        "Categorical covariate correlation" = "categorical",
        "Combined covariate correaltion" = "both"
      ),
      width = "30%"
    )
  }) # end of CorchoiceDist
  outputOptions(output, "CorchoiceDist", suspendWhenHidden = FALSE)
  
  output$spag_strat <- renderUI({ # choose variable to stratify by (must be time-independent)
    correct_opts <- input_list()[c("concov", "catcov")] %>%
      unlist(use.names = FALSE) %>%
      .[!str_detect(., "Baseline")]
    correct_opts <- str_replace_all(correct_opts, "[-| ()]", "_") %>% as.list()
    names(correct_opts) <- correct_opts %>% str_replace_all("_", " ")
    
    tipify(
      selectizeInput(
        inputId = "strat_choice",
        label = "Stratify by:",
        choices = correct_opts,
        width = "30%",
        multiple = TRUE
      ), "Continuous variables will be split along the median.",
      options = list(
        placement = "right",
        selector = "label.control-label"
      )
    )
  }) # end of output$spag_strat
  outputOptions(output, "spag_strat", suspendWhenHidden = FALSE)
  
  output$range_strat <- renderUI({ # choose variable to stratify by (must be time-independent)
    correct_opts <- input_list()[c("concov", "catcov")] %>%
      unlist(use.names = FALSE) %>%
      .[!str_detect(., "Baseline")]
    correct_opts <- str_replace_all(correct_opts, "[-| ()]", "_") %>% as.list()
    names(correct_opts) <- correct_opts %>% str_replace_all("_", " ")
    
    tipify(
      selectizeInput(
        inputId = "range_strat_choice",
        label = "Stratify by:",
        choices = correct_opts,
        width = "30%",
        multiple = TRUE
      ), "Continuous variables will be split along the median.",
      options = list(
        placement = "right",
        selector = "label.control-label"
      )
    )
  }) # end of output$range_strat
  outputOptions(output, "range_strat", suspendWhenHidden = FALSE)
  
  output$PagenumDose <- renderUI({ # Add a page function to view sampling
    req(dose_sched_obj())
    if (!is.null(dose_sched_obj())) {
      max_page <- length(dose_sched_obj())
      page_input <- numericInputIcon(
        inputId = "page_numDose",
        label = "Page number",
        value = 1,
        min = 1,
        max = max_page,
        width = "30%",
        size = "sm",
        help_text = tipify(
          span(id = "errortext", "Out of Range!"),
          title = paste0("Page number must be between 1 and ", max_page),
          placement = "right"
        )
      )
    }
  }) # end of PagenumSamp
  outputOptions(output, "PagenumDose", suspendWhenHidden = FALSE)
  
  
  output$PagenumSamp <- renderUI({ # Add a page function to view sampling
    req(plot_obj())
    if (!is.null(plot_obj())) {
      max_page <- length(plot_obj())
      page_input <- numericInputIcon(
        inputId = "page_numSamp",
        label = "Page number",
        value = 1,
        min = 1,
        max = max_page,
        width = "30%",
        size = "sm",
        help_text = tipify(
          span(id = "errortext", "Out of range!"),
          title = paste0("Page number must be between 1 and ", max_page),
          placement = "right"
        )
      )
    }
  }) # end of PagenumSamp
  outputOptions(output, "PagenumSamp", suspendWhenHidden = FALSE)
  
  observeEvent(({input$spagind_choice}),{
    if (!is.null(input$spagind_choice)){
      toggleElement(id = "spag_strat", input$spagind_choice, anim = T)
    }
  })
  
  output$ind_page <- renderUI({ # Add a page/TRT filter function to view ind. subjects
    if (!is.null(input$spagind_choice)) {
      if (input$spagind_choice == TRUE) {
        selectInput(
          inputId = "page_spagind",
          label = "Treatment arm",
          selected = unique(pkpd_data()$TRTNAME)[1],
          choices = unique(pkpd_data()$TRTNAME),
          width = "18%"
        )
      }
    }
  }) # End of ind_page
  
  output$subject_selection <- renderUI({
    selectInput(
      inputId = "select_subj",
      label = "Select subject ID:",
      choices = pkpd_data()$ID %>% unique()
    )
  })
  
  
  time_units_dataset <- reactive({
    mydata() %>%
      select(TIMEUNIT) %>% # time unit variable...
      unique() %>%
      str_to_title()
  })
  
  trtact_label <- "Days"
  conc_units <- 
    reactive({
      mydata() %>%
        #       # Units of concentration
        filter(str_detect(TYPENAME, "PK")) %>%
        group_by(UNIT) %>%
        select(UNIT) %>%
        unique() %>%
        as.character()
    })
  conc_label <- reactive({
    conc_label <- paste0("Concentration (", conc_units(), ")")
    return(conc_label)
  })
  
  max_ticks_mad <- reactive({ # Get max x-axis value for ticks
    pkpd_data() %>%
      select(NOMTIME) %>%
      filter(is.finite(NOMTIME)) %>%
      max(na.rm = TRUE)
  })
  # Function to detect time-interval between doses ---------------------------------------------------------
  ticks_mad <- function(data) {
    if (length(unique(data$PART)) > 1 || any(str_detect(data$TRTNAME, "q\\dwk"))) {
      data %>%
        # base X-axis ticks on days between doses,
        # e.g q4wk = 4 weeks * 7 days=28
        select(TRTNAME) %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        str_match_all("q\\dwk") %>%
        unique() %>%
        unlist() %>%
        str_match("\\d") %>%
        na.exclude() %>%
        as.integer() %>%
        multiply_by(7) %>%
        min(na.rm = TRUE)
    }
    else {
      7
    }
  }
  time_ticks_mad <- reactive({ticks_mad(mydata())})
  ### Startify plots with covariates ------------------------------------------------
  strat_choice <- reactive(input$strat_choice)
  strat_choice_d <- strat_choice %>% debounce(1000)
  
  strats <- eventReactive(
    {
      strat_choice_d()
      input_list_d()
      dataset()
      input$obs_select
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE,
    {
      source("gen_spag_obj.R")
      req(dataset)
      req(!is.null(input$obs_select))
      data <- dataset() %>% filter(NAME %in% input$obs_select)
      strat_list <- gen_spag_obj(data, stratify = strat_choice_d())
      strat_list <- lapply(
        strat_list,
        function(x) {
          x +
            facet_wrap(~ factor(TRTNAME,
                                levels = unique(TRTNAME) %>%
                                  str_sort(numeric = TRUE)
            ))
        }
      )
      return(strat_list)
    }
  )
  
  range_strat_choice <- reactive(input$range_strat_choice)
  range_strat_choice_d <- range_strat_choice %>% debounce(1000)
  
  range_strats <- eventReactive(
    {
      range_strat_choice_d()
      input_list_d()
      dataset()
      input$obs_select
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE,
    {
      req(dataset)
      req(!is.null(input$obs_select))
      data <- dataset() %>% filter(NAME %in% input$obs_select)
      strat_list <- plotRange_IQRdataGENERAL(
        data = data,
        stratify = range_strat_choice_d(),
        FLAGreturnObject = TRUE
      ) %>%
        unlist(F, F)
      strat_list <- lapply(
        strat_list,
        function(x) {
          x +
            facet_wrap(~ factor(TRTNAME,
                                levels = unique(TRTNAME) %>%
                                  str_sort(numeric = TRUE)
            ))
        }
      )
      return(strat_list)
    }
  )
  
  ### Get plots for individual subjects ------------------------
  label_indiplots <- # get the ID of the plotted object in indivplots
    function(label) {
      str_extract(label, "(?<=ID=)\\d*")
    }
  
  ggind <- eventReactive(
    {
      input$select_subj
      input$obs_select
    },{
      if (is.null(input$obs_select) || is.null(input$select_subj) || is.null(dataset())){
        return(NULL)
      }
      data <- dataset() %>% filter(NAME %in% input$obs_select)
      pdf(tempfile())
      data.xx <- data %>%
        filter(USUBJID == input$select_subj)
      gg <- try(plotIndiv_IQRdataGENERAL(data.xx,
                                         filename = NULL,
                                         FLAGreturnObject = TRUE
      ), silent = T)
      dev.off()
      if (is_empty(gg)){
        return(NULL)
      }
      return(gg)
    })
  
  ### Select observations to display ------------------------------------------------
  observeEvent(
    {
      dataset()
    },
    {
      req(dataset())
      choices <- obsNAMES(dataset())
      conc_name <- dataset() %>%
        filter(TYPENAME=="PK") %>% 
        select(NAME) %>% 
        unique() %>% 
        unlist(F,F)
      choices <- c(conc_name, choices[!(str_detect(choices, conc_name))])
      updateSelectInput(session = getDefaultReactiveDomain(), inputId = "obs_select", choices = choices)
      shinyjs::show("obs_select", anim = T)
    })
  obs_list <- eventReactive(
    {
      input$obs_select
    },
    {
      if (!is.null(input$obs_select)){
        return(input$obs_select)
      }
      else {
        1
      }
    })
  
  ### Show plot according to choice -------------------------------------------------
  cur_plot <- reactive({
    cur_plot <- switch(input$plot_type,
                       NULL = NULL,
                       "Spaghetti" = ({ # Stratify by selected Vars.
                         if (!is.null(input$spagind_choice)) {
                           if (input$spagind_choice == TRUE) {
                             if (!is.null(input$page_spagind)) {
                               ylabel <-  pkpd_data() %>% 
                                 select(NAME,UNIT) %>%
                                 unique %>%
                                 paste(collapse = " (") %>%
                                 paste0(.,")")
                               
                               ggind <- 
                                 ggplot(
                                   data = pk_data_rep_by_trt() %>%
                                     filter(TRTNAME == input$page_spagind),
                                   mapping = aes(x = TIME, y = LIDV)
                                 ) + 
                                 geom_line(
                                   data = pk_data_rep_by_trt() %>%
                                     filter(TRTNAME == input$page_spagind),
                                   aes(group = ID_rep_by_trt),
                                   size = 1, color = rgb(0.5, 0.5, 0.5), alpha = 0.3
                                 ) +
                                 geom_line(
                                   data = pkpd_data() %>%
                                     filter(TRTNAME == input$page_spagind),
                                   aes(group = ID), size = 1
                                 ) +
                                 geom_point(
                                   data = filter(pkpd_data(), CENS == 1, TRTNAME == input$page_spagind),
                                   color = "red", shape = 8, size = 2
                                 ) +
                                 # xgx_scale_x_time_units(
                                 #   units_dataset = time_units_dataset(),
                                 #   units_plot = time_units_plot,
                                 #   breaks = seq(0, max_ticks_mad() + 14, time_ticks_mad()),
                                 #   limits = c(0, max_ticks_mad() + 14)
                                 # ) +
                                 scale_y_log10() +
                                 xlab("Time (Days)") +
                                 labs(y = ylabel) +
                                 scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
                                 facet_wrap(~ `ID` + `TRTNAME Ascending`, ncol = 3) +
                                 theme(
                                   legend.position = "none",
                                   panel.grid.minor.x = ggplot2::element_line(color = rgb(0.9, 0.9, 0.9)),
                                   panel.grid.minor.y = ggplot2::element_line(color = rgb(0.9, 0.9, 0.9)),
                                   axis.title.y = element_text(size = 12)
                                 )
                               if (input$status_on == TRUE) {
                                 ggind <- mark_draft(ggind)
                               }
                               return(ggind)
                             }
                           }
                           else
                           {
                             grid <- marrangeGrob(
                               grobs = strats(),
                               nrow = length(strats()),
                               ncol = 1,
                               top = NULL
                             )
                             return(grid)
                           }
                         }
                       }),
                       "Individual plots" = ({ # Individual plots per subject
                         return(ggind())
                       }),
                       "Median range over time" = ({
                         grid <- marrangeGrob(
                           grobs = range_strats(),
                           nrow = length(range_strats()),
                           ncol = 1,
                           top = NULL
                         )
                         return(grid)
                       }),
                       "Covariate correlation" = ({
                         try(silent = T,
                             switch(input$cor_choiceDist,
                                    NULL,
                                    "continuous" = ({
                                      pdf(tempfile())
                                      # Remove duplicate variables that are
                                      ## both time-dependent and independent
                                      covnames <- covInfo(dataset())$COLNAME
                                      covnames <- covnames[!str_detect(covnames, "Baseline_")]
                                      covnames <- str_replace_all(covnames, "[-| ()]", "_")
                                      
                                      gg <-
                                        plotCorCov_IQRdataGENERAL(dataset(),
                                                                  covNames = covnames,
                                                                  FLAGreturnObject = TRUE
                                        )
                                      dev.off()
                                      gg <- gg + xlab("") + ylab("")
                                      return(gg)
                                    }),
                                    "categorical" = ({
                                      pdf(tempfile())
                                      gg <-
                                        plotCorCat_IQRdataGENERAL(dataset(),
                                                                  FLAGreturnObject = TRUE
                                        )
                                      dev.off()
                                      gg <- gg + xlab("") + ylab("")
                                      return(gg)
                                    }),
                                    "both" = ({
                                      pdf(tempfile())
                                      covnames <- covInfo(dataset())$COLNAME
                                      covnames <- covnames[!str_detect(covnames, "Baseline_")]
                                      covnames <- str_replace_all(covnames, "[-| ()]", "_")
                                      
                                      gg <-
                                        plotCorCovCat_IQRdataGENERAL(dataset(),
                                                                     covNames = covnames,
                                                                     FLAGreturnObject = TRUE
                                        )
                                      dev.off()
                                      gg <-
                                        gg +
                                        scale_x_discrete(labels = function(string) str_sort(string, numeric = TRUE)) +
                                        facet_grid(CNAME.y ~ CNAME.x, as.table = TRUE, scales = "free")
                                      gg <- gg + xlab("") + ylab("")
                                      return(gg)
                                    })
                             )
                         )
                       }),
                       "Nominal vs actual time" = ({
                         gg <- ggplot(data = dataset(), aes(x = TIME, y = NT)) +
                           geom_point() +
                           geom_smooth(method = "lm", color = "black", linetype = 8) +
                           xlab(paste0("Actual Time (", str_to_title(unique(dataset()$TIMEUNIT)), ")")) +
                           ylab(paste0(
                             "Nominal Time (",
                             str_to_title(unique(dataset()$TIMEUNIT)),
                             ")"
                           )) +
                           annotate("text",
                                    x = median(dataset()$TIME),
                                    y = max(dataset()$NT),
                                    label = paste0(
                                      "r = ",
                                      signif(
                                        x = cor(dataset()$TIME, dataset()$NT),
                                        digits = 3
                                      )
                                    )
                           )
                         
                         gdif <- ggplot(data = dataset(), aes(x = TIME, y = TIME - NT)) +
                           geom_point() +
                           xlab(paste0(
                             "Actual Time (",
                             str_to_title(unique(dataset()$TIMEUNIT)),
                             ")"
                           )) +
                           ylab(paste0(
                             "Difference between Actual and Nominal Time (",
                             str_to_title(unique(dataset()$TIMEUNIT)),
                             ")"
                           ))
                         
                         ghist <- ggplot(data = dataset(), aes(x = abs(TIME - NT))) +
                           geom_histogram() +
                           xlab(paste0(
                             "Absolute Difference between Actual and Nominal Time (",
                             str_to_title(unique(dataset()$TIMEUNIT)),
                             ")"
                           )) +
                           ylab("Count")
                         
                         if (input$status_on == TRUE) {
                           gg <- mark_draft(gg)
                           gdif <- mark_draft(gdif)
                           ghist <- mark_draft(ghist)
                         }
                         
                         grid <- marrangeGrob(
                           grobs = list(gg, gdif, ghist),
                           nrow = 3,
                           ncol = 1,
                           top = NULL
                         )
                         rv_dat$nomtime <- ggplotify::as.ggplot(grid[[1]])
                         return(grid)
                       }),
                       "Sampling schedule" = ({
                         return(plot_obj()[input$page_numSamp])
                       }),
                       "Dosing schedule" = ({
                         return(dose_sched_obj()[input$page_numDose])
                       }),
                       "Covariate distribution" = ({
                         if (is.null(input$page_choiceDist)) {
                           return(NULL)
                         } else if (input$page_choiceDist == "") {
                           return(NULL)
                         } else {
                           validate(need(dist_obj()[input$page_choiceDist], paste("No",input$page_choiceDist,"defined.")))
                           return(dist_obj()[input$page_choiceDist])
                         }
                       })
    )
    return(cur_plot)
  })
  
  observe({
    output$dataplot <- renderPlot(
      { # Render a plot according to user's choice
        if (is.null(input$file_path) || is.null(input$plot_type) || input$plot_type == "" || is.null(cur_plot())) {
          hide("condit_download_bttn")
          return(NULL)
        } else {
          rv_plot$title <- input$plot_type
          show("condit_download_bttn")
          return(cur_plot())
        }
      },
      height = function() {
        # Adjust plot height to fit window and avoid compression
        if (is.null(input$plot_type) || input$plot_type == "") {
          return(850)
        }
        if (input$plot_type == "Spaghetti" || input$plot_type == "Median range over time") {
          # make room according to the number of groupings
          if (input$plot_type == "Median range over time") {
            cov_list <- range_strat_choice_d()
          }
          else if (isTruthy(input$spagind_choice) & isTruthy(input$page_spagind)) {
            subrows <- pk_data_rep_by_trt() %>%
              filter(TRTNAME == input$page_spagind) %>%
              select(USUBJID) %>%
              unique %>%
              nrow %>% 
              multiply_by(75)
            return(subrows)
          }
          else {
            cov_list <- strat_choice_d()
          }
          obs_list <- obs_list()
          # Amount of plots to display at once
          plot_sum <- (length(cov_list) + 1) * length(obs_list)
          cov_num <- cov_list %>%
            subset(cov_list %in% catInfo(dataset())$COLNAME)
          cat_num <- setdiff(cov_list, cov_num) %>% length()
          plot_sum <-
            plot_sum +
            lapply(cov_num, function(x) plot_sum + length(unique(dataset()[x]))) %>%
            unlist() %>%
            sum()
          plot_sum <- plot_sum + 2 * cat_num
          return(plot_sum * 550)
        }
        if (input$plot_type == "Covariate distribution") {
          if (input$page_choiceDist == "categorical") {
            cat_amount <-
              catInfo(dataset()) %>%
              select(COLNAME) %>%
              length()
            return((cat_amount + 1) * 850) # account for TRT which is included automatically
          }
          else {
            cov_amount <-
              covInfo(dataset()) %>%
              select(COLNAME) %>%
              length()
            return(cov_amount * 1000)
          }
        }
        if (input$plot_type == "Sampling schedule") {
          return(850)
        }
        if (input$plot_type == "Nominal vs actual time") {
          return(1050)
        }
        if (input$plot_type == "Individual plots" && isTruthy(ggind())) {
          plot_amount <- (dim(ggind()[[1]][[1]])[1] - 1) * 550
          return(plot_amount)
        }
        else {
          return(850)
        }
      },
      res =
        if (str_detect(input$plot_type, "Covariate distribution") && str_detect(req(input$page_choiceDist), "categorical")) {
          50
        }
      else {
        100
      }
    ) # End of dataplot
  })
  # Function to detect time-interval between doses ---------------------------------------------------------
  ticks_mad <- function(data) {
    if (length(unique(data$PART)) > 1 || any(str_detect(data$TRTNAME, "q\\dwk"))) {
      data %>%
        # base X-axis ticks on days between doses,
        # e.g q4wk = 4 weeks * 7 days=28
        select(TRTNAME) %>%
        unique() %>%
        unlist(use.names = FALSE) %>%
        str_match_all("q\\dwk") %>%
        unique() %>%
        unlist() %>%
        str_match("\\d") %>%
        na.exclude() %>%
        as.integer() %>%
        multiply_by(7) %>%
        min(na.rm = TRUE)
    }
    else {
      7
    }
  }
  #### pkpd-dataset ----------------------------------------------------------------------------------
  time_units_plot <- "Days"
  
  pkpd_data <- eventReactive(
    {
      dataset()
      input$obs_select
    },
    {
      req(dataset, cancelOutput = TRUE)
      datasetLIDV <- dataset() %>%
        filter(NAME %in% obsNAMES(dataset())) %>%
        mutate("LIDV" = VALUE) %>% 
        filter(NAME == input$obs_select)
      # get all TRTNAME groups for sorting as a factor
      unique_trt <- as.data.frame(datasetLIDV) %>%
        select(TRTNAME) %>%
        unique() %>%
        unlist(use.names = FALSE)
      
      # seperate according to part (e.g., if it contains "q4wk")
      filter_by <- str_match(unique_trt, "q\\dwk") %>%
        as.character() %>%
        is.na() %>%
        sapply(isFALSE) # switch MAD groups to be at the end of the list
      
      # split into two groups, sort seperately and combine
      sorted_trt <- unique_trt %>%
        split(as.factor(filter_by)) %>%
        lapply(str_sort, numeric = TRUE, decreasing = FALSE) %>%
        unlist(use.names = FALSE)
      colnames(datasetLIDV) <- colnames(datasetLIDV) %>%
        str_replace_all(., "_", " ") %>%
        str_squish()
      orig.pkpd <- as.data.frame(datasetLIDV) %>%
        # Adept analysis dataset for use with XgX package
        mutate(
          ID = USUBJID, # ID   column
          TIME = TIME, # TIME column name, time relative to first dose
          NOMTIME = NT, # NOMINAL TIME column name
          EVID = EVID, # EVENT ID, >=1 is dose, otherwise measurement
          LIDV = LIDV, # DEPENDENT VARIABLE column name
          CENS = CENS, # CENSORING column name
          # CMT     = CMT,    # COMPARTMENT column
          DOSE = DOSE, # DOSE column here (numeric value)
          TRTACT = TRTNAME, # DOSE REGIMEN column here (character, with units)
          LIDV_NORM = LIDV / DOSE # Dose-normalized Value
          # LIDV_UNIT    = ifelse(CMT==2, "ng/ml", NA )
        ) %>%
        dplyr::arrange(DOSE) %>%
        # Create a factor for the treatment variable for plotting
        mutate(
          `TRTNAME Ascending` = factor(TRTACT,
                                       levels = sorted_trt
          ),
          `TRTNAME Descending` = factor(TRTACT,
                                        levels =
                                          rev(sorted_trt)
          )
        ) %>%
        plotly::select(-TRTACT) %>%
        filter(TIME >= 0) # Filter out negative times (meaning pre-trial)
      
      part <- attr(dataset(), "catInfo") %>%
        filter(COLNAME != "TRT")
      part$COLNAME <- str_replace_all(part$COLNAME, "_", " ") %>% str_squish()
      
      for (i in c(part$COLNAME)) {
        x <- part %>%
          filter(COLNAME == i) %>%
          select(VALUES) %>%
          str_split(",") %>%
          unlist() %>%
          as.numeric()
        y <- part %>%
          filter(COLNAME == i) %>%
          select(VALUETXT) %>%
          str_split(",") %>%
          unlist() %>%
          str_to_title()
        orig.pkpd[i] <- factor(orig.pkpd[i][[1]],
                               levels = x,
                               labels = y
        )
      }
      
      if (length(unique(orig.pkpd$PART)) == 2) {
        part <- "PART"
        x <- dataset() %>%
          select(PART) %>%
          unique() %>%
          unlist(F, F) %>%
          sort()
        
        y <- c("SAD", "MAD")
        
        orig.pkpd["PART"] <- factor(orig.pkpd["PART"][[1]],
                                    levels = x,
                                    labels = y
        )
      }
      
      x <- colnames(dataset()) %>% .[(str_detect(., "(?:(?i)Weight|(?i)WTBL)"))]
      y <- orig.pkpd %>%
        select(x) %>%
        unlist(use.names = FALSE) %>%
        median(na.rm = TRUE)
      if (!is_empty(x) & !is.null(y)) {
        orig.pkpd[[paste0("Median ", x)]] <- 
          factor(orig.pkpd[[x]] > y,
                 levels = c(FALSE, TRUE),
                 labels = c(
                   paste0(x, "<=", y),
                   paste0(x, ">", y)
                 )
          )
      }
      
      attr(orig.pkpd, "catInfo") <- attr(dataset(), "catInfo") # keep attributes of IQR object
      if (!is.null(orig.pkpd$WEIGHT)) {
        orig.pkpd %<>% mutate(WEIGHTB = WEIGHT) # rename WEIGHT to WEIGHTB
      }
      if (!is.null(orig.pkpd$WTBL)) {
        orig.pkpd %<>% mutate(WEIGHTB = WTBL) # rename WTBL to WEIGHTB
      }
      if (!is.null(orig.pkpd$Weight)) {
        orig.pkpd %<>% mutate(WEIGHTB = Weight) # rename Weight to WEIGHTB
      }
      return(orig.pkpd)
    }
  )
  ### convert pkpd-data to group by treatment arm --------------------------------------------------------
  pk_data_rep_by_trt <- reactive({
    c_pkpd_data <- pkpd_data()
    pk_data_rep_by_trt <- list()
    for (id in unique(c_pkpd_data$ID)) {
      indiv_data <- c_pkpd_data %>% subset(ID == id)
      itrtact <- unique(indiv_data$`TRTNAME Ascending`)
      pk_data_rep_by_trt[[as.character(id)]] <- c_pkpd_data %>%
        subset(`TRTNAME Ascending` == itrtact) %>%
        mutate(ID_rep_by_trt = ID, ID = id)
    }
    pk_data_rep_by_trt <- bind_rows(pk_data_rep_by_trt)
    return(pk_data_rep_by_trt)
  })
  
  ### download analysis dataset and current plot ----------------------------------------------------
  output$condit_download_bttn <- renderUI({
    req(any(input$format_svg, input$format_png))
    downloadBttn(
      outputId = "downloadPlot",
      style = "gradient",
      color = "primary"
    )
  })
  
  output$downloadData <- downloadHandler( # Download the cleaned dataset
    filename = function() {
      paste0("analysis_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(dataset(), cancelOutput = TRUE)
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- downloadHandler( # Download the current plot presented
    filename = function() {
      plot_title <- pluck(ggplot2::last_plot(),"labels")$title
      if (is.null(plot_title)) {
        if (input$tabs == "init_exp") {
          plot_title <- input$plot_type
        }
        else {
          plot_title <- "plot"
        }
      }
      if (all(input$format_png, input$format_svg)) {
        plot_title <- paste0(plot_title, ".zip")
      }
      else if (input$format_svg) {
        plot_title <- paste0(plot_title, "_", Sys.Date(), ".svg")
      }
      else if (input$format_png) {
        plot_title <- paste0(plot_title, "_", Sys.Date(), ".png")
      }
      return(plot_title)
    },
    content = function(file) {
      curdate <- Sys.time()
      THEME <- theme_bw() +
        theme(
          plot.title    = element_text(size = 18, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          # strip.text.x  = element_text(size = 16),
          # strip.text.y  = element_text(angle = 0),
          strip.text    = element_blank(),
          axis.text.x   = element_text(size = 13),
          axis.text.y   = element_text(size = 13),
          axis.title.x  = element_text(size = 16),
          axis.title.y  = element_text(size = 16),
          legend.title  = element_text(size = 16),
          legend.text   = element_text(size = 14),
          plot.caption  = element_text(hjust = 0)
        )
      
      width <- 15
      height <- 8
      
      if (!is.null(ggplot2::last_plot())) {
        curplot <- ggplot2::last_plot() +
          labs(caption = paste("Generated", curdate, "EDT"))
        if (input$plot_type == "Nominal vs actual time"){
          curplot <- rv_dat$nomtime + 
            labs(caption = paste("Generated", curdate, "EDT"))
          THEME <- theme(axis.title.x = element_blank(),
                         axis.text.x  = element_blank(),
                         axis.ticks.x = element_blank(),
                         plot.caption = element_text(hjust = 0.05, vjust = 5))
          height <- 10
        }
      }
      else {
        curplot <- ggplot() + theme_void()
      }
      
      
      if (all(input$format_png, input$format_svg)) {
        folder <- paste("current_plot", Sys.Date(), as.character(rdunif(1,100)), sep = "_")
        dir <- tempdir()
        cc <- 1
        while (dir.exists(file.path(dir, folder))) {
          folder <- str_replace(folder,"_.{1,3}$",paste0("_r",cc))
          cc <- cc + 1
        }
        dir.create(file.path(dir, folder))
        get_name_attrs <- attr_getter("names")
        plot_title <- pluck(curplot, get_name_attrs(curplot)[9])$title
        if (is.null(plot_title)) {
          if (input$tabs == "init_exp") {
            plot_title <- input$plot_type
          }
        }
        titl <- plot_title
        svg(
          filename = paste0(dir, "/", folder, "/", titl, ".svg"),
          antialias = "subpixel",
          width = width,
          height = height
        )
        print(curplot + THEME)
        dev.off()
        Cairo::CairoPNG(
          filename = paste0(dir, "/", folder, "/", titl, ".png"),
          width = width,
          height = height,
          units = "in",
          res = 150
        )
        print(curplot + THEME)
        dev.off()
        target <- zip::zip(file, list.files(), root = file.path(dir, folder))
        unlink(file.path(dir, folder))
        return(target)
      }
      else if (input$format_png) {
        layers <- curplot$layers %>%
          sapply(function(x) x$geom %>% class()) %>%
          first()
        if (any(str_detect(layers, "GeomSmooth"))) {
          curplot$layers[[match("GeomSmooth", layers)]]$size <- curplot$layers[[match("GeomSmooth", layers)]]$size + 1
        }
        Cairo::CairoPNG(
          filename = file,
          width = width,
          height = height,
          units = "in",
          res = 150
        )
        print(curplot + THEME)
        dev.off()
      }
      else if (input$format_svg) {
        svg(
          filename = file,
          antialias = "subpixel",
          width = width,
          height = height
        )
        print(curplot + THEME)
        dev.off()
      }
    }
  )
  
  # Control cursor style---------------------------------------------------------------------------------
  runjs("
          $(document).ready(function(e)
          {
          $('#sad_plot').css('cursor','default')
          })")
  
  anom_df <- reactiveValues()
  ### add selected subject to anomalies list when double-clicking their plot----------------------------
  observeEvent(
    {
      input$sad_dblc
    },
    ({
      req(input$spagind_choice)
      if (input$spagind_choice) {
        past_length <- length(report_columns$reporta$Subject)
        if (anyDuplicated(c(report_columns$reporta$Subject, input$sad_dblc$panelvar2)) == 0) {
          sub_trt <-
            mydata() %>%
            select(USUBJID, TRTNAME) %>%
            unique.array() %>%
            as.data.frame() %>%
            filter(USUBJID == input$sad_dblc$panelvar2) %>%
            select(TRTNAME) %>%
            unlist(F, F)
          report_columns$reporta <<- add_row(report_columns$reporta,
                                             Subject = input$sad_dblc$panelvar2,
                                             Observation = input$obs_select,
                                             `Treatment Arm` = sub_trt,
                                             Comments = ""
          )
          runjs("
            let sub_name = Shiny.shinyapp.$inputValues.sad_dblc.panelvar2;
            let title = `Added subject ${sub_name} to report.`;
                        Swal.fire({
                          icon: 'info', 
                          toast: true,
                          title: title,
                          timer: 2000,
                          showConfirmButton: false,
                          showCloseButton: true,
                          position: 'bottom-end',
                          showClass: {
                              backdrop: 'swal2-show',
                              popup: '',   
                              icon: ''             
                            },
                          hideClass: {
                              popup: 'fade',      
                            }
                          });
                          "
          )
        }
      }
    })
  )
  # review anomalous subjects -----------------------------------------------
  output$anoms_review_button <- reactive({
    return(nrow(report_columns$reporta) > 0)
  })
  outputOptions(output, "anoms_review_button", suspendWhenHidden = FALSE)
  
  observeEvent(
    {
      input$anoms_review
    },
    {
      showModal(
        modalDialog(
          easyClose = TRUE,
          footer = tagList(
            span(
              style = "margin-inline:2px;",
              "To submit this table:  "
            ),
            actionButton(
              inputId = "goto_qc",
              label = "Switch to report tab",
              `data-dismiss` = "modal"
            )
          ),
          title = "Review flagged subjects",
          DTOutput("pk_anom_review")
        )
      )
    }
  )
  
  observeEvent(
    {
      input$goto_qc
    },
    {
      updateTabItems(session, "tabs", "qc_tab")
      updateTabItems(session, "qc_subtabs", "Spec Upload")
      removeModal()
    }
  )
  
  output$pk_anom_review <-
    renderDataTable(report_columns$reporta,
                    options = list(
                      scrollX = FALSE,
                      deferRender = TRUE,
                      scroller = TRUE,
                      dom = "tp",
                      buttons = "excel",
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ),
                    rownames = FALSE,
                    editable = "cell"
    )
  
  observeEvent(
    {
      input$pk_anom_review_cell_edit
    },
    {
      # Compensate for col. number error
      insert <- input$pk_anom_review_cell_edit
      insert$col <- insert$col + 1
      
      report_columns$reporta <<-
        editData(
          report_columns$reporta,
          insert,
          "pk_anom_review"
        )
      report_columns$anom_comments <<- report_columns$reporta[["Comments"]]
    }
  )
  
  output$anom_title <- renderText({
    if (is.null(report_columns$reporta)) {
      return(" ")
    }
    else {
      return("Flagged subjects:")
    }
  })
  
  output$anom_report <-
    renderDataTable(report_columns$reporta,
                    options = list(
                      scrollX = FALSE,
                      deferRender = TRUE,
                      scroller = TRUE,
                      dom = "tp",
                      buttons = "excel",
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ),
                    rownames = FALSE,
                    editable = "cell"
    )
  
  observeEvent(
    {
      input$anom_report_cell_edit
    },
    {
      # Compensate for col. number error
      insert <- input$anom_report_cell_edit
      insert$col <- insert$col + 1
      
      report_columns$reporta <<-
        editData(
          report_columns$reporta,
          insert,
          "anom_report"
        )
      report_columns$anom_comments <<- report_columns$reporta[["Comments"]]
    }
  )
  
  output$rep_review <- renderUI({
    req(length(report_columns$reporto) > 0 || length(report_columns$reporta) > 0)
    actionButton(
      inputId = "review_report",
      label = "Review reported variables",
      width = "100%",
      style = "margin-bottom: 10px;"
    ) %>% 
      bs_embed_tooltip(title = "Review the reported variables/individuals, add comments if needed, and download the QC Report",
                       placement = "right", 
                       trigger = "focus")
  })
  outputOptions(output,"rep_review", suspendWhenHidden = TRUE)
  ### upload and generate QC report ----------------------------------------------------
  output$report_upload_button <- renderUI({
    req(input$file_path)
    actionButton(
      inputId = "report_upload",
      label = "Upload existing report",
      width = "100%"
    )
  })
  outputOptions(output,"report_upload_button", suspendWhenHidden = TRUE)
  
  observeEvent(
    {
      input$report_upload
    },
    {
      showModal(
        modalDialog(
          size = "s",
          easyClose = TRUE,
          tagList(
            fileInput(
              inputId = "prexist_report",
              label = "Upload existing report (.docx)",
              accept = c(".docx"),
              placeholder = "No file selected"
            ),
            tags$div(
              style = "color: #b50303;",
              "Note: Loading a report file will overwrite any previously reported variable/subject rows on the app. "
            )
          )
        )
      )
    }
  )
  
  prexist_report_path <- reactive({
    input$prexist_report$datapath
  })
  
  # Take the first row of a table and make it into a header
  row_to_header <- function(table) {
    colnames(table) <- table[1, ]
    table <- table[-1, ]
    return(table)
  }
  
  observeEvent(
    {
      input$prexist_report
    },
    {
      prex_report <- read_docx(prexist_report_path())
      prex_tables <- docx_extract_all_tbls(prex_report)
      
      if (length(prex_tables) > 0) {
        prex_qc <- prex_tables[[1]]
        if (any(names(prex_qc) != c("Variable", "Comments", "Status")) && !("Subject" %in% names(prex_qc))) {
          prex_qc %<>% set_names(.[1,]) %>% .[-1,]
        }
        
        if (!("Subject" %in% names(prex_qc)))
        {
          report_columns$reporto <- prex_qc
        }
        else {
          names(prex_qc) <- str_replace_all(names(prex_qc),"[.]", " ")
          report_columns$reporta <- prex_qc
        }
      }
      
      if (length(prex_tables) > 1) {
        prex_qc <- prex_tables[[1]]
        if (any(names(prex_qc) != c("Variable", "Comments", "Status"))) {
          prex_qc %<>% set_names(.[1,]) %>% .[-1,]
        }
        report_columns$reporto <- prex_qc
        
        anom_prex <- prex_tables[[2]]
        if (any(names(anom_prex) != c("Subject", "Treatment.Arm", "Comments"))) {
          anom_prex %<>% set_names(.[1,]) %>% .[-1,]
        }
        names(anom_prex) <- str_replace_all(names(anom_prex),"[.]", " ")
        report_columns$reporta <- anom_prex
      }
      
      removeModal()
    }
  )
  
  output$remove_report_cols <- renderUI({
    req(length(input$anom_report_rows_selected) > 0 || length(input$current_report_rows_selected) > 0)
    actionButton(
      inputId = "remove_cols",
      label = "",
      style = "margin-bottom: 10px; width: 120.11px;",
      icon = icon("eraser")
    )
  })
  
  # remove selected rows from report
  observeEvent(
    {
      input$remove_cols
    },
    {
      selected_anom <- input$anom_report_rows_selected
      selected_specs <- input$current_report_rows_selected
      if (!is.null(selected_anom)) {
        report_columns$reporta <<- report_columns$reporta[-selected_anom, ]
        anom_df$data <<- anom_df$data[-selected_anom]
        report_columns$anom_list <<- report_columns$anom_list[-selected_anom]
        report_columns$anom_trt <<- report_columns$anom_list[-selected_anom]
        report_columns$anom_comments <<- report_columns$anom_list[-selected_anom]
      }
      if (!is.null(selected_specs)) {
        report_columns$reporto <<- report_columns$reporto[-selected_specs, ]
      }
    }
  )
  # when clicking the "review reported variables" button, check if a username is logged and display it
  onclick(
    "review_report",
    runjs('
                  $("#review_report").tooltip("hide")
                  let cookieval = document.cookie.split("; ")[0];
                  let cookiedef = cookieval.split("=")[0];
                  let cookiename = cookieval.split("=")[1];
                  if (cookiename.length > 0 && cookiedef == "username") {
                    $("#username").val(cookiename);
                    Shiny.setInputValue("username", cookiename);
                  }

                  ')
  )
  # log an entered username whenever the input field changes
  # Cookie is kept for an (arbitrary) year
  onevent(
    "mouseleave",
    "username",
    runjs('
                  let username_elem = $("#username");
                  let username = username_elem.val();
                  let add_cookie = function (username){
                    if (username.trim().length) {
                    document.cookie = `username=${username};expires=${new Date(Date.now() + ( 3600 * 1000 * 24 * 365))}`
                    Shiny.setInputValue("username", username)
                    }
                  }
                  username_elem.on("blur change mouseout", function () {add_cookie(username)});
                  add_cookie(username);
                  ')
  )
  
  output$submit_report <- downloadHandler(
    filename = function() {
      pos <-
        str_locate(input$file_path$name, file_ext(input$file_path$name))[1]
      pos <- pos - 2
      
      # Get filename without extension
      name <- paste0(
        str_sub(input$file_path$name, end = pos) %>%
          str_to_upper(),
        " ",
        "QC Report ",
        format(Sys.Date(), "%d-%m-%Y"),
        ".docx"
      )
      return(name)
    },
    content = function(file) {
      report <- officer::read_docx() %>%
        body_add_par(
          paste(
            mydata()$COMPOUND %>% unique(),
            "QC Findings Report"
          ),
          style = "centered"
        ) %>%
        body_add_par(
          paste0(
            "Generated ",
            format(Sys.time(), "%d/%m/%Y, %k:%M EDT")
          ),
          style = "centered"
        ) %>%
        body_add_par(
          paste(
            "Prepared By:",
            str_replace(
              input$username, "^[a-z]",
              str_split(input$username, "")[1] %>%
                toupper()
            )
          ),
          style = "centered"
        ) %>%
        body_add_par(" ", style = "Normal")
      if (!is.null(input$specs_flag)) {
        if (input$specs_flag == TRUE && nrow(report_columns$reporto) > 0) {
          reporto <- report_columns$reporto %>% mutate("Status" = "")
          oflex <- flextable(reporto) %>%
            bold(j = 1) %>%
            bold(part = "header") %>%
            theme_box() %>%
            flextable::font(fontname = "Cambria", part = "all") %>%
            width(width = 6.3 / 3) %>%
            align(align = "center", part = "all")
          report %<>%
            body_add_flextable(oflex) %>%
            body_add_par(" ", style = "Normal")
        }
      }
      if (!is.null(input$anoms_flag)) {
        if (input$anoms_flag == TRUE && nrow(report_columns$reporta) > 0) {
          rflex <- flextable(report_columns$reporta) %>%
            bold(j = 1) %>%
            bold(part = "header") %>%
            theme_box() %>%
            flextable::font(fontname = "Cambria", part = "all") %>%
            width(width = 6.3 / 4) %>%
            align(align = "center", part = "all")
          report %<>%
            body_add_par("Flagged Subject List", style = "Normal") %>%
            body_add_par(" ", style = "Normal") %>%
            body_add_flextable(rflex) %>%
            body_add_par(" ", style = "Normal")
        }
      }
      if (!is.null(input$anomggs_flag)) {
        if (input$anomggs_flag == TRUE && nrow(report_columns$reporta) > 0) {
          report %<>%
            body_add_par("Flagged Subject Plots:", style = "Normal") %>%
            body_add_par(" ", style = "Normal")
          for (i in 1:nrow(report_columns$reporta)) {
            if (!("Observation" %in% names(report_columns$reporta))){
              obs_unit <- mydata() %>%
                filter(TRTNAME == report_columns$reporta$`Treatment Arm`[i]) %>%
                filter(TYPENAME == "PK") %>% 
                select(UNIT) %>% 
                unique() %>% 
                unlist(F,F)
              obs_name <- mydata() %>%
                filter(TRTNAME == report_columns$reporta$`Treatment Arm`[i]) %>%
                filter(TYPENAME == "PK") %>% 
                select(NAME) %>% 
                unique() %>% 
                unlist(F,F)
            }
            else {
              obs_name <- report_columns$reporta$Observation[i]
              obs_unit <- mydata() %>%
                filter(TRTNAME == report_columns$reporta$`Treatment Arm`[i]) %>%
                filter(NAME == obs_name) %>% 
                select(UNIT) %>% 
                unique() %>% 
                unlist(F,F)
            }
            
            gg <-
              mydata() %>%
              filter(TRTNAME == report_columns$reporta$`Treatment Arm`[i]) %>%
              filter(NAME == obs_name) %>%
              ggplot(aes(x = NT, y = VALUE)) +
              geom_line(
                data = mydata() %>%
                  filter(NAME == obs_name) %>%
                  filter(USUBJID == report_columns$reporta$Subject[i]),
                size = 1, color = "black"
              ) +
              geom_line(aes(group = USUBJID),
                        color = rgb(0.5, 0.5, 0.5),
                        alpha = 0.3,
                        size = 1
              ) +
              scale_y_log10() +
              # xgx_scale_x_time_units(
              #   units_dataset = time_units_dataset(),
              #   units_plot = time_units_plot
              # ) +
              labs(
                x = paste0("Time [", time_units_dataset(), "]"),
                y = paste0(obs_name," [", obs_unit, "]")
              )
            report %<>%
              body_add_par(paste0(report_columns$reporta$Subject[i]," - ",obs_name," - ", report_columns$reporta$`Treatment Arm`[i]),
                           style = "Normal"
              ) %>%
              body_add_gg(gg, style = "centered", height = 3)
          }
        }
      }
      
      print(report, target = file)
    }
  )
} # End of Server
