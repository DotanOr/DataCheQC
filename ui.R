# load libraries
source("www/load_libs.R")

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "DataCheQC",
    tags$li(
      class = "dropdown tasks-menu",
      tags$a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown", tags$i(class = "fa fa-tasks")),
      tags$ul(
        class = "dropdown-menu",
        style = "border:none;",
        tags$li(
          class = "header",
          style = "background-color: #5a55b0e3;color: rgb(255 255 255 / 95%);
                            border-top-right-radius:0px; border-top-left-radius:0px;",
          tags$b("Options")
        ),
        tags$li(
          tags$ul(
            class = "menu",
            style = "max-height:none!important;",
            tags$script('
                                $( document ).ready(function() {
                                  $(\'#format_choice button.checkbtn\').click(function(e) {e.stopPropagation()})
                                  $(\'#format_choice button.checkbtn input\').click(function(e) {e.stopPropagation()})
                                });
                                '),
            tags$ul(
              class = "menu keep-open-on-click",
              style = "max-height: none!important; background-color:rgb(236 240 245 / 50%)",
              tags$li(
                h4(
                  style = "text-align: center;
                                    border-bottom: 1px solid darkgray;
                                    margin-bottom: 10px;
                                    padding-bottom: 10px;
                                    rgb(236 240 245 / 35%);",
                  tags$b("Download analysis dataset"),
                  br(),
                  br(),
                  downloadBttn(
                    outputId = "downloadData",
                    style = "gradient",
                    color = "royal"
                  )
                ) # </h4>
                ,
                h4(
                  style = "text-align: center;",
                  tags$b("Download most recent plot"),
                  br(),
                  br(),
                  tags$div(id = "format_choice",
                           class="form-group shiny-input-checkboxgroup shiny-input-container shiny-input-container-inline shinyjs-resettable shiny-bound-input", 
                           role="group",
                           `aria-labelledby` ="format_choice-label",
                           tags$label(id = "format_choice-label", 
                                      class = "control-label",
                                      `for`="format_choice", 
                                      "Choose file format:"),
                           tags$div(class = "shiny-options-group",
                                    tags$button(class = "btn checkbtn btn-default active checking-style",
                                                style = "margin-left: -1px;",
                                                tags$span(
                                                  tags$i(class = "fa fa-check-square checking-align",
                                                         role = "presentation", 
                                                         `aria-label` = "chart icon",
                                                         style = "margin-right: 4px; vertical-align: middle;"),
                                                  ".png"
                                                )
                                    ),
                                    tags$button(class = "btn checkbtn btn-default checking-style",
                                                style = "margin-left: -4px;",
                                                tags$span(
                                                  tags$i(class="far fa-square checking-align", 
                                                         role="presentation",
                                                         `aria-label`="chart icon",
                                                         style="margin-right: 4px; vertical-align: middle;"),
                                                  ".svg"
                                                )
                                    )
                           )
                  ),
                  uiOutput("condit_download_bttn")
                ) # </h4>
              )
            )
          )
        )
      ) # </li>
    ) # End of dropdownMenu
  ), # End of Header
  dashboardSidebar(
    sidebarMenu(
      style = "padding-bottom: 10px;",
      id = "tabs",
      shiny::HTML('<li>
                <a href="#shiny-tab-qc_tab" data-toggle="tab" data-value="qc_tab">
                <i class="fa fa-fas fa-clipboard far"></i>
                <span style="padding-left: 8%">Data Specs</span>
                </a>
                </li>'),
      shiny::HTML('<li>
                <a href="#shiny-tab-init_exp" data-toggle="tab" data-value="init_exp">
                <i class="fa fa-play-circle far" style="width:49px;"></i>
                <span style="padding-left: 3%;">Visualization</span>
                </a>
                </li>
                ')
    ), # End of sidebarMenu
    div(
      style =
        "bottom:20px;left:50px;margin:0 auto;position:fixed;",
      tags$image(
        src = "teva_logo.png",
        width = "80px", height = "auto",
        style = "margin: 0 17px;"
      )
    ) # end of div
  ), # End of dashboardSidebar
  dashboardBody(
    add_loading_state(
      selector = c(".shiny-plot-output"),
      text = "Loading...",
      spinner = "hourglass",
      svgColor = "#555299",
      svgSize = "90px",
      messageFontSize = "28px",
      timeout = 1500
    ),
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Lobster"),
      useShinyjs(),
      useSweetAlert(),
      tags$style( # To prevent multiple scrollbars in the same window
        HTML("
           .wrapper {
             height: auto !important;
             position:relative;
             overflow-x:hidden;
             overflow-y:hidden
           }
           
           table.dataTable thead .sorting, table.dataTable thead .sorting_asc, table.dataTable thead .sorting_desc, table.dataTable thead .sorting_asc_disabled, table.dataTable thead .sorting_desc_disabled {
            text-align: center !important;
           }
           
           .main-header .logo {
              font-family: Lobster;
              font-size: xx-large;
              color: aliceblue;
           }
           
           .content-wrapper {
             min-height: 930px;
           }
           
           .selectize-dropdown-content {
           max-height: 300px;
           }
           .tab-pane.active {
           outline: none !important;
           }
            
          .checking-style {
            border-top-right-radius: 0px;
            border-bottom-right-radius: 0px;
            height: 34px;
            width: 68px;
           }
           
          .checking-align {
            margin-right: 4px;
            vertical-align: middle;
           }
           
           .main-sidebar {
           font-size: 17px;
           text-align: justify;
           }


           .main-sidebar .sidebar-menu>li>a>span {
           padding-left:20%;
           }

           .main-sidebar .sidebar-menu>li>a>.fa,
           .sidebar-menu>li>a>.glyphicon,
           .sidebar-menu>li>a>.ion {
           position: relative;
           width: 20%;
           padding-inline: 0;
           margin-inline: 0;
           }

           .main-sidebar .sidebar-menu>li>a:after {
           content: '';
           display: block;
           margin: auto;
           margin-top: 5px;
           height: 2px;
           width: 0px;
           background: transparent;
           transition: width .3s ease, background-color .3s ease;
           }

           .main-sidebar .sidebar-menu>li>a:hover:after {
           width: 50%;
           background: #555299;
           }

           .main-sidebar .sidebar-menu>.active>a:hover:after {
           width: 0px;
           background: transparent;
           }

           .shiny-notification {
           word-break: break-word;
           white-space: pre;
           }

           .sbs-alert {
           word-break: break-word;
           white-space: pre;
           }
           
           #sad_plot {
            margin-bottom: 10px;
           }

           /* custom scrollbar */
           ::-webkit-scrollbar {
           width: 15px;
           }

           ::-webkit-scrollbar-track {
           background-color: transparent;
           }

           ::-webkit-scrollbar-thumb {
           background-color: #b3b3b3;
           border-radius: 20px;
           border: 3px solid transparent;
           background-clip: content-box;
           }

           ::-webkit-scrollbar-thumb:hover {
           background-color: #858585;
           }

           #sw-content-layers_drop{
           max-height:225px;
           overflow-y:auto;
           background: rgba(255, 255, 255, 0.30);
           backdrop-filter: blur(10px);
           padding-left: 9px;
           }

           #sw-content-layers_drop > .sw-dropdown-in{
           background-color: transparent;
           }

           /* custom scrollbar */
           #sw-content-layers_drop::-webkit-scrollbar {
           width: 15px;
           }

           #sw-content-layers_drop::-webkit-scrollbar-track {
           background-color: transparent;
           }

           #sw-content-layers_drop::-webkit-scrollbar-thumb {
           background-color: #b3b3b3;
           border-radius: 20px;
           border: 3px solid transparent;
           background-clip: content-box;
           }

           #sw-content-layers_drop::-webkit-scrollbar-thumb:hover {
           background-color: #858585;
           }

           #sw-content-ranges_drop{
           background: rgba(255, 255, 255, 0.30);
           backdrop-filter: blur(10px);
           }

           #sw-content-ranges_drop > .sw-dropdown-in{
           background-color: transparent;
           }

           #layer_sliders > div > div > div > input {
           background-color: rgb(255 255 255 / 90%);
           }

           #filter_modal > div > div > div.modal-body {
           padding-top: 0px;
           }

           #modal_inputs .modal-dialog .modal-content {
            width: 375px;
           }

           #modal_inputs .modal-dialog .modal-content .modal-body {
            display: flex;
            flex-direction: column;
            align-items: flex-start;
           }

           #modal_inputs .modal-dialog .modal-content .modal-body .shiny-html-output .form-group {
            width: 345px;
           }

           #modal_inputs .modal-dialog .modal-content .modal-body .shiny-html-output .form-group input {
            width: 293px;
            display: inline-block;
            margin-right: 4px;
           }
           
           .nav-tabs > li > a {
            outline: none !important;
           }
           .nav > li > a {
            outline: none !important;
           }
           
           #spag_by_ind
             {
               white-space: nowrap;
             }
           ")
      )
    ),
    tabItems(
      tabItem(
        tabName = "qc_tab",
        fluidRow(column(width = 3,
                        box(
                          width = 12,
                          tabsetPanel(id = "qc_subtabs",
                                      type = "tabs",
                                      tabPanel(
                                        "Data Upload",
                                        br(),
                                        fileInput(
                                          inputId = "file_path",
                                          label = "Load dataset (.csv or .sas7 file)",
                                          placeholder = "No file selected",
                                          accept = c(".csv", ".sas7bdat")
                                        ),
                                        uiOutput("DoseNames"),
                                        uiOutput("ObsNames"),
                                        uiOutput("ConCovNames"),
                                        checkboxInput(
                                          inputId = "impute_flag",
                                          label = "Impute missing values by median",
                                          value = TRUE
                                        ),
                                        uiOutput("CatCovNames"),
                                        uiOutput("depConCovNames"),
                                        uiOutput("depCatCovNames"),
                                        checkboxInput(
                                          inputId = "clean_flag",
                                          label = "Clean dataset",
                                          value = F
                                        ),
                                        uiOutput("status_flag")
                                      ), # end of TabPanel
                                      tabPanel(
                                        "Spec Upload",
                                        br(),
                                        fileInput(
                                          inputId = "spec_file",
                                          label = "Choose specification file (.docx, .xlsx)",
                                          placeholder = "No file selected",
                                          accept = c(".docx", ".xlsx")
                                        ),
                                        uiOutput("switch_to_value"),
                                        uiOutput("opt_filter_flag"),
                                        uiOutput("pmxian_filter_flag"),
                                        uiOutput("add_selected"),
                                        uiOutput("align_columns"),
                                        uiOutput("review_nas_button"),
                                        uiOutput("rep_review"),
                                        uiOutput("report_upload_button"),
                                        bsAlert("report_alert"),
                                        bsModal("report_modal",
                                                "Review Report Variables",
                                                trigger = "review_report",
                                                textInput("username",
                                                          "User:", 
                                                          placeholder = "Enter your username to enable report generation"),
                                                textOutput("cur_title"),
                                                DTOutput("current_report"),
                                                textOutput("anom_title"),
                                                DTOutput("anom_report"),
                                                uiOutput("check_specs"),
                                                uiOutput("check_anoms"),
                                                uiOutput("check_anomggs"),
                                                uiOutput("remove_report_cols"),
                                                uiOutput("report_dl_bt")
                                        )
                                      )
                          )
                        )
        ), #end of TabsetPanel
        column(
          width = 9,
          conditionalPanel(
            condition = "input.file_path != 'null'",
            box(
              width = 12, collapsible = FALSE, collapsed = FALSE,
              title = "Inspect dataset variables",
              uiOutput("column_choose"),
              withSpinner(dataTableOutput("data_cols"), type = 5), 
              dataTableOutput("spec_cols")
            )
          )
        ))
      ), # End QC tabItem
      tabItem(
        "init_exp",
        tags$style(
          type = "text/css",
          ".recalculating {opacity: 1.0;}"
        ),
        fluidRow(
          box(
            width = 12,
            height = "auto",
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Graphical Exploration",
                tags$span(
                  style = "display: flex; justify-content: flex-end; height: 2.1rem;",
                  uiOutput("report_button")
                ),
                disabled(
                  selectInput(
                    inputId = "plot_type",
                    label = "Select plot type to render:",
                    choices = list(
                      "Choose plot type" = "", # default empty value
                      Observations = c("Spaghetti",
                                       "Median range over time",
                                       "Individual plots"),
                      Covariates = c("Covariate distribution",
                                     "Covariate correlation"),
                      Timings = c("Nominal vs actual time",
                                  "Dosing schedule",
                                  "Sampling schedule")
                    ),
                    width = "50%"
                  )
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Spaghetti' || input.plot_type == 'Median range over time' || input.plot_type == 'Individual plots'",
                  hidden(
                    selectInput(inputId = "obs_select",
                                label = "Select observation:",
                                choices = "",
                                multiple = F)
                  )
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Dosing schedule'",
                  uiOutput("PagenumDose")
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Sampling schedule'",
                  uiOutput("PagenumSamp")
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Covariate distribution'",
                  uiOutput("PagechoiceDist")
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Covariate correlation'",
                  uiOutput("CorchoiceDist")
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Spaghetti'",
                  hidden(uiOutput("spag_strat")),
                  tipify(
                    placement = "right",
                    checkboxInput(
                      inputId = "spagind_choice",
                      label = "Focus on individual profiles",
                      value = FALSE,
                      width = "200px"
                    ),
                    "This option enables review of individual subject data to locate anomalies."
                  ),
                  uiOutput("ind_page")
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Median range over time'",
                  uiOutput("range_strat")
                ),
                conditionalPanel(
                  condition = "input.plot_type == 'Individual plots'",
                  selectInput(
                    inputId = "select_subj",
                    label = "Select subject ID:",
                    choices = ""
                  )
                ),
                withSpinner(plotOutput("dataplot", height = "auto",dblclick = "sad_dblc") %>% 
                              tagAppendAttributes(style = "cursor: default;"), type = 5),
                conditionalPanel(
                  condition = "input.spagind_choice && output.anoms_review_button == true",
                  actionButton("anoms_review", label = "Review flagged subjects") %>% 
                    tagAppendAttributes(style = "position: absolute; top: 95px; right: 30px; margin-top: 5px;")
                ),
                bsAlert("list_alert")
              ), # End of tabPanel
              tabPanel(
                "Summary Tables",
                br(),
                conditionalPanel(
                  condition = "input.file_path != 'null'",
                  fluidRow(
                    style = "height: auto",
                    column(
                      width = 8,
                      box(
                        width = 12,
                        collpasible = TRUE,
                        collapsed = TRUE,
                        title = "Analysis dataset",
                        withSpinner(
                          type = 5,
                          dataTableOutput("clean_contents")
                        )
                      ) %>% tagAppendAttributes(style = "padding-left: 9px; padding-right: 0px;")
                    ) %>% tagAppendAttributes(style = "padding-left: 9px; padding-right: 0px;"),
                    column(
                      width = 4,
                      box(
                        width = 12,
                        height = "500px",
                        collpasible = TRUE,
                        collapsed = TRUE,
                        title = "Duplicated times, unanalyzed dataset",
                        withSpinner(dataTableOutput("duplicated_check"), type = 5)
                      )
                    ) %>% tagAppendAttributes(style = "padding-left: 4px; padding-right: 15px;")
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        width = 12, collpasible = TRUE, collapsed = TRUE,
                        title = "Summary of covariates",
                        withSpinner(dataTableOutput("cat_summary"), type = 5),
                        withSpinner(dataTableOutput("cov_summary"), type = 5)
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        width = 12, collpasible = TRUE, collapsed = TRUE,
                        title = "Summary of observations",
                        withSpinner(dataTableOutput("contents"), type = 5)
                      )
                    )
                  )
                )
              ) # End of Summary Tables tabPanel
            ) # End of tabsetPanel
          ) # End of box
        ) # End of fluidRow 1
      ) # End of "init_exp" tabItem
    ) # End of TabItems
  ) # End of DashboardBody
) # End FluidPage
# End UI
