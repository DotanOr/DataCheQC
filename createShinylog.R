createShinylog <- function(outputfilename, user = "App User"){
  COMPLIANCE_MODE_SCRIPT_NAME <- "DataExploR Shiny App Server"
  title <- "<TT>   File generation log"
  logfile_name <- paste(outputfilename, 
                        ".log", sep = "")
  
  if(str_detect(outputfilename,"figures")){
    reg_filename <- str_split(outputfilename,"figures/")[[1]][2]
  }
  else if(str_detect(outputfilename,"summary_tables")){
    reg_filename <- str_split(outputfilename,"summary_tables/")[[1]][2]
  }
  
  outputfilename <- stringr::str_replace_all("//", "/", outputfilename)
  user <- user
  time <- Sys.time()
  user_info <- paste0(COMPLIANCE_MODE_SCRIPT_NAME,
                      " | User: ", user,
                      " | Date: ", time)
  header <- paste("<TR>   Analysis file                       | ", 
                  COMPLIANCE_MODE_SCRIPT_NAME, sep = "")
  subheader <- paste("<TR>   File (relative to calling function) | ", 
                     reg_filename, sep = "")
  user <- paste("<TR>   Username                            | ", 
                user, 
                sep = "")
  time <- paste("<TR>   Date of creation                    | ", 
                time, 
                sep = "")
  
  max_char_length <- max(1, 
                         max(nchar(header)), 
                         nchar(time), 
                         nchar(user), 
                         nchar(subheader))
  blank_lines <- 
    paste(c("       ",  rep("=", max_char_length - 7), "\n"),
          collapse = "", sep = "")
  log_content <- c(title, 
                   blank_lines, 
                   subheader, 
                   user, 
                   time, 
                   header)
  write(log_content, 
        logfile_name, 
        append = FALSE)
}
