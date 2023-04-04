gen_spag_obj <- function(x,stratify) {
  # function takes data and generates a Spaghetti ggplot object (or list) without printing it 
  p <- plotSpaghetti_IQRdataGENERAL(data = x,
                                    stratify = stratify,
                                    FLAGreturnObject = TRUE)
  # dev.off()
  return(unlist(p, recursive = FALSE))
}
