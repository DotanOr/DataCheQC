gen_spag_obj <- function(x,stratify) {
  #function takes data and generates a Spaghetti ggplot object (or list) without printing it 
  require(IQRtools)
  cur_dev <- grDevices::dev.cur()   # store current device
  pdf(NULL, width = 6, height = 6)  # open null device
  grDevices::dev.control("enable")  # turn on recording for the null device
  null_dev <- grDevices::dev.cur()  # store null device
  
  # make sure we always clean up properly, even if something causes an error
  on.exit({
    grDevices::dev.off(null_dev)
    if (cur_dev > 1) grDevices::dev.set(cur_dev) # only set cur device if not null device
  })
  
  # plot
  pdf(tempfile())
  p<-plotSpaghetti_IQRdataGENERAL(data = x,
                                  stratify= stratify,
                                  FLAGreturnObject = TRUE)
  dev.off()
  return(unlist(p, recursive = FALSE))
}