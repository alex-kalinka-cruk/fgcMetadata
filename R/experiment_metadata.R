#' experiment_metadata
#' 
#' Add sample metadata to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param experiment_csv A path to an experiment '.csv' file.
#' @return Returns an object of class `fgcMeta`. Also saves data in `experiment.csv` in `meta$data_dir`.
#' @export
#' @importFrom dplyr %>% rename
experiment_metadata <- function(meta, experiment_csv){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!file.exists(experiment_csv)) stop(paste("unable to find",experiment_csv))
  
  tryCatch({
    exp <- read.csv(experiment_csv, stringsAsFactors = F) %>%
      dplyr::rename(title = CRISPR_screen_cell_model)
    write.csv(exp, file.path(meta$data_dir,"experiment.csv"), quote=F)
    meta$experiment <- exp
  },
  error = function(e) stop(paste("unable to add experiment table:",e))
  )
  return(meta)
}
