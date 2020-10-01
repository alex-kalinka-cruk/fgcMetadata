#' sample_metadata
#' 
#' Add sample metadata to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param sample_csv A path to a sample '.csv' file.
#' @return Returns an object of class `fgcMeta`. Also saves data in `sample.csv` in `meta$data_dir`.
#' @export
#' @importFrom dplyr %>% rename
sample_metadata <- function(meta, sample_csv){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!file.exists(sample_csv)) stop(paste("unable to find",sample_csv))
  
  tryCatch({
    samp <- read.csv(sample_csv, stringsAsFactors = F) %>%
      dplyr::rename(title = sample_identifier,
                    parent_experiment = parental_CRISPR_screen_cell_model)
    write.csv(samp, file.path(meta$data_dir,"sample.csv"), quote=F, row.names = F)
    meta$sample <- samp
  },
  error = function(e) stop(paste("unable to add sample table:",e))
  )
  return(meta)
}
