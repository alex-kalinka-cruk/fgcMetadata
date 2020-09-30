#' sample_metadata
#' 
#' Add sample metadata to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param sample_csv A path to a sample '.csv' file.
#' @export
#' @importFrom dplyr %>% rename right_join
sample_metadata <- function(meta, sample_csv){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!file.exists(sample_csv)) stop(paste("unable to find",sample_csv))
  tryCatch({
    samp <- read.csv(sample_csv, stringsAsFactors = F) %>%
      dplyr::rename(title = sample_identifier,
             parent_experiment = parental_CRISPR_screen_cell_model) %>%
      dplyr::right_join(metadata_tables$sample)
    write.csv(samp, file.path(meta$data_dir,"sample.csv"), quote=F)
    meta$sample <- samp
  },
  error = function(e) stop(paste("unable to add sample table:",e))
  )
  return(meta)
}