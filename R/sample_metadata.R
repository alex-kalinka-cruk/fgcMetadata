# Helper function.
.na_to_empty <- function(x) ifelse(is.na(x), "", x)


#' sample_metadata
#' 
#' Add sample metadata to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param sample_csv A path to a sample '.csv' file.
#' @return Returns an object of class `fgcMeta`. Also saves data in `sample.csv` in `meta$data_dir`.
#' @export
#' @importFrom dplyr %>% rename mutate_all
#' @importFrom stringr str_trim
sample_metadata <- function(meta, sample_csv){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!file.exists(sample_csv)) stop(paste("unable to find",sample_csv))
  
  tryCatch({
    # sample table.
    samp <- read.csv(sample_csv, stringsAsFactors = F) %>%
      dplyr::rename(title = sample_identifier,
                    parent_experiment = parental_CRISPR_screen_cell_model) %>%
      # strip any white-space around sample names and indices.
      dplyr::mutate(title = stringr::str_trim(title),
                    readable_label = stringr::str_trim(readable_label),
                    indexes = stringr::str_trim(indexes),
                    replicate = as.character(replicate)) %>%
      # convert NA entries to empty strings (so they appear in JSON output).
      dplyr::mutate_all(.na_to_empty)
    
    write.csv(samp, file.path(meta$data_dir,"sample.csv"), quote=F, row.names = F)
    meta$sample <- samp
  },
  error = function(e) stop(paste("unable to add sample table:",e))
  )
  return(meta)
}
