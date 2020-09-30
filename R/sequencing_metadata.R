#' sequencing_metadata
#' 
#' Add sample metadata to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param ci_xlsx A path to a CI sequencing request `.xlsx` file.
#' @export
#' @importFrom dplyr %>% rename select right_join
#' @importFrom tibble add_column
sequencing_metadata <- function(meta, ci_xlsx){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!file.exists(ci_xlsx)) stop(paste("unable to find",ci_xlsx))
  
  tryCatch({
    par_proj <- meta$project$title
    ci_data <- fgcMetadata::read_ci_submission(ci_xlsx)
    sqr <- ci_data$sequencing_request %>%
      tibble::add_column(parent_project = par_proj, .before = "sample_type") %>%
      dplyr::right_join(metadata_tables$sequencing_request)
    slx <- sqr$slx_identifier[1]
    
    seq_samp <- ci_data$sequenced_sample %>%
      dplyr::mutate(title = sample, parent_sequencing_request = slx) %>%
      dplyr::select(title, parent_sequencing_request, sample, indexes) %>%
      dplyr::right_join(metadata_tables$sequenced_sample)
      
    write.csv(sqr, file.path(meta$data_dir,"sequencing_request.csv"), quote=F)
    write.csv(seq_samp, file.path(meta$data_dir,"sequenced_sample.csv"), quote=F)
    meta$sequencing_request <- sqr
    meta$sequenced_sample <- seq_samp
  },
  error = function(e) stop(paste("unable to add sample table:",e))
  )
  return(meta)
}
