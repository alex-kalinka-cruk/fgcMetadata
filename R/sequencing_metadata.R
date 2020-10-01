#' sequencing_metadata
#' 
#' Add sample metadata to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param ci_xlsx A path to a CI sequencing request `.xlsx` file.
#' @return Returns an object of class `fgcMeta`. Also saves data in `sequencing_request.csv` and `sequenced_sample.csv` in `meta$data_dir`.
#' @export
#' @importFrom dplyr %>% rename select
#' @importFrom tibble add_column
sequencing_metadata <- function(meta, ci_xlsx){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!file.exists(ci_xlsx)) stop(paste("unable to find",ci_xlsx))
  
  tryCatch({
    par_proj <- meta$project$title
    ci_data <- fgcMetadata::read_ci_submission(ci_xlsx)
    sqr <- ci_data$sequencing_request %>%
      tibble::add_column(parent_project = par_proj, .before = "sample_type")
    slx <- sqr$slx_identifier[1]
    
    seq_samp <- ci_data$sequenced_sample %>%
      dplyr::mutate(title = sample, parent_sequencing_request = slx, 
                    r1_fastq = "NA", r2_fastq = "NA", counts_file = "NA",
                    external_count_file = "NA", external_library = "NA",
                    description = "NA") %>%
      dplyr::select(title, parent_sequencing_request, sample, indexes,
                    r1_fastq, r2_fastq, counts_file, external_count_file, 
                    external_library, description) 
      
    write.csv(sqr, file.path(meta$data_dir,"sequencing_request.csv"), quote=F, row.names = F)
    write.csv(seq_samp, file.path(meta$data_dir,"sequenced_sample.csv"), quote=F, row.names = F)
    meta$sequencing_request <- sqr
    meta$sequenced_sample <- seq_samp
  },
  error = function(e) stop(paste("unable to add sequencing_metadata tables:",e))
  )
  return(meta)
}
