#' make_analysis_config_JSON
#' 
#' Writes a pipeline analysis config JSON file for an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param file A file name to write the config JSON to.
#' @return Writes a JSON file to disk.
#' @export
#' @importFrom dplyr %>% rename select mutate
make_analysis_config_JSON <- function(meta, file){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!is.data.frame(meta$comparison_sample)) stop("please first run 'add_comparison()' to add comparisons to your analysis")
  
  tryCatch({
    # 1. samples.
    samps <- meta$sample %>%
      dplyr::rename(slx_id = SLX_id, label = readable_label, name = title) %>%
      dplyr::mutate(cosmic_id = "NA", r2_fastq_file = "NA") %>%
      dplyr::select(replicate, r1_fastq_file, indexes, counts_file, treatment, 
                    cell_model, label, slx_id, r2_fastq_file, class, name)
    
    # 2. comparisons.
    comps <- meta$comparison_sample %>%
      dplyr::group_by(parent_comparison)
  },
  error = function(e) stop(paste("unable to make analysis config JSON:",e))
  )
}
