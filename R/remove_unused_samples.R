#' remove_unused_samples
#' 
#' Remove any samples that are not used in any comparisons.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @return Returns an object of class `fgcMeta`.
#' @export
#' @importFrom magrittr %<>%
#' @importFrom dplyr filter
remove_unused_samples <- function(meta){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!is.data.frame(meta$comparison_sample)) stop("please first run add_comparison()")
  
  tryCatch({
    # Samples not used in any comparisons.
    missing_samps <- setdiff(meta$sample$title, meta$comparison_sample$sequenced_sample)
    if(length(missing_samps) > 0){
      meta$sample %<>%
        dplyr::filter(!title %in% missing_samps)
    }
  },
  error = function(e) stop(paste("unable to remove samples not used in any comparisons:",e))
  )
  return(meta)
}
