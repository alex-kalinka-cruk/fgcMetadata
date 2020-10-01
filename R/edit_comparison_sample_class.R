#' edit_comparison_sample_class
#' 
#' Make changes to the `class` of specific comparison samples to allow bespoke analysis comparisons.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param name A name for the comparison.
#' @param samples `sequenced_sample` IDs for the samples whose class is to be changed.
#' @param new_class The new class.
#' @return Returns an object of class `fgcMeta`. Also saves data to `comparison_sample.csv` in `meta$data_dir`.
#' @export
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
edit_comparison_sample_class <- function(meta, name, samples, new_class){
  if(!name %in% meta$comparison_sample$parent_comparison) 
    stop(paste("unable to find comparison",name,"in meta$comparison_sample$parent_comparison for editing sample classes"))
  if(!all(samples %in% meta$comparison_sample$sequenced_sample))
    stop(paste("unable to find comparison samples for editing in meta$comparison_sample$sequenced_sample:",
               paste(samples,collapse=", ")))
  tryCatch({
    meta$comparison_sample %<>%
      mutate(sample_class = ifelse((sequenced_sample %in% samples & parent_comparison == name), 
                                   new_class, sample_class))
    write.csv(meta$comparison_sample, file=file.path(meta$data_dir,"comparison_sample.csv"), quote=F, row.names = F)
  },
  error = function(e) stop(paste("unable to edit comparison sample classes:",e))
  )
  return(meta)
}
