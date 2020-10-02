#' add_meta
#' 
#' Adds a `meta` section (after all comparisons have been added) to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @return Returns an object of class `fgcMeta`. Also saves data in `sample.csv` in `meta$data_dir`.
#' @export
#' @importFrom dplyr %>% rename select mutate
add_meta <- function(meta){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  
  tryCatch({
    meta$meta <- meta$comparison_sample %>%
      dplyr::rename(name = parent_comparison, sample = sequenced_sample, class = sample_class) %>%
      dplyr::mutate(type = meta$comparison$screen_type[match(name,meta$comparison$title)],
                    batch = "") %>%
      dplyr::select(-title)
    },
    error = function(e) stop(paste("unable to add a meta section:",e))
  )
  return(meta)
}
