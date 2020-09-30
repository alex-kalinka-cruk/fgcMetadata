#' add_comparison
#' 
#' Adds a comparison to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param name A name for the comparison.
#' @param experiment The name of one or more experiments in `meta$experiment$title`.
#' @param screen_goal A screen goal, e.g. "lethality" or "sensitivity".
#' @param screen_type A single letter designating the screen type: "n" (knock-out), "a" (activation), or "i" (interference).
#' @param library_annotation The name of the library annotation, e.g. "yusa_v3_human.1".
#' @return Returns an object of class `fgcMeta`. Also saves/appends data to `comparison.csv` in `meta$data_dir`.
#' @export
add_comparison <- function(meta, name, experiment, plasmid, screen_goal, screen_type, library_annotation){
  tryCatch({
    par_analysis <- meta$analysis$title
    
  },
  error = function(e) stop(paste("unable to add comparison to comparison table:",e))
  )
  return(meta)
}
