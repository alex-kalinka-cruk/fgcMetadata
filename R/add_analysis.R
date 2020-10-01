#' add_analysis
#' 
#' Adds an analysis to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`. If an analysis already exists, nothing is done as only a single analysis can be associated with a given `fgcMeta` object.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param name A name for the analysis.
#' @param description A sentence describing the aim of the analysis.
#' @return Returns an object of class `fgcMeta`. Also saves data in `analysis.csv` in `meta$data_dir`.
#' @export
add_analysis <- function(meta, name, description){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(is.data.frame(meta$analysis)){
    warning(paste("one analysis allowed per 'fgcMeta' object - an analysis already exists:\n",meta$analysis))
    return(invisible())
  }
  
  tryCatch({
    meta$analysis <- data.frame(title = name, parent_project = meta$project$title,
                                description = description, stringsAsFactors = F)
    write.csv(meta$analysis, file = file.path(meta$data_dir,"analysis.csv"), quote=F, row.names = F)
  },
  error = function(e) stop(paste("unable to add analysis table:",e))
  )
  return(meta)
}
