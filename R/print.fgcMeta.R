#' print.fgcMeta
#' 
#' Prints a summary of an `fgcMeta` object to the console.
#' 
#' @param x An object of classs `fgcMeta`.
#' @param ... Arguments to be passed to `print`.
#' @return Prints a summary to the console.
#' @export
print.fgcMeta <- function(x, ...){
  if(!inherits(x,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(x)))
  
  proj <- x$project$title
  num_screens <- 0
  num_samples <- 0
  num_comparisons <- 0
  analysis_name <- "No analysis added"
  if(!is.data.frame(x$experiment)){
    next_fn <- "experiment_metadata()"
  }else if(!is.data.frame(x$sample)){
    num_screens <- nrow(x$experiment)
    next_fn <- "sample_metadata()"
  }else if(!is.data.frame(x$sequencing_request)){
    num_screens <- nrow(x$experiment)
    num_samples <- nrow(x$sample)
    next_fn <- "sequencing_metadata()"
  }else if(!is.character(x$plasmid)){
    num_screens <- nrow(x$experiment)
    num_samples <- nrow(x$sample)
    next_fn <- "add_plasmid()"
  }else if(!is.data.frame(x$analysis)){
    num_screens <- nrow(x$experiment)
    num_samples <- nrow(x$sample)
    next_fn <- "add_analysis()"
  }else if(!is.data.frame(x$comparison)){
    num_screens <- nrow(x$experiment)
    num_samples <- nrow(x$sample)
    analysis_name <- x$analysis$title
    next_fn <- "add_comparison()"
  }else if(!is.list(x$general)){
    num_screens <- nrow(x$experiment)
    num_samples <- nrow(x$sample)
    analysis_name <- x$analysis$title
    num_comparisons <- nrow(x$comparison)
    next_fn <- "add_general()"
  }else{
    num_screens <- nrow(x$experiment)
    num_samples <- nrow(x$sample)
    analysis_name <- x$analysis$title
    num_comparisons <- nrow(x$comparison)
    next_fn <- "make_analysis_config_JSON()"
  }
  cat(paste("*** fgcMeta summary ***\nProject:",proj,"\nAnalysis:",analysis_name,"\nNumber of comparisons:",num_comparisons,"\nNumber of screens:",num_screens,"\nNumber of samples:",num_samples,"\n\nNext function:",next_fn,"\n(you can add more comparisons using 'add_comparison()', but only one analysis is allowed per fgcMeta object)."))
}
