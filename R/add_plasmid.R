#' add_plasmid
#' 
#' Adds a plasmid experiment to an analysis in an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param name A plasmid sample name.
#' @param indexes A character string naming the indices, e.g. 'i701-i502'. Can be "NA".
#' @param slx_id The SLX ID. Can be `NULL` if `counts_file` is non-null.
#' @param counts_file A name for the counts file. Can be `NULL` if `slx_id` is non-null.
#' @return Returns an object of class `fgcMeta`. Also appends data to `sequenced_sample.csv` in `meta$data_dir`.
#' @export
add_plasmid <- function(meta, name, indexes, slx_id, counts_file){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  
}