#' add_plasmid
#' 
#' Adds a plasmid experiment to an analysis in an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param name A plasmid sample name.
#' @param indexes A character string naming the indices, e.g. 'i701-i502'. Can be `NA`.
#' @param slx_id The SLX ID. Can be `NA` if `counts_file` is not `NA`.
#' @param counts_file A name for the counts file. Can be `NA` if `slx_id` is not `NA`.
#' @param description A description of the plasmid. Defaults to `NA`.
#' @return Returns an object of class `fgcMeta`. Also appends data to `sequenced_sample.csv` in `meta$data_dir`.
#' @export
add_plasmid <- function(meta, name, indexes, slx_id, counts_file, description = NA){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!is.data.frame(meta$sequenced_sample)) stop("please first use 'sequencing_metadata() before adding a plasmid'")
  if(is.na(slx_id) & is.na(counts_file)) stop("only one of 'slx_id' and 'counts_file' can be NA")
  if(is.null(slx_id) | is.null(counts_file)) stop("please use 'NA' for missing data for either 'slx_id' or 'counts_file'")
  
  tryCatch({
    pl <- data.frame(title = name, parent_sequencing_request = as.character(slx_id), sample = name,
                     indexes = as.character(indexes), r1_fastq = "NA", r2_fastq = "NA",
                     counts_file = as.character(counts_file), 
                     external_count_file = "NA", external_library = "NA", description = as.character(description),
                     stringsAsFactors = F)
    meta$sequenced_sample <- rbind(meta$sequenced_sample, pl)
    if(is.na(meta$plasmid)){
      meta$plasmid <- name
    }else{
      meta$plasmid <- append(meta$plasmid, name)
    }
    write.table(meta$sequenced_sample, file.path(meta$data_dir,"sequenced_sample.csv"), 
                sep = ",", quote=F, col.names = F, row.names = F, append = T)
  },
  error = function(e) stop(paste("unable to add plasmid data:",e))
  )
  return(meta)
}
