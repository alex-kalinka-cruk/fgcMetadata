#' add_plasmid
#' 
#' Adds a plasmid experiment to an analysis in an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param name A plasmid sample name.
#' @param indices A character string naming the indices, e.g. 'i701-i502'. Can be `NA`.
#' @param slx_id The SLX ID. Can be `NA` if `counts_file` is not `NA`.
#' @param counts_file A name for the counts file. Can be `NA` if `slx_id` is not `NA`.
#' @param description A description of the plasmid. Defaults to `NA`.
#' @return Returns an object of class `fgcMeta`. Also appends data to `sample.csv` and `sequenced_sample.csv` in `meta$data_dir`.
#' @export
#' @importFrom dplyr bind_rows mutate_all
add_plasmid <- function(meta, name, indices, slx_id, counts_file, description = NA){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!is.data.frame(meta$sequenced_sample)) stop("please first use 'sequencing_metadata() before adding a plasmid'")
  if(is.na(slx_id) & is.na(counts_file)) stop("only one of 'slx_id' and 'counts_file' can be NA")
  if(is.null(slx_id) | is.null(counts_file)) stop("please use 'NA' for missing data for either 'slx_id' or 'counts_file'")
  
  tryCatch({
    # sample table.
    indices <- ifelse(is.na(indices),"NA",indices)
    meta$sample <- meta$sample %>%
      dplyr::bind_rows(data.frame(title = name, indexes = indices, readable_label = name,
                                  SLX_id = as.character(slx_id), counts_file = as.character(counts_file),
                                  class = "plasmid", replicate = "1", stringsAsFactors = F)) %>%
      # convert NA entries to empty strings (so they appear in JSON output).
      dplyr::mutate_all(.na_to_empty)
    
    # sequenced_sample table.
    pl_seq <- data.frame(title = name, parent_sequencing_request = as.character(slx_id), sample = name,
                     indexes = as.character(indices), r1_fastq = "NA", r2_fastq = "NA",
                     counts_file = as.character(counts_file), 
                     external_count_file = "NA", external_library = "NA", description = as.character(description),
                     stringsAsFactors = F)
    meta$sequenced_sample <- rbind(meta$sequenced_sample, pl_seq)
    
    if(is.na(meta$plasmid)){
      meta$plasmid <- name
    }else{
      meta$plasmid <- append(meta$plasmid, name)
    }
    write.table(meta$sample, file.path(meta$data_dir,"sample.csv"), 
                sep = ",", quote=F, col.names = F, row.names = F, append = T)
    write.table(meta$sequenced_sample, file.path(meta$data_dir,"sequenced_sample.csv"), 
                sep = ",", quote=F, col.names = F, row.names = F, append = T)
  },
  error = function(e) stop(paste("unable to add plasmid data:",e))
  )
  return(meta)
}
