#' read_ci_submission
#'
#' Read in data from a CIGC sequencing submission spreadsheet.
#'
#' @param file A path to a CIGC sequencing submission `.xlsx` file.
#' @return A named list containing the header information in `header` and a data frame in `samples` containing the samples and their indices.
#' @export
#' @importFrom openxlsx read.xlsx
#' @importFrom dplyr %>% select rename
#' @importFrom magrittr %<>%
read_ci_submission <- function(file){
  if(!file.exists(file)) stop(paste("unable to find",file))
  
  tryCatch({
    ci_data <- openxlsx::read.xlsx(file)
    # Header for 'sequencing_request' table.
    header_ids <- ci_data$`1`[7:26]
    sr_data <- ci_data$X3[7:26]
    if(!all(header_ids == ci_header_ids))
      stop(paste("differences in expected CI header IDs:\n Got:\n",
                 paste(header_ids,collapse=", "),"\nExpected:\n",paste(ci_header_ids,collapse=", ")))

    sqr <- data.frame(title = sr_data[1], sample_type = sr_data[14],
                      sample_source = sr_data[15], library_type = sr_data[8],
                      species = sr_data[16], slx_identifier = sr_data[1],
                      workflow = sr_data[3], sequencing_type = sr_data[4],
                      read_length = sr_data[5], number_of_lanes = sr_data[6],
                      index_type = sr_data[9], pool_size = sr_data[2],
                      av_library_length = sr_data[11], billing_info = sr_data[17],
                      po_number = sr_data[18], priority_status = sr_data[19])
    
    # Samples and indices for 'sequenced_sample' table.
    start_index <- grep("<Sample.Start>",ci_data[,1])
    if(length(start_index) == 0)
      stop(paste("unable to find sample start ('<Sample.Start>') in",file))
    cnames <- ci_data[start_index,]
    if(!"Name" %in% cnames || !"Index" %in% cnames)
      stop(paste("unable to find sample data columns in",file,":",cnames))
    data <- ci_data[(start_index+1):nrow(ci_data),]
    colnames(data) <- cnames
    rownames(data) <- NULL
    data %<>% 
      dplyr::select(Name, Index) %>%
      dplyr::rename(sample = Name, indexes = Index)
  },
  error = function(e) stop(paste("unable to extract data from ci submission:",e))
  )
  return(list(sequencing_request = sqr,
              sequenced_sample = data))
}
