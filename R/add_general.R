#' add_general
#' 
#' Adds general information for use in building an analysis config JSON file.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param user_email An email address.
#' @param process_methoda A character string, defaults to "basep".
#' @param process_methodn A character string, defaults to "base".
#' @export
#' @importFrom jsonlite unbox
add_general <- function(meta, user_email, process_methoda = "basep", process_methodn = "base"){
  meta$general <- list(process_methoda = jsonlite::unbox(process_methoda), 
                       process_methodn = jsonlite::unbox(process_methodn),
                       user_email = jsonlite::unbox(user_email),
                       project_name = jsonlite::unbox(meta$project$title),
                       name = jsonlite::unbox(meta$analysis$title))
  return(meta)
}
