#' start_fgc_project
#' 
#' Enter initial data for a project.
#' 
#' @param data_dir A path to a directory where the data tables will be saved.
#' @param project_id A project name (e.g. FGC_0001).
#' @param species A species name.
#' @param dir_entities A path to a directory containing entities in a file called `entities.rds`.
#' 
#' @return Returns an object of class `fgcMeta`. Also saves data in `project.csv` in `data_dir`.
#' @export
start_fgc_project <- function(data_dir, project_id, species, dir_entities = NULL){
  if(!dir.exists(data_dir)) dir.create(data_dir)
  
  tryCatch({
    proj <- data.frame(title = project_id, species = species, stringsAsFactors = F)
    write.csv(proj, file=file.path(data_dir,"project.csv"), quote=F, row.names = F)
  },
  error = function(e) stop(paste("unable to 'start_fgc_project':",e))
  )
  ret <- list(data_dir = data_dir,
              project = proj, experiment = NA, sample = NA, 
              sequencing_request = NA, sequenced_sample = NA,
              analysis = NA, comparison = NA, comparison_sample = NA,
              plasmid = NA)
  class(ret) <- "fgcMeta"
  return(ret)
}
