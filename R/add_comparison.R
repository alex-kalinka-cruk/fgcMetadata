#' add_comparison
#' 
#' Adds a comparison to an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param name A name for the comparison.
#' @param experiment The name of one or more experiments in `meta$experiment$title`.
#' @param plasmid A plasmid name - must exist in `meta$plasmid`.
#' @param time_point_days An integer giving the `time_point_duration_in_days` in `experiment`.
#' @param screen_goal A screen goal, e.g. "lethality" or "sensitivity".
#' @param screen_type A single letter designating the screen type: "n" (knock-out), "a" (activation), or "i" (interference).
#' @param library_annotation The name of the library annotation, e.g. "yusa_v3_human.1".
#' @param class_change Treat a given sample class as a different class, e.g. to treat baseline samples as controls: c("baseline" = "control").
#' @return Returns an object of class `fgcMeta`. Also saves/appends data to `comparison.csv` and `comparison_sample.csv` in `meta$data_dir`.
#' @export
#' @importFrom dplyr %>% filter left_join bind_rows
add_comparison <- function(meta, name, experiment, plasmid, time_point_days, screen_goal, screen_type, library_annotation, 
                           class_change = NULL){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!plasmid %in% meta$plasmid) stop(paste("unable to find",plasmid,"in 'meta$plasmid', please first run 'add_plasmid()'"))
  if(!is.numeric(time_point_days)) stop("'time_point_days' must be an integer")
  if(!all(experiment %in% meta$experiment$title))
    stop(paste("unable to find",experiment,"in 'meta$experiment$title'"))
  
  tryCatch({
    # comparison table.
    par_analysis <- meta$analysis$title
    comp <- data.frame(title = name, parent_analysis = par_analysis, 
                       screen_goal = screen_goal, screen_type = screen_type,
                       library_annotation = library_annotation)
    if(is.na(meta$comparison)){
      meta$comparison <- comp
      write.csv(meta$comparison, file=file.path(meta$data_dir,"comparison.csv"), quote=F)
    }else{
      meta$comparison <- rbind(meta$comparison, comp)
      write.table(meta$comparison, file=file.path(meta$data_dir,"comparison.csv"), 
                  sep = ",", quote=F, col.names = F, row.names = F, append = T)
    }
    
    # comparison_sample table.
    samps <- meta$sample %>%
      dplyr::filter(parent_experiment %in% experiment & time_point_duration_in_days == time_point_days) %>%
      dplyr::bind_rows(data.frame(title = plasmid, readable_label = plasmid,
                                  class = "plasmid", time_point_duration_in_days = as.character(time_point_days),
                                  replicate = "1", stringsAsFactors = F)) %>%
      dplyr::left_join(meta$sequenced_sample, by = c("title" = "sample"))

    if(nrow(samps) == 0) stop(paste("no samples found for comparison",name))
    csamps <- data.frame(title = paste(name,samps$time_point_duration_in_days,"days",samps$class,samps$replicate,sep="-"),
                         parent_comparison = name, sequenced_sample = samps$title,
                         sample_class = samps$class, stringsAsFactors = F)
    
    if(is.na(meta$comparison_sample)){
      meta$comparison_sample <- csamps
      write.csv(meta$comparison_sample, file=file.path(meta$data_dir,"comparison_sample.csv"), quote=F, row.names = F)
    }else{
      meta$comparison_sample <- rbind(meta$comparison_sample, csamps)
      write.table(meta$comparison_sample, file=file.path(meta$data_dir,"comparison_sample.csv"), 
                  sep = ",", quote=F, col.names = F, row.names = F, append = T)
    }
  },
  error = function(e) stop(paste("unable to add comparison to comparison table:",e))
  )
  return(meta)
}
