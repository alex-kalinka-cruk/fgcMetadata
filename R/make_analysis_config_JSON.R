#' make_analysis_config_JSON
#' 
#' Writes a pipeline analysis config JSON file for an object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' 
#' @param meta An object of class `fgcMeta`, as produced by `fgcMetadata::start_fgc_project`.
#' @param file A file name to write the config JSON to.
#' @return Writes a JSON file to disk.
#' @export
#' @importFrom dplyr %>% select rename group_by ungroup mutate left_join summarise
#' @importFrom jsonlite toJSON
make_analysis_config_JSON <- function(meta, file){
  if(!inherits(meta,"fgcMeta")) stop(paste("expecting an object of class 'fgcMeta', got:",class(meta)))
  if(!is.data.frame(meta$comparison_sample)) stop("please first run 'add_comparison()' to add comparisons to your analysis")
  if(!is.list(meta$general)) stop("please first run 'add_general()' to add a general section to your metadata")
  
  tryCatch({
    # 1. samples.
    samps <- meta$sample %>%
      dplyr::rename(slx_id = SLX_id, label = readable_label, name = title) %>%
      dplyr::mutate(r2_fastq_file = "", cosmic_id = "") %>%
      dplyr::select(name, class, replicate, r1_fastq_file, indexes, counts_file, treatment, 
                    cell_model, label, slx_id, r2_fastq_file, cosmic_id)
    
    # 2. comparisons.
    comps <- meta$comparison_sample %>%
      dplyr::rename(name = parent_comparison) %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(species = meta$project$species,
                       plasmid_samples = ifelse("plasmid" %in% sample_class, 
                                             list(sequenced_sample[sample_class=="plasmid"]),list()),
                       baseline_samples = ifelse("baseline" %in% sample_class, 
                                              list(sequenced_sample[sample_class=="baseline"]),list()),
                       control_samples = ifelse("control" %in% sample_class, 
                                             list(sequenced_sample[sample_class=="control"]),list()),
                       treatment_samples = ifelse("treatment" %in% sample_class, 
                                               list(sequenced_sample[sample_class=="treatment"]),list())) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(meta$comparison, by = c("name" = "title")) %>%
      dplyr::rename(goal = screen_goal, type = screen_type) %>%
      dplyr::select(-parent_analysis)
    
    # 3. qc.
    qc <- meta$sample %>%
      dplyr::rename(name = title, label = readable_label, slx_id = SLX_id) %>%
      dplyr::left_join(meta$experiment, by = c("parent_experiment" = "title")) %>%
      dplyr::select(slx_id, name, label, indexes, virus_batch, plasmid_batch, cas_activity, 
                    cell_population_doublings, index_plate, index_plate_well_id, library_dna_yield_ng.ul,
                    minimum_split_cell_number, date_transduced, time_point_duration_in_days,
                    total_number_of_splits, percent_transduction_preantibiotic, 
                    percent_transduction_postantibiotic, total_number_of_cells_transduced)
    
    config <- list(samples = samps, comparisons = comps, general = meta$general, meta = meta$meta, qc = qc)
    config_json <- gsub("\\{\\}", "\\[\\]", jsonlite::toJSON(config, pretty = T))
    cat(config_json, file = file)
  },
  error = function(e) stop(paste("unable to make analysis config JSON:",e))
  )
  return(invisible())
}
