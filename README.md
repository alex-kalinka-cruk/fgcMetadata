# `fgcMetadata`


## Summary

`fgcMetadata` is an `R` package containing functions to build an analysis config JSON file for use with the [AZ-CRUK CRISPR pipeline](https://bitbucket.astrazeneca.com/projects/DA/repos/az-cruk-crispr-pipeline/browse). The package also saves `csv` files containing **non-confidential** metadata tables.

## Installation

```r
# install.packages("devtools")
devtools::install_github("TDLorg/fgcMetadata")
```

## Usage

Build an analysis config object using pipes ('%>%'):

```r
library(fgcMetadata)

# exp_file and samp_file: csv files from bench scientists.
# ci_submission_file: CI submission '.xlsx' file.
md <- start_fgc_project(data_dir = out_dir, 
                        project_id = "fgc_999", 
                        species = "human") %>%
  experiment_metadata(exp_file) %>%
  sample_metadata(samp_file) %>%
  sequencing_metadata(ci_submission_file) %>%
  add_analysis(name = "analysis-1",
               description = "Differential lethality") %>%
  add_plasmid(name = "plasmid_name",
              indices = NA,
              slx_id = "plasmid_name",
              counts_file = "plasmid_counts.txt") %>%
  # Comparisons:
  # (just one below, but any number can be added).
  add_comparison(name = "DiffLeth-Day7",
                 experiment = c("FGC_999_Parental","FGC_999_KnockOut"),
                 plasmid = "plasmid_name",
                 time_point_days = 7,
                 screen_goal = "sensitivity",
                 screen_type = "n",
                 library_annotation = "yusa_v3_human.1") %>%
  remove_unused_samples() %>%
  add_general(user_email = "name@org.uk") %>%
  add_meta()
```

Print a summary to the screen:

```r
print(md)
```

Write an analysis config JSON file:

```r
make_analysis_config_JSON(md, file = "config.JSON")
```

## Bugs, Issues, Requests

Please contact [Alex Kalinka](mailto:alex.kalinka@cancer.org.uk)
