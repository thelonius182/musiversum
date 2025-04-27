pacman::p_load(
  conflicted,
  dplyr,
  fs,
  glue,
  readr,
  stringr,
  tidyr,
  yaml
)
conflict_prefer("pull", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)

# Get command-line arguments
args <- commandArgs(trailingOnly = TRUE)
qfn_resolved_results <- args[1]

# Read the RDS file
resolved_results <- read_rds(qfn_resolved_results)
write_tsv(resolved_results, "rere.tsv")
