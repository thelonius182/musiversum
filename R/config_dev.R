message("using DEV-config")

pacman::p_load(
  bslib,
  callr,
  conflicted,
  data.table,
  dplyr,
  DT,
  fs,
  futile.logger,
  git2r,
  glue,
  httr2,
  jsonlite,
  keyring,
  lubridate,
  purrr,
  readr,
  RMySQL,
  shiny,
  shinyjs,
  stringr,
  tidyr,
  yaml
)
conflict_prefer("pull", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)
source("R/utils.R", encoding = "UTF-8")
config <- read_yaml("config_dev.yaml")

db_host <- key_get(service = "sql-wpprd_host")
db_user <- key_get(service = "sql-wpprd_user")
db_password <- key_get(service = "sql-wpprd_pwd")
db_name <- key_get(service = "sql-wpprd_db")

lg_ini <- flog.appender(appender.file(config$log_file), name = config$log_slug)
git_info <- salsa_git_version(getwd())
flog.info(">>> START", name = config$log_slug)
flog.info(glue("git-branch: {git_info$git_branch}"), name = config$log_slug)
flog.info(glue("  commited: {git_info$ts}"), name = config$log_slug)
flog.info(glue("        by: {git_info$by}"), name = config$log_slug)
flog.info(glue("local repo: {git_info$path}"), name = config$log_slug)
flog.info(glue("  using db: {config$wpdb_env}"), name = config$log_slug)
