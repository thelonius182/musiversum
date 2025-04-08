message("using UBU-config")

pacman::p_load(httr2, jsonlite, readr, stringr, futile.logger, dplyr,
               purrr, glue, tidyr, yaml, shiny, git2r, lubridate)
source("R/utils.R", encoding = "UTF-8")
config <- read_yaml("config_ubu.yaml")

lg_ini <- flog.appender(appender.file(config$log_file), name = config$log_slug)
git_info <- salsa_git_version(getwd())
flog.info(">>> START", name = config$log_slug)
flog.info(glue("git-branch: {git_info$git_branch}"), name = config$log_slug)
flog.info(glue("  commited: {git_info$ts}"), name = config$log_slug)
flog.info(glue("        by: {git_info$by}"), name = config$log_slug)
flog.info(glue("local repo: {git_info$path}"), name = config$log_slug)
flog.info(glue("  using db: {config$wpdb_env}"), name = config$log_slug)
