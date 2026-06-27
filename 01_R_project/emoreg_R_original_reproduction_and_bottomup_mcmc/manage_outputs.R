# 输出管理入口：压缩或清理派生结果；不参与统计。 
PROJECT_ROOT <- local({
  find_root <- function(start) {
    start <- normalizePath(start, winslash = "/", mustWork = FALSE)
    if (!dir.exists(start)) start <- dirname(start)
    repeat {
      if (file.exists(file.path(start, ".emoreg_r_project"))) return(start)
      parent <- dirname(start)
      if (identical(parent, start)) stop("Cannot find .emoreg_r_project marker.", call. = FALSE)
      start <- parent
    }
  }
  cmd <- commandArgs(FALSE)
  hit <- grep("^--file=", cmd, value = TRUE)
  start <- if (length(hit)) dirname(sub("^--file=", "", hit[1])) else getwd()  # 不再依赖绝对路径兜底，统一从脚本路径或当前目录回溯项目根。
  find_root(start)
})
setwd(PROJECT_ROOT)
if (!requireNamespace("here", quietly = TRUE)) stop("Package 'here' is required. Run scripts/00_install_r_packages.R first.", call. = FALSE)
suppressMessages(here::i_am("manage_outputs.R"))
PROJECT_ROOT <- here::here()

source(here::here("R", "cli.R"))
source(here::here("R", "project.R"))

args <- parse_cli_args(list(  # action: list/zip/clean-organized；name: 压缩包名；include-raw-data: 是否连 raw 数据一起打包。
  action = "list",
  name = paste0("emoreg_outputs_", format(Sys.time(), "%Y%m%d_%H%M%S")),
  "include-raw-data" = FALSE
))

as_bool <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  tolower(as.character(x)) %in% c("1", "true", "yes", "y")
}

ps_quote <- function(x) {
  paste0("'", gsub("'", "''", normalizePath(x, winslash = "\\", mustWork = FALSE)), "'")
}

project_rel_paths <- function(include_raw_data = FALSE) {
  paths <- c(
    "README.md",
    "config",
    "run_smoke.R",
    "run_full.R",
    "manage_outputs.R",
    "route_A_original_reproduction/scripts",
    "route_A_original_reproduction/outputs",
    "route_B_bottomup_mcmc/scripts",
    "route_B_bottomup_mcmc/outputs",
    "data/processed"
  )
  if (include_raw_data) paths <- c(paths, "data/raw")
  paths[file.exists(file.path(PROJECT_ROOT, paths))]
}

zip_outputs <- function(name, include_raw_data = FALSE) {
  archive_dir <- ensure_dir(project_path("archives"))
  if (!grepl("\\.zip$", name, ignore.case = TRUE)) name <- paste0(name, ".zip")
  zip_path <- file.path(archive_dir, name)
  rel_paths <- project_rel_paths(include_raw_data = include_raw_data)
  abs_paths <- file.path(PROJECT_ROOT, rel_paths)
  if (file.exists(zip_path)) file.remove(zip_path)

  old <- getwd()
  setwd(PROJECT_ROOT)
  on.exit(setwd(old), add = TRUE)

  ok <- FALSE
  try({
    utils::zip(zipfile = zip_path, files = rel_paths, flags = "-r9X")
    ok <- file.exists(zip_path) && file.info(zip_path)$size > 0
  }, silent = TRUE)

  if (!ok && .Platform$OS.type == "windows") {
    ps_paths <- paste(vapply(abs_paths, ps_quote, FUN.VALUE = character(1)), collapse = ",")
    ps_zip <- ps_quote(zip_path)
    cmd <- paste0("Compress-Archive -Path ", ps_paths, " -DestinationPath ", ps_zip, " -Force")
    status <- system2("powershell", c("-NoProfile", "-Command", cmd))
    ok <- identical(as.integer(status), 0L) && file.exists(zip_path) && file.info(zip_path)$size > 0
  }

  if (!ok) stop("Zip failed. Please check whether zip or PowerShell Compress-Archive is available.", call. = FALSE)
  zip_path
}

clean_organized <- function() {
  targets <- c(
    project_path("route_A_original_reproduction", "outputs", "systems"),
    project_path("route_B_bottomup_mcmc", "outputs", "by_k")
  )
  existing <- targets[dir.exists(targets)]
  if (length(existing)) unlink(existing, recursive = TRUE, force = TRUE)
  existing
}

action <- tolower(as.character(args$action))
include_raw <- as_bool(args[["include-raw-data"]])

if (action == "list") {
  cat("Available actions:\n")
  cat("  zip              Compress analysis scripts and outputs into archives/<name>.zip\n")
  cat("  clean-organized  Remove only derived organized folders: Route A outputs/systems and Route B outputs/by_k\n")
  cat("  list             Show this help\n\n")
  cat("Examples:\n")
  cat("  Rscript manage_outputs.R --action zip --name smoke_results\n")
  cat("  Rscript manage_outputs.R --action clean-organized\n")
} else if (action == "zip") {
  out <- zip_outputs(as.character(args$name), include_raw_data = include_raw)
  cat("Archive written:\n")
  cat("  ", out, "\n", sep = "")
} else if (action == "clean-organized") {
  removed <- clean_organized()
  cat("Removed organized folders:\n")
  if (length(removed)) {
    cat(paste0("  ", removed, collapse = "\n"), "\n", sep = "")
  } else {
    cat("  None; organized folders did not exist.\n")
  }
} else {
  stop("Unknown action: ", action, call. = FALSE)
}
