# 公共路径/配置层：统一改成 here::here() + 相对配置；对应 MATLAB 中大量硬编码 load/save 路径，这里集中管理。 
find_project_root <- function(start = getwd()) {  # start: 从哪个目录开始向上找 .emoreg_r_project；作用：在 here 初始化前先用 base R 找到项目根目录。
  here <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(here, ".emoreg_r_project"))) return(here)
    parent <- dirname(here)
    if (identical(parent, here)) stop("Cannot find .emoreg_r_project marker.")
    here <- parent
  }
}

init_here_root <- function(start = getwd()) {  # start: 初始查找位置；作用：初始化 here 根目录并返回规范化后的项目根。
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package 'here' is required. Run scripts/00_install_r_packages.R first.", call. = FALSE)
  }
  root <- find_project_root(start)
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(root)
  suppressMessages(here::i_am("R/project.R"))
  normalizePath(here::here(), winslash = "/", mustWork = TRUE)
}

PROJECT_ROOT <- init_here_root()

project_path <- function(...) {  # ...: 追加到项目根目录后的子路径片段；作用：统一通过 here::here() 生成跨机器稳定路径。
  here::here(...)
}

resolve_project_path <- function(path, base = PROJECT_ROOT, mustWork = FALSE) {  # path: 配置里的路径字符串；base: 相对路径基准；mustWork: 是否要求路径已存在。
  if (is.null(path) || !length(path) || is.na(path) || !nzchar(path)) return(path)
  is_abs <- grepl("^([A-Za-z]:|/|\\\\)", path)
  target <- if (is_abs) path else file.path(base, path)
  normalizePath(target, winslash = "/", mustWork = mustWork)
}

read_simple_config <- function(path = project_path("config", "default.yml")) {  # path: YAML 配置文件路径；作用：读取简单 key:value 配置，供 Route A/B 共用。
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  out <- list()
  for (line in lines) {
    parts <- strsplit(line, ":", fixed = TRUE)[[1]]
    key <- trimws(parts[1])
    value <- trimws(paste(parts[-1], collapse = ":"))
    value <- gsub("^['\"]|['\"]$", "", value)
    parsed <- suppressWarnings(as.numeric(value))
    if (!is.na(parsed) && grepl("^[+-]?[0-9.]+([eE][+-]?[0-9]+)?$", value)) {
      out[[key]] <- parsed
    } else {
      out[[key]] <- value
    }
  }
  if ("source_data_root" %in% names(out)) out$source_data_root <- resolve_project_path(out$source_data_root, mustWork = FALSE)  # 把配置中的相对数据源路径解析成 here 根目录下的真实路径。
  out
}

CFG <- read_simple_config()

ensure_dir <- function(path) {  # path: 需要存在的目录；作用：统一创建输出目录并返回原路径，便于链式调用。
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

ensure_output_dirs <- function() {  # 无显式参数；作用：预创建 Route A/B、data/processed、logs 等标准输出目录。
  dirs <- c(
    project_path("route_A_original_reproduction"),
    project_path("route_A_original_reproduction", "scripts"),
    project_path("route_A_original_reproduction", "outputs"),
    project_path("route_A_original_reproduction", "outputs", "nifti"),
    project_path("route_A_original_reproduction", "outputs", "tables"),
    project_path("route_A_original_reproduction", "outputs", "preview"),
    project_path("route_A_original_reproduction", "outputs", "preview", "png"),
    project_path("route_B_bottomup_mcmc"),
    project_path("route_B_bottomup_mcmc", "scripts"),
    project_path("route_B_bottomup_mcmc", "outputs"),
    project_path("route_B_bottomup_mcmc", "outputs", "tables"),
    project_path("route_B_bottomup_mcmc", "outputs", "figures"),
    project_path("route_B_bottomup_mcmc", "outputs", "nifti"),
    project_path("data", "processed"),
    project_path("logs")
  )
  invisible(lapply(dirs, ensure_dir))
}

ensure_output_dirs()

suppressPackageStartupMessages({
  library(data.table)  # data.table：项目主数据表包，用于 fread/fwrite、分组汇总、宽长表转换；MATLAB 对应 table/矩阵整理但这里更高效。
})
