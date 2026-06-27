# 交互入口层：只依赖 base R；作用是从 run_full/run_smoke 这类总入口调度子脚本。 
runner_project_root <- function(start = getwd()) {  # start: 起始目录或脚本路径；作用：向上定位 .emoreg_r_project，得到项目根目录。
  start <- normalizePath(start, winslash = "/", mustWork = FALSE)
  if (!dir.exists(start)) start <- dirname(start)
  repeat {
    if (file.exists(file.path(start, ".emoreg_r_project"))) return(start)
    parent <- dirname(start)
    if (identical(parent, start)) stop("Cannot find .emoreg_r_project marker.", call. = FALSE)
    start <- parent
  }
}

runner_script_path <- function() {  # 无显式参数；作用：在 Rscript 或 source 场景下尽量定位当前脚本文件。
  cmd <- commandArgs(FALSE)
  hit <- grep("^--file=", cmd, value = TRUE)
  if (length(hit)) return(normalizePath(sub("^--file=", "", hit[1]), winslash = "/", mustWork = TRUE))
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE))
  }
  NA_character_
}

runner_start_root <- function(fallback = getwd()) {  # fallback: 无法识别脚本路径时的兜底起点；作用：切换工作目录到项目根。
  sp <- runner_script_path()
  root <- if (!is.na(sp)) runner_project_root(dirname(sp)) else runner_project_root(fallback)
  setwd(root)
  root
}

runner_rscript <- function() {  # 无显式参数；作用：返回当前 R 安装中的 Rscript 可执行文件路径。
  exe <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  normalizePath(exe, winslash = "/", mustWork = TRUE)
}

runner_run_r <- function(script, args = character(), root = getwd()) {  # script: 子脚本相对路径；args: 传给子脚本的命令行参数；root: 项目根目录。
  script_path <- file.path(root, script)
  if (!file.exists(script_path)) stop("Script not found: ", script_path, call. = FALSE)
  cat("\n>>> Rscript ", script, if (length(args)) paste0(" ", paste(args, collapse = " ")) else "", "\n", sep = "")
  flush.console()
  status <- system2(runner_rscript(), c(normalizePath(script_path, winslash = "/", mustWork = TRUE), args))
  if (!identical(as.integer(status), 0L)) {
    stop("Command failed with exit code ", status, ": Rscript ", script, call. = FALSE)
  }
  invisible(TRUE)
}

runner_ask_choice <- function(prompt, choices, default = choices[1]) {  # prompt: 提示文本；choices: 允许输入；default: 默认值；作用：入口脚本交互选 Route。
  choices <- toupper(choices)
  default <- toupper(default)
  repeat {
    ans <- toupper(trimws(readline(prompt)))
    if (!nzchar(ans) && !is.null(default)) ans <- default
    if (ans %in% choices) return(ans)
    cat("Please enter: ", paste(choices, collapse = " / "), "\n", sep = "")
  }
}

runner_ask_yes_no <- function(prompt, default = FALSE) {  # prompt: 提示文本；default: 默认布尔值；作用：交互决定是否清理输出。
  default_label <- if (isTRUE(default)) "Y" else "N"
  repeat {
    ans <- toupper(trimws(readline(prompt)))
    if (!nzchar(ans)) ans <- default_label
    if (ans %in% c("Y", "YES")) return(TRUE)
    if (ans %in% c("N", "NO")) return(FALSE)
    cat("Please enter Y or N.\n")
  }
}

runner_cli_args <- function(defaults = list()) {  # defaults: 默认参数列表；作用：给 run_full/run_smoke 这类入口解析命令行。
  args <- commandArgs(trailingOnly = TRUE)
  out <- defaults
  i <- 1L
  while (i <= length(args)) {
    key <- args[i]
    if (!startsWith(key, "--")) {
      i <- i + 1L
      next
    }
    key <- sub("^--", "", key)
    if (i == length(args) || startsWith(args[i + 1L], "--")) {
      out[[key]] <- TRUE
      i <- i + 1L
    } else {
      out[[key]] <- args[i + 1L]
      i <- i + 2L
    }
  }
  out
}

runner_as_bool <- function(x, default = FALSE) {  # x: 待解释的参数；default: 缺失时回退布尔值；作用：把 true/1/yes 等文本转布尔值。
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  if (is.logical(x)) return(isTRUE(x))
  y <- tolower(trimws(as.character(x)))
  if (!nzchar(y)) return(default)
  y %in% c("1", "true", "t", "yes", "y")
}

runner_clean_output_dir <- function(path, label = path) {  # path: 要清空重建的目录；label: 终端展示名称；作用：只清导出目录，不动原始数据。
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  cat("Cleaned output: ", label, "\n", sep = "")
  invisible(path)
}
