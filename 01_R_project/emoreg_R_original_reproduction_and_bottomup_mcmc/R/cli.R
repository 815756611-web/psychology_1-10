# CLI 层：只依赖 base R；作用是把 Rscript 参数解析从分析逻辑里拆出来。 
script_path <- function() {  # 无显式参数；作用：定位当前 Rscript 文件路径，供 set_project_workdir() 向上回溯项目根目录。
  cmd <- commandArgs(FALSE)
  hit <- grep("^--file=", cmd, value = TRUE)
  if (length(hit)) return(normalizePath(sub("^--file=", "", hit[1]), winslash = "/", mustWork = TRUE))
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

set_project_workdir <- function() {  # 无显式参数；作用：把工作目录切到项目根，替代 MATLAB 中每个脚本手工 cd/load 绝对路径。
  sp <- script_path()
  root <- if (file.info(sp)$isdir) sp else dirname(sp)
  repeat {
    if (file.exists(file.path(root, ".emoreg_r_project"))) break
    parent <- dirname(root)
    if (identical(parent, root)) stop("Cannot find .emoreg_r_project marker above script path.")
    root <- parent
  }
  setwd(root)
  if (requireNamespace("here", quietly = TRUE)) suppressMessages(here::i_am("R/cli.R"))  # 切到根目录后初始化 here，后续统一用 here::here() 取相对路径。
  invisible(if (requireNamespace("here", quietly = TRUE)) here::here() else root)
}

parse_cli_args <- function(defaults = list()) {  # defaults: 命令行默认参数列表；作用：解析 --key value/--flag 形式参数。
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

as_int_arg <- function(x, default = NULL) {  # x: 原始参数值；default: 缺失时回退值；作用：把 CLI 字符串安全转成整数。
  if (is.null(x) || isTRUE(is.na(x)) || identical(x, "")) return(default)
  as.integer(x)
}

as_num_arg <- function(x, default = NULL) {  # x: 原始参数值；default: 缺失时回退值；作用：把 CLI 字符串安全转成数值。
  if (is.null(x) || isTRUE(is.na(x)) || identical(x, "")) return(default)
  as.numeric(x)
}
