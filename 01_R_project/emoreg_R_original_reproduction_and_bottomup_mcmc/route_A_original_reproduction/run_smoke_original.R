# Route A 烟雾测试：用极小样本快速检查 MATLAB 对应主流程在 R 里能否从头跑通。 
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
suppressMessages(here::i_am("route_A_original_reproduction/run_smoke_original.R"))
PROJECT_ROOT <- here::here()
source(here::here("R", "runner.R"))

root <- runner_start_root()
runner_run_r("scripts/00_install_r_packages.R", root = root)  # 检查依赖。
runner_run_r("scripts/00_setup_relative_data.R", root = root)  # 准备项目相对原始数据。
runner_run_r("route_A_original_reproduction/scripts/01_prepare_behavior.R", root = root)  # 生成行为清理表。
runner_run_r("route_A_original_reproduction/scripts/02_original_reproduction.R", c("--dataset", "PIP", "--max-subjects", "8", "--bf-method", "jzs_exact", "--tag", "smoke"), root = root)  # 只跑 PIP 的小样本复现。
runner_run_r("route_A_original_reproduction/scripts/04_preview_original_outputs.R", root = root)  # 导出预览图。
runner_run_r("route_A_original_reproduction/scripts/05_organize_system_outputs.R", c("--tags", "smoke"), root = root)  # 整理 smoke 输出。

cat("\nOriginal reproduction smoke run complete.\n")
