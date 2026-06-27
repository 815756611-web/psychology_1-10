# Route A 总入口：对应 MATLAB 的 BayesfactorMap_MainAnalysis.m + ClusterControl.m + CreateConsensusMap.m 串联版。 
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
suppressMessages(here::i_am("route_A_original_reproduction/run_full_original.R"))
PROJECT_ROOT <- here::here()
source(here::here("R", "runner.R"))  # 只依赖 base R 的调度层；用于顺序运行 Route A 子脚本。

run_route_a_dataset <- function(dataset, root) {  # dataset: AHAB/PIP；root: 项目根目录；作用：对单个数据库运行核心 voxel 复现。
  runner_run_r(
    "route_A_original_reproduction/scripts/02_original_reproduction.R",
    c("--dataset", dataset, "--bf-method", "jzs_exact", "--tag", dataset),
    root = root
  )
}

root <- runner_start_root()
args <- runner_cli_args(list("clean-output" = NA))  # clean-output: 是否先删除 Route A outputs。
clean_arg <- args[["clean-output"]]
clean <- if (is.na(clean_arg)) {
  runner_ask_yes_no("Clean Route A outputs before full rerun? [Y/N], default N: ", default = FALSE)
} else {
  runner_as_bool(clean_arg, default = FALSE)
}

if (clean) {
  runner_clean_output_dir(file.path(root, "route_A_original_reproduction", "outputs"), "Route A outputs")
}

runner_run_r("scripts/00_install_r_packages.R", root = root)  # 安装/检查依赖。
runner_run_r("scripts/00_setup_relative_data.R", root = root)  # 镜像原始数据到 data/raw。
runner_run_r("route_A_original_reproduction/scripts/01_prepare_behavior.R", root = root)  # 清理行为数据，供后续验证分析。

for (dataset in c("AHAB", "PIP")) {
  run_route_a_dataset(dataset, root)
}

runner_run_r("route_A_original_reproduction/scripts/03_consensus_and_behavior.R", root = root)  # 对应 MATLAB CreateConsensusMap.m，但额外输出 Markdown/CSV。
runner_run_r("route_A_original_reproduction/scripts/04_preview_original_outputs.R", root = root)  # 快速预览 NIfTI。
runner_run_r("route_A_original_reproduction/scripts/05_organize_system_outputs.R", c("--tags", "AHAB,PIP,AHAB_PIP"), root = root)  # 把四系统和 ROI 连通簇整理成可读目录。

cat("\nOriginal reproduction full run complete for dataset(s): AHAB, PIP\n")
