# 烟雾测试入口：用小样本快速检查路径、依赖和核心输出是否能跑通。 
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
suppressMessages(here::i_am("run_smoke.R"))
PROJECT_ROOT <- here::here()
source(here::here("R", "runner.R"))

root <- runner_start_root()
runner_run_r("scripts/00_install_r_packages.R", root = root)  # 检查并安装 Route A/B 依赖。
runner_run_r("scripts/00_setup_relative_data.R", root = root)  # 把外部源数据镜像到项目相对目录。
runner_run_r("route_A_original_reproduction/scripts/01_prepare_behavior.R", root = root)  # 清理行为表。
runner_run_r("route_A_original_reproduction/scripts/02_original_reproduction.R", c("--dataset", "PIP", "--max-subjects", "8", "--bf-method", "bic_approx", "--tag", "smoke"), root = root)  # 小样本 Route A 核心复现。
runner_run_r("route_A_original_reproduction/scripts/04_preview_original_outputs.R", root = root)  # 生成 Route A 预览。
runner_run_r("route_A_original_reproduction/scripts/05_organize_system_outputs.R", c("--tags", "smoke"), root = root)  # 整理四系统输出。
runner_run_r("route_B_bottomup_mcmc/scripts/04_prepare_bottomup_features.R", c("--unit", "roi", "--max-subjects-per-study", "4", "--roi-source", "downloaded", "--tag", "smoke"), root = root)  # 小样本 Route B 特征抽取。
runner_run_r("route_B_bottomup_mcmc/scripts/05_bottomup_mcmc.R", c("--features", "route_B_bottomup_mcmc/outputs/tables/roi_subject_features_smoke.rds", "--iter", "250", "--burn", "100", "--thin", "5", "--k", "4", "--tag", "smoke"), root = root)  # 小链长 MCMC。
runner_run_r("route_B_bottomup_mcmc/scripts/06_interpret_mcmc_regions.R", c("--posterior", "route_B_bottomup_mcmc/outputs/tables/mcmc_posterior_smoke.csv", "--features", "route_B_bottomup_mcmc/outputs/tables/roi_subject_features_smoke.rds", "--tag", "smoke"), root = root)  # 解释聚类结果。
runner_run_r("route_B_bottomup_mcmc/scripts/07_organize_k_cluster_outputs.R", c("--tag", "smoke"), root = root)  # 按 K/cluster 整理 Route B 输出。

cat("\nSmoke run complete.\n")
