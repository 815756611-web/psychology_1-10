# Route B 总入口：ROI-MCMC 层级混合聚类工作流。 
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
suppressMessages(here::i_am("route_B_bottomup_mcmc/run_full_bottomup.R"))
PROJECT_ROOT <- here::here()
source(here::here("R", "runner.R"))  # 交互式子脚本调度层。

run_route_b_dataset <- function(dataset, root) {  # dataset: AHAB/PIP；root: 项目根目录；作用：依次执行 ROI 特征、MCMC、解释和整理。
  tag <- dataset
  features <- paste0("route_B_bottomup_mcmc/outputs/tables/roi_subject_features_", tag, ".rds")
  posterior <- paste0("route_B_bottomup_mcmc/outputs/tables/mcmc_posterior_", tag, ".csv")

  runner_run_r(  # 第一步：把原文系统图切成互斥 ROI，并抽取被试×ROI 特征。
    "route_B_bottomup_mcmc/scripts/04_prepare_bottomup_features.R",
    c("--unit", "roi", "--dataset", dataset, "--roi-source", "downloaded", "--tag", tag),
    root = root
  )
  runner_run_r(  # 第二步：比较 K=2:6 并保存后验结果。
    "route_B_bottomup_mcmc/scripts/05_bottomup_mcmc.R",
    c("--features", features, "--iter", "4000", "--burn", "2000", "--thin", "1", "--k-grid", "2:6", "--tag", tag),
    root = root
  )
  runner_run_r(  # 第三步：把 posterior 和行为验证整理成解释性表格。
    "route_B_bottomup_mcmc/scripts/06_interpret_mcmc_regions.R",
    c("--posterior", posterior, "--features", features, "--tag", tag),
    root = root
  )
  runner_run_r("route_B_bottomup_mcmc/scripts/07_organize_k_cluster_outputs.R", c("--tag", tag), root = root)
}

root <- runner_start_root()
args <- runner_cli_args(list("clean-output" = NA))  # clean-output: 是否重建 Route B outputs。
clean_arg <- args[["clean-output"]]
clean <- if (is.na(clean_arg)) {
  runner_ask_yes_no("Clean Route B outputs before full rerun? [Y/N], default N: ", default = FALSE)
} else {
  runner_as_bool(clean_arg, default = FALSE)
}

if (clean) {
  runner_clean_output_dir(file.path(root, "route_B_bottomup_mcmc", "outputs"), "Route B outputs")
}

runner_run_r("scripts/00_install_r_packages.R", root = root)  # 检查/安装依赖。
runner_run_r("scripts/00_setup_relative_data.R", root = root)  # 准备项目相对数据目录。

for (dataset in c("AHAB", "PIP")) {
  run_route_b_dataset(dataset, root)
}

runner_run_r("route_B_bottomup_mcmc/scripts/08_compare_dataset_clusters.R", c("--tag1", "AHAB", "--tag2", "PIP"), root = root)  # 跨样本匹配 cluster、生成模型共识图。

cat("\nBottom-up MCMC full run complete for dataset(s): AHAB, PIP\n")
