# 总入口：交互选择 Route A 或 Route B；仅负责调度，不做统计。 
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
suppressMessages(here::i_am("run_full.R"))
PROJECT_ROOT <- here::here()
source(here::here("R", "runner.R"))

root <- runner_start_root()  # 统一进入复制版项目根目录。
route <- runner_ask_choice(  # A=原文复现；B=ROI-MCMC 聚类。
  "Full route [A/B], A=original reproduction, B=bottom-up MCMC, default A: ",
  c("A", "B"),
  default = "A"
)
clean <- runner_ask_yes_no("Clean selected route outputs before rerun? [Y/N], default N: ", default = FALSE)  # 只清输出目录，不动原始数据。
clean_arg <- if (clean) "true" else "false"

if (identical(route, "A")) {
  runner_run_r("route_A_original_reproduction/run_full_original.R", c("--clean-output", clean_arg), root = root)
} else {
  runner_run_r("route_B_bottomup_mcmc/run_full_bottomup.R", c("--clean-output", clean_arg), root = root)
}

cat("\nSelected full route complete: ", route, "\n", sep = "")
