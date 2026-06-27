# 依赖安装脚本：集中声明 Route A/B 用到的包及用途；MATLAB 原文依赖 CANlab/SPM，这里改成 R 包环境。 
options(download.file.method = "wininet")
cran <- "https://cran.rstudio.com"
needed <- c(
  "here",  # 跨系统项目根目录定位；统一替代副本中的绝对路径。
  "RNifti",  # 读写 NIfTI 脑图；对应 MATLAB fmri_data/write/read 图像 IO。
  "data.table",  # 高性能表格处理；对应 MATLAB table/矩阵整理。
  "readxl",  # Excel 行为表兜底读取。
  "R.matlab",  # 如需读 .mat 结果时使用；当前主流程主要走 NIfTI/CSV。
  "mmand",  # 三维 connected components；对应 MATLAB region/cluster 处理的加速版本。
  "mvtnorm",  # 多元正态抽样与密度；Route B MCMC 混合模型核心依赖。
  "loo",  # WAIC/PSIS-LOO；Route B 的 K 比较指标。
  "coda",  # MCMC 诊断兼容工具；当前主要用于扩展分析时兼容。
  "matrixStats"  # 矩阵统计辅助；提高后验矩阵计算稳定性与速度。
)

missing <- needed[!vapply(needed, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) {
  install.packages(missing, repos = cran)
}

cat("R package check complete.\n")
