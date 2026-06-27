# Route B 第三步：把推荐 K 的 posterior 结果转成文字化解释和行为关联。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 公共路径。
source("R/nifti_utils.R")  # 路径/图像兼容辅助。
source("R/parcels.R")  # 兼容旧辅助函数。
source("R/interpretation.R")  # 标签和重叠解释函数。

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x  # 空值兜底。

args <- parse_cli_args(list(  # posterior: 05 脚本输出的 posterior CSV；tag: 输出标签；features: 对应的 RDS 特征包。
  posterior = project_path("route_B_bottomup_mcmc", "outputs", "tables", "mcmc_posterior_full.csv"),
  tag = "full",
  features = NA
))
tag <- as.character(args$tag)
posterior_path <- as.character(args$posterior)
if (!file.exists(posterior_path)) posterior_path <- project_path(posterior_path)
if (!file.exists(posterior_path)) stop("Posterior CSV not found: ", args$posterior)

features_path <- args$features
if (is.na(features_path)) {
  features_path <- project_path("route_B_bottomup_mcmc", "outputs", "tables", paste0("roi_subject_features_", tag, ".rds"))
} else if (!file.exists(features_path)) {
  features_path <- project_path(features_path)
}
bundle <- if (file.exists(features_path)) readRDS(features_path) else list(analysis_unit = "unknown")  # 如果找不到 RDS，仍允许最小化解释 posterior CSV。
analysis_unit <- bundle$analysis_unit %||% "unknown"

posterior <- fread(posterior_path)
table_dir <- project_path("route_B_bottomup_mcmc", "outputs", "tables")
cluster_profile_path <- file.path(table_dir, paste0("mcmc_cluster_profile_", tag, ".csv"))
cluster_profile <- if (file.exists(cluster_profile_path)) fread(cluster_profile_path) else data.table()

pool_metadata <- data.table(  # 明确 Route B 的分析单位是“所有 ROI 的同一个观察池”，不是按四系统分组建模。
  n_roi_units = nrow(posterior),
  n_source_labels_retained = uniqueN(posterior$source_system),
  note = "Route B pools all original ROI observations in one model. source_system is retained only inside roi_table/mcmc_posterior for provenance and is not used for grouping."
)
fwrite(pool_metadata, file.path(table_dir, paste0("mcmc_roi_pool_metadata_", tag, ".csv")))
uncertainty <- posterior[, .(
  n_rois = .N,
  mean_max_posterior = mean(max_posterior, na.rm = TRUE),
  n_stable_080 = sum(max_posterior >= 0.80, na.rm = TRUE),
  n_stable_090 = sum(max_posterior >= 0.90, na.rm = TRUE),
  n_uncertain_below_060 = sum(max_posterior < 0.60, na.rm = TRUE)
), by = cluster]
fwrite(uncertainty, file.path(table_dir, paste0("mcmc_cluster_uncertainty_", tag, ".csv")))

behavior_path <- project_path("data", "processed", "behavior_clean.csv")
cluster_subject_path <- file.path(table_dir, paste0("mcmc_cluster_subject_effects_", tag, ".csv"))
if (file.exists(behavior_path) && file.exists(cluster_subject_path)) {
  behavior <- fread(behavior_path)
  cluster_subject <- fread(cluster_subject_path)
  validation <- merge(cluster_subject, behavior[, .(dataset, subject, reg_success, emotion_reactivity)],  # 只做简单外部行为相关，不反推类别数。
    by = c("dataset", "subject"), all.x = TRUE
  )
  cors <- validation[, .(
    n = sum(is.finite(reg_success) & is.finite(reappraisal_effect)),
    cor_success_reappraisal_effect = suppressWarnings(cor(reg_success, reappraisal_effect, use = "pairwise.complete.obs")),
    cor_success_emotion_generation = suppressWarnings(cor(reg_success, emotion_generation, use = "pairwise.complete.obs")),
    cor_emotionreact_emotion_generation = suppressWarnings(cor(emotion_reactivity, emotion_generation, use = "pairwise.complete.obs"))
  ), by = cluster]
  fwrite(validation, file.path(table_dir, paste0("mcmc_cluster_subject_effects_with_behavior_", tag, ".csv")))
  fwrite(cors, file.path(table_dir, paste0("mcmc_cluster_behavior_correlations_", tag, ".csv")))
} else {
  cors <- data.table()
}

model_comparison_path <- file.path(table_dir, paste0("mcmc_model_comparison_", tag, ".csv"))
model_comparison <- if (file.exists(model_comparison_path)) fread(model_comparison_path) else data.table()  # 如果有 K 比较表，就从中读取 overall_recommended_K。
best_k_line <- if (nrow(model_comparison)) {
  if ("overall_recommended_K" %in% names(model_comparison) &&
      any(model_comparison$overall_recommended_K %in% TRUE, na.rm = TRUE)) {
    selected <- model_comparison[overall_recommended_K == TRUE][1]
    paste0(
      "本次选择 K=", selected$k,
      "；依据是 overall_recommended_K，综合模型表现、归属稳定性、共聚类结构和可解释性",
      if ("overall_score" %in% names(selected)) paste0("（overall_score=", sprintf("%.4f", selected$overall_score), "）") else "",
      "。"
    )
  } else {
    paste0("本次选择 K=", model_comparison[order(bic_approx)][1, k], "；旧结果缺少 overall_recommended_K，回退为近似 BIC 最小。")
  }
} else {
  "本次没有找到 K 比较表，解释基于当前 posterior 文件。"
}

cluster_lines <- if (nrow(cluster_profile)) {
  apply(cluster_profile, 1, function(row) {
    paste0(
      "- Cluster ", row[["cluster"]], "：", row[["route_b_label"]], "；ROI 数=",
      row[["n_rois"]], "；平均最大后验归属概率=",
      sprintf("%.2f", as.numeric(row[["mean_max_posterior"]])),
      "；Emotion generation 均值=", sprintf("%.4f", as.numeric(row[["emotion_generation_mean"]])),
      "；Reappraisal effect 均值=", sprintf("%.4f", as.numeric(row[["reappraisal_effect_mean"]])), "。"
    )
  })
} else {
  "- 未找到 cluster profile 表。"
}

pool_line <- paste0(
  "本次共有 ", pool_metadata$n_roi_units,
  " 个原文 ROI 进入同一个模型。保留了 ",
  pool_metadata$n_source_labels_retained,
  " 个来源标签作为追溯信息，但没有按来源标签分组建模或分系统解释。"
)

behavior_lines <- if (exists("cors") && nrow(cors)) {
  apply(cors, 1, function(row) {
    paste0(
      "- Cluster ", row[["cluster"]], "：重评效应与 reappraisal success 的相关 r=",
      sprintf("%.3f", as.numeric(row[["cor_success_reappraisal_effect"]])),
      "；情绪生成与 emotion reactivity 的相关 r=",
      sprintf("%.3f", as.numeric(row[["cor_emotionreact_emotion_generation"]])), "。"
    )
  })
} else {
  "- 未找到行为数据或 cluster-subject 表，未进行行为验证。"
}

md <- c(
  "# 路线 B：贝叶斯 MCMC 聚类结果说明",
  "",
  paste0("Tag：", tag),
  "",
  "分析单位：原文 ROI 连通簇；所有 ROI 作为同一个观察池进入模型。",
  "",
  "## 1. K 值",
  "",
  best_k_line,
  "",
  "## 2. Cluster 画像",
  "",
  cluster_lines,
  "",
  "## 3. 观察池说明",
  "",
  "路线 B 不按原文四类系统分别建模，也不把系统标签作为分组变量。",
  "",
  pool_line,
  "",
  "## 4. 分类不确定性",
  "",
  "查看 `mcmc_cluster_uncertainty_*.csv` 和 `mcmc_posterior_*.csv`。`max_posterior >= 0.80` 可视为相对稳定，低于 0.60 应解释为 mixed/uncertain unit。",
  "",
  "## 5. 行为验证",
  "",
  behavior_lines,
  "",
  "## 6. 文件读取关系",
  "",
  "- 输入：`roi_subject_features_*.csv` 与 `roi_table_*.csv`",
  "- 后验分类：`mcmc_posterior_*.csv`",
  "- Cluster 画像：`mcmc_cluster_profile_*.csv`",
  "- ROI 观察池元数据：`mcmc_roi_pool_metadata_*.csv`",
  "- 行为验证：`mcmc_cluster_behavior_correlations_*.csv`"
)
writeLines(md, file.path(table_dir, paste0("mcmc_interpretation_", tag, ".md")), useBytes = TRUE)

cat("Route B interpretation written:\n")
cat("  ", file.path(table_dir, paste0("mcmc_interpretation_", tag, ".md")), "\n", sep = "")
