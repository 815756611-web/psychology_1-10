# Route B 第四步：把每个 K 的结果整理成目录树。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 路径和目录函数。
source("R/nifti_utils.R")  # NIfTI 写盘。
source("R/bayes_factor.R")  # cluster 画像时的 BF 指标。
source("R/interpretation.R")  # Route A 风格角色标签与 Route B 新标签函数。

args <- parse_cli_args(list(  # tag: 输出标签；fit: 可选 mcmc_fit_*.rds 路径。
  tag = "full",
  fit = NA
))

write_md <- function(path, lines) {  # path: Markdown 路径；lines: 文本行向量；作用：写 README。
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con)
}

clean_folder_name <- function(x) {  # x: 任意文本；作用：清理成安全英文目录名，避免中文和特殊符号影响路径兼容。
  x <- tolower(x)
  x <- gsub("[^a-z0-9_\\-]+", "_", x)
  gsub("^_|_$", "", x)
}

fmt <- function(x, digits = 4) {  # x: 数值；digits: 保留位数；作用：统一表格展示格式。
  ifelse(is.finite(x), format(round(x, digits), scientific = FALSE), "NA")
}

md_table <- function(dt, cols) {  # dt: data.table；cols: 输出列；作用：生成 Markdown 表格。
  if (!nrow(dt)) return("暂无可写入的结果。")
  view <- copy(dt[, ..cols])
  for (nm in names(view)) view[[nm]] <- as.character(view[[nm]])
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  body <- apply(view, 1, function(x) paste0("| ", paste(x, collapse = " | "), " |"))
  paste(c(header, sep, body), collapse = "\n")
}

effect_row <- function(x, prefix, bf_method = "bic_approx") {  # x: cluster 的被试级特征；prefix: 指标前缀；bf_method: BF 方法；作用：生成 cluster 画像行。
  x <- x[is.finite(x)]
  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) s <- 1e-8
  t <- m / (s / sqrt(max(n, 1L)))
  bf <- t_to_two_log_bf(ifelse(is.finite(t), t, 0), n, method = bf_method, signed = FALSE)
  out <- data.table(
    n = n,
    mean = m,
    sd = s,
    t = t,
    two_log_bf = bf,
    signed_two_log_bf = sign(t) * abs(bf),
    p_gt0 = stats::pnorm(0, mean = m, sd = s / sqrt(max(n, 1L)), lower.tail = FALSE),
    p_lt0 = stats::pnorm(0, mean = m, sd = s / sqrt(max(n, 1L)), lower.tail = TRUE)
  )
  setnames(out, names(out), paste0(prefix, "_", names(out)))
  out
}

normalise_unit_summary <- function(x) {  # x: ROI 摘要表；作用：兼容旧版字段名。
  x <- as.data.table(x)
  ren <- c(
    emotion_mean = "emotion_generation_mean",
    emotion_2logbf = "emotion_generation_2logbf",
    emotion_signed_2logbf = "emotion_generation_signed_2logbf",
    reappraisal_mean = "reappraisal_effect_mean",
    reappraisal_2logbf = "reappraisal_effect_2logbf",
    reappraisal_signed_2logbf = "reappraisal_effect_signed_2logbf"
  )
  for (old in names(ren)) {
    if (old %in% names(x) && !(ren[[old]] %in% names(x))) setnames(x, old, ren[[old]])
  }
  if (!("roi_id" %in% names(x)) && "parcel_id" %in% names(x)) x[, roi_id := parcel_id]
  if (!("source_system" %in% names(x))) x[, source_system := "unknown_original_roi_source"]
  x
}

cluster_outputs_for_fit <- function(fit, bundle, roi_summary, k, model_support, tag, k_dir) {  # fit: 一个 K 的 MCMC 结果；bundle: 04 脚本特征包；roi_summary: ROI 摘要；k/tag/k_dir: 当前输出定位。
  feature_cols <- bundle$feature_cols
  subject_features <- as.data.table(bundle$subject_features)
  labels <- bundle$labels

  post <- as.data.table(fit$posterior)
  setnames(post, paste0("p_cluster_", seq_len(k)))
  posterior <- data.table(roi_id = fit$roi_ids)
  posterior <- cbind(posterior, post)
  posterior[, cluster := fit$cluster]  # ROI 的最大后验类别。
  posterior[, max_posterior := do.call(pmax, .SD), .SDcols = paste0("p_cluster_", seq_len(k))]  # ROI 的最大后验概率。
  posterior <- merge(roi_summary, posterior, by = "roi_id", all.y = TRUE)
  posterior[, roi_role_from_route_a_effects := mapply(
    role_from_effects,
    emotion_generation_mean,
    reappraisal_effect_mean,
    emotion_generation_2logbf,
    reappraisal_effect_2logbf
  )]

  assign <- posterior[, .(roi_id, cluster)]
  cluster_subject <- merge(subject_features, assign, by = "roi_id", allow.cartesian = TRUE)
  cluster_subject <- cluster_subject[, .(
    emotion_generation = mean(emotion_generation, na.rm = TRUE),
    reappraisal_effect = mean(reappraisal_effect, na.rm = TRUE),
    look_neg_base = mean(look_neg_base, na.rm = TRUE),
    reg_neg_base = mean(reg_neg_base, na.rm = TRUE)
  ), by = .(cluster, dataset, subject)]

  cluster_profile <- cluster_subject[, {  # 当前 K 下每个非空 cluster 的效应画像。
    cbind(
      effect_row(emotion_generation, "emotion_generation", as.character(CFG$bf_method)),
      effect_row(reappraisal_effect, "reappraisal_effect", as.character(CFG$bf_method)),
      effect_row(look_neg_base, "look_neg_base", as.character(CFG$bf_method)),
      effect_row(reg_neg_base, "reg_neg_base", as.character(CFG$bf_method))
    )
  }, by = cluster]
  cluster_profile[, n_rois := posterior[, .N, by = cluster][cluster_profile, on = "cluster"]$N]
  cluster_profile[, mean_max_posterior := posterior[, .(
    mean_max_posterior = mean(max_posterior)
  ), by = cluster][cluster_profile, on = "cluster"]$mean_max_posterior]
  cluster_profile[, role := mapply(
    role_from_effects,
    emotion_generation_mean,
    reappraisal_effect_mean,
    emotion_generation_two_log_bf,
    reappraisal_effect_two_log_bf
  )]
  cluster_profile[, route_b_label := mapply(
    cluster_label_from_profile,
    emotion_generation_mean,
    reappraisal_effect_mean,
    emotion_generation_two_log_bf,
    reappraisal_effect_two_log_bf,
    look_neg_base_mean,
    reg_neg_base_mean
  )]
  setorder(cluster_profile, cluster)

  fwrite(model_support, file.path(k_dir, "model_support.csv"))
  fwrite(posterior, file.path(k_dir, "posterior_roi_assignment.csv"))
  fwrite(cluster_profile, file.path(k_dir, "cluster_profile.csv"))
  fwrite(cluster_subject, file.path(k_dir, "cluster_subject_effects.csv"))

  cluster_arr <- array(0, dim = dim(labels))  # 把 ROI 标签重新投回体素空间，写成整张 cluster label 图。
  for (cl in seq_len(k)) {  # 只为非空 cluster 生成目录；空 cluster 只记入 empty_clusters.csv。
    ids <- posterior[cluster == cl, roi_id]
    cluster_arr[labels %in% ids] <- cl
  }
  write_nifti_array(
    array(as.numeric(cluster_arr), dim = dim(labels)),
    bundle$reference_nifti,
    file.path(k_dir, paste0("cluster_labels_K", sprintf("%02d", k), "_", tag, ".nii.gz"))
  )

  empty_cluster_rows <- list()
  for (cl in seq_len(k)) {
    prof <- cluster_profile[cluster == cl]
    label <- if (nrow(prof)) prof$route_b_label[1] else "empty_or_uncertain"
    cl_rois <- posterior[cluster == cl]
    if (!nrow(cl_rois)) {
      empty_cluster_rows[[length(empty_cluster_rows) + 1L]] <- data.table(
        cluster = cl,
        reason = "No ROI had this cluster as its maximum posterior assignment under this K."
      )
      next
    }
    cl_dir <- ensure_dir(file.path(k_dir, clean_folder_name(sprintf("cluster_%02d_%s", cl, label))))
    cl_subject <- cluster_subject[cluster == cl]
    ids <- cl_rois$roi_id
    cl_mask <- array(as.numeric(labels %in% ids), dim = dim(labels))
    write_nifti_array(cl_mask, bundle$reference_nifti, file.path(cl_dir, "cluster_mask.nii.gz"))
    fwrite(cl_rois, file.path(cl_dir, "cluster_roi_table.csv"))
    fwrite(cl_subject, file.path(cl_dir, "cluster_subject_effects.csv"))
    fwrite(prof, file.path(cl_dir, "cluster_profile.csv"))

    write_md(file.path(cl_dir, "README.md"), c(
      paste0("# K=", k, " / Cluster ", cl, "：", label),
      "",
      "分入原因：该 cluster 由贝叶斯高斯混合模型在当前 K 值下估计得到，ROI 按最大后验概率归入此类。",
      "",
      "命名依据：根据 cluster 内 ROI 的四个特征均值、Bayes factor 近似值和方向进行规则化命名。",
      "",
      paste0("- ROI 数量：", nrow(cl_rois)),
      paste0("- 平均最大后验概率：", if (nrow(prof)) fmt(prof$mean_max_posterior[1]) else "NA"),
      paste0("- 情绪生成均值：", if (nrow(prof)) fmt(prof$emotion_generation_mean[1]) else "NA"),
      paste0("- 重评效应均值：", if (nrow(prof)) fmt(prof$reappraisal_effect_mean[1]) else "NA"),
      paste0("- 情绪生成 2logBF：", if (nrow(prof)) fmt(prof$emotion_generation_two_log_bf[1]) else "NA"),
      paste0("- 重评效应 2logBF：", if (nrow(prof)) fmt(prof$reappraisal_effect_two_log_bf[1]) else "NA"),
      "",
      "支撑数据：",
      "",
      "- `cluster_mask.nii.gz`：该 cluster 的脑图。",
      "- `cluster_roi_table.csv`：该 cluster 内 ROI、来源系统、后验概率和 ROI 组水平统计。",
      "- `cluster_subject_effects.csv`：该 cluster 在每个被试上的平均特征值。",
      "- `cluster_profile.csv`：该 cluster 的汇总画像和命名标签。"
    ))
  }
  empty_clusters <- rbindlist(empty_cluster_rows, fill = TRUE)
  if (nrow(empty_clusters)) fwrite(empty_clusters, file.path(k_dir, "empty_clusters.csv"))

  cluster_view <- cluster_profile[, .(
    cluster,
    route_b_label,
    n_rois,
    mean_max_posterior = fmt(mean_max_posterior),
    emotion_generation_mean = fmt(emotion_generation_mean),
    reappraisal_effect_mean = fmt(reappraisal_effect_mean),
    emotion_generation_two_log_bf = fmt(emotion_generation_two_log_bf),
    reappraisal_effect_two_log_bf = fmt(reappraisal_effect_two_log_bf)
  )]
  non_empty_cluster_count <- nrow(cluster_view)
  empty_cluster_count <- nrow(empty_clusters)
  write_md(file.path(k_dir, "README.md"), c(
    paste0("# 路线 B：K=", k, " 聚类整理"),
    "",
    "这里是当前 K 值下的完整聚类整理结果。",
    "",
    "K 值可行性支撑：",
    "",
    paste0("- mean_loglik：", fmt(model_support$mean_loglik[1])),
    paste0("- requested_K：", model_support$requested_K[1]),
    paste0("- occupied_K：", model_support$occupied_K[1]),
    paste0("- effective_K：", model_support$effective_K[1]),
    paste0("- empty_cluster_count：", model_support$empty_cluster_count[1]),
    paste0("- BIC 近似值：", fmt(model_support$bic_approx[1])),
    paste0("- delta_BIC：", fmt(model_support$delta_bic[1]), "；越接近 0 表示相对支持越强。"),
    paste0("- overall_score：", fmt(model_support$overall_score[1])),
    paste0("- 归属稳定性得分：", fmt(model_support$assignment_stability_score[1])),
    paste0("- 共聚类结构得分：", fmt(model_support$coclustering_score[1])),
    paste0("- 可解释性得分：", fmt(model_support$interpretability_score[1])),
    paste0("- 是否为 overall_recommended_K：", model_support$overall_recommended_K[1]),
    paste0("- mean_membership_entropy：", fmt(model_support$mean_membership_entropy[1]), "；越低表示分类越清晰。"),
    paste0("- kept_draws：", model_support$kept_draws[1]),
    paste0("- 是否为当前 tag 下最低 BIC：", model_support$is_lowest_bic[1]),
    paste0("- 请求的 K 值：", k),
    paste0("- 实际非空 cluster 数：", non_empty_cluster_count),
    paste0("- 空 cluster 数：", empty_cluster_count),
    "",
    "说明：K 是模型允许的潜在成分数，不等于最终一定会出现的非空脑区类别数。README 只展开至少分到 1 个 ROI 的非空 cluster；未被任何 ROI 作为最大后验类别的成分会记录在 `empty_clusters.csv`，不会生成空白 cluster 文件夹。",
    "",
    "聚类摘要：",
    "",
    md_table(cluster_view, c(
      "cluster", "route_b_label", "n_rois", "mean_max_posterior",
      "emotion_generation_mean", "reappraisal_effect_mean",
      "emotion_generation_two_log_bf", "reappraisal_effect_two_log_bf"
    )),
    "",
    "主要文件：",
    "",
    "- `model_support.csv`：当前 K 的模型支撑指标。",
    "- `posterior_roi_assignment.csv`：ROI 的后验聚类概率。",
    "- `cluster_profile.csv`：各 cluster 的效应画像。",
    "- `cluster_subject_effects.csv`：各 cluster 的被试级平均特征。",
    "- `cluster_labels_K*.nii.gz`：当前 K 的整体彩色标签图。",
    "- `cluster_*` 文件夹：每个非空 cluster 的脑图和数据。",
    if (nrow(empty_clusters)) "- `empty_clusters.csv`：当前 K 下未分到任何 ROI 的空 cluster 记录。" else NULL
  ))
}

tag <- as.character(args$tag)  # 当前要整理的 tag，例如 AHAB/PIP/smoke。
table_dir <- project_path("route_B_bottomup_mcmc", "outputs", "tables")
by_k_dir <- project_path("route_B_bottomup_mcmc", "outputs", "by_k")
tag_dir <- file.path(by_k_dir, tag)

fit_path <- as.character(args$fit)
if (is.na(fit_path) || !nzchar(fit_path)) {
  fit_path <- file.path(table_dir, paste0("mcmc_fit_", tag, ".rds"))
} else if (!file.exists(fit_path)) {
  fit_path <- project_path(fit_path)
}
if (!file.exists(fit_path)) stop("MCMC fit RDS not found: ", fit_path)

obj <- readRDS(fit_path)  # 读入 05 脚本保存的完整 MCMC 对象。
bundle <- obj$feature_bundle
if (!identical(bundle$analysis_unit, "route_b_roi")) {
  stop("Route B organizer expects pooled original-ROI feature bundle.")
}
roi_summary <- normalise_unit_summary(bundle$roi_summary)
model_comparison <- as.data.table(obj$model_comparison)
needed_metric_cols <- c(
  "aic_approx", "bic_roi_approx", "waic", "elpd_waic", "p_waic",
  "looic", "elpd_loo", "p_loo", "pareto_k_max", "pareto_k_bad", "loo_method",
  "mean_max_posterior", "stable_080_fraction", "stable_090_fraction",
  "uncertain_060_fraction", "mean_coclustering_entropy", "coclustering_sharpness",
  "confident_pair_fraction", "interpretable_cluster_fraction",
  "model_performance_score", "assignment_stability_score", "coclustering_score",
  "interpretability_score", "overall_score", "overall_recommended_K",
  "requested_K", "occupied_K", "effective_K", "minimum_cluster_size", "maximum_cluster_size"
)
for (col in needed_metric_cols) {
  if (!(col %in% names(model_comparison))) model_comparison[, (col) := NA]
}
model_comparison[is.na(requested_K), requested_K := k]
if ("terminal_cluster_count" %in% names(model_comparison)) {
  model_comparison[is.na(occupied_K), occupied_K := terminal_cluster_count]
}
model_comparison[is.na(occupied_K), occupied_K := k - empty_cluster_count]
model_comparison[is.na(effective_K), effective_K := occupied_K]
model_comparison[is.na(empty_cluster_count), empty_cluster_count := requested_K - occupied_K]
model_comparison[, delta_bic := bic_approx - min(bic_approx, na.rm = TRUE)]
model_comparison[, is_lowest_bic := delta_bic == min(delta_bic, na.rm = TRUE)]
setorder(model_comparison, k)

unlink(tag_dir, recursive = TRUE, force = TRUE)
invisible(ensure_dir(by_k_dir))
invisible(ensure_dir(tag_dir))

write_md(file.path(by_k_dir, "README.md"), c(
  "# 路线 B：按 K 值整理输出",
  "",
  "这里按 tag 和 K 值整理路线 B 的 MCMC 聚类结果。基础结果仍保留在 `outputs/tables` 和 `outputs/nifti`。",
  "",
  "每个 K 文件夹中包含模型支撑信息、ROI 后验分配表、cluster 画像和每个 cluster 的脑区数据。"
))

fwrite(model_comparison, file.path(tag_dir, "model_comparison_all_K.csv"))
write_md(file.path(tag_dir, "README.md"), c(
  paste0("# 路线 B：", tag, " 的 K 值整理"),
  "",
  "本层用于比较不同 K 值下的聚类可行性。",
  "",
  "判断依据：overall_recommended_K 不是只看 BIC，而是综合模型表现、ROI 归属稳定性、MCMC 后验共聚类结构和 cluster 可解释性。BIC、AIC、WAIC、LOOIC 仍保留为模型表现指标。",
  "",
  md_table(model_comparison[, .(
    k,
    requested_K,
    occupied_K,
    effective_K,
    empty_cluster_count,
    kept_draws,
    mean_loglik = fmt(mean_loglik),
    aic_approx = fmt(aic_approx),
    bic_approx = fmt(bic_approx),
    waic = fmt(waic),
    looic = fmt(looic),
    delta_bic = fmt(delta_bic),
    mean_membership_entropy = fmt(mean_membership_entropy),
    mean_max_posterior = fmt(mean_max_posterior),
    coclustering_sharpness = fmt(coclustering_sharpness),
    interpretable_cluster_fraction = fmt(interpretable_cluster_fraction),
    overall_score = fmt(overall_score),
    overall_recommended_K,
    is_lowest_bic
  )], c(
    "k", "requested_K", "occupied_K", "effective_K", "empty_cluster_count",
    "kept_draws", "mean_loglik", "aic_approx", "bic_approx", "waic", "looic",
    "delta_bic", "mean_membership_entropy", "mean_max_posterior",
    "coclustering_sharpness", "interpretable_cluster_fraction",
    "overall_score", "overall_recommended_K", "is_lowest_bic"
  )),
  "",
  "每个 `K_XX` 文件夹都包含该 K 下的 cluster 文件夹和支撑数据。"
))

for (k_name in names(obj$all_fits)) {  # 对每个 K 都独立建一个 K_XX 文件夹。
  k_value <- as.integer(k_name)
  fit <- obj$all_fits[[k_name]]
  k_dir <- ensure_dir(file.path(tag_dir, sprintf("K_%02d", k_value)))
  support <- model_comparison[k == k_value]
  cluster_outputs_for_fit(fit, bundle, roi_summary, k_value, support, tag, k_dir)
}

cat("Route B organized K/cluster outputs written:\n")
cat("  ", tag_dir, "\n", sep = "")
