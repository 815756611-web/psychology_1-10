# Route B 第二步：拟合 ROI 层级贝叶斯混合模型并比较 K。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 配置和路径。
source("R/nifti_utils.R")  # NIfTI 写盘。
source("R/bayes_factor.R")  # cluster 画像时的 BF 指标。
source("R/parcels.R")  # 保留旧辅助函数兼容。
source("R/mcmc_mixture.R")  # Route B 的核心 MCMC 采样器。
source("R/interpretation.R")  # Route A 风格角色标签与 cluster 标签函数。

parse_k_grid <- function(x, fallback = 4L) {  # x: CLI 的 K 设置字符串；fallback: 缺省 K；作用：支持 "2:6" 或 "2,3,4" 两种写法。
  if (is.null(x) || isTRUE(is.na(x)) || !nzchar(as.character(x))) return(fallback)
  x <- as.character(x)
  if (grepl("^[0-9]+:[0-9]+$", x)) {
    p <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
    return(seq(p[1], p[2]))
  }
  as.integer(strsplit(x, ",", fixed = TRUE)[[1]])
}

effect_row <- function(x, prefix, bf_method = "bic_approx") {  # x: 一个 cluster 的被试级特征；prefix: 前缀名；bf_method: BF 计算方法。
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

normalise_unit_summary <- function(x) {  # x: ROI 摘要表；作用：兼容旧字段名，统一到 emotion_generation_* / reappraisal_effect_* 风格。
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

log_mean_exp <- function(x) {  # x: 对数似然向量；作用：稳定计算 log(mean(exp(x)))，供 WAIC/LOO 使用。
  m <- max(x)
  m + log(mean(exp(x - m)))
}

predictive_criteria <- function(pointwise_loglik) {  # pointwise_loglik: 每次保留抽样×每个 ROI 的 loglik 矩阵；作用：计算 WAIC/PSIS-LOO。
  ll <- as.matrix(pointwise_loglik)
  if (!length(ll) || !nrow(ll) || !ncol(ll)) {
    return(data.table(
      n_pointwise_units = ncol(ll),
      elpd_waic = NA_real_,
      p_waic = NA_real_,
      waic = NA_real_,
      elpd_loo = NA_real_,
      p_loo = NA_real_,
      looic = NA_real_,
      pareto_k_max = NA_real_,
      pareto_k_bad = NA_integer_,
      loo_method = "not_available"
    ))
  }

  lppd_i <- apply(ll, 2, log_mean_exp)
  p_waic_i <- apply(ll, 2, stats::var)
  elpd_waic <- sum(lppd_i - p_waic_i)
  p_waic <- sum(p_waic_i)
  waic <- -2 * elpd_waic

  out <- data.table(
    n_pointwise_units = ncol(ll),
    elpd_waic = elpd_waic,
    p_waic = p_waic,
    waic = waic,
    elpd_loo = NA_real_,
    p_loo = NA_real_,
    looic = NA_real_,
    pareto_k_max = NA_real_,
    pareto_k_bad = NA_integer_,
    loo_method = "manual_waic_only"
  )

  if (requireNamespace("loo", quietly = TRUE)) {
    loo_obj <- tryCatch(
      loo::loo(ll),
      error = function(e) e
    )
    if (!inherits(loo_obj, "error")) {
      est <- loo_obj[["estimates"]]
      pareto_k <- loo_obj[["diagnostics"]][["pareto_k"]]
      out[, `:=`(
        elpd_loo = est["elpd_loo", "Estimate"],
        p_loo = est["p_loo", "Estimate"],
        looic = est["looic", "Estimate"],
        pareto_k_max = suppressWarnings(max(pareto_k, na.rm = TRUE)),
        pareto_k_bad = sum(pareto_k > 0.7, na.rm = TRUE),
        loo_method = "psis_loo"
      )]
    } else {
      out[, loo_method := paste0("loo_failed: ", conditionMessage(loo_obj))]
    }
  }

  out
}

bounded01 <- function(x) {  # x: 数值；作用：把评分裁剪到 0~1。
  pmax(0, pmin(1, x))
}

rank_score <- function(x, lower_is_better = TRUE) {  # x: 一列模型指标；lower_is_better: 是否越小越优；作用：把不同量纲指标转成可合成分数。
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (!any(ok)) return(out)
  if (sum(ok) == 1L) {
    out[ok] <- 1
    return(out)
  }
  value <- if (lower_is_better) x else -x
  r <- rank(value, ties.method = "average", na.last = "keep")
  max_r <- max(r[ok], na.rm = TRUE)
  out[ok] <- (max_r - r[ok]) / max(max_r - 1, 1)
  bounded01(out)
}

write_md <- function(path, lines) {  # path: Markdown 路径；lines: 文本行向量；作用：写 K 比较说明。
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con)
}

fmt <- function(x, digits = 4) {  # x: 数值；digits: 保留位数；作用：统一 Markdown 输出格式。
  ifelse(is.finite(x), format(round(x, digits), scientific = FALSE), "NA")
}

md_table <- function(dt, cols) {  # dt: data.table；cols: 输出列；作用：生成 Markdown 表格。
  if (!nrow(dt)) return("暂无结果。")
  view <- copy(dt[, ..cols])
  for (nm in names(view)) view[[nm]] <- as.character(view[[nm]])
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  body <- apply(view, 1, function(x) paste0("| ", paste(x, collapse = " | "), " |"))
  paste(c(header, sep, body), collapse = "\n")
}

cluster_profile_for_fit <- function(fit, roi_summary, subject_features, feature_cols, k) {  # fit: 一个 K 的 MCMC 结果；roi_summary: ROI 摘要；subject_features: 被试×ROI 表；feature_cols: 入模特征；k: 请求 K。
  post <- as.data.table(fit$posterior)
  setnames(post, paste0("p_cluster_", seq_len(k)))
  posterior <- data.table(roi_id = fit$roi_ids)
  posterior <- cbind(posterior, post)
  posterior[, cluster := fit$cluster]  # 每个 ROI 的最大后验类别。
  posterior[, max_posterior := do.call(pmax, .SD), .SDcols = paste0("p_cluster_", seq_len(k))]  # 每个 ROI 的最大后验概率。
  posterior <- merge(roi_summary, posterior, by = "roi_id", all.y = TRUE)

  assign <- posterior[, .(roi_id, cluster)]
  cluster_subject <- merge(subject_features, assign, by = "roi_id", allow.cartesian = TRUE)
  cluster_subject <- cluster_subject[, .(
    emotion_generation = mean(emotion_generation, na.rm = TRUE),
    reappraisal_effect = mean(reappraisal_effect, na.rm = TRUE),
    look_neg_base = mean(look_neg_base, na.rm = TRUE),
    reg_neg_base = mean(reg_neg_base, na.rm = TRUE)
  ), by = .(cluster, dataset, subject)]

  cluster_profile <- cluster_subject[, {  # 每个 cluster 汇总被试级均值、t 和 BF，形成可解释画像。
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
  list(posterior = posterior, cluster_subject = cluster_subject, cluster_profile = cluster_profile)
}

k_posterior_diagnostics <- function(fit, roi_summary, subject_features, feature_cols, k) {  # fit 等参数同上；作用：把一个 K 的后验结果压缩成 occupied/effective/stability 指标。
  profile_obj <- cluster_profile_for_fit(fit, roi_summary, subject_features, feature_cols, k)
  posterior <- profile_obj$posterior
  cluster_profile <- profile_obj$cluster_profile
  maxp <- posterior$max_posterior
  co <- coclustering_metrics(fit$z_draws)
  non_empty <- uniqueN(posterior$cluster)
  empty <- k - non_empty
  cluster_size <- posterior[, .(
    cluster_size = .N,
    cluster_mean_max_posterior = mean(max_posterior, na.rm = TRUE),
    cluster_mean_entropy = mean(-rowSums(as.matrix(.SD) * log(as.matrix(.SD) + 1e-15)), na.rm = TRUE)
  ), by = cluster, .SDcols = paste0("p_cluster_", seq_len(k))]
  effective <- cluster_size[
    cluster_size >= 2 &
      cluster_mean_max_posterior >= 0.70 &
      cluster_mean_entropy <= log(pmax(k, 2)) * 0.50
  ]
  label <- cluster_profile$route_b_label
  interpretable <- label != "gradient-or-uncertain"
  bf_values <- abs(unlist(cluster_profile[, .(
    emotion_generation_two_log_bf,
    reappraisal_effect_two_log_bf,
    look_neg_base_two_log_bf,
    reg_neg_base_two_log_bf
  )], use.names = FALSE))
  effect_strength <- mean(pmin(bf_values / as.numeric(CFG$bf_threshold_2log), 1), na.rm = TRUE)
  size_balance <- if (nrow(cluster_profile) > 1L) {
    min(cluster_profile$n_rois, na.rm = TRUE) / max(cluster_profile$n_rois, na.rm = TRUE)
  } else {
    0
  }
  data.table(
    requested_K = k,
    occupied_K = non_empty,
    effective_K = nrow(effective),
    terminal_cluster_count = non_empty,
    empty_cluster_count = empty,
    minimum_cluster_size = ifelse(nrow(cluster_size), min(cluster_size$cluster_size), NA_integer_),
    maximum_cluster_size = ifelse(nrow(cluster_size), max(cluster_size$cluster_size), NA_integer_),
    mean_max_posterior = mean(maxp, na.rm = TRUE),
    mean_max_posterior_probability = mean(maxp, na.rm = TRUE),
    stable_080_fraction = mean(maxp >= 0.80, na.rm = TRUE),
    stable_090_fraction = mean(maxp >= 0.90, na.rm = TRUE),
    uncertain_060_fraction = mean(maxp < 0.60, na.rm = TRUE),
    interpretable_cluster_fraction = ifelse(nrow(cluster_profile), mean(interpretable, na.rm = TRUE), NA_real_),
    cluster_size_balance = size_balance,
    effect_strength_score = effect_strength
  )[, cbind(.SD, co)]
}

score_k_grid <- function(model_comparison) {  # model_comparison: 各 K 的指标表；作用：合成 overall_score 并自动给出 overall_recommended_K。
  x <- copy(model_comparison)
  x[, delta_bic := bic_approx - min(bic_approx, na.rm = TRUE)]
  x[, delta_aic := aic_approx - min(aic_approx, na.rm = TRUE)]
  if ("waic" %in% names(x)) x[, delta_waic := waic - min(waic, na.rm = TRUE)] else x[, delta_waic := NA_real_]
  if ("looic" %in% names(x)) x[, delta_looic := looic - min(looic, na.rm = TRUE)] else x[, delta_looic := NA_real_]

  model_mat <- cbind(
    rank_score(x$bic_approx, TRUE),
    rank_score(x$aic_approx, TRUE),
    rank_score(x$waic, TRUE),
    rank_score(x$looic, TRUE)
  )
  x[, model_performance_score := rowMeans(model_mat, na.rm = TRUE)]
  x[!is.finite(model_performance_score), model_performance_score := 0]

  entropy_conf <- bounded01(1 - x$mean_membership_entropy / log(pmax(x$k, 2)))
  x[, assignment_stability_score := bounded01(
    0.40 * mean_max_posterior +
      0.35 * stable_080_fraction +
      0.25 * entropy_conf
  )]
  x[, coclustering_score := bounded01(
    0.50 * coclustering_sharpness +
      0.50 * confident_pair_fraction
  )]
  x[, interpretability_score := bounded01(
    0.45 * interpretable_cluster_fraction +
      0.25 * (terminal_cluster_count / k) +
      0.15 * cluster_size_balance +
      0.15 * effect_strength_score
  )]
  x[, overall_score := bounded01(
    0.40 * model_performance_score +
      0.25 * assignment_stability_score +
      0.20 * coclustering_score +
      0.15 * interpretability_score
  )]
  setorder(x, -overall_score, bic_approx, k)
  x[, overall_recommended_K := FALSE]
  x[1, overall_recommended_K := TRUE]
  setorder(x, k)
  x
}

args <- parse_cli_args(list(  # features: 04 脚本输出的 RDS；iter/burn/thin: 链长度；k/k-grid: K 设定；alpha: Dirichlet 参数；tag: 输出标签。
  features = project_path("route_B_bottomup_mcmc", "outputs", "tables", "roi_subject_features_full.rds"),
  iter = CFG$mcmc_iter,
  burn = CFG$mcmc_burn,
  thin = CFG$mcmc_thin,
  k = CFG$mcmc_k,
  "k-grid" = NA,
  alpha = CFG$mcmc_alpha,
  tag = "full"
))
features_path <- as.character(args$features)
if (!file.exists(features_path)) features_path <- project_path(features_path)
if (!file.exists(features_path)) stop("Feature RDS not found: ", args$features)

iter <- as_int_arg(args$iter, as.integer(CFG$mcmc_iter))
burn <- as_int_arg(args$burn, as.integer(CFG$mcmc_burn))
thin <- as_int_arg(args$thin, as.integer(CFG$mcmc_thin))
k_grid <- parse_k_grid(args[["k-grid"]], as_int_arg(args$k, as.integer(CFG$mcmc_k)))
alpha <- as_num_arg(args$alpha, as.numeric(CFG$mcmc_alpha))
tag <- as.character(args$tag)

bundle <- readRDS(features_path)  # 读取 Route B 特征包。
if (!identical(bundle$analysis_unit, "route_b_roi")) {
  stop("Route B now expects pooled original-ROI features: roi_subject_features_*.rds.")
}
roi_summary <- normalise_unit_summary(bundle$roi_summary)
subject_features <- as.data.table(bundle$subject_features)
feature_cols <- bundle$feature_cols
table_dir <- project_path("route_B_bottomup_mcmc", "outputs", "tables")
nifti_dir <- project_path("route_B_bottomup_mcmc", "outputs", "nifti")

message("Running Route B Bayesian hierarchical ROI mixture clustering. K grid: ", paste(k_grid, collapse = ", "))
fits <- list()
model_rows <- list()
for (k in k_grid) {  # 对每个请求 K 独立拟合，并保留空 cluster 诊断而不是直接隐藏。
  message("Route B MCMC K=", k, ", iter=", iter)
  fit <- run_roi_mcmc_mixture(
    subject_features,
    feature_cols = feature_cols,
    k = k,
    iter = iter,
    burn = burn,
    thin = thin,
    alpha = alpha,
    seed = as.integer(CFG$random_seed) + k
  )
  unit_n_for_bic <- nrow(subject_features)
  n_roi_units <- length(fit$roi_ids)
  n_params <- k * length(feature_cols) + k * length(feature_cols) * (length(feature_cols) + 1) / 2 + (k - 1)
  aic <- -2 * fit$mean_loglik + 2 * n_params
  bic <- -2 * fit$mean_loglik + n_params * log(unit_n_for_bic)
  bic_roi <- -2 * fit$mean_loglik + n_params * log(n_roi_units)
  pred <- predictive_criteria(fit$pointwise_loglik)  # 模型表现：WAIC/LOO 等。
  entropy <- {
    p <- fit$posterior
    -mean(rowSums(p * log(p + 1e-15)))
  }
  diag <- k_posterior_diagnostics(fit, roi_summary, subject_features, feature_cols, k)
  co <- posterior_coclustering_matrix(fit$z_draws)  # 共聚类结构：后验上两个 ROI 归为同类的概率。
  co_dt <- as.data.table(co)
  setnames(co_dt, paste0("roi_", fit$roi_ids))
  co_dt[, roi_id := fit$roi_ids]
  setcolorder(co_dt, "roi_id")
  fwrite(co_dt, file.path(table_dir, sprintf("mcmc_coclustering_K%02d_%s.csv", k, tag)))
  model_rows[[as.character(k)]] <- cbind(data.table(
    k = k,
    model_family = "Bayesian hierarchical ROI mixture clustering",
    kept_draws = fit$kept,
    mean_loglik = fit$mean_loglik,
    n_roi_units = n_roi_units,
    n_subject_feature_rows = unit_n_for_bic,
    n_parameters_approx = n_params,
    aic_approx = aic,
    bic_approx = bic,
    bic_roi_approx = bic_roi,
    mean_membership_entropy = entropy
  ), pred, diag)
  fits[[as.character(k)]] <- fit
}
model_comparison <- rbindlist(model_rows)
model_comparison <- score_k_grid(model_comparison)
# Downstream maps and tables use the multi-metric recommended K.
selected_k <- model_comparison[overall_recommended_K == TRUE][1, k]  # 多指标自动推荐 K，而不是只看 BIC。
fit <- fits[[as.character(selected_k)]]

post <- as.data.table(fit$posterior)
setnames(post, paste0("p_cluster_", seq_len(selected_k)))
posterior <- data.table(roi_id = fit$roi_ids)  # 推荐 K 下的 ROI 后验分配表。
posterior <- cbind(posterior, post)
posterior[, cluster := fit$cluster]
posterior[, max_posterior := do.call(pmax, .SD), .SDcols = paste0("p_cluster_", seq_len(selected_k))]
posterior <- merge(roi_summary, posterior, by = "roi_id", all.y = TRUE)
posterior[, roi_role_from_route_a_effects := mapply(
  role_from_effects,
  emotion_generation_mean,
  reappraisal_effect_mean,
  emotion_generation_2logbf,
  reappraisal_effect_2logbf
)]

posterior_path <- file.path(table_dir, paste0("mcmc_posterior_", tag, ".csv"))
fwrite(model_comparison, file.path(table_dir, paste0("mcmc_model_comparison_", tag, ".csv")))
fwrite(model_comparison[overall_recommended_K == TRUE], file.path(table_dir, paste0("mcmc_overall_recommended_K_", tag, ".csv")))
fwrite(posterior, posterior_path)
selected_co_path <- file.path(table_dir, paste0("mcmc_coclustering_", tag, ".csv"))
file.copy(file.path(table_dir, sprintf("mcmc_coclustering_K%02d_%s.csv", selected_k, tag)), selected_co_path, overwrite = TRUE)
saveRDS(list(
  fit = fit,
  all_fits = fits,
  model_comparison = model_comparison,
  posterior = posterior,
  feature_bundle = bundle,
  selected_k = selected_k,
  selected_by = "overall_recommended_K",
  recommendation_weights = c(
    model_performance_score = 0.40,
    assignment_stability_score = 0.25,
    coclustering_score = 0.20,
    interpretability_score = 0.15
  )
),
  file.path(table_dir, paste0("mcmc_fit_", tag, ".rds"))
)
fwrite(data.table(iteration = seq_along(fit$loglik), loglik = fit$loglik), file.path(table_dir, paste0("mcmc_loglik_", tag, ".csv")))

labels <- bundle$labels
cluster_arr <- array(0, dim = dim(labels))
for (cl in seq_len(selected_k)) {
  ids <- posterior[cluster == cl, roi_id]
  cluster_arr[labels %in% ids] <- cl
  one <- array(as.numeric(labels %in% ids), dim = dim(labels))
  write_nifti_array(one, bundle$reference_nifti, file.path(nifti_dir, paste0("mcmc_cluster_", cl, "_", tag, ".nii.gz")))
}
write_nifti_array(array(as.numeric(cluster_arr), dim = dim(labels)), bundle$reference_nifti, file.path(nifti_dir, paste0("mcmc_cluster_labels_", tag, ".nii.gz")))

assign <- posterior[, .(roi_id, cluster)]
cluster_subject <- merge(subject_features, assign, by = "roi_id", allow.cartesian = TRUE)  # 把 ROI 类别回灌到被试级观测。
cluster_subject <- cluster_subject[, .(
  emotion_generation = mean(emotion_generation, na.rm = TRUE),
  reappraisal_effect = mean(reappraisal_effect, na.rm = TRUE),
  look_neg_base = mean(look_neg_base, na.rm = TRUE),
  reg_neg_base = mean(reg_neg_base, na.rm = TRUE)
), by = .(cluster, dataset, subject)]
fwrite(cluster_subject, file.path(table_dir, paste0("mcmc_cluster_subject_effects_", tag, ".csv")))

cluster_profile <- cluster_subject[, {
  cbind(
    effect_row(emotion_generation, "emotion_generation", as.character(CFG$bf_method)),
    effect_row(reappraisal_effect, "reappraisal_effect", as.character(CFG$bf_method)),
    effect_row(look_neg_base, "look_neg_base", as.character(CFG$bf_method)),
    effect_row(reg_neg_base, "reg_neg_base", as.character(CFG$bf_method))
  )
}, by = cluster]
cluster_profile[, n_rois := posterior[, .N, by = cluster][cluster_profile, on = "cluster"]$N]
cluster_profile[, mean_max_posterior := posterior[, .(mean_max_posterior = mean(max_posterior)), by = cluster][cluster_profile, on = "cluster"]$mean_max_posterior]
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
fwrite(cluster_profile, file.path(table_dir, paste0("mcmc_cluster_profile_", tag, ".csv")))

pool_note <- data.table(
  n_roi_units = nrow(posterior),
  n_source_labels_retained = uniqueN(posterior$source_system),
  note = "Route B uses Bayesian hierarchical ROI mixture clustering. All original ROI observations are pooled in one model; source_system is retained only for provenance and is not used for grouping."
)
fwrite(pool_note, file.path(table_dir, paste0("mcmc_roi_pool_metadata_", tag, ".csv")))

model_view <- copy(model_comparison)
for (nm in c(
  "mean_loglik", "aic_approx", "bic_approx", "waic", "looic", "mean_membership_entropy",
  "mean_max_posterior", "mean_max_posterior_probability", "stable_080_fraction", "mean_coclustering_entropy",
  "coclustering_sharpness", "confident_pair_fraction", "interpretable_cluster_fraction",
  "model_performance_score", "assignment_stability_score", "coclustering_score",
  "interpretability_score", "overall_score"
)) {
  if (nm %in% names(model_view)) model_view[, (nm) := fmt(get(nm))]
}
recommended <- model_comparison[overall_recommended_K == TRUE][1]
write_md(file.path(table_dir, paste0("mcmc_k_evaluation_", tag, ".md")), c(
  paste0("# 路线 B：", tag, " 的贝叶斯层级混合聚类 K 评估"),
  "",
  "本报告用于比较 K=2 到 K=6 之间不同终端 cluster 数的可行性。模型是贝叶斯层级 ROI 混合聚类：ROI 是终端分析单位，每个 ROI 内保留被试级观测，cluster 参数通过 NIW-Gaussian 层级混合模型进行 MCMC 抽样。",
  "",
  "## overall_recommended_K",
  "",
  paste0("- 推荐 K：", selected_k),
  paste0("- 推荐依据：overall_score=", fmt(recommended$overall_score), "；不是只看 BIC，而是综合模型表现、归属稳定性、共聚类结构和解释性。"),
  paste0("- 推荐 K 下的实际非空终端 cluster 数：", recommended$terminal_cluster_count),
  "",
  "## 评分逻辑",
  "",
  "- model_performance_score：由 BIC、AIC、WAIC、LOOIC 的相对排名合成，越高越好。",
  "- assignment_stability_score：由平均最大后验概率、稳定 ROI 比例和低分类熵合成，越高越好。",
  "- coclustering_score：由 MCMC 后验共聚类矩阵的清晰度和高置信成对关系比例合成，越高越好。",
  "- interpretability_score：由可解释 cluster 比例、非空 cluster 比例、cluster 大小均衡性和效应强度合成，越高越好。",
  "- overall_score：0.40*模型表现 + 0.25*归属稳定性 + 0.20*共聚类结构 + 0.15*解释性。",
  "",
  "## K 比较表",
  "",
  md_table(model_view, c(
    "requested_K", "occupied_K", "effective_K", "empty_cluster_count",
    "minimum_cluster_size", "maximum_cluster_size", "aic_approx", "bic_approx",
    "waic", "looic", "mean_max_posterior", "stable_080_fraction",
    "coclustering_sharpness", "confident_pair_fraction",
    "interpretable_cluster_fraction", "overall_score", "overall_recommended_K"
  )),
  "",
  "## 输出文件",
  "",
  paste0("- `mcmc_model_comparison_", tag, ".csv`：完整 K 评估表。"),
  paste0("- `mcmc_overall_recommended_K_", tag, ".csv`：自动推荐 K 的单行结果。"),
  paste0("- `mcmc_coclustering_KXX_", tag, ".csv`：每个 K 的 ROI 后验共聚类矩阵。"),
  paste0("- `mcmc_coclustering_", tag, ".csv`：推荐 K 对应的共聚类矩阵。"),
  paste0("- `mcmc_fit_", tag, ".rds`：保存每个 K 的 MCMC 后验对象，包括 z_draws。")
))

cat("Route B MCMC complete:\n")
cat("  overall_recommended_K = ", selected_k, "\n", sep = "")
cat("  ", posterior_path, "\n", sep = "")
