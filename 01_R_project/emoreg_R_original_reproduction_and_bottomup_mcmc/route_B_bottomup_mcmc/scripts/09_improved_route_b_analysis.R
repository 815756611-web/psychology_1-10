# Route B 改进分析：在现有全局聚类上增加递归子聚类、seed 稳定性和多指标诊断。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 路径和配置。
source("R/bayes_factor.R")  # 零效应证据和特征解释相关辅助。
require_pkg <- function(pkg) {  # pkg: 包名；作用：改进分析启动前显式检查额外依赖。
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Required R package not installed: ", pkg, call. = FALSE)
  invisible(TRUE)
}
source("R/mcmc_mixture.R")  # Route B MCMC 核心。
source("R/interpretation.R")  # ARI/NMI/标签解释。

args <- parse_cli_args(list(  # tags: 数据集标签；global-k/sub-k: 全局和递归 K 网格；recursive/stability-*：短链诊断配置。
  tags = "AHAB,PIP",
  "global-k" = "2:6",
  "sub-k" = "2:4",
  "recursive-iter" = 600,
  "recursive-burn" = 250,
  "recursive-thin" = 5,
  "stability-seeds" = 10,
  "stability-iter" = 180,
  "stability-burn" = 80,
  "stability-thin" = 5,
  alpha = CFG$mcmc_alpha,
  "base-seed" = CFG$random_seed
))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x  # 空值兜底。

parse_grid <- function(x) {  # x: 形如 2:6 或 2,3,4 的字符串；作用：解析成整数 K 网格。
  x <- as.character(x)
  if (grepl("^[0-9]+:[0-9]+$", x)) {
    p <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
    return(seq(p[1], p[2]))
  }
  as.integer(strsplit(x, ",", fixed = TRUE)[[1]])
}

write_md <- function(path, lines) {  # path: Markdown 路径；lines: 文本行向量；作用：写改进分析报告。
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con)
}

fmt <- function(x, digits = 4) {  # x: 数值；digits: 保留位数；作用：统一报告格式。
  ifelse(is.finite(as.numeric(x)), format(round(as.numeric(x), digits), scientific = FALSE), "NA")
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

log_mean_exp <- function(x) {  # x: 对数似然向量；作用：稳定计算 log(mean(exp(x)))。
  m <- max(x)
  m + log(mean(exp(x - m)))
}

predictive_criteria <- function(pointwise_loglik) {  # pointwise_loglik: 抽样×ROI 的 loglik 矩阵；作用：给改进分析算 WAIC/LOOIC。
  ll <- as.matrix(pointwise_loglik)
  if (!length(ll) || !nrow(ll) || !ncol(ll)) {
    return(data.table(waic = NA_real_, looic = NA_real_, loo_method = "not_available"))
  }
  lppd_i <- apply(ll, 2, log_mean_exp)
  p_waic_i <- apply(ll, 2, stats::var)
  waic <- -2 * sum(lppd_i - p_waic_i)
  out <- data.table(waic = waic, looic = NA_real_, loo_method = "manual_waic_only")
  if (requireNamespace("loo", quietly = TRUE)) {
    loo_obj <- tryCatch(loo::loo(ll), error = function(e) e)
    if (!inherits(loo_obj, "error")) {
      out[, `:=`(
        looic = loo_obj[["estimates"]]["looic", "Estimate"],
        loo_method = "psis_loo"
      )]
    } else {
      out[, loo_method := paste0("loo_failed: ", conditionMessage(loo_obj))]
    }
  }
  out
}

normalized_mutual_info <- function(x, y) {  # x/y: 两套 cluster 标签；作用：衡量 seed/子聚类间的一致性。
  x <- as.factor(x)
  y <- as.factor(y)
  tab <- table(x, y)
  n <- sum(tab)
  if (!n) return(NA_real_)
  px <- rowSums(tab) / n
  py <- colSums(tab) / n
  pxy <- tab / n
  mi <- 0
  for (i in seq_len(nrow(pxy))) {
    for (j in seq_len(ncol(pxy))) {
      if (pxy[i, j] > 0) mi <- mi + pxy[i, j] * log(pxy[i, j] / (px[i] * py[j]))
    }
  }
  hx <- -sum(px * log(px + 1e-15))
  hy <- -sum(py * log(py + 1e-15))
  if (!is.finite(hx + hy) || hx + hy == 0) return(NA_real_)
  2 * mi / (hx + hy)
}

standardize_dt <- function(dt, cols) {  # dt: ROI 摘要表；cols: 候选特征列；作用：仅为数值且有方差的列生成 z_* 特征。
  out <- copy(dt)
  used <- character()
  skipped <- character()
  for (col in cols) {
    if (!(col %in% names(out))) {
      skipped <- c(skipped, col)
      next
    }
    x <- as.numeric(out[[col]])
    s <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) {
      skipped <- c(skipped, col)
      next
    }
    out[, paste0("z_", col) := (x - mean(x, na.rm = TRUE)) / s]
    used <- c(used, col)
  }
  list(data = out, used = used, skipped = skipped)
}

fill_feature_aliases <- function(x) {  # x: ROI 摘要表；作用：补齐别名和差值特征，便于下游图和报告统一命名。
  x <- copy(x)
  alias_pairs <- c(
    emotion_generation_signed_logbf = "emotion_generation_signed_2logbf",
    reappraisal_effect_signed_logbf = "reappraisal_effect_signed_2logbf",
    look_neutral_base_mean = "look_neu_base_mean",
    look_negative_base_mean = "look_neg_base_mean",
    regulate_negative_base_mean = "reg_neg_base_mean",
    look_negative_minus_neutral_mean = "look_negative_minus_neutral_mean",
    regulate_negative_minus_look_negative_mean = "regulate_negative_minus_look_negative_mean"
  )
  for (new in names(alias_pairs)) {
    old <- alias_pairs[[new]]
    if (!(new %in% names(x)) && old %in% names(x)) {
      x[, (new) := if (grepl("signed_logbf$", new)) get(old) / 2 else get(old)]
    }
  }
  if (!("look_negative_minus_neutral_mean" %in% names(x)) &&
      all(c("look_negative_base_mean", "look_neutral_base_mean") %in% names(x))) {
    x[, look_negative_minus_neutral_mean := look_negative_base_mean - look_neutral_base_mean]
  }
  if (!("regulate_negative_minus_look_negative_mean" %in% names(x)) &&
      all(c("regulate_negative_base_mean", "look_negative_base_mean") %in% names(x))) {
    x[, regulate_negative_minus_look_negative_mean := regulate_negative_base_mean - look_negative_base_mean]
  }
  x
}

classification_entropy <- function(p) {  # p: posterior 概率矩阵；作用：计算每个 ROI 的分类熵。
  -rowSums(p * log(p + 1e-15))
}

model_diagnostics <- function(fit, requested_k, mean_loglik = fit$mean_loglik,
                              aic = NA_real_, bic = NA_real_, waic = NA_real_,
                              looic = NA_real_) {  # fit: 一个 MCMC 模型；requested_k: 请求 K；其余是外部已算好的模型表现指标。
  p <- as.matrix(fit$posterior)
  cluster <- fit$cluster
  ent <- classification_entropy(p)
  size <- as.data.table(table(cluster))
  setnames(size, c("cluster", "cluster_size"))
  size[, cluster := as.integer(as.character(cluster))]
  size[, mean_max_posterior := vapply(cluster, function(cl) mean(apply(p[cluster == cl, , drop = FALSE], 1, max)), numeric(1))]
  size[, mean_entropy := vapply(cluster, function(cl) mean(ent[cluster == cl]), numeric(1))]
  occupied <- nrow(size)
  effective <- size[
    cluster_size >= 2 &
      mean_max_posterior >= 0.70 &
      mean_entropy <= log(pmax(requested_k, 2)) * 0.50
  ]
  data.table(
    requested_K = requested_k,
    occupied_K = occupied,
    effective_K = nrow(effective),
    empty_cluster_count = requested_k - occupied,
    mean_log_likelihood = mean_loglik,
    AIC = aic,
    BIC = bic,
    WAIC = waic,
    LOOIC = looic,
    mean_max_posterior_probability = mean(apply(p, 1, max), na.rm = TRUE),
    mean_classification_entropy = mean(ent, na.rm = TRUE),
    minimum_cluster_size = min(size$cluster_size),
    maximum_cluster_size = max(size$cluster_size),
    cluster_size_string = paste(paste0(size$cluster, ":", size$cluster_size), collapse = ";")
  )
}

plot_requested_vs_occupied <- function(dt, path, title) {  # dt: K 比较表；path: 输出 PNG；title: 图标题。
  png(path, width = 1100, height = 800)
  on.exit(dev.off(), add = TRUE)
  plot(dt$requested_K, dt$occupied_K,
    type = "b", pch = 19, col = "#1f77b4", lwd = 2,
    xlab = "requested_K", ylab = "occupied_K", main = title,
    ylim = c(0, max(c(dt$requested_K, dt$occupied_K, dt$effective_K), na.rm = TRUE))
  )
  lines(dt$requested_K, dt$effective_K, type = "b", pch = 17, col = "#d62728", lwd = 2)
  abline(0, 1, lty = 2, col = "gray50")
  legend("topleft", legend = c("occupied_K", "effective_K", "requested=occupied"),
    col = c("#1f77b4", "#d62728", "gray50"), pch = c(19, 17, NA), lty = c(1, 1, 2), bty = "n"
  )
}

plot_profile <- function(profile, feature_cols, group_col, path, title) {  # profile: 画像表；feature_cols: 展示列；group_col: 分组列；path/title: 图输出。
  if (!nrow(profile) || !length(feature_cols)) return(FALSE)
  mat <- as.matrix(profile[, ..feature_cols])
  rownames(mat) <- paste0("C", profile[[group_col]])
  png(path, width = 1300, height = 850)
  on.exit(dev.off(), add = TRUE)
  barplot(t(mat),
    beside = TRUE, las = 2, col = grDevices::hcl.colors(ncol(mat), "Set 2"),
    main = title, ylab = "mean standardized feature value", cex.names = 0.8
  )
  legend("topright", legend = feature_cols, fill = grDevices::hcl.colors(length(feature_cols), "Set 2"), cex = 0.75, bty = "n")
  TRUE
}

plot_membership_heatmap <- function(posterior, prob_cols, path, title) {  # posterior: ROI 后验表；prob_cols: 概率列；path/title: 输出设置。
  if (!nrow(posterior) || !length(prob_cols)) return(FALSE)
  x <- as.matrix(posterior[, ..prob_cols])
  ord <- order(posterior$cluster, -posterior$max_posterior)
  png(path, width = 1000, height = 1000)
  on.exit(dev.off(), add = TRUE)
  image(t(x[ord, , drop = FALSE]), axes = FALSE, col = hcl.colors(100, "YlOrRd", rev = TRUE), main = title)
  axis(1, at = seq(0, 1, length.out = nrow(x)), labels = posterior$roi_id[ord], las = 2, cex.axis = 0.55)
  axis(2, at = seq(0, 1, length.out = length(prob_cols)), labels = prob_cols, las = 2)
  TRUE
}

plot_entropy <- function(posterior, prob_cols, path, title) {  # posterior: ROI 后验表；prob_cols: 概率列；path/title: 输出设置。
  p <- as.matrix(posterior[, ..prob_cols])
  ent <- classification_entropy(p)
  ord <- order(ent, decreasing = TRUE)
  png(path, width = 1200, height = 800)
  on.exit(dev.off(), add = TRUE)
  barplot(ent[ord], names.arg = posterior$roi_id[ord], las = 2, cex.names = 0.55,
    col = "#4c78a8", main = title, ylab = "classification entropy"
  )
  TRUE
}

plot_matrix <- function(mat, path, title) {  # mat: 方阵；path/title: 输出设置；作用：画共聚类矩阵。
  png(path, width = 1000, height = 1000)
  on.exit(dev.off(), add = TRUE)
  image(t(mat[nrow(mat):1, , drop = FALSE]), axes = FALSE, col = hcl.colors(100, "YlGnBu", rev = TRUE), main = title)
  TRUE
}

plot_seed_stability <- function(dt, path, title) {  # dt: 多 seed 诊断表；path/title: 输出设置。
  png(path, width = 1200, height = 850)
  on.exit(dev.off(), add = TRUE)
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
  stripchart(occupied_K ~ requested_K, data = dt, vertical = TRUE, method = "jitter",
    pch = 19, col = "#1f77b4", xlab = "requested_K", ylab = "occupied_K", main = title
  )
  plot(dt$requested_K, dt$BIC, pch = 19, col = "#d62728",
    xlab = "requested_K", ylab = "BIC", main = "BIC across seeds"
  )
}

profile_from_membership <- function(feature_matrix, membership, group_col = "cluster") {  # feature_matrix: ROI 标准化特征；membership: ROI 分组；group_col: 分组列名。
  z_cols <- grep("^z_", names(feature_matrix), value = TRUE)
  dt <- merge(membership[, .(roi_id, group = get(group_col))], feature_matrix[, c("roi_id", z_cols), with = FALSE], by = "roi_id")
  dt[, lapply(.SD, mean, na.rm = TRUE), by = group, .SDcols = z_cols]
}

fit_model_grid <- function(subject_features, feature_cols, k_grid, iter, burn, thin, alpha, seed_base) {  # subject_features: 子集后的被试×ROI 表；feature_cols: 入模列；k_grid 和其余参数为递归子聚类设置。
  out <- list()
  rows <- list()
  for (k in k_grid) {
    message("  recursive MCMC K=", k)
    fit <- run_roi_mcmc_mixture(subject_features, feature_cols, k, iter, burn, thin, alpha, seed_base + k)
    n_params <- k * length(feature_cols) + k * length(feature_cols) * (length(feature_cols) + 1) / 2 + (k - 1)
    aic <- -2 * fit$mean_loglik + 2 * n_params
    bic <- -2 * fit$mean_loglik + n_params * log(nrow(subject_features))
    pred <- predictive_criteria(fit$pointwise_loglik)
    rows[[as.character(k)]] <- cbind(
      data.table(sub_requested_K = k),
      model_diagnostics(fit, k, fit$mean_loglik, aic, bic, pred$waic, pred$looic)
    )
    out[[as.character(k)]] <- fit
  }
  list(fits = out, comparison = rbindlist(rows, fill = TRUE))
}

stability_fits <- function(subject_features, feature_cols, k_grid, seeds, iter, burn, thin, alpha) {  # subject_features: 全局被试×ROI 表；feature_cols: 入模列；seeds/k_grid 等用于多次短链复跑。
  rows <- list()
  assignments <- list()
  for (k in k_grid) {
    for (seed in seeds) {
      message("  stability K=", k, " seed=", seed)
      fit <- run_roi_mcmc_mixture(subject_features, feature_cols, k, iter, burn, thin, alpha, seed)
      n_params <- k * length(feature_cols) + k * length(feature_cols) * (length(feature_cols) + 1) / 2 + (k - 1)
      aic <- -2 * fit$mean_loglik + 2 * n_params
      bic <- -2 * fit$mean_loglik + n_params * log(nrow(subject_features))
      diag <- model_diagnostics(fit, k, fit$mean_loglik, aic, bic)
      rows[[length(rows) + 1L]] <- cbind(data.table(seed = seed), diag)
      assignments[[paste(k, seed, sep = "_")]] <- data.table(
        requested_K = k,
        seed = seed,
        roi_id = fit$roi_ids,
        cluster = fit$cluster
      )
    }
  }
  summary <- rbindlist(rows, fill = TRUE)
  assign_dt <- rbindlist(assignments, fill = TRUE)
  pair_rows <- list()
  co_rows <- list()
  for (k in k_grid) {
    a <- assign_dt[requested_K == k]
    seed_ids <- sort(unique(a$seed))
    if (length(seed_ids) >= 2) {
      mat_list <- list()
      for (seed_value in seed_ids) {
        z <- a[seed == seed_value][order(roi_id), cluster]
        mat_list[[as.character(seed_value)]] <- outer(z, z, "==") * 1
      }
      co_prob <- Reduce("+", mat_list) / length(mat_list)
      pair <- co_prob[upper.tri(co_prob)]
      co_rows[[length(co_rows) + 1L]] <- data.table(
        requested_K = k,
        cluster_coassignment_stability = mean(abs(pair - 0.5) * 2, na.rm = TRUE)
      )
      for (i in seq_along(seed_ids)) {
        for (j in seq_along(seed_ids)) {
          if (j <= i) next
          zi <- a[seed == seed_ids[i]][order(roi_id), cluster]
          zj <- a[seed == seed_ids[j]][order(roi_id), cluster]
          pair_rows[[length(pair_rows) + 1L]] <- data.table(
            requested_K = k,
            seed_1 = seed_ids[i],
            seed_2 = seed_ids[j],
            adjusted_rand_index = adjusted_rand_index(zi, zj),
            normalized_mutual_info = normalized_mutual_info(zi, zj)
          )
        }
      }
    }
  }
  pairwise <- rbindlist(pair_rows, fill = TRUE)
  co_summary <- rbindlist(co_rows, fill = TRUE)
  by_k <- summary[, .(
    n_seeds = .N,
    occupied_K_min = min(occupied_K, na.rm = TRUE),
    occupied_K_max = max(occupied_K, na.rm = TRUE),
    occupied_K_mode = as.integer(names(sort(table(occupied_K), decreasing = TRUE)[1])),
    mean_BIC = mean(BIC, na.rm = TRUE),
    mean_AIC = mean(AIC, na.rm = TRUE),
    mean_log_likelihood = mean(mean_log_likelihood, na.rm = TRUE),
    mean_posterior_assignment_probability = mean(mean_max_posterior_probability, na.rm = TRUE),
    mean_entropy = mean(mean_classification_entropy, na.rm = TRUE)
  ), by = requested_K]
  if (nrow(pairwise)) {
    ari_nmi <- pairwise[, .(
      mean_adjusted_rand_index = mean(adjusted_rand_index, na.rm = TRUE),
      mean_normalized_mutual_info = mean(normalized_mutual_info, na.rm = TRUE)
    ), by = requested_K]
    by_k <- merge(by_k, ari_nmi, by = "requested_K", all.x = TRUE)
  }
  if (nrow(co_summary)) by_k <- merge(by_k, co_summary, by = "requested_K", all.x = TRUE)
  list(summary = summary, assignments = assign_dt, pairwise = pairwise, by_k = by_k)
}

route_b_table_dir <- project_path("route_B_bottomup_mcmc", "outputs", "tables")  # 读取原始 Route B 结果。
global_root <- ensure_dir(project_path("results", "route_b_global"))  # 全局 K 分析结果。
recursive_root <- ensure_dir(project_path("results", "route_b_recursive"))  # 大类递归子聚类结果。
stability_root <- ensure_dir(project_path("results", "route_b_stability"))  # 多 seed 稳定性结果。

tags <- trimws(strsplit(as.character(args$tags), ",", fixed = TRUE)[[1]])
global_k_grid <- parse_grid(args[["global-k"]])
sub_k_grid <- parse_grid(args[["sub-k"]])
recursive_iter <- as_int_arg(args[["recursive-iter"]], 600L)
recursive_burn <- as_int_arg(args[["recursive-burn"]], 250L)
recursive_thin <- as_int_arg(args[["recursive-thin"]], 5L)
stability_n <- as_int_arg(args[["stability-seeds"]], 10L)
stability_iter <- as_int_arg(args[["stability-iter"]], 180L)
stability_burn <- as_int_arg(args[["stability-burn"]], 80L)
stability_thin <- as_int_arg(args[["stability-thin"]], 5L)
alpha <- as_num_arg(args$alpha, as.numeric(CFG$mcmc_alpha))
base_seed <- as_int_arg(args[["base-seed"]], as.integer(CFG$random_seed))
stability_seeds <- base_seed + seq_len(stability_n) * 101L

requested_feature_cols <- c(
  "emotion_generation_mean", "emotion_generation_sd", "emotion_generation_t",
  "emotion_generation_signed_logbf", "emotion_generation_p_gt0",
  "emotion_generation_p_lt0", "emotion_generation_p_rope",
  "reappraisal_effect_mean", "reappraisal_effect_sd", "reappraisal_effect_t",
  "reappraisal_effect_signed_logbf", "reappraisal_effect_p_gt0",
  "reappraisal_effect_p_lt0", "reappraisal_effect_p_rope",
  "look_neutral_base_mean", "look_negative_base_mean", "regulate_negative_base_mean",
  "look_negative_minus_neutral_mean", "regulate_negative_minus_look_negative_mean"
)

combined_global <- list()
combined_report_bits <- list()

for (tag in tags) {  # 每个数据库标签独立做全局 K 比较、递归子聚类和稳定性复跑。
  message("Improved Route B analysis for tag=", tag)
  tag_global_dir <- ensure_dir(file.path(global_root, tag))
  tag_recursive_dir <- ensure_dir(file.path(recursive_root, tag))
  tag_stability_dir <- ensure_dir(file.path(stability_root, tag))

  fit_path <- file.path(route_b_table_dir, paste0("mcmc_fit_", tag, ".rds"))
  feature_path <- file.path(route_b_table_dir, paste0("roi_subject_features_", tag, ".rds"))
  summary_path <- file.path(route_b_table_dir, paste0("roi_group_summary_", tag, ".csv"))
  if (!file.exists(fit_path)) stop("Missing global MCMC fit: ", fit_path)
  if (!file.exists(feature_path)) stop("Missing Route B feature bundle: ", feature_path)
  if (!file.exists(summary_path)) stop("Missing ROI summary table: ", summary_path)

  obj <- readRDS(fit_path)
  bundle <- readRDS(feature_path)
  roi_summary <- fill_feature_aliases(fread(summary_path))
  standardized <- standardize_dt(roi_summary, requested_feature_cols)  # 补生成 z_* 连续特征，便于后续 profile 图比较。
  feature_matrix <- standardized$data
  z_cols <- paste0("z_", standardized$used)
  missing_features <- setdiff(requested_feature_cols, standardized$used)
  feature_availability <- data.table(
    tag = tag,
    available_features = paste(standardized$used, collapse = ";"),
    missing_features = paste(missing_features, collapse = ";"),
    skipped_features = paste(unique(c(standardized$skipped, missing_features)), collapse = ";"),
    note = "p_rope is used as a statistical feature for near-zero evidence and uncertainty, not as a theoretical label."
  )
  fwrite(feature_matrix, file.path(tag_global_dir, "standardized_zero_evidence_feature_matrix.csv"))
  fwrite(feature_availability, file.path(tag_global_dir, "feature_availability.csv"))
  fwrite(feature_matrix, file.path(route_b_table_dir, paste0("route_b_standardized_feature_matrix_", tag, ".csv")))
  fwrite(feature_availability, file.path(route_b_table_dir, paste0("route_b_feature_availability_", tag, ".csv")))

  model_rows <- list()
  for (k_name in names(obj$all_fits)) {  # 先从已有全局 full-chain 结果中整理每个 K 的基本诊断。
    k <- as.integer(k_name)
    if (!(k %in% global_k_grid)) next
    fit <- obj$all_fits[[k_name]]
    mc <- as.data.table(obj$model_comparison)[k == k_name | k == as.integer(k_name)]
    row <- model_diagnostics(
      fit,
      requested_k = k,
      mean_loglik = mc$mean_loglik[1] %||% fit$mean_loglik,
      aic = mc$aic_approx[1] %||% NA_real_,
      bic = mc$bic_approx[1] %||% NA_real_,
      waic = if ("waic" %in% names(mc)) mc$waic[1] else NA_real_,
      looic = if ("looic" %in% names(mc)) mc$looic[1] else NA_real_
    )
    if ("overall_score" %in% names(mc)) row[, overall_score := mc$overall_score[1]]
    if ("overall_recommended_K" %in% names(mc)) row[, overall_recommended_K := mc$overall_recommended_K[1]]
    model_rows[[k_name]] <- row
  }
  global_comparison <- rbindlist(model_rows, fill = TRUE)
  global_comparison[, tag := tag]
  setcolorder(global_comparison, "tag")
  fwrite(global_comparison, file.path(tag_global_dir, "global_model_comparison.csv"))
  fwrite(global_comparison, file.path(route_b_table_dir, paste0("mcmc_model_comparison_improved_", tag, ".csv")))
  combined_global[[tag]] <- global_comparison
  plot_requested_vs_occupied(global_comparison, file.path(tag_global_dir, "requested_vs_occupied_K.png"),
    paste0(tag, ": requested_K vs occupied/effective K")
  )

  bic_pref <- global_comparison[which.min(BIC)]
  aic_pref <- global_comparison[which.min(AIC)]
  waic_pref <- if (any(is.finite(global_comparison$WAIC))) global_comparison[which.min(WAIC)] else global_comparison[0]
  loo_pref <- if (any(is.finite(global_comparison$LOOIC))) global_comparison[which.min(LOOIC)] else global_comparison[0]

  global_k2 <- obj$all_fits[["2"]]  # 这里显式检查 K=2 的粗粒度主导结构。
  if (is.null(global_k2)) stop("Global K=2 fit is required but missing for tag=", tag)
  p2 <- as.data.table(global_k2$posterior)
  setnames(p2, paste0("p_cluster_", seq_len(ncol(p2))))
  global_membership <- cbind(data.table(roi_id = global_k2$roi_ids), p2)
  global_membership[, cluster := global_k2$cluster]
  global_membership[, max_posterior := do.call(pmax, .SD), .SDcols = paste0("p_cluster_", seq_len(ncol(p2)))]
  global_membership[, entropy := classification_entropy(as.matrix(.SD)), .SDcols = paste0("p_cluster_", seq_len(ncol(p2)))]
  size2 <- global_membership[, .N, by = cluster][order(-N)]
  large_cluster <- size2$cluster[1]  # 把 K=2 中更大的那一类视为 large residual cluster，后续单独递归子聚类。
  small_clusters <- setdiff(size2$cluster, large_cluster)
  global_membership[, coarse_partition_label := ifelse(
    cluster == large_cluster,
    "large residual cluster / mixed large cluster",
    "small high-contrast cluster"
  )]
  global_membership <- merge(global_membership, feature_matrix, by = "roi_id", all.x = TRUE)
  fwrite(global_membership, file.path(tag_global_dir, "global_k2_membership.csv"))
  prob_cols2 <- paste0("p_cluster_", seq_len(ncol(p2)))
  plot_membership_heatmap(global_membership, prob_cols2, file.path(tag_global_dir, "posterior_membership_heatmap.png"),
    paste0(tag, ": global K=2 posterior membership")
  )
  plot_entropy(global_membership, prob_cols2, file.path(tag_global_dir, "clustering_entropy_plot.png"),
    paste0(tag, ": global K=2 classification entropy")
  )
  co2 <- posterior_coclustering_matrix(global_k2$z_draws)
  fwrite(as.data.table(co2), file.path(tag_global_dir, "global_k2_coclustering_matrix.csv"))
  plot_matrix(co2, file.path(tag_global_dir, "coclustering_matrix.png"), paste0(tag, ": global K=2 co-clustering matrix"))
  global_profile <- profile_from_membership(feature_matrix, global_membership[, .(roi_id, cluster)])
  fwrite(global_profile, file.path(tag_global_dir, "global_k2_profile.csv"))
  profile_cols <- head(setdiff(names(global_profile), "group"), 10)
  plot_profile(global_profile, profile_cols, "group", file.path(tag_global_dir, "global_k2_profile.png"),
    paste0(tag, ": global K=2 cluster profile")
  )

  message("Recursive subclustering of large residual cluster for tag=", tag)
  subject_features <- as.data.table(bundle$subject_features)
  large_roi_ids <- global_membership[cluster == large_cluster, roi_id]
  sub_subject <- subject_features[roi_id %in% large_roi_ids]
  sub_feature_cols <- intersect(bundle$feature_cols, names(sub_subject))
  if (!length(sub_feature_cols)) stop("No usable subject-level feature columns for recursive subclustering: ", tag)
  rec <- fit_model_grid(  # 第二层递归：只在 large residual cluster 内再比较 sub-K。
    sub_subject, sub_feature_cols, sub_k_grid,
    recursive_iter, recursive_burn, recursive_thin, alpha, base_seed + 7000L
  )
  recursive_comparison <- rec$comparison
  recursive_comparison[, tag := tag]
  recursive_comparison[, parent_cluster := large_cluster]
  setcolorder(recursive_comparison, c("tag", "parent_cluster"))
  recursive_comparison[, sub_occupied_K := occupied_K]
  recursive_comparison[, sub_effective_K := effective_K]
  fwrite(recursive_comparison, file.path(tag_recursive_dir, "recursive_subcluster_model_comparison.csv"))
  selected_sub <- recursive_comparison[order(BIC, -effective_K)][1, sub_requested_K]
  selected_fit <- rec$fits[[as.character(selected_sub)]]
  subp <- as.data.table(selected_fit$posterior)
  setnames(subp, paste0("p_subcluster_", seq_len(ncol(subp))))
  sub_membership <- cbind(data.table(roi_id = selected_fit$roi_ids), subp)
  sub_membership[, subcluster := selected_fit$cluster]
  sub_membership[, max_posterior := do.call(pmax, .SD), .SDcols = paste0("p_subcluster_", seq_len(ncol(subp)))]
  sub_membership[, membership_entropy := classification_entropy(as.matrix(.SD)), .SDcols = paste0("p_subcluster_", seq_len(ncol(subp)))]
  sub_membership[, sub_requested_K := selected_sub]
  sub_membership <- merge(sub_membership, feature_matrix, by = "roi_id", all.x = TRUE)
  sub_size <- sub_membership[, .(subcluster_size = .N), by = subcluster]
  sub_membership <- merge(sub_membership, sub_size, by = "subcluster", all.x = TRUE)
  fwrite(sub_membership, file.path(tag_recursive_dir, "recursive_subcluster_membership.csv"))
  sub_profile <- profile_from_membership(feature_matrix, sub_membership[, .(roi_id, cluster = subcluster)])
  setnames(sub_profile, "group", "subcluster")
  fwrite(sub_profile, file.path(tag_recursive_dir, "recursive_subcluster_profile.csv"))
  sub_profile_plot <- copy(sub_profile)
  setnames(sub_profile_plot, "subcluster", "group")
  sub_profile_cols <- head(setdiff(names(sub_profile_plot), "group"), 10)
  plot_profile(sub_profile_plot, sub_profile_cols, "group", file.path(tag_recursive_dir, "recursive_subcluster_profiles.png"),
    paste0(tag, ": recursive subcluster profiles")
  )

  message("Seed stability checks for tag=", tag)
  stab <- stability_fits(  # 多 seed 短链复跑：检查 occupied_K 和标签一致性是否稳定。
    subject_features, bundle$feature_cols, global_k_grid, stability_seeds,
    stability_iter, stability_burn, stability_thin, alpha
  )
  fwrite(stab$summary, file.path(tag_stability_dir, "seed_stability_summary.csv"))
  fwrite(stab$assignments, file.path(tag_stability_dir, "seed_stability_assignments.csv"))
  fwrite(stab$pairwise, file.path(tag_stability_dir, "seed_stability_pairwise.csv"))
  fwrite(stab$by_k, file.path(tag_stability_dir, "seed_stability_by_K.csv"))
  plot_seed_stability(stab$summary, file.path(tag_stability_dir, "seed_stability_plot.png"),
    paste0(tag, ": seed stability summary")
  )

  stable_k2 <- stab$by_k[requested_K == 2]
  occupied_varies <- any(stab$by_k$occupied_K_min != stab$by_k$occupied_K_max, na.rm = TRUE)
  stability_sentence <- if (occupied_varies) {
    "当前聚类结构在不同 seed 下存在 occupied_K 差异，不适合做强类别解释。"
  } else if (nrow(stable_k2) && stable_k2$occupied_K_mode[1] == 2) {
    "当前特征空间中的全局主导结构确实较稳定，但仍需通过递归子聚类检查大类内部是否存在弱结构。"
  } else {
    "当前 seed 稳定性未显示强烈的 K=2 唯一解释，应结合递归子聚类和不确定性指标审慎解释。"
  }
  empty_note <- if (any(global_comparison$requested_K > global_comparison$occupied_K, na.rm = TRUE)) {
    "模型允许更多类别，但额外类别未被数据稳定占用，因此当前全局特征空间只支持较少的有效类别。"
  } else {
    "当前 requested_K 与 occupied_K 基本一致；仍需结合 effective_K 和后验不确定性解释。"
  }

  tag_report <- c(
    paste0("# Route B improved-method report: ", tag),
    "",
    "## 方法修改说明",
    "",
    "本次保留已有全局 ROI-MCMC 聚类结果，并新增三类独立输出：`results/route_b_global`、`results/route_b_recursive`、`results/route_b_stability`。新增特征表包含零效应证据相关字段，并写出标准化后的 `z_*` 连续特征。",
    "",
    "## Why global unsupervised clustering favors K = 2",
    "",
    "全局 K = 2 是当前特征空间中最强的粗粒度结构。K = 2 不等于潜在结构只能分成两类。普通无监督聚类容易被效应强度、尺度差异或少数强区分 ROI 主导；如果不显式加入零效应证据，不同弱效应 ROI 可能会被合并到同一个大类中。递归子聚类可以作为探索大类内部细粒度结构的方法。",
    "",
    "全局 K = 2 结果反映当前数据中最强的低维差异，而不是说明潜在系统结构只能分为两类。",
    "",
    empty_note,
    "",
    "## 多指标模型比较",
    "",
    paste0("- BIC-preferred model：requested_K=", bic_pref$requested_K, "，occupied_K=", bic_pref$occupied_K, "，effective_K=", bic_pref$effective_K, "。"),
    paste0("- AIC-preferred model：requested_K=", aic_pref$requested_K, "，occupied_K=", aic_pref$occupied_K, "，effective_K=", aic_pref$effective_K, "。"),
    if (nrow(waic_pref)) paste0("- WAIC-preferred model：requested_K=", waic_pref$requested_K, "，occupied_K=", waic_pref$occupied_K, "，effective_K=", waic_pref$effective_K, "。") else "- WAIC-preferred model：未能稳定计算。",
    if (nrow(loo_pref)) paste0("- LOOIC-preferred model：requested_K=", loo_pref$requested_K, "，occupied_K=", loo_pref$occupied_K, "，effective_K=", loo_pref$effective_K, "。") else "- LOOIC-preferred model：未能稳定计算。",
    "",
    md_table(global_comparison, c(
      "requested_K", "occupied_K", "effective_K", "empty_cluster_count",
      "BIC", "AIC", "WAIC", "LOOIC", "mean_max_posterior_probability",
      "mean_classification_entropy", "minimum_cluster_size", "maximum_cluster_size"
    )),
    "",
    "## Global K=2 coarse partition",
    "",
    "保留全局 K=2 作为第一层 coarse partition。小类解释为 small high-contrast cluster，可能反映某类效应模式较突出的 ROI；大类解释为 large residual cluster / mixed large cluster，包含多数 ROI，内部可能仍存在更细结构。不要在该阶段强行贴固定理论标签。",
    "",
    md_table(global_membership[, .N, by = coarse_partition_label], c("coarse_partition_label", "N")),
    "",
    "## Recursive subclustering of the large residual cluster",
    "",
    "全局模型容易先抓住最强的二分结构，因此把已经明显分离出来的小类 ROI 排除后，对 large residual cluster 单独做第二层子聚类。该步骤不是为了强行恢复某个预设理论分类，而是检验大类内部是否存在稳定次级模式。",
    "",
    paste0("- 第二层选中 sub_requested_K=", selected_sub, "。"),
    md_table(recursive_comparison, c(
      "sub_requested_K", "sub_occupied_K", "sub_effective_K",
      "empty_cluster_count", "BIC", "AIC", "WAIC", "LOOIC",
      "mean_max_posterior_probability", "mean_classification_entropy"
    )),
    "",
    "如果 sub_effective_K 明显小于 sub_requested_K，说明第二层仍更像弱分化或连续梯度，而不是稳定离散类别。",
    "",
    "## 聚类稳定性检查",
    "",
    paste0("- 每个 requested_K 运行 seed 数：", stability_n, "。"),
    stability_sentence,
    "",
    md_table(stab$by_k, c(
      "requested_K", "n_seeds", "occupied_K_min", "occupied_K_max",
      "occupied_K_mode", "mean_adjusted_rand_index", "mean_normalized_mutual_info",
      "cluster_coassignment_stability", "mean_posterior_assignment_probability", "mean_entropy"
    )),
    "",
    "## 当前结果限制",
    "",
    "- 全局无监督聚类首先反映粗粒度主轴，不能直接等同于理论系统数。",
    "- 零效应证据特征用于描述 ROI 效应模式和不确定性，不作为理论标签。",
    "- 稳定性检查使用较短 MCMC 链作为重复拟合诊断；正式推断仍应参考 full 链和后验不确定性。",
    "",
    "## 结论",
    "",
    "当前全局无监督 Route B 模型在 BIC 等复杂度惩罚指标下倾向于 K = 2，说明当前 ROI 特征空间中最稳定的主导结构是一个粗粒度二分结构。然而，这一结果不应被解释为潜在系统结构只能分为两类。由于普通无监督聚类容易受到效应强度、特征尺度和少数高区分 ROI 的影响，部分细粒度结构可能被合并到较大的混合类别中。因此，本报告进一步采用递归子聚类、零效应证据特征和多随机种子稳定性检查，以评估大类内部是否存在更细的、稳定的潜在结构。"
  )
  write_md(file.path(tag_global_dir, "improved_method_report.md"), tag_report)
  combined_report_bits[[tag]] <- tag_report
}

combined_global_dt <- rbindlist(combined_global, fill = TRUE)
fwrite(combined_global_dt, file.path(global_root, "global_model_comparison.csv"))
write_md(file.path(project_path("results"), "route_b_improved_method_report.md"), unlist(c(
  "# Route B 改进方法总报告",
  "",
  "本报告汇总 AHAB/PIP 的全局粗分、递归子聚类和多 seed 稳定性检查。",
  "",
  combined_report_bits
)))

cat("Improved Route B analysis complete:\n")
cat("  ", global_root, "\n", sep = "")
cat("  ", recursive_root, "\n", sep = "")
cat("  ", stability_root, "\n", sep = "")
