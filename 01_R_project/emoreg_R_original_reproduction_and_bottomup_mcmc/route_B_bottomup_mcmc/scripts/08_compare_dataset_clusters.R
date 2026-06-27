# Route B 跨样本比较：匹配 AHAB/PIP 的 ROI cluster，并生成模型共识图。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 路径和输出目录。
source("R/nifti_utils.R")  # NIfTI 写盘。
source("R/interpretation.R")  # ARI/Dice/Jaccard 等比较工具。

args <- parse_cli_args(list(  # tag1/tag2: 需要比较的两个数据库标签。
  tag1 = "AHAB",
  tag2 = "PIP"
))

write_md <- function(path, lines) {  # path: Markdown 路径；lines: 文本行向量；作用：写跨样本比较说明。
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con)
}

fmt <- function(x, digits = 4) {  # x: 数值；digits: 保留位数；作用：统一 README 数字格式。
  ifelse(is.finite(x), format(round(x, digits), scientific = FALSE), "NA")
}

entropy_of <- function(x) {  # x: cluster 标签向量；作用：度量单个标签分布的离散程度。
  tab <- table(x)
  p <- as.numeric(tab) / sum(tab)
  -sum(p * log(p + 1e-15))
}

normalized_mutual_info <- function(x, y) {  # x/y: 两套 ROI cluster 标签；作用：衡量 AHAB/PIP 标签信息共享程度。
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

selected_model_row <- function(obj, tag) {  # obj: 一个 tag 的 mcmc_fit_*.rds；tag: 标签名；作用：读出当前 tag 真正被选中的 K 行。
  mc <- as.data.table(obj$model_comparison)
  mc[, delta_bic := bic_approx - min(bic_approx, na.rm = TRUE)]
  if ("overall_recommended_K" %in% names(mc) && any(mc$overall_recommended_K %in% TRUE, na.rm = TRUE)) {
    mc[, is_selected := overall_recommended_K %in% TRUE]
    selection_rule <- "overall_recommended_K"
  } else {
    mc[, is_selected := delta_bic == min(delta_bic, na.rm = TRUE)]
    selection_rule <- "lowest_BIC_fallback"
  }
  row <- mc[is_selected == TRUE][1]
  row[, tag := tag]
  row[, selection_rule := selection_rule]
  setcolorder(row, c("tag", setdiff(names(row), "tag")))
  row
}

cluster_mask <- function(labels, roi_ids) {  # labels: ROI 标签图；roi_ids: 某个 cluster 的 ROI 集合；作用：把 ROI 集合重新映射到体素空间。
  labels %in% roi_ids
}

tag1 <- as.character(args$tag1)
tag2 <- as.character(args$tag2)
table_dir <- project_path("route_B_bottomup_mcmc", "outputs", "tables")
out_dir <- project_path("route_B_bottomup_mcmc", "outputs", "dataset_comparison", paste(tag1, tag2, sep = "_"))
invisible(ensure_dir(out_dir))
invisible(ensure_dir(file.path(out_dir, "nifti")))

fit_path1 <- file.path(table_dir, paste0("mcmc_fit_", tag1, ".rds"))
fit_path2 <- file.path(table_dir, paste0("mcmc_fit_", tag2, ".rds"))
if (!file.exists(fit_path1)) stop("Missing MCMC fit: ", fit_path1)
if (!file.exists(fit_path2)) stop("Missing MCMC fit: ", fit_path2)

obj1 <- readRDS(fit_path1)
obj2 <- readRDS(fit_path2)
post1 <- as.data.table(obj1$posterior)
post2 <- as.data.table(obj2$posterior)
labels <- obj1$feature_bundle$labels
ref <- obj1$feature_bundle$reference_nifti

model_rows <- rbindlist(list(
  selected_model_row(obj1, tag1),
  selected_model_row(obj2, tag2)
), fill = TRUE)
fwrite(model_rows, file.path(out_dir, "selected_model_evaluation.csv"))

common <- merge(  # 只在共同 ROI 上比较 AHAB/PIP 的最大后验归属。
  post1[, .(roi_id, cluster_1 = cluster, max_posterior_1 = max_posterior)],
  post2[, .(roi_id, cluster_2 = cluster, max_posterior_2 = max_posterior)],
  by = "roi_id"
)
agreement <- data.table(
  tag1 = tag1,
  tag2 = tag2,
  n_common_roi = nrow(common),
  adjusted_rand_index = adjusted_rand_index(common$cluster_1, common$cluster_2),
  normalized_mutual_info = normalized_mutual_info(common$cluster_1, common$cluster_2),
  mean_max_posterior_tag1 = mean(common$max_posterior_1, na.rm = TRUE),
  mean_max_posterior_tag2 = mean(common$max_posterior_2, na.rm = TRUE),
  entropy_tag1 = entropy_of(common$cluster_1),
  entropy_tag2 = entropy_of(common$cluster_2)
)
fwrite(agreement, file.path(out_dir, "cluster_assignment_agreement.csv"))

clusters1 <- sort(unique(post1$cluster))
clusters2 <- sort(unique(post2$cluster))
pair_rows <- list()
for (cl1 in clusters1) {
  ids1 <- post1[cluster == cl1, roi_id]
  m1 <- cluster_mask(labels, ids1)
  for (cl2 in clusters2) {
    ids2 <- post2[cluster == cl2, roi_id]
    m2 <- cluster_mask(labels, ids2)
    sim <- dice_jaccard(m1, m2)
    sim[, `:=`(
      tag1 = tag1,
      tag2 = tag2,
      cluster_tag1 = cl1,
      cluster_tag2 = cl2,
      shared_roi = length(intersect(ids1, ids2)),
      roi_tag1 = length(ids1),
      roi_tag2 = length(ids2)
    )]
    pair_rows[[length(pair_rows) + 1L]] <- sim
  }
}
pairs <- rbindlist(pair_rows, fill = TRUE)
setcolorder(pairs, c("tag1", "tag2", "cluster_tag1", "cluster_tag2", "dice", "jaccard",
  "intersection_voxels", "a_voxels", "b_voxels", "shared_roi", "roi_tag1", "roi_tag2"))
setorder(pairs, -dice, -jaccard)
fwrite(pairs, file.path(out_dir, "cluster_pair_spatial_similarity.csv"))

available <- copy(pairs)
matched_rows <- list()
used_1 <- integer()
used_2 <- integer()
while (nrow(available)) {  # 用贪心方式做一对一匹配：先拿 Dice 最高的一对，再剔除对应 cluster。
  best <- available[1]
  matched_rows[[length(matched_rows) + 1L]] <- best
  used_1 <- c(used_1, best$cluster_tag1)
  used_2 <- c(used_2, best$cluster_tag2)
  available <- available[!(cluster_tag1 %in% used_1 | cluster_tag2 %in% used_2)]
}
matched <- rbindlist(matched_rows, fill = TRUE)
if (nrow(matched)) {
  matched[, match_id := seq_len(.N)]
  setcolorder(matched, c("match_id", setdiff(names(matched), "match_id")))
}

consensus_labels <- array(0, dim = dim(labels))  # 把匹配成功的 cluster 交集写成一张标签图。
consensus_rows <- list()
for (i in seq_len(nrow(matched))) {
  cl1 <- matched$cluster_tag1[i]
  cl2 <- matched$cluster_tag2[i]
  ids1 <- post1[cluster == cl1, roi_id]
  ids2 <- post2[cluster == cl2, roi_id]
  shared_ids <- intersect(ids1, ids2)
  consensus <- cluster_mask(labels, shared_ids)
  n_vox <- sum(consensus)
  if (n_vox > 0) {
    consensus_labels[consensus] <- matched$match_id[i]
    out_path <- file.path(out_dir, "nifti", sprintf("m%02d_%s%02d_%s%02d.nii.gz",
      matched$match_id[i], tag1, cl1, tag2, cl2))
    write_nifti_array(array(as.numeric(consensus), dim = dim(labels)), ref, out_path)
  } else {
    out_path <- NA_character_
  }
  consensus_rows[[length(consensus_rows) + 1L]] <- data.table(
    match_id = matched$match_id[i],
    tag1 = tag1,
    cluster_tag1 = cl1,
    tag2 = tag2,
    cluster_tag2 = cl2,
    shared_roi = length(shared_ids),
    consensus_voxels = n_vox,
    consensus_nifti = out_path
  )
}
consensus_dt <- rbindlist(consensus_rows, fill = TRUE)
matched <- merge(matched, consensus_dt, by = c("match_id", "tag1", "cluster_tag1", "tag2", "cluster_tag2"), all.x = TRUE)
fwrite(matched, file.path(out_dir, "matched_cluster_consensus.csv"))

if (sum(consensus_labels != 0) > 0) {
  write_nifti_array(array(as.numeric(consensus_labels), dim = dim(labels)), ref,
    file.path(out_dir, "nifti", paste0("labels_", tag1, "_", tag2, ".nii.gz")))
}

match_lines <- if (nrow(matched)) {
  paste0(
    "- match ", matched$match_id, "：",
    tag1, " cluster ", matched$cluster_tag1,
    " vs ", tag2, " cluster ", matched$cluster_tag2,
    "；Dice=", fmt(matched$dice),
    "；Jaccard=", fmt(matched$jaccard),
    "；共识体素=", fmt(matched$consensus_voxels, 0)
  )
} else {
  "- 未找到可匹配的 cluster。"
}

write_md(file.path(out_dir, "README.md"), c(
  paste0("# 路线 B：", tag1, " 与 ", tag2, " 的共识图比较与跨样本稳定性检验"),
  "",
  "用途：比较两个数据库是否支持相似的 ROI 聚类结构。这里使用路线 B 的模型评价与聚类匹配方法，不使用路线 A 的 product map。",
  "",
  "方法：",
  "",
  "- AHAB 和 PIP 分别用路线 B 的 ROI-MCMC 混合模型拟合。",
  "- 每个数据库内部先按 overall_recommended_K 选择 K；如果旧结果没有该列，则回退到最低近似 BIC。模型表同时保留 AIC、BIC、WAIC 和 PSIS-LOO/LOOIC。",
  "- 在共同 ROI 上比较两个数据库的最大后验 cluster 归属，计算 ARI 和 NMI。",
  "- 对两个数据库的 cluster 两两计算空间 Dice/Jaccard，再按 Dice 从高到低做一对一匹配。",
  "- 只对匹配上的 cluster 生成二值共识图：某个 ROI 必须同时属于 AHAB 的该 cluster 和 PIP 的匹配 cluster，才会进入对应共识图。",
  "",
  "当前模型选择：",
  "",
  paste0("- ", tag1, "：选中 K=", model_rows[tag == tag1, k][1], "；BIC=", fmt(model_rows[tag == tag1, bic_approx][1]), "；AIC=", fmt(model_rows[tag == tag1, aic_approx][1]), "；WAIC=", fmt(model_rows[tag == tag1, waic][1]), "；LOOIC=", fmt(model_rows[tag == tag1, looic][1])),
  paste0("- ", tag2, "：选中 K=", model_rows[tag == tag2, k][1], "；BIC=", fmt(model_rows[tag == tag2, bic_approx][1]), "；AIC=", fmt(model_rows[tag == tag2, aic_approx][1]), "；WAIC=", fmt(model_rows[tag == tag2, waic][1]), "；LOOIC=", fmt(model_rows[tag == tag2, looic][1])),
  "",
  "当前一致性结果：",
  "",
  paste0("- 共同 ROI 数：", agreement$n_common_roi[1]),
  paste0("- ARI：", fmt(agreement$adjusted_rand_index[1]), "；NMI：", fmt(agreement$normalized_mutual_info[1])),
  paste0("- 平均最大后验概率：", tag1, "=", fmt(agreement$mean_max_posterior_tag1[1]), "；", tag2, "=", fmt(agreement$mean_max_posterior_tag2[1])),
  paste0("- 匹配 cluster 数：", nrow(matched)),
  "",
  "匹配 cluster 与共识图：",
  "",
  match_lines,
  "",
  "主要文件：",
  "",
  "- `selected_model_evaluation.csv`：每个数据库选中 K 和模型支撑指标。",
  "- `cluster_assignment_agreement.csv`：共同 ROI 上的 ARI/NMI 和后验置信度。",
  "- `cluster_pair_spatial_similarity.csv`：所有 cluster 两两空间相似性。",
  "- `matched_cluster_consensus.csv`：一对一匹配结果和二值共识图路径。",
  "- `nifti/`：二值模型共识图，其中 `labels_AHAB_PIP.nii.gz` 是把匹配结果合并成标签图。"
))

cat("Route B dataset cluster comparison complete:\n")
cat("  ", out_dir, "\n", sep = "")
