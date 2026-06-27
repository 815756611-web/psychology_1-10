# Route A 共识图与行为验证：对应 MATLAB CreateConsensusMap.m；R 版额外补了行为相关和 Markdown 报告。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 配置和路径。
source("R/nifti_utils.R")  # RNifti 图像 IO。
source("R/clusters3d.R")  # 三维平滑函数 smooth_array_3d()。
source("R/stats_maps.R")  # 被试级对比提取。
source("R/interpretation.R")  # Dice/Jaccard 等重叠指标。

args <- parse_cli_args(list(tag1 = "AHAB", tag2 = "PIP"))  # tag1/tag2: 要做共识的两个数据库标签。
tags <- c(as.character(args$tag1), as.character(args$tag2))
systems <- c("common_appraisal", "reappraisal_only", "non_modifiable_emotion", "modifiable_emotion")
display <- c(
  common_appraisal = "Common Appraisal",
  reappraisal_only = "Reappraisal Only",
  non_modifiable_emotion = "Non-modifiable Emotion",
  modifiable_emotion = "Modifiable Emotion"
)

write_md <- function(path, lines) {  # path: Markdown 输出路径；lines: 文本行向量；作用：统一 UTF-8 写报告。
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con)
}

fmt <- function(x, digits = 4) {  # x: 数值；digits: 保留位数；作用：把表格数字格式化成报告友好的字符。
  ifelse(is.finite(x), format(round(x, digits), scientific = FALSE), "NA")
}

md_table <- function(dt, cols) {  # dt: data.table；cols: 输出列名；作用：生成 Markdown 表格字符串。
  if (!nrow(dt)) return("暂无结果。")
  view <- copy(dt[, ..cols])
  for (nm in names(view)) view[[nm]] <- as.character(view[[nm]])
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  body <- apply(view, 1, function(x) paste0("| ", paste(x, collapse = " | "), " |"))
  paste(c(header, sep, body), collapse = "\n")
}

nifti_dir <- project_path("route_A_original_reproduction", "outputs", "nifti")  # Route A 系统图目录。
table_dir <- project_path("route_A_original_reproduction", "outputs", "tables")  # 共识统计与报告目录。
ref <- reference_beta_path()
ref_dim <- dim(read_nifti(ref))

consensus_rows <- list()
for (sys in systems) {
  paths <- file.path(nifti_dir, paste0(sys, "_after_cluster_", tags, ".nii.gz"))
  if (!all(file.exists(paths))) {
    warning("Missing after-cluster maps for ", sys, "; skipping consensus.")
    next
  }
  a1 <- as.numeric(read_nifti(paths[1])) > 0
  a2 <- as.numeric(read_nifti(paths[2])) > 0
  s1 <- smooth_array_3d(array(as.numeric(a1), dim = ref_dim), fwhm = as.numeric(CFG$consensus_smooth_fwhm_vox))  # 对应 MATLAB preprocess(...,'smooth',3)。
  s2 <- smooth_array_3d(array(as.numeric(a2), dim = ref_dim), fwhm = as.numeric(CFG$consensus_smooth_fwhm_vox))  # 两个样本都先平滑。
  product <- s1 * s2  # 对应 MATLAB obj3.dat = obj1.dat .* obj2.dat。
  consensus <- product > as.numeric(CFG$consensus_product_threshold)  # 对应 MATLAB Bar=0.01 的阈值化。
  out <- file.path(nifti_dir, paste0(sys, "_consensus_", paste(tags, collapse = "_"), ".nii.gz"))
  write_nifti_array(array(as.numeric(consensus), dim = ref_dim), ref, out)
  consensus_rows[[sys]] <- data.table(
    system = sys,
    display_name = display[[sys]],
    tag1 = tags[1],
    tag2 = tags[2],
    consensus_voxels = sum(consensus),
    output_nifti = out
  )
}
consensus_summary <- rbindlist(consensus_rows, fill = TRUE)
fwrite(consensus_summary, file.path(table_dir, "consensus_system_summary.csv"))

system_maps <- read_system_maps()
overlap_summary <- data.table()
if (length(system_maps) && nrow(consensus_summary)) {
  overlap <- list()
  for (i in seq_len(nrow(consensus_summary))) {
    sys <- consensus_summary$system[i]
    con <- read_nifti_vec(consensus_summary$output_nifti[i]) > 0  # 当前 R 共识图。
    target <- system_maps[[display[[sys]]]]
    if (!is.null(target) && file.exists(target)) {
      ov <- dice_jaccard(con, read_nifti_vec(target) > 0)  # MATLAB 原版用点到点距离校验；R 版改用更直观的 Dice/Jaccard/coverage。
      ov[, `:=`(system = sys, original_map = basename(target))]
      overlap[[length(overlap) + 1L]] <- ov
    }
  }
  if (length(overlap)) {
    overlap_summary <- rbindlist(overlap)
    fwrite(overlap_summary, file.path(table_dir, "consensus_vs_downloaded_system_maps.csv"))
  }
}

if (nrow(consensus_summary)) {
  consensus_view <- copy(consensus_summary)
  consensus_view[, consensus_voxels := fmt(consensus_voxels, 0)]
  overlap_view <- copy(overlap_summary)
  if (nrow(overlap_view)) {
    overlap_view[, `:=`(
      dice = fmt(dice),
      jaccard = fmt(jaccard),
      intersection_voxels = fmt(intersection_voxels, 0),
      a_voxels = fmt(a_voxels, 0),
      b_voxels = fmt(b_voxels, 0)
    )]
  }
  write_md(file.path(table_dir, paste0(paste(tags, collapse = "_"), "_product_map_consensus.md")), c(
    paste0("# 路线 A：", tags[1], " 与 ", tags[2], " 的共识图比较与跨样本稳定性检验"),
    "",
    "用途：比较两个数据库在原文四个理论系统上的空间共识。这里使用的是路线 A 的原文复现逻辑，不是路线 B 的 MCMC 聚类。",
    "",
    "方法：",
    "",
    paste0("- 对每个系统分别读取 `", tags[1], "` 和 `", tags[2], "` 的 `*_after_cluster_*.nii.gz` 二值系统图。"),
    paste0("- 先对两个二值图做三维平滑，`consensus_smooth_fwhm_vox = ", CFG$consensus_smooth_fwhm_vox, "`。"),
    "- 逐体素相乘得到 product map：两个数据库都支持的位置乘积更高。",
    paste0("- 使用阈值 `consensus_product_threshold = ", CFG$consensus_product_threshold, "` 二值化，得到 AHAB/PIP 共识图。"),
    "- 共识图文件写入 `route_A_original_reproduction/outputs/nifti`，文件名为 `*_consensus_AHAB_PIP.nii.gz`。",
    "",
    "当前共识图体素数：",
    "",
    md_table(consensus_view, c("system", "display_name", "tag1", "tag2", "consensus_voxels", "output_nifti")),
    "",
    "跨样本稳定性检验：",
    "",
    paste0("- 样本 1：", tags[1], "；样本 2：", tags[2], "。"),
    "- 每个系统的共识图只保留两个样本在平滑 product map 后共同支持的位置，因此可作为 AHAB/PIP 跨样本稳定区域。",
    "- 下表的 Dice/Jaccard 用于检查本次跨样本共识区域与下载到的原文系统图是否空间一致。",
    "",
    "与下载的原文系统图的空间重叠：",
    "",
    md_table(overlap_view, c("system", "original_map", "dice", "jaccard", "intersection_voxels", "a_voxels", "b_voxels")),
    "",
    "如何理解：Dice/Jaccard 越高，说明本次 R 复现的 AHAB/PIP 共识图与下载到的原文系统图空间越接近。体素数差异通常来自 BF 近似实现、平滑阈值、cluster size 阈值、输入 beta 图版本和 NIfTI 网格/header 处理差异。"
  ))
}

comparison_rows <- list()
if (length(system_maps)) {
  for (sys in systems) {
    target <- system_maps[[display[[sys]]]]
    if (is.null(target) || !file.exists(target)) next
    original <- read_nifti_vec(target) > 0
    for (tag in tags) {
      route_file <- file.path(nifti_dir, paste0(sys, "_after_cluster_", tag, ".nii.gz"))
      if (!file.exists(route_file)) next
      route <- read_nifti_vec(route_file) > 0
      ov <- dice_jaccard(route, original)
      ov[, `:=`(
        system = sys,
        tag = tag,
        map_type = "after_cluster",
        route_file = basename(route_file),
        original_file = basename(target)
      )]
      comparison_rows[[length(comparison_rows) + 1L]] <- ov
    }
    consensus_file <- file.path(nifti_dir, paste0(sys, "_consensus_", paste(tags, collapse = "_"), ".nii.gz"))
    if (file.exists(consensus_file)) {
      route <- read_nifti_vec(consensus_file) > 0
      ov <- dice_jaccard(route, original)
      ov[, `:=`(
        system = sys,
        tag = paste(tags, collapse = "_"),
        map_type = "consensus_product_map",
        route_file = basename(consensus_file),
        original_file = basename(target)
      )]
      comparison_rows[[length(comparison_rows) + 1L]] <- ov
    }
  }
}

if (length(comparison_rows)) {
  comparison <- rbindlist(comparison_rows, fill = TRUE)
  comparison[, `:=`(
    route_voxels = a_voxels,
    original_voxels = b_voxels,
    route_only_voxels = a_voxels - intersection_voxels,
    original_only_voxels = b_voxels - intersection_voxels,
    original_coverage = fifelse(b_voxels > 0, intersection_voxels / b_voxels, 0),
    route_extra_fraction = fifelse(a_voxels > 0, (a_voxels - intersection_voxels) / a_voxels, 0),
    route_to_original_ratio = fifelse(b_voxels > 0, a_voxels / b_voxels, NA_real_)
  )]
  comparison <- comparison[, .(
    system, tag, map_type, route_voxels, original_voxels,
    intersection_voxels, route_only_voxels, original_only_voxels,
    dice, jaccard, original_coverage, route_extra_fraction,
    route_to_original_ratio, route_file, original_file
  )]
  fwrite(comparison, file.path(table_dir, "route_A_vs_original_system_maps_detailed.csv"))

  route_summary_files <- file.path(table_dir, paste0("original_system_voxel_summary_", tags, ".csv"))
  route_summary <- rbindlist(lapply(route_summary_files[file.exists(route_summary_files)], fread), fill = TRUE)
  consensus_only <- comparison[map_type == "consensus_product_map"]
  after_only <- comparison[map_type == "after_cluster"]
  summary_view <- copy(route_summary)
  if (nrow(summary_view)) {
    summary_view[, `:=`(
      bf_threshold_2log = fmt(bf_threshold_2log, 3),
      voxels_before_cluster = fmt(voxels_before_cluster, 0),
      voxels_after_cluster = fmt(voxels_after_cluster, 0)
    )]
  }
  consensus_view <- copy(consensus_only)
  if (nrow(consensus_view)) {
    consensus_view[, `:=`(
      route_voxels = fmt(route_voxels, 0),
      original_voxels = fmt(original_voxels, 0),
      intersection_voxels = fmt(intersection_voxels, 0),
      original_coverage = fmt(original_coverage, 3),
      route_extra_fraction = fmt(route_extra_fraction, 3),
      route_to_original_ratio = fmt(route_to_original_ratio, 3),
      dice = fmt(dice, 3),
      jaccard = fmt(jaccard, 3)
    )]
  }
  after_view <- copy(after_only)
  if (nrow(after_view)) {
    after_view[, `:=`(
      route_voxels = fmt(route_voxels, 0),
      original_voxels = fmt(original_voxels, 0),
      original_coverage = fmt(original_coverage, 3),
      route_extra_fraction = fmt(route_extra_fraction, 3),
      dice = fmt(dice, 3),
      jaccard = fmt(jaccard, 3)
    )]
  }
  write_md(file.path(table_dir, "route_A_vs_original_comparison.md"), c(
    "# Route A comparison with published system maps",
    "",
    paste0("Generated from current Route A outputs. BF method: `", CFG$bf_method,
      "`; BF threshold on 2*log(BF): `", CFG$bf_threshold_2log,
      "`; cluster minimum: `", CFG$cluster_min_voxels, "` voxels."),
    "",
    "Original-paper threshold reset used here:",
    "",
    "- JZS Bayes factor with medium Cauchy scale r = sqrt(2)/2.",
    "- Evidence for the alternative: BF > 10, equivalent to 2*log(BF) about 4.6.",
    "- Evidence for the null: BF < 1/10, equivalent to signed 2*log(BF) below -4.6.",
    "- Consensus maps follow the original product-map step: smooth both binary maps with FWHM 3 voxels, multiply them, then threshold product > 0.01.",
    "",
    "Consensus map versus published maps:",
    "",
    md_table(consensus_view, c(
      "system", "route_voxels", "original_voxels", "intersection_voxels",
      "original_coverage", "route_extra_fraction", "route_to_original_ratio", "dice", "jaccard"
    )),
    "",
    "Single-dataset after-cluster maps versus published maps:",
    "",
    md_table(after_view, c(
      "system", "tag", "route_voxels", "original_voxels",
      "original_coverage", "route_extra_fraction", "dice", "jaccard"
    )),
    "",
    "Route A voxel summary:",
    "",
    md_table(summary_view, c(
      "dataset", "system", "n_subjects", "bf_method",
      "bf_threshold_2log", "voxels_before_cluster", "voxels_after_cluster", "cluster_min_voxels"
    )),
    "",
    "Interpretation:",
    "",
    "- The consensus maps still cover nearly all published-map voxels, so the issue is not missing the original regions.",
    "- The outward expansion mainly comes from the consensus product-map step: smoothing plus a low product threshold expands borders beyond the hard binary intersection.",
    "- The detailed numeric table is `route_A_vs_original_system_maps_detailed.csv`."
  ))
}

behavior_path <- project_path("data", "processed", "behavior_clean.rds")
if (!file.exists(behavior_path)) {
  warning("Behavior file not found. Run route_A_original_reproduction/scripts/01_prepare_behavior.R before behavior correlations.")
} else if (nrow(consensus_summary)) {
  behavior <- readRDS(behavior_path)  # 行为清理表。
  beta <- pivot_beta_maps(discover_beta_maps())  # 被试级 beta 图宽表。
  rows <- list()
  masks <- lapply(consensus_summary$output_nifti, function(p) read_nifti_vec(p) > 0)
  names(masks) <- consensus_summary$system
  for (i in seq_len(nrow(beta))) {
    v <- contrast_subject_vectors(beta, i)
    for (sys in names(masks)) {
      m <- masks[[sys]]
      rows[[length(rows) + 1L]] <- data.table(
        dataset = beta$dataset[i],
        subject = beta$subject[i],
        system = sys,
        emotion_generation_mean = mean(v$emotion_generation[m], na.rm = TRUE),
        reappraisal_effect_mean = mean(v$reappraisal_effect[m], na.rm = TRUE)
      )
    }
  }
  system_subject <- rbindlist(rows)
  system_subject <- merge(system_subject, behavior, by = c("dataset", "subject"), all.x = TRUE)
  fwrite(system_subject, file.path(table_dir, "consensus_system_subject_effects_with_behavior.csv"))
  cors <- system_subject[
    is.finite(reg_success),
    .(
      n = .N,
      cor_success_reappraisal_effect = suppressWarnings(cor(reg_success, reappraisal_effect_mean, use = "pairwise.complete.obs")),
      cor_emotionreact_emotion_generation = suppressWarnings(cor(emotion_reactivity, emotion_generation_mean, use = "pairwise.complete.obs"))
    ),
    by = .(dataset, system)
  ]
  fwrite(cors, file.path(table_dir, "consensus_behavior_correlations.csv"))
}

cat("Consensus and behavior follow-up complete.\n")
