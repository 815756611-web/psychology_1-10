# Route B 第一步：构造 ROI 终端单位并抽取被试×ROI 特征。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 配置和路径。
source("R/nifti_utils.R")  # NIfTI IO。
source("R/bayes_factor.R")  # ROI 摘要时的 BF 指标。
source("R/stats_maps.R")  # 被试级 contrast 向量。
source("R/clusters3d.R")  # 连通簇工具。
source("R/parcels.R")  # 旧版 parcel 辅助；当前 unit 固定为 roi。
source("R/roi_features.R")  # Route B ROI 切块与特征汇总核心。

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x  # 空值兜底，便于从 CFG 读取可选参数。

args <- parse_cli_args(list(  # unit: 当前只支持 roi；dataset: AHAB/PIP/all；roi-source: ROI 来源图；rope: 近零区间；tag: 输出标签。
  unit = "roi",
  dataset = "all",
  "max-subjects-per-study" = NA,
  "roi-source" = "auto",
  "min-roi-voxels" = CFG$route_b_min_roi_voxels %||% CFG$cluster_min_voxels,
  "connectivity" = 26,
  "rope" = CFG$route_b_rope %||% 0.05,
  seed = CFG$random_seed,
  tag = "full"
))

unit <- match.arg(as.character(args$unit), c("roi"))
dataset_arg <- toupper(as.character(args$dataset))
max_per <- as_int_arg(args[["max-subjects-per-study"]], NA_integer_)
roi_source <- as.character(args[["roi-source"]])
min_roi_voxels <- as_int_arg(args[["min-roi-voxels"]], as.integer(CFG$cluster_min_voxels))
connectivity <- as_int_arg(args[["connectivity"]], 26L)
rope <- as_num_arg(args[["rope"]], 0.05)
seed <- as_int_arg(args$seed, as.integer(CFG$random_seed))
tag <- as.character(args$tag)

beta <- pivot_beta_maps(discover_beta_maps())  # 读入所有被试的三条件 beta 图索引。
if (!identical(dataset_arg, "ALL")) {
  beta <- beta[toupper(dataset) == dataset_arg]
  if (!nrow(beta)) stop("No beta maps found for dataset=", dataset_arg)
}
setorder(beta, dataset, subject)
selection_mode <- "all_subjects"
if (!is.na(max_per) && max_per > 0) {
  set.seed(seed)
  beta <- beta[, .SD[sample.int(.N, min(max_per, .N))], by = dataset]  # 每个数据库内独立子采样，保持 AHAB/PIP 平衡。
  selection_mode <- "random_without_replacement_by_dataset"
}
setorder(beta, dataset, subject)

nifti_dir <- project_path("route_B_bottomup_mcmc", "outputs", "nifti")
table_dir <- project_path("route_B_bottomup_mcmc", "outputs", "tables")

selected_subjects <- beta[, .(
  route = "B",
  tag = tag,
  subject = subject,
  seed = seed,
  requested_max_subjects_per_study = if (is.na(max_per)) NA_integer_ else max_per,
  selected_n_by_dataset = .N,
  selection = selection_mode
), by = dataset]
fwrite(selected_subjects, file.path(table_dir, paste0("selected_subjects_", tag, ".csv")))

message("Building Route B ROI units from original system maps: source=", roi_source)
system_maps <- route_b_system_map_index(source = roi_source, tag = tag)  # 选择原文下载图、Route A 共识图或 after-cluster 图作为 ROI 来源。
roi <- make_route_b_roi_labels(system_maps, min_voxels = min_roi_voxels, connectivity = connectivity)  # 把四系统图拆成互斥 ROI 连通簇。
if (!nrow(roi$table)) stop("No ROI survived min-roi-voxels=", min_roi_voxels)

labels_path <- file.path(nifti_dir, paste0("route_b_roi_labels_", tag, ".nii.gz"))  # 记录每个体素属于哪个 ROI 编号。
write_nifti_array(array(as.numeric(roi$labels), dim = dim(roi$labels)), roi$reference_nifti, labels_path)

message("Extracting subject-by-ROI features. All ROI observations are pooled for Route B MCMC.")
subject_features <- subject_roi_features(beta, roi$labels, roi$table)  # 核心输出：每个被试、每个 ROI 的 4+ 个特征值。
roi_summary <- summarise_roi_features(
  subject_features,
  roi$table,
  rope = rope,
  bf_method = as.character(CFG$bf_method)
)

# These ROI-level summaries support interpretation; the MCMC feature set stays smaller.
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
available_feature_cols <- intersect(requested_feature_cols, names(roi_summary))
missing_feature_cols <- setdiff(requested_feature_cols, names(roi_summary))
standardized_feature_matrix <- copy(roi_summary[, c("roi_id", "source_system", available_feature_cols), with = FALSE])  # 标准化矩阵只用于画像/改进分析，不进主要似然。
if (length(available_feature_cols)) {
  for (col in available_feature_cols) {  # 对连续 ROI 摘要做标准化，供 profile 图和改进分析复用。
    z <- as.numeric(standardized_feature_matrix[[col]])
    mu <- mean(z, na.rm = TRUE)
    sig <- stats::sd(z, na.rm = TRUE)
    if (!is.finite(sig) || sig == 0) {
      standardized_feature_matrix[, paste0("z_", col) := NA_real_]
    } else {
      standardized_feature_matrix[, paste0("z_", col) := (z - mu) / sig]  # 每个 ROI 摘要特征做 z-score，便于跨特征同图比较。
    }
  }
}
feature_availability <- data.table(
  tag = tag,
  available_features = paste(available_feature_cols, collapse = ";"),
  missing_features = paste(missing_feature_cols, collapse = ";"),
  skipped_features = paste(missing_feature_cols, collapse = ";"),
  note = "Continuous ROI-level features are standardized into z_* columns for downstream Route B interpretation and improved-method analyses."
)

fwrite(system_maps, file.path(table_dir, paste0("route_b_system_map_index_", tag, ".csv")))
fwrite(subject_features, file.path(table_dir, paste0("roi_subject_features_", tag, ".csv")))
fwrite(roi$table, file.path(table_dir, paste0("roi_table_", tag, ".csv")))
fwrite(roi_summary, file.path(table_dir, paste0("roi_group_summary_", tag, ".csv")))
fwrite(standardized_feature_matrix, file.path(table_dir, paste0("route_b_standardized_feature_matrix_", tag, ".csv")))
fwrite(feature_availability, file.path(table_dir, paste0("route_b_feature_availability_", tag, ".csv")))

bundle <- list(  # 这个 RDS 是 Route B 主入口，后续 05/06/07/08/09 都从这里读。
  analysis_unit = "route_b_roi",
  tag = tag,
  roi_source = roi_source,
  pooled_observations = TRUE,
  min_roi_voxels = min_roi_voxels,
  connectivity = connectivity,
  rope = rope,
  beta_index = beta,
  system_maps = system_maps,
  labels = roi$labels,
  labels_path = labels_path,
  roi_table = roi$table,
  roi_summary = roi_summary,
  standardized_feature_matrix = standardized_feature_matrix,
  requested_feature_cols = requested_feature_cols,
  available_feature_cols = available_feature_cols,
  missing_feature_cols = missing_feature_cols,
  subject_features = subject_features,
  reference_nifti = roi$reference_nifti,
  feature_cols = c("emotion_generation", "reappraisal_effect", "look_neg_base", "reg_neg_base")  # 主模型只用 4 个核心特征，把解释性列留在画像层。
)
out_rds <- file.path(table_dir, paste0("roi_subject_features_", tag, ".rds"))
saveRDS(bundle, out_rds)
cat("Route B pooled ROI features prepared:\n")
cat("  ", out_rds, "\n", sep = "")
