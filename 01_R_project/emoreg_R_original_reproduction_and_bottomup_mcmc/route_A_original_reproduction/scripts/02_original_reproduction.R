# Route A 核心复现脚本：对应 MATLAB BayesfactorMap_MainAnalysis.m；差异是 R 版把 IO、t 图、BF、cluster control 拆成独立模块。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到复制版项目根目录。
source("R/project.R")  # 配置和输出目录。
source("R/nifti_utils.R")  # RNifti 图像 IO。
source("R/bayes_factor.R")  # 2*log(BF) 转换，替代 MATLAB estimateBayesFactor。
source("R/stats_maps.R")  # t 图与条件差值。
source("R/clusters3d.R")  # 连通簇控制，替代 MATLAB region + 手工阈值。

args <- parse_cli_args(list(  # dataset: AHAB/PIP；max-subjects: smoke 子样本；bf-method: jzs_exact/bic_approx；seed: 抽样种子；tag: 输出标签。
  dataset = "PIP",
  "max-subjects" = NA,
  "bf-method" = CFG$bf_method,
  seed = CFG$random_seed,
  tag = NA
))
dataset_arg <- as.character(args$dataset)
tag <- if (is.na(args$tag)) dataset_arg else as.character(args$tag)  # 输出文件名标签，full run 通常等于数据集名。
max_subjects <- as_int_arg(args[["max-subjects"]], NA_integer_)  # 可选子样本上限，仅 smoke 用。
bf_method <- as.character(args[["bf-method"]])  # jzs_exact 更贴近原文 MATLAB；bic_approx 更快。
seed <- as_int_arg(args$seed, as.integer(CFG$random_seed))  # 只用于子样本抽取。
bf_thr <- as.numeric(CFG$bf_threshold_2log)  # 原文 BF=10 对应 2logBF≈4.6。
cluster_min <- as.integer(CFG$cluster_min_voxels)  # 对应 MATLAB ClusterControl.m 中 >14 体素。

beta <- pivot_beta_maps(discover_beta_maps())  # 扫描 data/raw/beta_maps 并转成每被试三条件宽表。
wide <- beta[dataset == dataset_arg]  # 当前数据库的所有被试。
if (!nrow(wide)) stop("No beta maps for dataset ", dataset_arg)
setorder(wide, subject)
selection_mode <- "all_subjects"
# Smoke runs can subsample subjects; full runs keep all available subjects.
if (!is.na(max_subjects) && max_subjects > 0) {
  if (nrow(wide) > max_subjects) {
    set.seed(seed)
    wide <- wide[sample.int(nrow(wide), max_subjects)]  # 与 MATLAB 原文不同：R 版可做 smoke 子采样。
    selection_mode <- "random_without_replacement"
  } else {
    selection_mode <- "all_available_subjects_because_n_le_max"
  }
}
setorder(wide, subject)

message("Running original-style reproduction for ", dataset_arg, " with n=", nrow(wide))
ref <- wide$LookNeu[1]  # 第一张图作为输出 header 模板。
ref_dim <- dim(read_nifti(ref))  # 输出 mask 要恢复成三维数组。

# Compute the original contrast maps and single-condition t maps.
stat_reapp <- compute_contrast_stats(wide, "reappraisal_effect")
stat_emotion <- compute_contrast_stats(wide, "emotion_generation")
stat_neg <- compute_single_condition_t(wide$LookNeg)
stat_reg <- compute_single_condition_t(wide$RegNeg)

bf_reapp <- t_to_two_log_bf(stat_reapp$t, stat_reapp$n, method = bf_method, signed = FALSE)  # 对应 MATLAB BF_RegNeg。
bf_emotion <- t_to_two_log_bf(stat_emotion$t, stat_emotion$n, method = bf_method, signed = FALSE)  # 对应 MATLAB BF_NegNeu。

# Four system masks follow the original BF/t-rule definitions.
common_appraisal <- bf_reapp > bf_thr &
  bf_emotion > bf_thr &
  stat_reapp$t > 0 &
  stat_emotion$t > 0 &
  stat_neg$t > 0

reappraisal_only <- bf_reapp > bf_thr &
  bf_emotion < -bf_thr &
  stat_reapp$t > 0 &
  stat_reg$t > 0

non_modifiable_emotion <- bf_reapp < -bf_thr &
  bf_emotion > bf_thr &
  stat_emotion$t > 0 &
  stat_neg$t > 0

modifiable_emotion <- bf_reapp > bf_thr &
  bf_emotion > bf_thr &
  stat_emotion$t > 0 &
  stat_reapp$t < 0 &
  stat_neg$t > 0

systems <- list(
  common_appraisal = common_appraisal,
  reappraisal_only = reappraisal_only,
  non_modifiable_emotion = non_modifiable_emotion,
  modifiable_emotion = modifiable_emotion
)

nifti_dir <- project_path("route_A_original_reproduction", "outputs", "nifti")  # Route A NIfTI 输出目录。
table_dir <- project_path("route_A_original_reproduction", "outputs", "tables")  # Route A CSV/逻辑表输出目录。

selected_subjects <- data.table(
  route = "A",
  tag = tag,
  dataset = dataset_arg,
  subject = wide$subject,
  seed = seed,
  requested_max_subjects = if (is.na(max_subjects)) NA_integer_ else max_subjects,
  selected_n = nrow(wide),
  selection = selection_mode
)
fwrite(selected_subjects, file.path(table_dir, paste0("selected_subjects_", tag, ".csv")))

write_nifti_vec(stat_reapp$t, ref, file.path(nifti_dir, paste0("t_reappraisal_effect_", tag, ".nii.gz")))  # MATLAB tRegNeg。
write_nifti_vec(stat_emotion$t, ref, file.path(nifti_dir, paste0("t_emotion_generation_", tag, ".nii.gz")))  # MATLAB tNegNeu。
write_nifti_vec(bf_reapp, ref, file.path(nifti_dir, paste0("two_log_bf_reappraisal_effect_", tag, ".nii.gz")))  # MATLAB BF_RegNeg。
write_nifti_vec(bf_emotion, ref, file.path(nifti_dir, paste0("two_log_bf_emotion_generation_", tag, ".nii.gz")))  # MATLAB BF_NegNeu。

summary_rows <- list()
for (nm in names(systems)) {
  mask <- systems[[nm]]  # 同时输出原始规则图和 cluster-size-controlled 图。
  arr <- array(mask, dim = ref_dim)  # 从逻辑向量恢复到三维二值图。
  cc <- keep_clusters_by_extent(arr, min_voxels = cluster_min)  # 对应 MATLAB ClusterControl.m 的 >=15 体素簇筛选。
  before_path <- file.path(nifti_dir, paste0(nm, "_before_cluster_", tag, ".nii.gz"))
  after_path <- file.path(nifti_dir, paste0(nm, "_after_cluster_", tag, ".nii.gz"))
  write_nifti_array(array(as.numeric(arr), dim = ref_dim), ref, before_path)
  write_nifti_array(array(as.numeric(cc), dim = ref_dim), ref, after_path)
  summary_rows[[nm]] <- data.table(
    dataset = dataset_arg,
    tag = tag,
    system = nm,
    n_subjects = nrow(wide),
    bf_method = bf_method,
    bf_threshold_2log = bf_thr,
    voxels_before_cluster = sum(arr),
    voxels_after_cluster = sum(cc),
    cluster_min_voxels = cluster_min
  )
}

summary <- rbindlist(summary_rows)
fwrite(summary, file.path(table_dir, paste0("original_system_voxel_summary_", tag, ".csv")))

defs <- data.table(  # 额外保存“R 逻辑 <-> MATLAB 逻辑”对照表，便于核查。
  system = names(systems),
  original_matlab_logic = c(
    "BF(Reg-Neg)>T, BF(Neg-Neu)>T, t(Reg-Neg)>0, t(Neg-Neu)>0, t(LookNeg)>0",
    "BF(Reg-Neg)>T, BF(Neg-Neu)<-T, t(Reg-Neg)>0, t(RegNeg)>0",
    "BF(Reg-Neg)<-T, BF(Neg-Neu)>T, t(Neg-Neu)>0, t(LookNeg)>0",
    "BF(Reg-Neg)>T, BF(Neg-Neu)>T, t(Neg-Neu)>0, t(Reg-Neg)<0, t(LookNeg)>0"
  )
)
fwrite(defs, file.path(table_dir, "original_four_system_logic.csv"))

cat("Original-style reproduction complete for ", dataset_arg, ":\n", sep = "")
cat("  ", file.path(table_dir, paste0("original_system_voxel_summary_", tag, ".csv")), "\n", sep = "")
