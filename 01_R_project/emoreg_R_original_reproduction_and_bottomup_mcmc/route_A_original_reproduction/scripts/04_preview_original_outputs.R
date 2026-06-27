# Route A 预览脚本：批量导出 PNG 和摘要表。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 路径与目录函数。
source("R/nifti_utils.R")  # NIfTI IO。
source("R/nifti_viewer.R")  # 轻量 PNG/摘要导出工具。

args <- parse_cli_args(list("export-nii" = FALSE))  # export-nii: 是否另存无压缩 .nii 给外部浏览器。
export_nii <- isTRUE(args[["export-nii"]]) || identical(tolower(as.character(args[["export-nii"]])), "true")

nifti_dir <- project_path("route_A_original_reproduction", "outputs", "nifti")
preview_dir <- project_path("route_A_original_reproduction", "outputs", "preview")
png_dir <- file.path(preview_dir, "png")
nii_dir <- file.path(preview_dir, "uncompressed_nii")
invisible(ensure_dir(preview_dir))
invisible(ensure_dir(png_dir))

files <- list.files(nifti_dir, pattern = "\\.nii\\.gz$", full.names = TRUE)
if (!length(files)) stop("No .nii.gz files found under ", nifti_dir, call. = FALSE)

summary <- nifti_file_summary(files)  # 汇总每个输出图的尺寸/分布信息。
summary[, map_type := vapply(name, classify_nifti_map, character(1))]  # 自动区分 t 图/BF 图/二值系统图。
summary[, how_to_read := "RNifti::readNifti(path); values are a 3D voxel array"]  # 给使用者的读取提示。
fwrite(summary, file.path(preview_dir, "nifti_file_summary.csv"))

mricrogl_recommendations <- copy(summary)
mricrogl_recommendations[, recommended_for_mricrogl := fifelse(
  nonzero_voxels == 0,
  "No",
  fifelse(map_type == "binary_system_mask", "Yes, preferably as overlay on a template", "Yes")
)]
mricrogl_recommendations[, reason := fifelse(
  nonzero_voxels == 0,
  "All voxels are zero, so MRIcroGL will show a blank/black image.",
  fifelse(
    map_type == "binary_system_mask",
    "This is a sparse binary system mask; it is easier to view as an overlay on an anatomical template.",
    "This map has continuous values and should be visible directly."
  )
)]
mricrogl_recommendations <- mricrogl_recommendations[
  ,
  .(name, map_type, nonzero_voxels, min, max, recommended_for_mricrogl, reason, file)
]
fwrite(mricrogl_recommendations, file.path(preview_dir, "mricrogl_open_recommendations.csv"))

dictionary <- data.table(  # 明确写出 Route A 文件名与 MATLAB 变量名的一一对应。
  file_pattern = c(
    "t_emotion_generation_*.nii.gz",
    "t_reappraisal_effect_*.nii.gz",
    "two_log_bf_emotion_generation_*.nii.gz",
    "two_log_bf_reappraisal_effect_*.nii.gz",
    "common_appraisal_*_cluster_*.nii.gz",
    "reappraisal_only_*_cluster_*.nii.gz",
    "non_modifiable_emotion_*_cluster_*.nii.gz",
    "modifiable_emotion_*_cluster_*.nii.gz"
  ),
  original_matlab_name = c(
    "tNegNeu",
    "tRegNeg",
    "BF_NegNeu = estimateBayesFactor(tNegNeu, 't')",
    "BF_RegNeg = estimateBayesFactor(tRegNeg, 't')",
    "CommonAppraisal_SystemIdx",
    "ReappraisalOnly_SystemIdx",
    "NonmodifibleEmotion_SystemIdx",
    "ModifibleEmotion_SystemIdx"
  ),
  paper_meaning = c(
    "Emotion generation contrast: Look negative minus Look neutral, group one-sample t map.",
    "Reappraisal effect contrast: Regulate negative minus Look negative, group one-sample t map.",
    "Paper-style Bayes factor evidence for emotion generation; values are 2*log(BF10).",
    "Paper-style Bayes factor evidence for reappraisal effect; values are 2*log(BF10).",
    "Common appraisal system mask from BF/t logical rules.",
    "Reappraisal-only system mask from BF/t logical rules.",
    "Non-modifiable emotion system mask from BF/t logical rules.",
    "Modifiable emotion system mask from BF/t logical rules."
  ),
  view_tip = c(
    "Use a signed color scale; positive means LookNeg > LookNeu.",
    "Use a signed color scale; negative means RegNeg < LookNeg.",
    "Threshold around 4.6 for BF10 about 10; around -4.6 for BF10 about 0.1.",
    "Threshold around 4.6 for BF10 about 10; around -4.6 for BF10 about 0.1.",
    "Binary mask: 1 means voxel belongs to the reproduced system.",
    "Binary mask: 1 means voxel belongs to the reproduced system.",
    "Binary mask: 1 means voxel belongs to the reproduced system.",
    "Binary mask: 1 means voxel belongs to the reproduced system."
  )
)
fwrite(dictionary, file.path(preview_dir, "output_file_dictionary.csv"))

for (path in files) {
  out_png <- file.path(png_dir, sub("\\.nii\\.gz$", ".png", basename(path)))  # 每个 NIfTI 导出一张代表性切片 PNG。
  write_nifti_slice_png(path, out_png)
  if (export_nii) {
    out_nii <- file.path(nii_dir, sub("\\.nii\\.gz$", ".nii", basename(path)))
    write_uncompressed_nifti(path, out_nii)
  }
}

cat("NIfTI previews and summaries written under:\n")
cat("  ", preview_dir, "\n", sep = "")
