# NIfTI IO 层：直接依赖 RNifti 与 data.table；对应 MATLAB/CANlab 里的 fmri_data/image_math/read/write 等图像对象读写。 
require_pkg <- function(pkg) {  # pkg: 需要检查的包名；作用：在运行前显式报错缺失依赖，避免半程失败。
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Run scripts/00_install_r_packages.R first.", pkg), call. = FALSE)
  }
}

require_pkg("RNifti")  # RNifti：读取/写入 .nii/.nii.gz，并保留模板 header；是 R 里替代 MATLAB fmri_data/fmri_mask_image 的核心包。

read_nifti <- function(path, internal = FALSE) {  # path: NIfTI 路径；internal: RNifti 是否返回内部表示；作用：统一图像读取入口。
  RNifti::readNifti(path, internal = internal)
}

read_nifti_vec <- function(path) {  # path: NIfTI 路径；作用：把三维图拉平成数值向量，便于逐体素统计；对应 MATLAB 中 .dat 向量化访问。
  as.numeric(read_nifti(path))
}

write_nifti_vec <- function(vec, reference_path, out_path, dtype = "float") {  # vec: 拉平后的体素向量；reference_path: 模板图；out_path: 输出路径；dtype: 数据类型。
  ref <- read_nifti(reference_path)
  arr <- array(vec, dim = dim(ref))
  RNifti::writeNifti(arr, file = out_path, template = ref, datatype = dtype)
  invisible(out_path)
}

write_nifti_array <- function(arr, reference_path, out_path, dtype = "float") {  # arr: 三维数组；reference_path: 模板图；out_path: 输出路径；dtype: 写盘类型。
  ref <- read_nifti(reference_path)
  RNifti::writeNifti(arr, file = out_path, template = ref, datatype = dtype)
  invisible(out_path)
}

discover_beta_maps <- function(beta_root = project_path("data", "raw", "beta_maps")) {  # beta_root: 原始 beta 图根目录；作用：扫描并解析 AHAB/PIP 被试与条件文件名。
  files <- list.files(beta_root, pattern = "\\.nii\\.gz$", recursive = TRUE, full.names = TRUE)
  rx <- "^([^_]+)_Subject([0-9]+)_(LookNeg|LookNeu|RegNeg)_Beta\\.nii\\.gz$"
  rows <- lapply(files, function(path) {
    name <- basename(path)
    m <- regexec(rx, name)
    parts <- regmatches(name, m)[[1]]
    if (length(parts) == 0) return(NULL)
    data.table(
      dataset = parts[2],  # 数据库名：AHAB/PIP。
      subject = as.integer(parts[3]),  # 被试编号。
      condition = parts[4],  # 条件名：LookNeu/LookNeg/RegNeg。
      path = normalizePath(path, winslash = "/", mustWork = TRUE)  # 标准化绝对路径，避免不同工作目录下失效。
    )
  })
  dt <- rbindlist(rows, fill = TRUE)
  if (!nrow(dt)) stop("No beta maps found under ", beta_root)
  setorder(dt, dataset, subject, condition)
  dt
}

pivot_beta_maps <- function(beta_dt) {  # beta_dt: discover_beta_maps() 的长表；作用：转成每被试一行，三条件三列，便于对比计算。
  wide <- dcast(beta_dt, dataset + subject ~ condition, value.var = "path")
  needed <- c("LookNeu", "LookNeg", "RegNeg")
  missing <- setdiff(needed, names(wide))
  if (length(missing)) stop("Missing condition columns: ", paste(missing, collapse = ", "))
  wide <- wide[complete.cases(wide[, ..needed])]
  setorder(wide, dataset, subject)
  wide
}

reference_beta_path <- function() {  # 无显式参数；作用：返回第一张 beta 图作为输出 NIfTI header 模板。
  discover_beta_maps()[1, path]
}

read_system_maps <- function(system_root = project_path("data", "raw", "system_maps")) {  # system_root: 原文系统图目录；作用：读入官方发布系统图索引供 Route A 对照。
  files <- list.files(system_root, pattern = "\\.nii$", full.names = TRUE)
  nm <- tools::file_path_sans_ext(basename(files))
  setNames(files, nm)
}

safe_vec <- function(x) {  # x: 体素向量；作用：把 NaN/Inf 设为 0，保证后续 t/BF 计算与写盘稳定。
  x[!is.finite(x)] <- 0
  x
}
