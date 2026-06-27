# 轻量预览层：依赖 RNifti/data.table/grDevices；作用是替代 MATLAB orthviews/montage 的快速检查版，不参与正式统计。 
nifti_file_summary <- function(files) {  # files: NIfTI 文件路径向量；作用：汇总尺寸、体素间距、分位数和非零体素数。
  rows <- lapply(files, function(path) {
    img <- read_nifti(path)
    vals <- as.numeric(img)
    vals <- vals[is.finite(vals)]
    nz <- vals[vals != 0]
    qs <- if (length(vals)) stats::quantile(vals, probs = c(0.01, 0.5, 0.99), na.rm = TRUE) else c(NA, NA, NA)
    data.table(
      file = normalizePath(path, winslash = "/", mustWork = TRUE),
      name = basename(path),
      size_kb = round(file.info(path)$size / 1024, 1),
      dim_i = dim(img)[1],
      dim_j = dim(img)[2],
      dim_k = dim(img)[3],
      voxel_i_mm = attr(img, "pixdim")[1],
      voxel_j_mm = attr(img, "pixdim")[2],
      voxel_k_mm = attr(img, "pixdim")[3],
      finite_voxels = length(vals),
      nonzero_voxels = length(nz),
      min = if (length(vals)) min(vals) else NA_real_,
      q01 = unname(qs[1]),
      median = unname(qs[2]),
      mean = if (length(vals)) mean(vals) else NA_real_,
      q99 = unname(qs[3]),
      max = if (length(vals)) max(vals) else NA_real_
    )
  })
  rbindlist(rows, fill = TRUE)
}

classify_nifti_map <- function(name) {  # name: 文件名；作用：按 t 图/BF 图/二值系统图分类，决定后续配色。
  if (grepl("^t_", name)) return("group_t_map")
  if (grepl("^two_log_bf_", name)) return("paper_style_2log_bayes_factor_map")
  if (grepl("_before_cluster_|_after_cluster_|_consensus_", name)) return("binary_system_mask")
  "other_nifti_map"
}

slice_index_with_signal <- function(arr) {  # arr: 三维数组；作用：找到信号量最大的轴向切片，避免导出纯空白预览。
  score <- vapply(seq_len(dim(arr)[3]), function(k) sum(abs(arr[, , k]), na.rm = TRUE), numeric(1))
  if (!any(score > 0)) return(ceiling(dim(arr)[3] / 2))
  which.max(score)
}

signed_palette <- function(n = 255) {  # n: 颜色数；作用：给含正负值的 t/BF 图配发散色带。
  grDevices::colorRampPalette(c("#204a87", "#f7f7f7", "#b2182b"))(n)
}

positive_palette <- function(n = 255) {  # n: 颜色数；作用：给仅正值连续图配顺序色带。
  grDevices::colorRampPalette(c("#f7f7f7", "#fff7bc", "#fec44f", "#d95f0e", "#7f0000"))(n)
}

mask_palette <- function() {  # 无显式参数；作用：给二值 mask 配 0/1 两色。
  c("#f7f7f7", "#d7191c")
}

write_nifti_slice_png <- function(path, out_path, title = NULL) {  # path: 输入 NIfTI；out_path: PNG 输出；title: 可选标题。
  img <- read_nifti(path)
  arr <- array(as.numeric(img), dim = dim(img))
  arr[!is.finite(arr)] <- 0
  z <- slice_index_with_signal(arr)
  slice <- arr[, , z]
  map_type <- classify_nifti_map(basename(path))
  ensure_dir(dirname(out_path))

  grDevices::png(out_path, width = 1300, height = 1000, res = 150)
  on.exit(grDevices::dev.off(), add = TRUE)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(1, 1, 3, 1), bg = "white")

  display <- t(apply(slice, 2, rev))  # 旋转到更接近常见神经影像浏览方向。
  if (map_type == "binary_system_mask") {
    display <- ifelse(display != 0, 1, 0)
    graphics::image(display, axes = FALSE, asp = 1, col = mask_palette(), main = title %||% basename(path))
  } else if (any(display < 0, na.rm = TRUE)) {
    lim <- max(abs(display), na.rm = TRUE)
    graphics::image(display, axes = FALSE, asp = 1, col = signed_palette(), zlim = c(-lim, lim), main = title %||% basename(path))
  } else {
    graphics::image(display, axes = FALSE, asp = 1, col = positive_palette(), main = title %||% basename(path))
  }
  graphics::mtext(sprintf("Axial slice k=%s, dim=%s", z, paste(dim(img), collapse = "x")), side = 1, line = -1)
  invisible(out_path)
}

`%||%` <- function(x, y) {  # x: 首选值；y: 兜底值；作用：预览脚本里的空值合并运算。
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) y else x
}

write_uncompressed_nifti <- function(path, out_path) {  # path: 输入 .nii.gz；out_path: 输出 .nii；作用：给 MRIcroGL 等工具生成无压缩副本。
  img <- read_nifti(path)
  ensure_dir(dirname(out_path))
  RNifti::writeNifti(img, file = out_path)
  invisible(out_path)
}
