# 数据镜像脚本：把 bundle 里的外部材料链接/复制进 data/raw；对应 MATLAB 中脚本直接 load 原始绝对路径。 
source("R/cli.R")  # 引入命令行和工作目录定位函数。
set_project_workdir()  # 强制切到当前复制版项目根目录。
source("R/project.R")  # 引入 CFG、project_path、ensure_dir 等公共函数。

source_root <- resolve_project_path(CFG$source_data_root, mustWork = TRUE)  # 外部源材料根目录；配置里现在保存为相对路径，再统一解析到实际位置。
raw_root <- project_path("data", "raw")  # 项目内部原始数据镜像目录。
invisible(ensure_dir(raw_root))

link_or_copy_file <- function(source, destination) {  # source: 源文件；destination: 目标文件；作用：优先硬链接，失败再复制，节省空间。
  ensure_dir(dirname(destination))
  if (file.exists(destination)) return(invisible(FALSE))
  ok <- suppressWarnings(file.link(source, destination))
  if (!isTRUE(ok)) {
    ok <- file.copy(source, destination, overwrite = FALSE, copy.date = TRUE)
  }
  if (!isTRUE(ok)) stop("Could not link or copy: ", source, call. = FALSE)
  invisible(TRUE)
}

mirror_files <- function(source_dir, destination_dir, pattern = NULL) {  # source_dir: 源目录；destination_dir: 目标目录；pattern: 可选文件名过滤。
  source_dir <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  ensure_dir(destination_dir)
  files <- list.files(source_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  info <- file.info(files)
  files <- files[!is.na(info$isdir) & info$isdir == FALSE]  # 丢弃 file.info() 无法解析的 NA 条目，避免后续 normalizePath(NA) 中断。
  if (!is.null(pattern)) files <- files[grepl(pattern, basename(files))]
  for (source in files) {
    rel <- substring(normalizePath(source, winslash = "/", mustWork = TRUE), nchar(source_dir) + 2L)  # 保留源目录内部相对结构。
    destination <- file.path(destination_dir, rel)  # 在 data/raw 下镜像成同构目录。
    link_or_copy_file(source, destination)
  }
  length(files)
}

counts <- list(  # 四类数据分别镜像：beta 图、原文系统图、PET 图、行为元数据。
  beta_maps = mirror_files(
    file.path(source_root, "01_first_level_beta_maps_neurovault"),
    file.path(raw_root, "beta_maps"),
    pattern = "\\.nii\\.gz$"
  ),
  system_maps = mirror_files(
    file.path(source_root, "05_system_component_maps", "CANlab_2024_Bo_EmotionRegulation_BayesFactor"),
    file.path(raw_root, "system_maps")
  ),
  pet_maps = mirror_files(
    file.path(source_root, "03_pet_and_neurotransmitter_maps"),
    file.path(raw_root, "pet_maps")
  ),
  behavior = mirror_files(
    file.path(source_root, "02_behavioral_and_subject_metadata"),
    file.path(raw_root, "behavior")
  )
)

writeLines(paste0("Source: ", source_root), file.path(raw_root, "SOURCE_DATA_LOCATION.txt"), useBytes = TRUE)

cat("Relative data files are ready under ", raw_root, "\n", sep = "")
cat("Linked/copied file counts:\n")
for (nm in names(counts)) cat("  ", nm, ": ", counts[[nm]], "\n", sep = "")
