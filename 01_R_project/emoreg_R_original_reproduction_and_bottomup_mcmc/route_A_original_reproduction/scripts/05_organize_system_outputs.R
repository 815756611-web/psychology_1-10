# Route A 整理脚本：把四系统图和互斥 ROI 簇整理成文件夹，便于人工检查和后续 Route B 引用。 
source("R/cli.R")  # CLI 和工作目录工具。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 路径与 data.table。
source("R/nifti_utils.R")  # NIfTI IO。
source("R/clusters3d.R")  # 连通簇工具。
source("R/roi_features.R")  # 复用 label_connected_components() 等 ROI 工具。

args <- parse_cli_args(list(  # tags: 只整理哪些标签；min-roi-voxels: ROI 最小体素数；connectivity: 6/18/26 邻域。
  tags = "all",
  "min-roi-voxels" = CFG$cluster_min_voxels,
  connectivity = 26
))

parse_tags <- function(x) {  # x: 逗号分隔标签字符串；作用：解析成字符向量，all 则返回空向量表示不过滤。
  x <- as.character(x)
  if (!nzchar(x) || identical(tolower(x), "all")) return(character())
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

write_md <- function(path, lines) {  # path: Markdown 输出路径；lines: 文本行向量；作用：统一 UTF-8 写 README。
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con)
}

clean_folder_name <- function(x) {  # x: 任意文件夹名文本；作用：清理成安全的英文 slug，避免中文/符号影响路径兼容性。
  x <- tolower(x)
  x <- gsub("[^a-z0-9_]+", "_", x)
  gsub("^_|_$", "", x)
}

md_table <- function(dt, cols) {  # dt: data.table；cols: 列名；作用：生成 Markdown 表格。
  if (!nrow(dt)) return("暂无可写入的结果。")
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  body <- apply(dt[, ..cols], 1, function(x) paste0("| ", paste(x, collapse = " | "), " |"))
  paste(c(header, sep, body), collapse = "\n")
}

map_index <- function(nifti_dir, systems, tag_filter) {  # nifti_dir: Route A NIfTI 目录；systems: 四系统顺序；tag_filter: 可选标签过滤。
  files <- list.files(nifti_dir, pattern = "\\.nii\\.gz$", full.names = TRUE)
  rows <- list()
  for (path in files) {
    stem <- sub("\\.nii\\.gz$", "", basename(path))
    for (system in systems) {
      prefix <- paste0(system, "_")
      if (!startsWith(stem, prefix)) next
      rest <- sub(prefix, "", stem, fixed = TRUE)
      map_type <- NA_character_
      tag <- NA_character_
      if (startsWith(rest, "after_cluster_")) {
        map_type <- "after_cluster"
        tag <- sub("^after_cluster_", "", rest)
      } else if (startsWith(rest, "before_cluster_")) {
        map_type <- "before_cluster"
        tag <- sub("^before_cluster_", "", rest)
      } else if (startsWith(rest, "consensus_")) {
        map_type <- "consensus"
        tag <- sub("^consensus_", "", rest)
      }
      if (is.na(map_type)) next
      if (length(tag_filter) && !(tag %in% tag_filter)) next
      rows[[length(rows) + 1L]] <- data.table(
        system = system,
        map_type = map_type,
        tag = tag,
        path = normalizePath(path, winslash = "/", mustWork = TRUE)
      )
    }
  }
  rbindlist(rows, fill = TRUE)
}

system_order <- c("reappraisal_only", "common_appraisal", "non_modifiable_emotion", "modifiable_emotion")
system_folders <- c(
  reappraisal_only = "reapp",
  common_appraisal = "common",
  non_modifiable_emotion = "nonmod",
  modifiable_emotion = "mod"
)
system_labels <- c(
  reappraisal_only = "重评专属系统",
  common_appraisal = "共同评价系统",
  non_modifiable_emotion = "不可调节情绪系统",
  modifiable_emotion = "可调节情绪系统"
)
system_reason <- c(
  reappraisal_only = "重评效应有 BF 支持，同时情绪生成没有稳定 BF 支持；用于定位更偏重认知重评的区域。",
  common_appraisal = "情绪生成和重评效应同时有 BF 支持；用于定位两种过程共享的评价相关区域。",
  non_modifiable_emotion = "情绪生成有 BF 支持，同时有证据不支持重评调节；用于定位较难被重评改变的情绪反应区域。",
  modifiable_emotion = "情绪生成有 BF 支持，且重评方向表现为下降调节；用于定位可被重评改变的情绪反应区域。"
)

map_type_code <- function(x) {  # x: before_cluster/after_cluster/consensus；作用：缩短 Windows 输出路径。
  out <- c(before_cluster = "bc", after_cluster = "ac", consensus = "con")[x]
  ifelse(is.na(out), clean_folder_name(x), out)
}

tag_filter <- parse_tags(args$tags)
min_roi_voxels <- as_int_arg(args[["min-roi-voxels"]], as.integer(CFG$cluster_min_voxels))  # 与 MATLAB ClusterControl 的 15 体素阈值一致。
connectivity <- as_int_arg(args$connectivity, 26L)  # 三维连通定义。

nifti_dir <- project_path("route_A_original_reproduction", "outputs", "nifti")
table_dir <- project_path("route_A_original_reproduction", "outputs", "tables")
systems_dir <- project_path("route_A_original_reproduction", "outputs", "systems")

idx <- map_index(nifti_dir, system_order, tag_filter)  # 汇总 after_cluster/before_cluster/consensus 图。
if (!nrow(idx)) stop("No Route A system NIfTI files found for tags=", args$tags)
idx[, system_priority := match(system, system_order)]
setorder(idx, system_priority, tag, map_type)

unlink(systems_dir, recursive = TRUE, force = TRUE)
invisible(ensure_dir(systems_dir))

write_md(file.path(systems_dir, "README.md"), c(
  "# 路线 A：四系统整理输出",
  "",
  "这里按原文四个系统整理路线 A 的输出。基础结果仍保留在 `outputs/nifti` 和 `outputs/tables`。",
  "",
  "整理规则：",
  "",
  "- 系统顺序：`reappraisal_only` -> `common_appraisal` -> `non_modifiable_emotion` -> `modifiable_emotion`。",
  "- 同一个体素如果已经被前一个系统接收，后面的系统不会再重复生成 ROI。",
  "- 只有非空 NIfTI 才会复制或写入；空白图不会放入系统文件夹。",
  "- ROI 来自系统图的三维连通簇，默认至少保留 15 个体素。",
  "",
  "四个系统文件夹：",
  "",
  paste0("- `", unname(system_folders[system_order]), "`：", unname(system_labels[system_order]), " (`", system_order, "`)")
))

assigned_by_key <- list()
all_roi_rows <- list()

for (sys in system_order) {
  system_dir <- ensure_dir(file.path(systems_dir, unname(system_folders[sys])))
  maps_dir <- ensure_dir(file.path(system_dir, "maps"))
  roi_dir <- ensure_dir(file.path(system_dir, "roi"))
  maps_readme <- file.path(maps_dir, "README.md")
  roi_readme <- file.path(roi_dir, "README.md")

  sys_idx <- idx[system == sys]
  map_rows <- list()
  roi_rows <- list()

  logic_path <- file.path(table_dir, "original_four_system_logic.csv")
  if (file.exists(logic_path)) {
    logic <- fread(logic_path)[system == sys]
    if (nrow(logic)) fwrite(logic, file.path(system_dir, "original_logic.csv"))
  }

  summary_files <- list.files(table_dir, pattern = "^original_system_voxel_summary_.*\\.csv$", full.names = TRUE)
  summary_rows <- rbindlist(lapply(summary_files, function(p) fread(p)[system == sys]), fill = TRUE)
  if (nrow(summary_rows)) fwrite(summary_rows, file.path(system_dir, "system_voxel_summary.csv"))

  for (i in seq_len(nrow(sys_idx))) {
    row <- sys_idx[i]
    img <- read_nifti(row$path)
    arr <- array(as.numeric(img) > 0, dim = dim(img))  # 当前系统图的二值 mask。
    original_voxels <- sum(arr)
    if (original_voxels > 0) {
      copied_path <- file.path(maps_dir, paste0("orig_", map_type_code(row$map_type), "_", row$tag, ".nii.gz"))
      file.copy(row$path, copied_path, overwrite = TRUE)
      map_rows[[length(map_rows) + 1L]] <- data.table(
        system = sys,
        tag = row$tag,
        map_type = row$map_type,
        original_voxels = original_voxels,
        copied_file = basename(copied_path),
        source_nifti = basename(row$path)
      )
    }

    if (!(row$map_type %in% c("after_cluster", "consensus"))) next

    key <- paste(row$map_type, row$tag, sep = "__")
    if (is.null(assigned_by_key[[key]])) assigned_by_key[[key]] <- array(FALSE, dim = dim(arr))  # 每个 tag/map_type 维护一个“已被前序系统占用”的体素表。
    overlap_voxels <- sum(arr & assigned_by_key[[key]])
    exclusive <- arr & !assigned_by_key[[key]]  # 做互斥 ROI 分配，防止一个体素出现在多个系统 ROI 里。
    assigned_by_key[[key]] <- assigned_by_key[[key]] | exclusive
    exclusive_voxels <- sum(exclusive)
    if (exclusive_voxels == 0) next

    exclusive_path <- file.path(maps_dir, paste0("ex_", map_type_code(row$map_type), "_", row$tag, ".nii.gz"))
    write_nifti_array(array(as.numeric(exclusive), dim = dim(exclusive)), row$path, exclusive_path)

    cc <- label_connected_components(exclusive, min_voxels = min_roi_voxels, connectivity = connectivity)  # 连通簇提取对应 MATLAB region() 的后续整理。
    if (!nrow(cc$table)) next

    for (component_id in cc$table$component_id) {
      comp <- cc$table[["component_id"]] == component_id
      comp <- cc$table[comp]
      comp_mask <- cc$labels == component_id
      if (sum(comp_mask) == 0) next
      roi_name <- clean_folder_name(sprintf("r%03d_%s_%s", component_id, map_type_code(row$map_type), row$tag))
      one_roi_dir <- ensure_dir(file.path(roi_dir, roi_name))
      roi_mask_path <- file.path(one_roi_dir, "mask.nii.gz")
      write_nifti_array(array(as.numeric(comp_mask), dim = dim(comp_mask)), row$path, roi_mask_path)

      out <- data.table(
        route = "A",
        system = sys,
        system_label = unname(system_labels[sys]),
        tag = row$tag,
        map_type = row$map_type,
        roi_folder = roi_name,
        component_id = component_id,
        n_voxels = comp$n_voxels,
        centroid_i = round(comp$centroid_i, 2),
        centroid_j = round(comp$centroid_j, 2),
        centroid_k = round(comp$centroid_k, 2),
        original_voxels_in_system_map = original_voxels,
        exclusive_voxels_after_overlap_removal = exclusive_voxels,
        overlap_voxels_removed_before_assignment = overlap_voxels,
        source_nifti = basename(row$path)
      )
      fwrite(out, file.path(one_roi_dir, "roi_summary.csv"))
      write_md(file.path(one_roi_dir, "README.md"), c(
        paste0("# ", roi_name),
        "",
        paste0("所属系统：", unname(system_labels[sys]), " (`", sys, "`)"),
        "",
        paste0("分入原因：", unname(system_reason[sys])),
        "",
        paste0("该 ROI 来自 `", row$map_type, "` 图 `", row$tag, "` 的非重叠三维连通簇。"),
        paste0("体素数：", comp$n_voxels, "；体素坐标质心：(",
          round(comp$centroid_i, 2), ", ", round(comp$centroid_j, 2), ", ", round(comp$centroid_k, 2), ")。"),
        "",
        "支撑数据：",
        "",
        "- `mask.nii.gz`：该 ROI 的脑图。",
        "- `roi_summary.csv`：体素数、质心、来源系统图和重叠移除信息。"
      ))
      roi_rows[[length(roi_rows) + 1L]] <- out
      all_roi_rows[[length(all_roi_rows) + 1L]] <- out
    }
  }

  maps_dt <- rbindlist(map_rows, fill = TRUE)
  rois_dt <- rbindlist(roi_rows, fill = TRUE)
  if (nrow(maps_dt)) fwrite(maps_dt, file.path(maps_dir, "system_map_inventory.csv"))
  if (nrow(rois_dt)) fwrite(rois_dt, file.path(roi_dir, "roi_component_summary.csv"))

  write_md(maps_readme, c(
    paste0("# ", unname(system_labels[sys]), "：系统图"),
    "",
    "这里存放该系统的非空系统图。空白图不复制到这里。",
    "",
    if (nrow(maps_dt)) md_table(maps_dt[, .(
      tag, map_type, original_voxels, copied_file, source_nifti
    )], c("tag", "map_type", "original_voxels", "copied_file", "source_nifti")) else "当前标签下没有非空系统图。"
  ))

  write_md(roi_readme, c(
    paste0("# ", unname(system_labels[sys]), "：ROI 连通簇"),
    "",
    "这里的 ROI 是从该系统非空图中提取的三维连通簇。",
    "",
    "为了避免同一脑区重复出现，脚本已经按固定系统顺序做互斥分配；已经进入前面系统的体素不会再进入本系统。",
    "",
    if (nrow(rois_dt)) md_table(rois_dt[, .(
      tag, map_type, roi_folder, n_voxels, centroid_i, centroid_j, centroid_k
    )], c("tag", "map_type", "roi_folder", "n_voxels", "centroid_i", "centroid_j", "centroid_k")) else "当前标签下没有达到体素数阈值的非空 ROI。"
  ))

  write_md(file.path(system_dir, "README.md"), c(
    paste0("# ", unname(system_labels[sys])),
    "",
    paste0("系统代码：`", sys, "`"),
    "",
    paste0("分入原则：", unname(system_reason[sys])),
    "",
    "包含内容：",
    "",
    "- `original_logic.csv`：原文对应的 BF/t 判定逻辑。",
    "- `system_voxel_summary.csv`：该系统在不同 tag 下的体素数量和簇控制摘要。",
    "- `maps`：非空系统图和互斥系统图。",
    "- `roi`：非重叠 ROI 连通簇和对应说明。",
    "",
    "本文件夹中的 ROI 已经过互斥整理，避免同一 ROI 同时出现在多个系统中。"
  ))
}

all_rois <- rbindlist(all_roi_rows, fill = TRUE)
if (nrow(all_rois)) {
  fwrite(all_rois, file.path(systems_dir, "all_roi_components.csv"))
}

cat("Route A organized system outputs written:\n")
cat("  ", systems_dir, "\n", sep = "")
