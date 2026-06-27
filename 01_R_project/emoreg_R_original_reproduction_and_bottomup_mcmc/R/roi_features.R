# Route B ROI 层：直接对应原文四系统图的连通簇整理；不同于 MATLAB，R 版把 ROI 标签、特征抽取和后验解释拆成模块。 
clean_system_name <- function(x) {  # x: 原始系统图文件名；作用：标准化成 route_b_system_map_index() 可识别的四系统代码名。
  x <- tolower(x)
  x <- gsub("\\.nii(\\.gz)?$", "", x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x <- sub("^common_appraisal.*", "common_appraisal", x)
  x <- sub("^reappraisal_only.*", "reappraisal_only", x)
  x <- sub("^non_modifiable_emotion.*", "non_modifiable_emotion", x)
  x <- sub("^modifiable_emotion.*", "modifiable_emotion", x)
  x
}

route_b_system_map_index <- function(source = c("auto", "downloaded", "route_a_consensus", "route_a_after_cluster"),
                                     tag = "full") {  # source: ROI 来源图选择；tag: Route A/下载图的标签；作用：构建 Route B 所用系统图索引。
  source <- match.arg(source)
  downloaded <- function() {  # 直接使用原文发布的系统图；与 MATLAB 原脚本最接近。
    files <- list.files(project_path("data", "raw", "system_maps"), pattern = "\\.nii(\\.gz)?$", full.names = TRUE)
    if (!length(files)) return(data.table())
    data.table(
      system = clean_system_name(basename(files)),
      path = normalizePath(files, winslash = "/", mustWork = TRUE),
      map_source = "downloaded_original_system_maps"
    )[system %in% c("common_appraisal", "reappraisal_only", "non_modifiable_emotion", "modifiable_emotion")]
  }
  consensus <- function() {  # 使用 Route A 生成的 AHAB/PIP 共识图。
    files <- list.files(project_path("route_A_original_reproduction", "outputs", "nifti"),
      pattern = paste0("_consensus_.*", tag, ".*\\.nii\\.gz$|_consensus_.*\\.nii\\.gz$"),
      full.names = TRUE
    )
    if (!length(files)) return(data.table())
    data.table(
      system = clean_system_name(basename(files)),
      path = normalizePath(files, winslash = "/", mustWork = TRUE),
      map_source = "route_a_consensus_maps"
    )[system %in% c("common_appraisal", "reappraisal_only", "non_modifiable_emotion", "modifiable_emotion")]
  }
  after_cluster <- function() {  # 使用 Route A 单样本 after-cluster 图；用于更细粒度试验。
    files <- list.files(project_path("route_A_original_reproduction", "outputs", "nifti"),
      pattern = paste0("_after_cluster_", tag, "\\.nii\\.gz$"),
      full.names = TRUE
    )
    if (!length(files)) return(data.table())
    data.table(
      system = clean_system_name(basename(files)),
      path = normalizePath(files, winslash = "/", mustWork = TRUE),
      map_source = paste0("route_a_after_cluster_", tag)
    )[system %in% c("common_appraisal", "reappraisal_only", "non_modifiable_emotion", "modifiable_emotion")]
  }

  out <- switch(
    source,
    downloaded = downloaded(),
    route_a_consensus = consensus(),
    route_a_after_cluster = after_cluster(),
    auto = {
      x <- consensus()
      if (uniqueN(x$system) >= 4) x else downloaded()
    }
  )
  if (!nrow(out)) stop("No usable Route B system maps found for source=", source, ".")
  out <- out[order(match(system, c("reappraisal_only", "common_appraisal", "non_modifiable_emotion", "modifiable_emotion")))]
  out[, .SD[1], by = system]
}

label_connected_components <- function(mask, min_voxels = 15, connectivity = 26) {  # mask: 二值脑图；min_voxels: 最小 ROI 体素数；connectivity: 6/18/26 邻域。
  mask <- array(as.logical(mask), dim = dim(mask))
  dims <- dim(mask)
  if (requireNamespace("mmand", quietly = TRUE)) {
    raw_labels <- mmand::components(array(as.numeric(mask), dim = dims), component_kernel(connectivity))
    raw_ids <- sort(unique(as.integer(raw_labels[!is.na(raw_labels)])))
    raw_ids <- raw_ids[raw_ids > 0]
    labels <- array(0L, dim = dims)
    rows <- list()
    component_id <- 0L
    for (raw_id in raw_ids) {
      idx <- which(!is.na(raw_labels) & raw_labels == raw_id)
      if (length(idx) < min_voxels) next
      component_id <- component_id + 1L
      labels[idx] <- component_id  # 把当前连通簇标成一个 ROI 编号。
      cxyz <- arrayInd(idx, .dim = dims)
      rows[[length(rows) + 1L]] <- data.table(
        component_id = component_id,
        n_voxels = length(idx),
        centroid_i = mean(cxyz[, 1]),
        centroid_j = mean(cxyz[, 2]),
        centroid_k = mean(cxyz[, 3])
      )
    }
    return(list(labels = labels, table = rbindlist(rows, fill = TRUE)))
  }

  true_idx <- which(mask)  # 无 mmand 时改走纯 R 的 BFS 连通域搜索。
  labels <- array(0L, dim = dims)
  if (!length(true_idx)) return(list(labels = labels, table = data.table()))

  pos_of <- integer(prod(dims))
  pos_of[true_idx] <- seq_along(true_idx)
  visited <- rep(FALSE, length(true_idx))
  coords <- arrayInd(true_idx, .dim = dims)
  offsets <- neighbor_offsets(dims, connectivity)
  rows <- list()
  component_id <- 0L

  for (start_pos in seq_along(true_idx)) {
    if (visited[start_pos]) next
    queue <- integer(length(true_idx))
    queue[1L] <- start_pos
    tail <- 1L
    head <- 1L
    visited[start_pos] <- TRUE
    while (head <= tail) {
      p <- queue[head]
      head <- head + 1L
      xyz <- coords[p, ]
      nb <- sweep(offsets, 2, xyz, "+")
      ok <- nb[, 1] >= 1 & nb[, 1] <= dims[1] &
        nb[, 2] >= 1 & nb[, 2] <= dims[2] &
        nb[, 3] >= 1 & nb[, 3] <= dims[3]
      nb <- nb[ok, , drop = FALSE]
      lin <- nb[, 1] + (nb[, 2] - 1) * dims[1] + (nb[, 3] - 1) * dims[1] * dims[2]
      nb_pos <- pos_of[lin]
      nb_pos <- nb_pos[nb_pos > 0 & !visited[nb_pos]]
      if (length(nb_pos)) {
        visited[nb_pos] <- TRUE
        queue[(tail + 1L):(tail + length(nb_pos))] <- nb_pos
        tail <- tail + length(nb_pos)
      }
    }
    members <- queue[seq_len(tail)]
    if (length(members) < min_voxels) next
    component_id <- component_id + 1L
    labels[true_idx[members]] <- component_id
    cxyz <- coords[members, , drop = FALSE]
    rows[[length(rows) + 1L]] <- data.table(
      component_id = component_id,
      n_voxels = length(members),
      centroid_i = mean(cxyz[, 1]),
      centroid_j = mean(cxyz[, 2]),
      centroid_k = mean(cxyz[, 3])
    )
  }
  list(labels = labels, table = rbindlist(rows, fill = TRUE))
}

make_route_b_roi_labels <- function(system_maps, min_voxels = 15, connectivity = 26) {  # system_maps: 四系统图索引；min_voxels/connectivity: ROI 连通簇阈值定义。
  ref <- system_maps$path[1]
  ref_dim <- dim(read_nifti(ref))
  global_labels <- array(0L, dim = ref_dim)
  roi_rows <- list()
  next_roi <- 0L
  overlap_voxels <- 0L

  for (i in seq_len(nrow(system_maps))) {  # 体素按系统顺序首次命中即归属，避免 ROI 跨系统重复。
    sys <- system_maps$system[i]
    path <- system_maps$path[i]
    arr <- read_nifti(path) > 0
    if (!identical(dim(arr), ref_dim)) stop("System map dimensions differ: ", path)
    cc <- label_connected_components(arr, min_voxels = min_voxels, connectivity = connectivity)
    if (!nrow(cc$table)) next
    for (component_id in cc$table$component_id) {
      comp_id <- component_id
      mask <- cc$labels == comp_id
      overlap_voxels <- overlap_voxels + sum(mask & global_labels > 0)
      assign_mask <- mask & global_labels == 0  # 与前序系统重叠的体素不再重复分配，保证 ROI 互斥。
      if (sum(assign_mask) < min_voxels) next
      next_roi <- next_roi + 1L
      global_labels[assign_mask] <- next_roi
      one <- cc$table[cc$table[["component_id"]] == comp_id]
      one[, `:=`(
        roi_id = next_roi,
        source_system = sys,
        map_source = system_maps$map_source[i],
        source_path = path,
        n_voxels_assigned = sum(assign_mask)
      )]
      roi_rows[[length(roi_rows) + 1L]] <- one
    }
  }
  table <- rbindlist(roi_rows, fill = TRUE)
  setcolorder(table, c("roi_id", "source_system", "component_id", "n_voxels", "n_voxels_assigned",
    "centroid_i", "centroid_j", "centroid_k", "map_source", "source_path"))
  attr(table, "overlap_voxels_skipped") <- overlap_voxels
  list(labels = global_labels, table = table, reference_nifti = ref)
}

roi_means <- function(vec, labels, roi_table) {  # vec: 体素向量；labels: ROI 标签图；roi_table: ROI 元数据；作用：复用 parcel_means 计算 ROI 均值。
  parcel_means(vec, labels, nrow(roi_table))
}

subject_roi_features <- function(wide, labels, roi_table) {  # wide: 被试宽表；labels: ROI 标签图；roi_table: ROI 元数据；作用：构建被试×ROI 特征表。
  n_rois <- nrow(roi_table)
  rows <- vector("list", nrow(wide))
  for (i in seq_len(nrow(wide))) {
    message("ROI extraction subject ", i, "/", nrow(wide), " ", wide$dataset[i], "_", wide$subject[i])
    v <- contrast_subject_vectors(wide, i)
    rows[[i]] <- data.table(
      dataset = wide$dataset[i],
      subject = wide$subject[i],
      roi_id = roi_table$roi_id,
      source_system = roi_table$source_system,
      emotion_generation = roi_means(v$emotion_generation, labels, roi_table),  # Route B 核心特征 1。
      reappraisal_effect = roi_means(v$reappraisal_effect, labels, roi_table),  # Route B 核心特征 2。
      look_neu_base = roi_means(v$look_neu, labels, roi_table),  # 中性观看基线。
      look_neg_base = roi_means(v$look_neg, labels, roi_table),  # 负性观看基线。
      reg_neg_base = roi_means(v$reg_neg, labels, roi_table),  # 重评负性基线。
      look_negative_minus_neutral = roi_means(v$look_neg - v$look_neu, labels, roi_table),  # 解释性补充特征。
      regulate_negative_minus_look_negative = roi_means(v$reg_neg - v$look_neg, labels, roi_table)  # 解释性补充特征。
    )
  }
  rbindlist(rows)
}

effect_summary <- function(x, rope = 0.05) {  # x: 一个 ROI 在所有被试上的特征值；rope: 实用零效应区间半宽；作用：求均值、t 和零效应概率。
  x <- x[is.finite(x)]
  n <- length(x)
  if (!n) {
    return(list(n = 0L, mean = NA_real_, sd = NA_real_, t = 0, p_gt0 = NA_real_, p_lt0 = NA_real_, p_rope = NA_real_))
  }
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) s <- 1e-8
  se <- s / sqrt(n)
  t <- m / se
  list(
    n = n,
    mean = m,
    sd = s,
    t = t,
    p_gt0 = stats::pnorm(0, mean = m, sd = se, lower.tail = FALSE),
    p_lt0 = stats::pnorm(0, mean = m, sd = se, lower.tail = TRUE),
      p_rope = stats::pnorm(rope, mean = m, sd = se) - stats::pnorm(-rope, mean = m, sd = se)
  )
}

summarise_roi_features <- function(subject_features, roi_table, rope = 0.05, bf_method = "bic_approx") {  # subject_features: 被试×ROI 特征；roi_table: ROI 元数据；rope: 近零区间；bf_method: BF 计算方式。
  feature_cols <- intersect(
    c(
      "emotion_generation", "reappraisal_effect",
      "look_neu_base", "look_neg_base", "reg_neg_base",
      "look_negative_minus_neutral", "regulate_negative_minus_look_negative"
    ),
    names(subject_features)
  )
  rows <- subject_features[, {
    vals <- list()
    for (col in feature_cols) {  # 每个 ROI、每个特征都同时保留均值/t/BF/近零概率，供 Route B 画像与解释使用。
      st <- effect_summary(get(col), rope = rope)
      bf <- t_to_two_log_bf(ifelse(is.finite(st$t), st$t, 0), st$n, method = bf_method, signed = FALSE)
      vals[[paste0(col, "_n")]] <- st$n
      vals[[paste0(col, "_mean")]] <- st$mean
      vals[[paste0(col, "_sd")]] <- st$sd
      vals[[paste0(col, "_t")]] <- st$t
      vals[[paste0(col, "_2logbf")]] <- bf
      vals[[paste0(col, "_signed_2logbf")]] <- sign(st$t) * abs(bf)
      vals[[paste0(col, "_signed_logbf")]] <- sign(st$t) * abs(bf) / 2
      vals[[paste0(col, "_p_gt0")]] <- st$p_gt0
      vals[[paste0(col, "_p_lt0")]] <- st$p_lt0
      vals[[paste0(col, "_p_rope")]] <- st$p_rope
    }
    as.data.table(vals)
  }, by = .(roi_id, source_system)]
  out <- merge(roi_table, rows, by = c("roi_id", "source_system"), all.x = TRUE)
  alias_pairs <- c(
    look_neutral_base = "look_neu_base",
    look_negative_base = "look_neg_base",
    regulate_negative_base = "reg_neg_base"
  )
  suffixes <- c("_n", "_mean", "_sd", "_t", "_2logbf", "_signed_2logbf", "_signed_logbf", "_p_gt0", "_p_lt0", "_p_rope")
  for (alias in names(alias_pairs)) {  # 给 look_neu/look_neg/reg_neg 增加更直观的别名，方便报告和图示读取。
    base <- alias_pairs[[alias]]
    for (suffix in suffixes) {
      old <- paste0(base, suffix)
      new <- paste0(alias, suffix)
      if (old %in% names(out) && !(new %in% names(out))) out[, (new) := get(old)]
    }
  }
  out
}
