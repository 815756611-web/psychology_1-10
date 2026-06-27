# 旧版 parcel 原型层：主要给早期 Route B/调试用；正式分析已切到原文 ROI 连通簇，因此这里只保留英文实现并加解释。 
make_brain_mask <- function(paths, max_images = 12, eps = 1e-8) {  # paths: NIfTI 路径向量；max_images: 最多抽多少图估计脑掩模；eps: 视为非零的阈值。
  selected <- head(paths, max_images)
  mask <- NULL
  for (path in selected) {
    arr <- read_nifti(path)
    current <- is.finite(arr) & abs(arr) > eps
    mask <- if (is.null(mask)) current else (mask | current)
  }
  mask
}

make_grid_parcels <- function(mask, block_size = 6, min_voxels = 15) {  # mask: 脑掩模；block_size: 立方体边长；min_voxels: 最小体素数。
  dims <- dim(mask)
  labels <- array(0L, dim = dims)
  rows <- list()
  pid <- 0L
  for (x0 in seq(1, dims[1], by = block_size)) {
    for (y0 in seq(1, dims[2], by = block_size)) {
      for (z0 in seq(1, dims[3], by = block_size)) {
        xs <- x0:min(x0 + block_size - 1, dims[1])
        ys <- y0:min(y0 + block_size - 1, dims[2])
        zs <- z0:min(z0 + block_size - 1, dims[3])
        block <- mask[xs, ys, zs, drop = FALSE]  # 当前小立方体中真正属于脑的体素。
        nvox <- sum(block)
        if (nvox < min_voxels) next
        pid <- pid + 1L
        sub <- labels[xs, ys, zs, drop = FALSE]
        sub[block] <- pid
        labels[xs, ys, zs] <- sub
        coords <- which(block, arr.ind = TRUE)
        rows[[length(rows) + 1L]] <- data.table(
          parcel_id = pid,
          n_voxels = nvox,
          centroid_i = mean(coords[, 1] + x0 - 1),
          centroid_j = mean(coords[, 2] + y0 - 1),
          centroid_k = mean(coords[, 3] + z0 - 1)
        )
      }
    }
  }
  list(labels = labels, table = rbindlist(rows))
}

parcel_means <- function(vec, labels, n_parcels = max(labels)) {  # vec: 体素向量；labels: parcel 标签数组；n_parcels: parcel 总数。
  lab <- as.integer(labels)
  valid <- lab > 0 & is.finite(vec)
  sums <- numeric(n_parcels)
  if (any(valid)) {
    summed <- rowsum(vec[valid], group = lab[valid], reorder = FALSE)
    sums[as.integer(rownames(summed))] <- as.numeric(summed[, 1])
  }
  counts <- tabulate(lab[valid], nbins = n_parcels)
  out <- rep(NA_real_, n_parcels)
  ok <- counts > 0
  out[ok] <- sums[ok] / counts[ok]
  out
}

subject_parcel_features <- function(wide, labels, parcel_table) {  # wide: 被试宽表；labels: parcel 标签；parcel_table: parcel 元数据。
  n_parcels <- nrow(parcel_table)
  rows <- vector("list", nrow(wide))
  for (i in seq_len(nrow(wide))) {
    message("Parcel extraction subject ", i, "/", nrow(wide), " ", wide$dataset[i], "_", wide$subject[i])
    v <- contrast_subject_vectors(wide, i)
    e <- parcel_means(v$emotion_generation, labels, n_parcels)  # 每个 parcel 的情绪生成均值。
    r <- parcel_means(v$reappraisal_effect, labels, n_parcels)  # 每个 parcel 的重评效应均值。
    rows[[i]] <- data.table(
      dataset = wide$dataset[i],
      subject = wide$subject[i],
      parcel_id = parcel_table$parcel_id,
      emotion_generation = e,
      reappraisal_effect = r,
      look_neg_base = parcel_means(v$look_neg, labels, n_parcels),
      reg_neg_base = parcel_means(v$reg_neg, labels, n_parcels)
    )
  }
  rbindlist(rows)
}

default_bf_method <- function() {  # 无显式参数；作用：优先用 config 里的 BF 方法，否则回退到 bic_approx。
  if (exists("CFG", inherits = TRUE) && !is.null(CFG$bf_method)) return(as.character(CFG$bf_method))
  "bic_approx"
}

summarise_parcel_features <- function(subject_features, parcel_table, bf_method = default_bf_method()) {  # subject_features: 被试×parcel 特征表；parcel_table: parcel 元数据；bf_method: BF 计算方法。
  summarise_one <- function(x) {  # x: 一个 parcel 在所有被试上的单一特征值；作用：求 n/mean/sd/t，供 parcel 解释。
    n <- sum(is.finite(x))
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) s <- 1e-8
    t <- m / (s / sqrt(max(n, 1L)))
    list(n = n, mean = m, sd = s, t = ifelse(is.finite(t), t, 0))
  }
  tab <- subject_features[, {  # 这里是 parcel 级别的效应摘要；正式 Route B 与原文 ROI 路线对应更弱，只保留为备选原型。
    e <- summarise_one(emotion_generation)
    r <- summarise_one(reappraisal_effect)
    ln <- summarise_one(look_neg_base)
    rg <- summarise_one(reg_neg_base)
    .(
      n_subjects = min(e$n, r$n, ln$n, rg$n),
      emotion_mean = e$mean,
      emotion_sd = e$sd,
      emotion_t = e$t,
      reappraisal_mean = r$mean,
      reappraisal_sd = r$sd,
      reappraisal_t = r$t,
      look_neg_base_mean = ln$mean,
      look_neg_base_sd = ln$sd,
      look_neg_base_t = ln$t,
      reg_neg_base_mean = rg$mean,
      reg_neg_base_sd = rg$sd,
      reg_neg_base_t = rg$t
    )
  }, by = parcel_id]
  tab[, emotion_2logbf := t_to_two_log_bf(emotion_t, n_subjects, method = bf_method, signed = FALSE)]
  tab[, reappraisal_2logbf := t_to_two_log_bf(reappraisal_t, n_subjects, method = bf_method, signed = FALSE)]
  tab[, look_neg_base_2logbf := t_to_two_log_bf(look_neg_base_t, n_subjects, method = bf_method, signed = FALSE)]
  tab[, reg_neg_base_2logbf := t_to_two_log_bf(reg_neg_base_t, n_subjects, method = bf_method, signed = FALSE)]
  tab[, emotion_signed_2logbf := sign(emotion_t) * abs(emotion_2logbf)]
  tab[, reappraisal_signed_2logbf := sign(reappraisal_t) * abs(reappraisal_2logbf)]
  tab[, look_neg_base_signed_2logbf := sign(look_neg_base_t) * abs(look_neg_base_2logbf)]
  tab[, reg_neg_base_signed_2logbf := sign(reg_neg_base_t) * abs(reg_neg_base_2logbf)]
  merge(parcel_table, tab, by = "parcel_id", all.x = TRUE)
}
