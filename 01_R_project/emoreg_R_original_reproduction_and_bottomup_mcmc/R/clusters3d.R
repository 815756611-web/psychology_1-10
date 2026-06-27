# 三维连通簇层：对应 MATLAB 的 region()/region2fmri_data() + “>=15 体素” 簇控制；R 版同时服务 Route A 和 Route B ROI 切块。 
neighbor_offsets <- function(dim, connectivity = 26) {  # dim: 体素数组尺寸；connectivity: 6/18/26 邻域；作用：生成 BFS 连通邻居偏移。
  deltas <- expand.grid(dx = -1:1, dy = -1:1, dz = -1:1)
  deltas <- deltas[!(deltas$dx == 0 & deltas$dy == 0 & deltas$dz == 0), ]
  if (connectivity == 6) {
    deltas <- deltas[rowSums(abs(deltas)) == 1, ]
  } else if (connectivity == 18) {
    deltas <- deltas[rowSums(abs(deltas)) <= 2, ]
  }
  cbind(deltas$dx, deltas$dy, deltas$dz)
}

component_kernel <- function(connectivity = 26) {  # connectivity: 6/18/26 邻域；作用：给 mmand::components 构造三维连通核。
  grid <- expand.grid(dx = -1:1, dy = -1:1, dz = -1:1)
  man <- rowSums(abs(grid))
  keep <- if (connectivity == 6) {
    man <= 1
  } else if (connectivity == 18) {
    man <= 2
  } else {
    rep(TRUE, nrow(grid))
  }
  k <- array(FALSE, dim = c(3, 3, 3))
  k[cbind(grid$dx[keep] + 2, grid$dy[keep] + 2, grid$dz[keep] + 2)] <- TRUE
  k
}

keep_clusters_by_extent <- function(mask, min_voxels = 15, connectivity = 26) {  # mask: 二值脑图；min_voxels: 最小保留体素数；connectivity: 邻域定义。
  if (requireNamespace("mmand", quietly = TRUE)) {
    labels <- mmand::components(array(as.numeric(mask), dim = dim(mask)), component_kernel(connectivity))  # mmand：更快的 3D 组件标记；无包时下面走纯 R BFS。
    ids <- as.integer(labels[!is.na(labels)])
    counts <- tabulate(ids)
    keep_ids <- which(counts >= min_voxels)
    return(!is.na(labels) & labels %in% keep_ids)
  }

  dims <- dim(mask)
  true_idx <- which(mask)
  if (!length(true_idx)) return(mask)
  pos_of <- integer(prod(dims))
  pos_of[true_idx] <- seq_along(true_idx)
  visited <- rep(FALSE, length(true_idx))
  keep <- rep(FALSE, length(true_idx))
  coords <- arrayInd(true_idx, .dim = dims)
  offsets <- neighbor_offsets(dims, connectivity)
  for (start_pos in seq_along(true_idx)) {
    if (visited[start_pos]) next
    queue <- integer(length(true_idx))
    queue[1L] <- start_pos
    tail <- 1L
    visited[start_pos] <- TRUE
    head <- 1L
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
    if (tail >= min_voxels) keep[queue[seq_len(tail)]] <- TRUE
  }
  out <- array(FALSE, dim = dims)
  out[true_idx[keep]] <- TRUE
  out
}

gaussian_kernel1d <- function(fwhm, radius = ceiling(3 * fwhm / 2.355)) {  # fwhm: 平滑半高宽；radius: 核半径；作用：生成一维高斯核用于 consensus 平滑。
  sigma <- fwhm / 2.355
  x <- -radius:radius
  k <- exp(-(x^2) / (2 * sigma^2))
  k / sum(k)
}

smooth_array_3d <- function(arr, fwhm = 3) {  # arr: 三维数组；fwhm: 平滑核宽度；作用：复现 MATLAB preprocess(...,'smooth',3) 的 product-map 预平滑思路。
  k <- gaussian_kernel1d(fwhm)
  smooth_one_axis <- function(a, margin) {  # a: 待平滑数组；margin: 当前轴编号；作用：逐轴 separable convolution。
    out <- a
    dims <- dim(a)
    if (margin == 1) {
      for (j in seq_len(dims[2])) for (l in seq_len(dims[3])) {
        out[, j, l] <- stats::filter(a[, j, l], k, sides = 2, circular = FALSE)
      }
    } else if (margin == 2) {
      for (i in seq_len(dims[1])) for (l in seq_len(dims[3])) {
        out[i, , l] <- stats::filter(a[i, , l], k, sides = 2, circular = FALSE)
      }
    } else {
      for (i in seq_len(dims[1])) for (j in seq_len(dims[2])) {
        out[i, j, ] <- stats::filter(a[i, j, ], k, sides = 2, circular = FALSE)
      }
    }
    out[is.na(out)] <- 0
    out
  }
  arr <- smooth_one_axis(arr, 1)
  arr <- smooth_one_axis(arr, 2)
  arr <- smooth_one_axis(arr, 3)
  arr
}
