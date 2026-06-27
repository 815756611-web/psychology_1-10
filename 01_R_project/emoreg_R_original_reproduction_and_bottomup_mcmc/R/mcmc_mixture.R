# Route B MCMC 核心层：直接依赖 mvtnorm；目标是对 ROI 的潜在类别结构做后验推断。 
require_pkg("mvtnorm")  # mvtnorm：提供 rmvnorm/dmvnorm，多元正态采样和似然计算是高斯混合模型的核心。

standardize_matrix <- function(x) {  # x: 数值矩阵；作用：按列 z-score，避免某个特征量纲主导 Route B 聚类。
  center <- colMeans(x, na.rm = TRUE)
  scale <- apply(x, 2, sd, na.rm = TRUE)
  scale[!is.finite(scale) | scale == 0] <- 1
  list(x = sweep(sweep(x, 2, center, "-"), 2, scale, "/"), center = center, scale = scale)
}

rinvwishart <- function(df, scale) {  # df: 逆 Wishart 自由度；scale: 尺度矩阵；作用：采样协方差先验的后验更新值。
  solve(stats::rWishart(1, df = df, Sigma = solve(scale))[, , 1])
}

rdirichlet1 <- function(alpha) {  # alpha: Dirichlet 参数向量；作用：采样 cluster 混合权重。
  x <- stats::rgamma(length(alpha), shape = alpha, rate = 1)
  x / sum(x)
}

sample_categorical_rows <- function(prob) {  # prob: 每行一个观测、每列一个类别的后验概率；作用：按行抽样离散 cluster 标签。
  cdf <- t(apply(prob, 1, cumsum))
  cdf[, ncol(cdf)] <- 1
  u <- stats::runif(nrow(prob))
  rowSums(cdf < u) + 1L
}

sample_niw <- function(x, z, k, mu0, kappa0, nu0, psi0) {  # x: 观测矩阵；z: 当前标签；k: 成分数；其余为 NIW 先验超参数。
  d <- ncol(x)
  mu <- matrix(0, k, d)
  sigma <- array(0, dim = c(d, d, k))
  for (cl in seq_len(k)) {
    xk <- x[z == cl, , drop = FALSE]
    nk <- nrow(xk)
    if (nk == 0) {
      kappa_n <- kappa0
      nu_n <- nu0
      mu_n <- mu0
      psi_n <- psi0
    } else {
      xbar <- colMeans(xk)  # 当前 cluster 的经验均值。
      centered <- sweep(xk, 2, xbar, "-")
      scatter <- crossprod(centered)
      kappa_n <- kappa0 + nk
      nu_n <- nu0 + nk
      mu_n <- (kappa0 * mu0 + nk * xbar) / kappa_n
      diff <- matrix(xbar - mu0, ncol = 1)
      psi_n <- psi0 + scatter + (kappa0 * nk / kappa_n) * (diff %*% t(diff))
    }
    sigma[, , cl] <- rinvwishart(nu_n, psi_n) + diag(1e-6, d)
    mu[cl, ] <- as.numeric(mvtnorm::rmvnorm(1, mean = mu_n, sigma = sigma[, , cl] / kappa_n))
  }
  list(mu = mu, sigma = sigma)
}

align_labels_greedy <- function(mu, z, ref) {  # mu: 当前链的 cluster 均值；z: 当前标签；ref: 初始 kmeans 中心；作用：减少 label switching。
  k <- nrow(ref)
  dist <- as.matrix(stats::dist(rbind(ref, mu)))[seq_len(k), k + seq_len(k), drop = FALSE]
  map <- integer(k)
  used <- rep(FALSE, k)
  for (target in seq_len(k)) {
    d <- dist[target, ]
    d[used] <- Inf
    old <- which.min(d)
    used[old] <- TRUE
    map[old] <- target
  }
  map[z]
}

run_mcmc_mixture <- function(x, k = 4, iter = 1200, burn = 500, thin = 5, alpha = 1, seed = 20260608) {  # x: 行级观测矩阵；k: 请求成分数；iter/burn/thin: MCMC 链设置；alpha: Dirichlet 浓度；seed: 随机种子。
  set.seed(seed)
  n <- nrow(x)
  d <- ncol(x)
  km <- stats::kmeans(x, centers = k, nstart = 25)
  z <- km$cluster
  ref <- km$centers
  mu0 <- rep(0, d)
  kappa0 <- 0.05
  nu0 <- d + 3
  psi0 <- diag(1.5, d)
  posterior <- matrix(0, n, k)
  loglik <- numeric(0)
  z_draws <- matrix(NA_integer_, nrow = 0L, ncol = n)
  kept <- 0L
  for (it in seq_len(iter)) {  # Gibbs 顺序：先权重，再参数，最后标签。
    counts <- tabulate(z, nbins = k)  # 当前 cluster 计数。
    pi <- rdirichlet1(alpha + counts)  # Gibbs 更新混合权重。
    pars <- sample_niw(x, z, k, mu0, kappa0, nu0, psi0)  # Gibbs 更新各 cluster 的均值和协方差。
    logp <- matrix(0, n, k)
    for (cl in seq_len(k)) {
      logp[, cl] <- log(pi[cl] + 1e-15) +
        mvtnorm::dmvnorm(x, mean = pars$mu[cl, ], sigma = pars$sigma[, , cl], log = TRUE)
    }
    logp <- logp - apply(logp, 1, max)
    p <- exp(logp)
    p <- p / rowSums(p)
    z <- sample_categorical_rows(p)
    if (it > burn && ((it - burn) %% thin == 0)) {
      za <- align_labels_greedy(pars$mu, z, ref)  # 保存抽样前做标签对齐，便于后验均值和共聚类矩阵解释。
      posterior[cbind(seq_len(n), za)] <- posterior[cbind(seq_len(n), za)] + 1
      z_draws <- rbind(z_draws, za)
      kept <- kept + 1L
      ll <- sum(log(rowSums(exp(logp) * matrix(pi, n, k, byrow = TRUE)) + 1e-15))
      loglik <- c(loglik, ll)
    }
    if (it %% 100 == 0) message("MCMC iter ", it, "/", iter)
  }
  post <- posterior / max(kept, 1L)
  list(
    posterior = post,
    cluster = max.col(post),
    kept = kept,
    mean_loglik = mean(loglik),
    loglik = loglik,
    z_draws = z_draws,
    ref_centers = ref
  )
}

log_sum_exp <- function(x) {  # x: 对数概率向量；作用：稳定地计算 log(sum(exp(x)))。
  m <- max(x)
  m + log(sum(exp(x - m)))
}

build_roi_observation_list <- function(dt, feature_cols) {  # dt: 被试×ROI 特征表；feature_cols: 进入 MCMC 的列；作用：按 ROI 聚合被试观测矩阵。
  roi_ids <- sort(unique(dt$roi_id))
  obs <- vector("list", length(roi_ids))
  names(obs) <- as.character(roi_ids)
  for (i in seq_along(roi_ids)) {
    x <- as.matrix(dt[roi_id == roi_ids[i], ..feature_cols])
    x <- x[stats::complete.cases(x), , drop = FALSE]
    obs[[i]] <- x
  }
  keep <- lengths(obs) > 0
  list(roi_ids = roi_ids[keep], obs = obs[keep])
}

sample_niw_from_cluster_observations <- function(obs, z, k, mu0, kappa0, nu0, psi0) {  # obs: 每个 ROI 的被试观测列表；z: ROI 标签；k 与其余参数同上。
  d <- length(mu0)
  mu <- matrix(0, k, d)
  sigma <- array(0, dim = c(d, d, k))
  for (cl in seq_len(k)) {
    idx <- which(z == cl)
    xk <- if (length(idx)) do.call(rbind, obs[idx]) else matrix(numeric(0), ncol = d)  # 把属于同一 cluster 的多个 ROI 的被试观测池化。
    nk <- nrow(xk)
    if (nk == 0) {
      kappa_n <- kappa0
      nu_n <- nu0
      mu_n <- mu0
      psi_n <- psi0
    } else {
      xbar <- colMeans(xk)
      centered <- sweep(xk, 2, xbar, "-")
      scatter <- crossprod(centered)
      kappa_n <- kappa0 + nk
      nu_n <- nu0 + nk
      mu_n <- (kappa0 * mu0 + nk * xbar) / kappa_n
      diff <- matrix(xbar - mu0, ncol = 1)
      psi_n <- psi0 + scatter + (kappa0 * nk / kappa_n) * (diff %*% t(diff))
    }
    sigma[, , cl] <- rinvwishart(nu_n, psi_n) + diag(1e-6, d)
    mu[cl, ] <- as.numeric(mvtnorm::rmvnorm(1, mean = mu_n, sigma = sigma[, , cl] / kappa_n))
  }
  list(mu = mu, sigma = sigma)
}

run_roi_mcmc_mixture <- function(subject_features,
                                 feature_cols = c("emotion_generation", "reappraisal_effect", "look_neg_base", "reg_neg_base"),
                                 k = 4,
                                 iter = 1200,
                                 burn = 500,
                                 thin = 5,
                                 alpha = 1,
                                 seed = 20260608) {  # subject_features: 被试×ROI 表；feature_cols: 入模特征；其余是 MCMC 设置。
  set.seed(seed)
  dt <- as.data.table(subject_features)
  scale_info <- standardize_matrix(as.matrix(dt[, ..feature_cols]))  # 先按被试级观测标准化，再进行 ROI 聚类。
  for (j in seq_along(feature_cols)) {
    dt[[feature_cols[j]]] <- scale_info$x[, j]
  }
  built <- build_roi_observation_list(dt, feature_cols)
  roi_ids <- built$roi_ids
  obs <- built$obs
  n_roi <- length(obs)
  d <- length(feature_cols)
  if (n_roi < k) stop("Number of ROI units (", n_roi, ") is smaller than K=", k, ".")

  roi_means <- t(vapply(obs, colMeans, FUN.VALUE = numeric(d)))
  km <- stats::kmeans(roi_means, centers = k, nstart = 25)  # 用 ROI 平均特征初始化标签，降低链起点过差的风险。
  z <- km$cluster
  ref <- km$centers
  mu0 <- rep(0, d)
  kappa0 <- 0.05
  nu0 <- d + 3
  psi0 <- diag(1.5, d)
  posterior <- matrix(0, n_roi, k)
  loglik <- numeric(0)
  pointwise_loglik <- matrix(NA_real_, nrow = 0L, ncol = n_roi)
  z_draws <- matrix(NA_integer_, nrow = 0L, ncol = n_roi)
  kept <- 0L

  for (it in seq_len(iter)) {  # 每轮依次更新 cluster 权重、参数和 ROI 归属。
    counts <- tabulate(z, nbins = k)  # ROI 数量层面的类别计数。
    pi <- rdirichlet1(alpha + counts)  # 更新 cluster 权重。
    pars <- sample_niw_from_cluster_observations(obs, z, k, mu0, kappa0, nu0, psi0)  # 用所有被试观测更新 cluster 参数。
    logp <- matrix(0, n_roi, k)
    for (cl in seq_len(k)) {
      for (r in seq_len(n_roi)) {
        logp[r, cl] <- log(pi[cl] + 1e-15) +
          sum(mvtnorm::dmvnorm(obs[[r]], mean = pars$mu[cl, ], sigma = pars$sigma[, , cl], log = TRUE))  # ROI 的似然是其内部所有被试观测 loglik 之和。
      }
    }
    norm_logp <- logp
    for (r in seq_len(n_roi)) norm_logp[r, ] <- norm_logp[r, ] - max(norm_logp[r, ])
    prob <- exp(norm_logp)
    prob <- prob / rowSums(prob)
    z <- sample_categorical_rows(prob)

    if (it > burn && ((it - burn) %% thin == 0)) {
      za <- align_labels_greedy(pars$mu, z, ref)  # 对齐标签后再累计 posterior，便于跨抽样解释。
      posterior[cbind(seq_len(n_roi), za)] <- posterior[cbind(seq_len(n_roi), za)] + 1
      z_draws <- rbind(z_draws, za)
      kept <- kept + 1L
      pointwise <- apply(logp, 1, log_sum_exp)
      loglik <- c(loglik, sum(pointwise))
      pointwise_loglik <- rbind(pointwise_loglik, pointwise)
    }
    if (it %% 100 == 0) message("ROI MCMC iter ", it, "/", iter)
  }

  post <- posterior / max(kept, 1L)
  list(
    posterior = post,
    cluster = max.col(post),
    roi_ids = roi_ids,
    kept = kept,
    mean_loglik = mean(loglik),
    loglik = loglik,
    pointwise_loglik = pointwise_loglik,
    z_draws = z_draws,
    ref_centers = ref,
    feature_cols = feature_cols,
    center = scale_info$center,
    scale = scale_info$scale
  )
}

posterior_coclustering_matrix <- function(z_draws) {  # z_draws: 每行一次保留抽样、每列一个 ROI 的标签；作用：计算 ROI 两两共聚类概率矩阵。
  z_draws <- as.matrix(z_draws)
  if (!nrow(z_draws) || !ncol(z_draws)) {
    return(matrix(NA_real_, nrow = ncol(z_draws), ncol = ncol(z_draws)))
  }
  n <- ncol(z_draws)
  out <- matrix(0, n, n)
  for (i in seq_len(nrow(z_draws))) {
    z <- z_draws[i, ]
    out <- out + outer(z, z, FUN = "==")
  }
  out / nrow(z_draws)
}

coclustering_metrics <- function(z_draws) {  # z_draws: MCMC 保存的标签抽样；作用：把共聚类矩阵压缩成清晰度/熵/高置信比例指标。
  co <- posterior_coclustering_matrix(z_draws)
  if (!length(co) || all(!is.finite(co))) {
    return(data.table(
      mean_coclustering_entropy = NA_real_,
      coclustering_sharpness = NA_real_,
      confident_pair_fraction = NA_real_
    ))
  }
  pair <- co[upper.tri(co)]
  ent <- -(pair * log(pair + 1e-15) + (1 - pair) * log(1 - pair + 1e-15))
  data.table(
    mean_coclustering_entropy = mean(ent, na.rm = TRUE),
    coclustering_sharpness = mean(abs(pair - 0.5) * 2, na.rm = TRUE),
    confident_pair_fraction = mean(pair <= 0.10 | pair >= 0.90, na.rm = TRUE)
  )
}
