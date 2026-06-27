# Bayes factor 层：对应 MATLAB 中 estimateBayesFactor(t,'t')；R 版额外保留 BIC 近似与精确 JZS 两种实现。 
bf10_bic_from_t <- function(t, n) {  # t: t 统计量；n: 样本量；作用：把 t 值转成 BF10 的 BIC 近似，供快速检查或旧版 Route B 使用。
  df <- max(n - 1, 1)
  r2 <- t^2 / (t^2 + df)
  r2 <- pmin(pmax(r2, 0), 1 - 1e-12)
  log_bf10 <- (-n * log1p(-r2) - log(n)) / 2
  log_bf10 <- pmax(pmin(log_bf10, 50), -50)
  exp(log_bf10)
}

two_log_bf_bic <- function(t, n, signed = TRUE) {  # t: t 统计量；n: 样本量；signed: 是否保留 t 方向符号；作用：输出论文风格的 2*log(BF)。
  val <- 2 * log(bf10_bic_from_t(t, n))
  if (signed) val <- sign(t) * val
  val[!is.finite(val)] <- 0
  val
}

.bf_rule_cache <- new.env(parent = emptyenv())

jzs_rscale_value <- function(rscale) {  # rscale: "medium"/"wide"/数值；作用：把 JZS 先验尺度名字转成具体 Cauchy 宽度。
  if (is.numeric(rscale)) return(as.numeric(rscale)[1])
  switch(
    as.character(rscale)[1],
    medium = sqrt(2) / 2,
    wide = 1,
    ultrawide = sqrt(2),
    stop("Unknown JZS rscale: ", rscale, call. = FALSE)
  )
}

jzs_laguerre_rule <- function(order = 512, alpha = -0.5) {  # order: 求积节点数；alpha: 广义 Laguerre 参数；作用：缓存 JZS 积分的数值求积规则。
  key <- paste(order, alpha, sep = "_")
  if (exists(key, envir = .bf_rule_cache, inherits = FALSE)) {
    return(get(key, envir = .bf_rule_cache, inherits = FALSE))
  }
  i <- seq_len(order)
  diag_vals <- 2 * i - 1 + alpha
  off_vals <- sqrt(seq_len(order - 1) * (seq_len(order - 1) + alpha))
  jacobi <- diag(diag_vals, order)
  jacobi[cbind(seq_len(order - 1), 2:order)] <- off_vals
  jacobi[cbind(2:order, seq_len(order - 1))] <- off_vals
  eig <- eigen(jacobi, symmetric = TRUE)
  ord <- order(eig$values)
  rule <- list(
    nodes = eig$values[ord],
    weights = gamma(alpha + 1) * eig$vectors[1, ord]^2
  )
  assign(key, rule, envir = .bf_rule_cache)
  rule
}

jzs_log_bf10_scalar_adaptive <- function(t_abs, n, r) {  # t_abs: |t|；n: 样本量；r: JZS 先验尺度；作用：对少数数值不稳体素走 adaptive integrate 兜底。
  nu <- n - 1
  power <- (nu + 1) / 2
  t2 <- t_abs^2
  log_den <- -power * log1p(t2 / nu)
  integrand <- function(g) {
    a <- 1 + n * g * r^2
    a^(-0.5) *
      exp(-power * log1p(t2 / (nu * a))) *
      (2 * pi)^(-0.5) * g^(-1.5) * exp(-1 / (2 * g))
  }
  integral <- integrate(integrand, lower = 0, upper = Inf, rel.tol = 1e-8)$value
  log(integral) - log_den
}

two_log_bf_jzs_exact <- function(t, n, rscale = "medium", signed = TRUE, quadrature_order = 512) {  # t: t 统计量；n: 样本量；rscale: JZS 先验尺度；signed: 是否带方向；quadrature_order: 求积精度。
  r <- jzs_rscale_value(rscale)
  out <- numeric(length(t))
  ok <- is.finite(t)
  if (!any(ok)) return(out)

  t_abs <- abs(t[ok])
  nu <- n - 1
  power <- (nu + 1) / 2
  t2 <- t_abs^2
  log_den <- -power * log1p(t2 / nu)

  rule <- jzs_laguerre_rule(order = quadrature_order, alpha = -0.5)  # 这里实现 MATLAB estimateBayesFactor 背后的 JZS 一维积分。
  integral <- numeric(length(t_abs))
  for (k in seq_along(rule$nodes)) {
    g <- 1 / (2 * rule$nodes[k])
    a <- 1 + n * g * r^2
    integral <- integral +
      (rule$weights[k] / sqrt(pi)) *
        a^(-0.5) *
        exp(-power * log1p(t2 / (nu * a)))
  }

  log_bf <- log(integral) - log_den
  bad <- !is.finite(log_bf)
  if (any(bad)) {
    log_bf[bad] <- vapply(t_abs[bad], jzs_log_bf10_scalar_adaptive, numeric(1), n = n, r = r)
  }
  log_bf <- pmax(pmin(log_bf, 50), -50)
  val <- 2 * log_bf
  if (signed) val <- sign(t[ok]) * val
  out[ok] <- val
  out
}

t_to_two_log_bf <- function(t, n, method = c("bic_approx", "jzs_exact"), signed = TRUE) {  # t: t 统计量；n: 样本量；method: 近似或精确 JZS；signed: 是否保留方向。
  method <- match.arg(method)
  if (method == "bic_approx") return(two_log_bf_bic(t, n, signed = signed))
  two_log_bf_jzs_exact(t, n, signed = signed)
}
