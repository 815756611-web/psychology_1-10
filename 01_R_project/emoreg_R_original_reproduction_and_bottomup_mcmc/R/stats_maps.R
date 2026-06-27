# 逐体素统计层：只依赖 base R + 本项目 NIfTI 工具；对应 MATLAB 主脚本里 image_math + ttest 生成 t 图。 
compute_t_from_stack <- function(paths_or_vectors, transform = identity) {  # paths_or_vectors: 被试图路径或向量列表；transform: 每个被试读入后的变换函数。
  n <- 0L
  sums <- NULL
  sums_sq <- NULL
  for (item in paths_or_vectors) {
    x <- if (is.character(item)) read_nifti_vec(item) else as.numeric(item)  # 支持从 NIfTI 路径或现成向量两种入口。
    x <- transform(x)  # 允许外部先做差值、筛选或重编码。
    x <- safe_vec(x)  # 把异常值清成 0，避免某个体素污染整列统计。
    if (is.null(sums)) {
      sums <- numeric(length(x))
      sums_sq <- numeric(length(x))
    }
    sums <- sums + x
    sums_sq <- sums_sq + x^2
    n <- n + 1L
  }
  mean <- sums / n  # 逐体素均值。
  var <- (sums_sq - sums^2 / n) / max(n - 1L, 1L)  # 逐体素无偏样本方差。
  sd <- sqrt(pmax(var, 1e-12))  # 给极小方差加下界，避免除以 0。
  t <- mean / (sd / sqrt(n))  # 单样本 t；与 MATLAB 的 ttest(RegMinusNeg) / ttest(NegMinusNeu) 对应。
  list(n = n, mean = mean, sd = sd, t = t)
}

compute_contrast_stats <- function(wide, contrast = c("emotion_generation", "reappraisal_effect")) {  # wide: 三条件宽表；contrast: 选择 Neg-Neu 或 Reg-Neg 对比。
  contrast <- match.arg(contrast)
  n <- 0L
  sums <- NULL
  sums_sq <- NULL
  for (i in seq_len(nrow(wide))) {
    neu <- read_nifti_vec(wide$LookNeu[i])  # Look neutral beta。
    neg <- read_nifti_vec(wide$LookNeg[i])  # Look negative beta。
    reg <- read_nifti_vec(wide$RegNeg[i])  # Regulate negative beta。
    x <- switch(
      contrast,  # MATLAB 对应：NegMinusNeu = Whole_Neg - Whole_Neu；RegMinusNeg = Whole_Reg - Whole_Neg。
      emotion_generation = neg - neu,
      reappraisal_effect = reg - neg
    )
    x <- safe_vec(x)
    if (is.null(sums)) {
      sums <- numeric(length(x))
      sums_sq <- numeric(length(x))
    }
    sums <- sums + x
    sums_sq <- sums_sq + x^2
    n <- n + 1L
  }
  mean <- sums / n
  var <- (sums_sq - sums^2 / n) / max(n - 1L, 1L)
  sd <- sqrt(pmax(var, 1e-12))
  t <- mean / (sd / sqrt(n))  # 输出结构与 MATLAB t 图一致，但 R 这里显式保存 mean/sd 供后续 ROI 汇总重用。
  list(n = n, mean = mean, sd = sd, t = t)
}

compute_single_condition_t <- function(paths) {  # paths: 单一条件的一组 beta 图路径；作用：生成 LookNeg 或 RegNeg 的单条件 t 图。
  compute_t_from_stack(as.list(paths))
}

contrast_subject_vectors <- function(wide, i) {  # wide: 宽表；i: 第 i 个被试；作用：提取一个被试的所有核心对比向量供 ROI/MCMC 层复用。
  neu <- read_nifti_vec(wide$LookNeu[i])
  neg <- read_nifti_vec(wide$LookNeg[i])
  reg <- read_nifti_vec(wide$RegNeg[i])
  list(
    emotion_generation = safe_vec(neg - neu),  # 对应 MATLAB NegMinusNeu。
    reappraisal_effect = safe_vec(reg - neg),  # 对应 MATLAB RegMinusNeg。
    look_neu = safe_vec(neu),  # 原始中性观看条件。
    look_neg = safe_vec(neg),  # 原始负性观看条件。
    reg_neg = safe_vec(reg)  # 原始重评负性条件。
  )
}
