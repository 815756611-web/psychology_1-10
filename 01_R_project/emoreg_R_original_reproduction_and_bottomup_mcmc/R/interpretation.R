# 解释层：汇总空间重叠、cluster 标签和跨样本一致性。 
dice_jaccard <- function(a, b) {  # a/b: 两个二值体素掩模；作用：计算 Dice/Jaccard 与交并体素数。
  a <- as.logical(a)
  b <- as.logical(b)
  inter <- sum(a & b, na.rm = TRUE)
  na <- sum(a, na.rm = TRUE)
  nb <- sum(b, na.rm = TRUE)
  union <- sum(a | b, na.rm = TRUE)
  data.table(
    dice = ifelse(na + nb > 0, 2 * inter / (na + nb), 0),
    jaccard = ifelse(union > 0, inter / union, 0),
    intersection_voxels = inter,
    a_voxels = na,
    b_voxels = nb
  )
}

role_from_effects <- function(emotion_mean, reappraisal_mean, emotion_bf, reappraisal_bf, threshold = 4.6) {  # 前四个参数分别是情绪生成/重评均值与 BF；threshold: 2logBF 阈值。
  emotion_evidence <- is.finite(emotion_bf) && emotion_bf > threshold && emotion_mean > 0
  reappraisal_evidence <- is.finite(reappraisal_bf) && reappraisal_bf > threshold
  reappraisal_null_evidence <- is.finite(reappraisal_bf) && reappraisal_bf < -threshold

  if (!emotion_evidence && reappraisal_evidence && reappraisal_mean > 0) {
    return("Reappraisal-only-like: regulation/reappraisal engagement with little emotion-generation evidence")
  }
  if (emotion_evidence && reappraisal_evidence && reappraisal_mean > 0) {
    return("Common-appraisal-like: shared positive emotion-generation and reappraisal response")
  }
  if (emotion_evidence && reappraisal_null_evidence) {
    return("Non-modifiable-emotion-like: emotion generation with evidence against reappraisal modulation")
  }
  if (emotion_evidence && reappraisal_evidence && reappraisal_mean < 0) {
    return("Modifiable-emotion-like: emotion generation down-regulated during reappraisal")
  }
  "Gradient/other: not cleanly captured by the four original logical systems"
}

adjusted_rand_index <- function(x, y) {  # x/y: 两个聚类分配向量；作用：比较 AHAB/PIP 或 seed 间聚类一致性。
  x <- as.factor(x)
  y <- as.factor(y)
  tab <- table(x, y)
  choose2 <- function(z) z * (z - 1) / 2
  sum_comb <- sum(choose2(tab))
  row_comb <- sum(choose2(rowSums(tab)))
  col_comb <- sum(choose2(colSums(tab)))
  total_comb <- choose2(sum(tab))
  expected <- row_comb * col_comb / total_comb
  max_index <- (row_comb + col_comb) / 2
  denom <- max_index - expected
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  (sum_comb - expected) / denom
}

cluster_label_from_profile <- function(emotion_mean, reappraisal_mean, emotion_bf, reappraisal_bf,
                                       look_neg_mean = NA_real_, reg_neg_mean = NA_real_,
                                       threshold = 4.6) {  # 额外基线均值用于区分 mixed 类型。
  role <- role_from_effects(emotion_mean, reappraisal_mean, emotion_bf, reappraisal_bf, threshold)
  if (startsWith(role, "Reappraisal-only-like")) return("reappraisal-selective-like")
  if (startsWith(role, "Common-appraisal-like")) return("common-appraisal-like")
  if (startsWith(role, "Non-modifiable-emotion-like")) return("non-modifiable-emotion-like")
  if (startsWith(role, "Modifiable-emotion-like")) return("modifiable-emotion-like")
  if (is.finite(reappraisal_mean) && reappraisal_mean > 0 && is.finite(reg_neg_mean) && reg_neg_mean > 0) {
    return("reappraisal-positive-mixed")
  }
  if (is.finite(emotion_mean) && emotion_mean > 0 && is.finite(reappraisal_mean) && abs(reappraisal_mean) < abs(emotion_mean) / 3) {
    return("emotion-generation-stable-mixed")
  }
  "gradient-or-uncertain"
}
