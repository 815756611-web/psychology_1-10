# 行为预处理：对应 MATLAB Figure 5 行为相关分析前的人评分抽取，但 R 版先统一清洗成可复用 CSV/RDS。 
source("R/cli.R")  # CLI/工作目录定位。
set_project_workdir()  # 切到项目根目录。
source("R/project.R")  # 公共路径和 data.table 依赖。

behavior_dir <- project_path("data", "raw", "behavior")  # 行为元数据镜像目录。
csv_files <- list.files(behavior_dir, pattern = "MetaInfo_IDoff_full\\.csv$", recursive = TRUE, full.names = TRUE)
xlsx_files <- list.files(behavior_dir, pattern = "MetaInfo_IDoff.*\\.xlsx$", recursive = TRUE, full.names = TRUE)
if (length(csv_files)) {
  raw <- fread(csv_files[1])  # data.table::fread：优先读 CSV。
} else {
  if (!length(xlsx_files)) stop("MetaInfo_IDoff data was not found under ", behavior_dir)
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is required to read the Excel fallback.", call. = FALSE)  # readxl：Excel 兜底读取。
  raw <- as.data.table(readxl::read_excel(xlsx_files[1]))  # 与 MATLAB 的 table2array/readtable 用途相近。
}
setnames(raw, make.names(names(raw)))

needed <- c("SubID", "Sample", "RegNeg_Rating", "LookNeg_Rating", "LookNeuRating")
missing <- setdiff(needed, names(raw))
if (length(missing)) stop("Behavior file is missing columns: ", paste(missing, collapse = ", "))

dt <- copy(raw)
dt[, dataset := as.character(Sample)]  # 数据库标签：AHAB/PIP。
dt[, subject := as.integer(SubID)]  # 被试编号。
dt[, reg_success := as.numeric(LookNeg_Rating) - as.numeric(RegNeg_Rating)]  # 对应 MATLAB Success = Neg_rating - Reg_rating。
dt[, emotion_reactivity := as.numeric(LookNeg_Rating) - as.numeric(LookNeuRating)]  # 对应 MATLAB EmoAct = Neg_rating - Neu_rating。
dt[, reg_rating := as.numeric(RegNeg_Rating)]  # 重评评分。
dt[, look_neg_rating := as.numeric(LookNeg_Rating)]  # 负性观看评分。
dt[, look_neu_rating := as.numeric(LookNeuRating)]  # 中性观看评分。

cols <- c(
  "dataset", "subject", "Age", "Sex", "ERQReappraisal", "ERQSuppression",
  "strategy1_reappraise_yn_final", "strategy1_best_fit_final",
  "reg_rating", "look_neg_rating", "look_neu_rating",
  "reg_success", "emotion_reactivity"
)
cols <- intersect(cols, names(dt))
out <- dt[, ..cols]
setorder(out, dataset, subject)

out_csv <- project_path("data", "processed", "behavior_clean.csv")  # 给后续脚本直接 fread。
out_rds <- project_path("data", "processed", "behavior_clean.rds")  # 给后续脚本直接 readRDS。
fwrite(out, out_csv)
saveRDS(out, out_rds)

summary <- out[, .(
  n = .N,
  mean_reg_success = mean(reg_success, na.rm = TRUE),
  mean_emotion_reactivity = mean(emotion_reactivity, na.rm = TRUE)
), by = dataset]
fwrite(summary, project_path("data", "processed", "behavior_summary.csv"))

cat("Behavior data prepared:\n")
cat("  ", out_csv, "\n", sep = "")
cat("  ", out_rds, "\n", sep = "")
