# 原文复现与路线 B 贝叶斯 MCMC 聚类说明

项目位置：

当前副本项目根目录：`emoreg_R_original_reproduction_and_bottomup_mcmc`

这个项目现在按文档中的两条路线组织：

- 路线 A：复现原论文的 Bayes factor systems identification。
- 路线 B：基于原文系统图/ROI 连通簇的贝叶斯混合聚类扩展。

## 运行入口

进入项目根目录后，在 VS Code 的 R 控制台中运行：

```r
source("run_smoke.R")
```

正式全量运行：

```r
source("run_full.R")
```

`run_full.R` 不会自动连续运行 A+B。它会先询问运行路线 A 还是路线 B，并询问是否清理所选路线的历史 `outputs`。

full 模式一旦选择某条路线，该路线会固定运行 AHAB 和 PIP 两个数据库：

- 路线 A：分别跑 AHAB 和 PIP 后，使用空间相似性 product map 方法生成二值共识图，并输出 AHAB/PIP 的空间重叠比较。
- 路线 B：分别跑 AHAB 和 PIP 的 ROI-MCMC 聚类后，使用聚类模型评价指标比较两套结果，包括 selected K、AIC、BIC、WAIC、LOOIC、membership entropy、ARI/NMI、cluster Dice/Jaccard，并生成基于共享 ROI 归类的二值共识图。

也可以只运行路线 B：

```r
source("route_B_bottomup_mcmc/run_smoke_bottomup.R")
source("route_B_bottomup_mcmc/run_full_bottomup.R")
```

## 数据读取位置

程序统一从项目内部相对路径读取数据：

- 一阶 beta 图：`data/raw/beta_maps`
- 行为数据：`data/raw/behavior`
- 原文四类系统图：`data/raw/system_maps`
- PET / 神经递质图：`data/raw/pet_maps`

这些数据来自此前下载的资料目录：

副本 bundle 内的相对来源目录：`../../02_source_materials/A_systems_identification_emotion_regulation`

`scripts/00_setup_relative_data.R` 会把原始资料整理进 `data/raw`。后续脚本只依赖本项目内部路径。

## R 包与分析方式

使用的主要 R 包：

- `data.table`：快速整理 beta 图索引、行为表和输出表。
- `RNifti`：读取和写出 `.nii.gz` 脑影像文件。
- `readxl`：读取可能存在的 Excel 行为/元数据表。
- `R.matlab`：读取 MATLAB `.mat` 文件，主要用于兼容原始资料。
- `mmand`：三维连通簇标记和簇大小控制。
- `mvtnorm`：路线 B 贝叶斯高斯混合模型中的多元正态计算。
- `loo`：路线 B 中基于 pointwise log-likelihood 计算 WAIC 和 PSIS-LOO/LOOIC。
- `coda`：MCMC 链诊断相关工具。
- `matrixStats`：矩阵统计辅助计算。

总体分析方式：

- 路线 A 是原文复现：读取一阶 beta 图，在体素水平计算 contrast、t 图和 Bayes factor 图，再按原文规则生成四类系统 mask。
- 路线 B 是探索性扩展：使用原文系统图中的 ROI 连通簇作为分析单位，把全部 ROI 放入同一个贝叶斯高斯混合模型，用 MCMC 估计潜在 cluster。
- `smoke` 只检查程序路径和模型流程，不作为正式科研结果；`full` 才使用全部被试做正式分析。

## 路线 A：原文复现

位置：`route_A_original_reproduction`

主要脚本：

- `scripts/01_prepare_behavior.R`：整理行为评分，生成 `data/processed/behavior_clean.csv`。
- `scripts/02_original_reproduction.R`：复现原文 MATLAB 主分析，生成 t 图、Bayes factor 图和四类系统 mask。
- `scripts/03_consensus_and_behavior.R`：生成 AHAB/PIP 共识图，并做行为相关。
- `scripts/04_preview_original_outputs.R`：为 NIfTI 输出生成 PNG 预览和文件摘要。

路线 A 的核心逻辑：

1. 每个被试读取三个条件 beta 图：`Look neutral`、`Look negative`、`Regulate negative`。
2. 构造两个 contrast：
   - 情绪生成：`Look negative - Look neutral`
   - 重评效应：`Regulate negative - Look negative`
3. 对每个体素做组水平单样本 t 检验。
4. 将 t 图转为 `2 * log(BF10)`。
5. 按原文四套 BF/t 规则生成四类系统：
   - `reappraisal_only`
   - `common_appraisal`
   - `non_modifiable_emotion`
   - `modifiable_emotion`
6. 对系统 mask 做空间连通簇大小控制，默认保留至少 15 个体素的簇。
7. 在 full 模式中分别跑 AHAB 和 PIP，再生成共识图和行为验证表。

主要输出：

- `route_A_original_reproduction/outputs/nifti`：t 图、BF 图、四类系统图、共识图。
- `route_A_original_reproduction/outputs/tables`：系统规则、体素数、共识图、行为相关。
- `route_A_original_reproduction/outputs/tables/AHAB_PIP_product_map_consensus.md`：AHAB/PIP 在四个原文系统上的 product map 二值共识图比较说明。
- `route_A_original_reproduction/outputs/preview`：NIfTI 摘要、MRIcroGL 建议、PNG 预览。
- `route_A_original_reproduction/outputs/systems`：按原文四个系统整理后的结果。每个系统一个文件夹，内部包含非空系统图、互斥 ROI 连通簇、简短 `README.md` 和支撑表。已经分到前面系统的 ROI 不会再复制到后面的系统里。

## 路线 B：贝叶斯 MCMC 聚类

位置：`route_B_bottomup_mcmc`

路线 B 现在使用原文 ROI 作为分析单位：

- `smoke` 测试：使用原文系统图中的 ROI 连通簇，取少量被试快速检查路径和模型流程。
- `full` 正式分析：仍然使用原文 ROI 连通簇，但使用全部被试。

注意：路线 B 建模时不按原文四类系统分别分析，也不把系统标签作为分组变量。所有 ROI 作为一个总观察池进入同一个贝叶斯混合模型。`source_system` 只保留在 `roi_table` 和 `mcmc_posterior` 中，作为必要时追溯 ROI 来源的元数据。

主要脚本：

- `scripts/04_prepare_bottomup_features.R`：从原文系统图生成 ROI 连通簇，并提取每个被试×ROI 的四个特征。
- `scripts/05_bottomup_mcmc.R`：把全部 ROI 放入同一个贝叶斯高斯混合模型做 MCMC 聚类。
- `scripts/06_interpret_mcmc_regions.R`：生成 cluster 画像、分类不确定性、ROI 来源元数据和行为验证。

每个被试、每个分析单位提取四个值：

- `emotion_generation`：`Look negative - Look neutral`
- `reappraisal_effect`：`Regulate negative - Look negative`
- `look_neg_base`：`Look negative` 单条件 beta
- `reg_neg_base`：`Regulate negative` 单条件 beta

smoke 模式固定 `K = 4`，用于快速检查 ROI 版本是否能运行。full 模式默认尝试 `K = 2:6`，并输出 AIC、BIC、WAIC、PSIS-LOO/LOOIC、log-likelihood、分类熵、后验归属稳定性、共聚类结构和解释性评分用于比较。

主要输出：

- `route_B_bottomup_mcmc/outputs/tables/roi_subject_features_*.csv`：被试×ROI 的四个特征。
- `route_B_bottomup_mcmc/outputs/tables/roi_table_*.csv`：每个原文 ROI 连通簇的位置、体素数和来源信息。
- `route_B_bottomup_mcmc/outputs/tables/roi_group_summary_*.csv`：每个 ROI 的组水平效应摘要。
- `route_B_bottomup_mcmc/outputs/tables/mcmc_posterior_*.csv`：每个分析单位属于各 cluster 的后验概率。
- `route_B_bottomup_mcmc/outputs/tables/mcmc_cluster_profile_*.csv`：每个 cluster 的效应画像和解释标签。
- `route_B_bottomup_mcmc/outputs/tables/mcmc_roi_pool_metadata_*.csv`：ROI 总观察池记录；不按系统分组。
- `route_B_bottomup_mcmc/outputs/tables/mcmc_cluster_behavior_correlations_*.csv`：cluster 活动与行为指标的相关。
- `route_B_bottomup_mcmc/outputs/nifti/mcmc_cluster_*.nii.gz`：每个 Bayesian cluster 的脑图。
- `route_B_bottomup_mcmc/outputs/by_k`：按 tag 和 K 值整理后的聚类结果。每个 `K_XX` 文件夹包含模型支撑信息、cluster 画像、非空 cluster 文件夹和对应脑区数据；空 cluster 只记录在 `empty_clusters.csv`，不生成空白脑图。
- `route_B_bottomup_mcmc/outputs/dataset_comparison/AHAB_PIP`：AHAB 与 PIP 的聚类模型比较、cluster 匹配表和二值共识脑图。

关于 K 值的解释：路线 B 中的 `K=5` 表示模型允许 5 个潜在成分，但如果某些成分没有任何 ROI 以最大后验概率归入，就会成为空 cluster。输出文件夹只展开非空 cluster，所以 K=5 目录里可能只看到 2 个 cluster；空的 3 个会写在该 K 文件夹的 `empty_clusters.csv`。

## 输出清理与压缩

根目录有一个管理脚本：`manage_outputs.R`。

查看可用操作：

```r
system2(file.path(R.home("bin"), "Rscript"), c("manage_outputs.R", "--action", "list"))
```

压缩当前分析脚本和输出：

```r
system2(file.path(R.home("bin"), "Rscript"), c("manage_outputs.R", "--action", "zip", "--name", "smoke_results"))
```

清理自动整理出来的目录：

```r
system2(file.path(R.home("bin"), "Rscript"), c("manage_outputs.R", "--action", "clean-organized"))
```

`clean-organized` 只删除 `route_A_original_reproduction/outputs/systems` 和 `route_B_bottomup_mcmc/outputs/by_k`，不会删除原始数据和基础分析输出。

full 入口也有清理功能：运行 `run_full.R` 时回答 `Y`，会在重新运行前删除所选路线的整个 `outputs` 文件夹，然后重新生成结果。

## smoke 与 full 的区别

`smoke` 是路径和逻辑检查，不是正式结果：

- 路线 A 从 PIP 中随机不放回抽取 8 名被试。
- 路线 B 使用原文 ROI 连通簇，并在每个数据集内随机不放回抽取 4 名被试，即 AHAB 4 名 + PIP 4 名。
- 随机种子来自 `config/default.yml` 中的 `random_seed: 20260608`，所以每次 smoke 抽到的样本可复现。
- 抽中的被试会写入 `selected_subjects_smoke.csv`，分别位于路线 A 和路线 B 的 `outputs/tables` 文件夹。
- MCMC 迭代较少，固定 `K = 4`。

`full` 是正式分析：

- 路线 A 使用全部已下载 beta 图，分别跑 AHAB 和 PIP，并生成 product map 二值共识图。
- 路线 B 使用全部被试，分别跑 AHAB 和 PIP，并以原文 ROI 连通簇作为主要单位。
- 路线 B 默认比较 `K = 2:6`，再基于 `overall_recommended_K` 输出解释结果和 AHAB/PIP 聚类一致性评价；推荐逻辑综合模型表现、归属稳定性、共聚类结构和可解释性，不只依赖最低 BIC。

## 注意事项

- 路线 A 是原文复现，四类系统由理论规则决定，不是机器学习聚类。
- 路线 A 中的 cluster control 只是空间连通簇大小控制，不是 MCMC 聚类。
- 路线 B 是探索性扩展。它使用原文 ROI，但聚类时不按原文系统分开，而是把全部 ROI 放入同一个模型中估计潜在类别。
- R 版默认使用可运行性更好的 BF 近似实现，逻辑与原文一致，但逐体素 BF 数值不保证与 MATLAB/CANlab 完全逐点相同。
- `.nii.gz` 是压缩 NIfTI 脑影像文件，不是普通压缩包。推荐先看 `outputs/preview/png`，再用 MRIcroGL 打开非空 NIfTI。
