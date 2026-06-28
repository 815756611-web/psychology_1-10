clear  % 原文 Figure 5 的系统平均激活-行为相关脚本；R 没有逐图复做整张散点图版式，只保留简化行为验证表。
load('C:\Users\KeBo\Documents\GitHub\KeBo2023_EmotionReg_BayesFactor\Final_SystemComponentMap_Consensus_AfterClusterControl.mat')  % 载入四系统 after-cluster 共识图索引。

load('C:\Users\KeBo\Dropbox (Dartmouth College)\2021_Ke_Bo_reappraisal_Gianaros\Data\fMRI_data\AHAB_FullData_Meta.mat')  % R 对应 route_A/scripts/01_prepare_behavior.R 和 beta 图镜像。

RegNeg_AHAB=image_math(Whole_Reg,Whole_Neg,'minus')  % Reg-Look 对比；R 对应 reappraisal_effect。
load('C:\Users\KeBo\Dropbox (Dartmouth College)\2021_Ke_Bo_reappraisal_Gianaros\Data\fMRI_data\PIP_FullData_Meta.mat')

RegNeg_PIP=image_math(Whole_Reg,Whole_Neg,'minus')  % PIP 的 Reg-Look 对比。

Reg_rating_AHAB=table2array(RegNeg_AHAB.metadata_table(:,9))
Neg_rating_AHAB=table2array(RegNeg_AHAB.metadata_table(:,10))
Neu_rating_AHAB=table2array(RegNeg_AHAB.metadata_table(:,11))

Reg_rating_PIP=table2array(RegNeg_PIP.metadata_table(:,9))
Neg_rating_PIP=table2array(RegNeg_PIP.metadata_table(:,10))
Neu_rating_PIP=table2array(RegNeg_PIP.metadata_table(:,11))


Success_AHAB=Neg_rating_AHAB-Reg_rating_AHAB;  % R 对应 reg_success。
EmoAct_AHAB=Neg_rating_AHAB-Neu_rating_AHAB;  % R 对应 emotion_reactivity。


EmoAct_PIP=Neg_rating_PIP-Neu_rating_PIP;
Success_PIP=Neg_rating_PIP-Reg_rating_PIP;

Success=[Success_AHAB;Success_PIP];  % 合并两个数据库的行为向量。
Reg_Neg=image_math(RegNeg_AHAB,RegNeg_PIP,'concatenate')  % 合并两个数据库的 Reg-Look 图；R 版只在 cluster/ROI 层做相关摘要，不做这一版 whole-image 拼接散点。


%%%%%%%%%%Univariate%%%%%
[R(1) P(1)]=corr(mean(Reg_Neg.dat(indexOverlap,:),1)',Success);  % Common appraisal 的平均激活与成功相关；R 版近似对应 consensus_behavior_correlations.csv。
[R(2) P(2)]=corr(mean(Reg_Neg.dat(indexReappraisalOnly,:),1)',Success);
[R(3) P(3)]=corr(mean(Reg_Neg.dat(indexReappraisal_D,:),1)',Success);
[R(4) P(4)]=corr(mean(Reg_Neg.dat(indexLookOnly,:),1)',Success);

%%%%%%%

figure
dotcolor1=[0.3010 0.7450 0.9330];
dotcolor2=[124 141 204]/256;
dotsize=500;
linewidth=3;
fontsize=12;
subplot(1,4,2)
scatter(mean(Reg_Neg.dat(indexOverlap,:),1)',Success,dotsize,dotcolor1,'.');
h=lsline
hold on
scatter(mean(Reg_Neg.dat(indexOverlap,183:end),1)',Success(183:end),dotsize,dotcolor2,'.');

set(h(1),'color','#FF594C','linewidth',linewidth)

set(gca,'linewidth',linewidth,'fontsize',fontsize,'Fontweight','bold')

ylabel('Reappraisal Success')
h = xlabel('Map Activation (Beta value)','FontSize',fontsize,'Fontweight','bold');
get(h)
h = ylabel('Reappraisal Success','FontSize',fontsize,'Fontweight','bold');
get(h)

title('Common Appraisal')
%%%%%%%%%%%%%%%%%
subplot(1,4,1)
scatter(mean(Reg_Neg.dat(indexReappraisalOnly,:),1)',Success,dotsize,dotcolor,'.');
h=lsline
hold on
scatter(mean(Reg_Neg.dat(indexReappraisalOnly,183:end),1)',Success(183:end),dotsize,dotcolor2,'.');

set(h(1),'color','r','linewidth',linewidth)

set(gca,'linewidth',linewidth,'fontsize',fontsize,'Fontweight','bold')

ylabel('Reappraisal Success')
h = xlabel('Map Activation (Beta value)','FontSize',fontsize,'Fontweight','bold');
get(h)
h = ylabel('Reappraisal Success','FontSize',fontsize,'Fontweight','bold');
get(h)

title('Reappraisal Only')
%%%%%%%

subplot(1,4,3)
scatter(mean(Reg_Neg.dat(indexLookOnly,:),1)',Success,dotsize,dotcolor,'.');
h=lsline
hold on
scatter(mean(Reg_Neg.dat(indexLookOnly,183:end),1)',Success(183:end),dotsize,dotcolor2,'.');

set(h(1),'color','#FF594C','linewidth',linewidth)

set(gca,'linewidth',linewidth,'fontsize',fontsize,'Fontweight','bold')

ylabel('Reappraisal Success')
h = xlabel('Map Activation (Beta value)','FontSize',fontsize,'Fontweight','bold');
get(h)
h = ylabel('Reappraisal Success','FontSize',fontsize,'Fontweight','bold');
get(h)

title('Un-Modifiable Emotion generation')

%%%%%%%%%%%%%%%%
subplot(1,4,4)

scatter(mean(Reg_Neg.dat(indexReappraisal_D,:),1)',Success,dotsize,dotcolor,'.');
h=lsline
hold on
scatter(mean(Reg_Neg.dat(indexReappraisal_D,183:end),1)',Success(183:end),dotsize,dotcolor2,'.');

set(h(1),'color','#FF594C','linewidth',linewidth)

set(gca,'linewidth',linewidth,'fontsize',fontsize,'Fontweight','bold')

ylabel('Reappraisal Success')
h = xlabel('Map Activation (Beta value)','FontSize',fontsize,'Fontweight','bold');
get(h)
h = ylabel('Reappraisal Success','FontSize',fontsize,'Fontweight','bold');
get(h)

title('Modifiable Emotion generation')
