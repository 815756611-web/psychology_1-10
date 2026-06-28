clear  % 原文共识图脚本：平滑 PIP/AHAB 的 after-cluster 图并做 product map；R 对应 route_A_original_reproduction/scripts/03_consensus_and_behavior.R。
load('C:\Users\KeBo\Documents\GitHub\KeBo2023_EmotionReg_BayesFactor\SystemComponents_Study1_AfterClusterControl.mat')   %%%%Load cluster-controlled BF map from PIP；R 版从 *_after_cluster_PIP.nii.gz 读取。
PIP_LookOnly=indexLookOnly;
PIP_ReappraisalOnly=indexReappraisalOnly;
PIP_Overlap=indexOverlap;
PIP_Target=indexReappraisal_D;
BF_tstat1_PIP=CommonAppraisal_CC;
BF_tstat2_PIP=ReappraisalOnly_CC
BF_tstat3_PIP=NonModifiableEmo_CC;
BF_tstat4_PIP=ModifiableEmo_CC;


load('C:\Users\KeBo\Documents\GitHub\KeBo2023_EmotionReg_BayesFactor\SystemComponents_Study2_AfterClusterControl.mat')   %%%%Load cluster-controlled BF map from AHAB；R 版从 *_after_cluster_AHAB.nii.gz 读取。
AHAB_LookOnly=indexLookOnly;
AHAB_ReappraisalOnly=indexReappraisalOnly;
AHAB_Overlap=indexOverlap;
AHAB_Target=indexReappraisal_D;
BF_tstat1_AHAB=CommonAppraisal_CC;
BF_tstat2_AHAB=ReappraisalOnly_CC;
BF_tstat3_AHAB=NonModifiableEmo_CC;
BF_tstat4_AHAB=ModifiableEmo_CC;


C1 = intersect( PIP_LookOnly,AHAB_LookOnly )  % 先看索引交集；R 版正式报告更强调 Dice/Jaccard/coverage。
C2 = intersect( PIP_ReappraisalOnly,AHAB_ReappraisalOnly )
C3 = intersect( PIP_Overlap,AHAB_Overlap )
C4 = intersect( PIP_Target,AHAB_Target )

Bar=0.01; %%% product-map 阈值；R 对应 CFG$consensus_product_threshold。
%%%%%%%%%%%%%%%%%%%%%%%
obj1 = preprocess(BF_tstat1_PIP, 'smooth', 3)  % R 对应 smooth_array_3d(..., fwhm=3)。
obj2 = preprocess(BF_tstat1_AHAB, 'smooth', 3)

% obj3 = image_math(obj1, obj2,'plus' )  %%%Negative vs Neutral


obj3 =obj1;
obj3.dat=obj1.dat.*obj2.dat;  % R 对应 product <- s1 * s2。

% index=find(obj3.dat<0.01 & obj3.dat>0)
% obj3.dat(index)
orthviews(obj3)
%%%%%%
% There are eight possibility after adding two smoothed map
% 
%
%

%%%%%

% orthviews(NegMinusNeu)

% BF_tstat1 = threshold(obj3, [-0.1 0.5],'raw-outside')
indexOverlap=find(obj3.dat>Bar);  % R 对应 consensus <- product > 0.01。
A=zeros(242868,1);
A(indexOverlap)=1;
CommonAppraisal_CC.dat=A;  % R 版会把共识图直接写成 *_consensus_AHAB_PIP.nii.gz。
orthviews(CommonAppraisal_CC)
%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%
obj1 = preprocess(BF_tstat2_PIP, 'smooth', 3)
obj2 = preprocess(BF_tstat2_AHAB, 'smooth', 3)

% obj3 = image_math(obj1, obj2,'plus' )  %%%Negative vs Neutral

obj3 =obj1;
obj3.dat=obj1.dat.*obj2.dat;

figure
montage(obj3)
orthviews(obj3)

% BF_tstat2 = threshold(obj3, [-0.1 0.5],'raw-outside')
indexReappraisalOnly=find(obj3.dat>Bar);
A=zeros(242868,1);
A(indexReappraisalOnly)=1;
ReappraisalOnly_CC.dat=A;
orthviews(ReappraisalOnly_CC)
%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%
obj1 = preprocess(BF_tstat3_PIP, 'smooth', 3)
obj2 = preprocess(BF_tstat3_AHAB, 'smooth', 3)

% obj3 = image_math(obj1, obj2,'plus' )  %%%Negative vs Neutral

obj3 =obj1;
obj3.dat=obj1.dat.*obj2.dat;

% orthviews(NegMinusNeu)

% BF_tstat3 = threshold(obj3, [-0.1 0.5],'raw-outside')
indexLookOnly=find(obj3.dat>Bar);
A=zeros(242868,1);
A(indexLookOnly)=1;
NonModifiableEmo_CC.dat=A;
%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%
obj1 = preprocess(BF_tstat4_PIP, 'smooth', 3)
obj2 = preprocess(BF_tstat4_AHAB, 'smooth', 3)

% obj3 = image_math(obj1, obj2,'plus' )  %%%Negative vs Neutral

obj3 =obj1;
obj3.dat=obj1.dat.*obj2.dat;
% orthviews(NegMinusNeu)

% BF_tstat4 = threshold(obj3, [-0.1 0.5],'raw-outside')
indexReappraisal_D=find(obj3.dat>Bar);
A=zeros(242868,1);
A(indexReappraisal_D)=1;
ModifiableEmo_CC.dat=A;
%%%%%%%%%%%%%%%%%%%


figure
montage(ModifiableEmo_CC)

BF_tstat1_CC=CommonAppraisal_CC;
BF_tstat2_CC=ReappraisalOnly_CC;
BF_tstat3_CC=NonModifiableEmo_CC;
BF_tstat4_CC=ModifiableEmo_CC;

CommonAppraisal=CommonAppraisal_CC;
ReappraisalOnly=ReappraisalOnly_CC;
NonModifiableEmo=NonModifiableEmo_CC;
ModifiableEmo=ModifiableEmo_CC;

%%%%%%%%%Validation check___ Compute the distance between consensus map and maps from each dataset%%%%%%%%%%
%%Load the map after cluster control %%%
load('C:\Users\KeBo\Documents\GitHub\KeBo2023_EmotionReg_BayesFactor\Final_SystemComponentMap_Consensus_AfterClusterControl.mat')

Y=BF_tstat1_CC.volInfo.xyzlist(indexOverlap,:);
X1=BF_tstat1_AHAB.volInfo.xyzlist(AHAB_Overlap,:);
X2=BF_tstat1_PIP.volInfo.xyzlist(PIP_Overlap,:);

D_AHAB = pdist2(X1,Y,'euclidean' );  %%%Point to Point distance
D_PIP = pdist2(X2,Y,'euclidean' );  %%%Point to Point distance
D_Min_EachV_AHAB=min(D_AHAB,[],1);  %%%%Compute minimum distance with AHAB for each voxel in joint map
D_Min_EachV_PIP=min(D_PIP,[],1);    %%%%Compute minimum distance with PIP for each voxel in joint map
max_D=max([D_Min_EachV_AHAB;D_Min_EachV_PIP])  %%%% Max distance across AHAB and PIP
max_D_Across_All1=max(max_D)

%%%%%

Y=BF_tstat2_CC.volInfo.xyzlist(indexReappraisalOnly,:);
X1=BF_tstat2_AHAB.volInfo.xyzlist(AHAB_ReappraisalOnly,:);
X2=BF_tstat2_PIP.volInfo.xyzlist(PIP_ReappraisalOnly,:);

D_AHAB = pdist2(X1,Y,'euclidean' );  %%%Point to Point distance
D_PIP = pdist2(X2,Y,'euclidean' );  %%%Point to Point distance
D_Min_EachV_AHAB=min(D_AHAB,[],1);  %%%%Compute minimum distance with AHAB for each voxel in joint map
D_Min_EachV_PIP=min(D_PIP,[],1);    %%%%Compute minimum distance with PIP for each voxel in joint map
max_D=max([D_Min_EachV_AHAB;D_Min_EachV_PIP])  %%%% Max distance across AHAB and PIP
max_D_Across_All2=max(max_D)


%%%%%%%%%%%%%%%%%%%

Y=BF_tstat3_CC.volInfo.xyzlist(indexLookOnly,:);
X1=BF_tstat3_AHAB.volInfo.xyzlist(AHAB_LookOnly,:);
X2=BF_tstat3_PIP.volInfo.xyzlist(PIP_LookOnly,:);

D_AHAB = pdist2(X1,Y,'euclidean' );  %%%Point to Point distance
D_PIP = pdist2(X2,Y,'euclidean' );  %%%Point to Point distance
D_Min_EachV_AHAB=min(D_AHAB,[],1);  %%%%Compute minimum distance with AHAB for each voxel in joint map
D_Min_EachV_PIP=min(D_PIP,[],1);    %%%%Compute minimum distance with PIP for each voxel in joint map
max_D=max([D_Min_EachV_AHAB;D_Min_EachV_PIP])  %%%% Max distance across AHAB and PIP
max_D_Across_All3=max(max_D)

%%%%%%%%%%%%%%%%%%%

Y=BF_tstat3_CC.volInfo.xyzlist(indexReappraisal_D,:);
X1=BF_tstat3_AHAB.volInfo.xyzlist(AHAB_Target,:);
X2=BF_tstat3_PIP.volInfo.xyzlist(PIP_Target,:);

D_AHAB = pdist2(X1,Y,'euclidean' );  %%%Point to Point distance
D_PIP = pdist2(X2,Y,'euclidean' );  %%%Point to Point distance
D_Min_EachV_AHAB=min(D_AHAB,[],1);  %%%%Compute minimum distance with AHAB for each voxel in joint map
D_Min_EachV_PIP=min(D_PIP,[],1);    %%%%Compute minimum distance with PIP for each voxel in joint map
max_D=max([D_Min_EachV_AHAB;D_Min_EachV_PIP])  %%%% Max distance across AHAB and PIP
max_D_Across_All4=max(max_D)
