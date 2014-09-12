[ModelDescription]
activity duration analysis on a discrete-time framework for recurrent events, multiple states, and competing risks
using a random-intercepts MNL model
by Kihong Kim

[Choice]
nextActG

[Beta]
ASC_ST	0	-10	10	1
ASC_H2	0	-10	10	0
ASC_H3	0	-10	10	0
ASC_ES	0	-10	10	0
ASC_EO	0	-10	10	0
ASC_HC	0	-20	20	0
ASC_PB	0	-10	10	0
ASC_SH	0	-10	10	0
ASC_SR	0	-10	10	0

ZERO	0	-10	10	1
SIGMA_H2	0	-10	10	0
SIGMA_H3	0	-10	10	0
SIGMA_ES	0	-10	10	0
SIGMA_EO	0	-10	10	0
SIGMA_HC	0	-10	10	0
SIGMA_PB	0	-10	10	0
SIGMA_SH	0	-10	10	0
SIGMA_SR	0	-10	10	0

LogDur_H2	0	-10	10	0
LogDur_H3	0	-10	10	0
LogDur_ES	0	-10	10	0
LogDur_EO	0	-10	10	0
LogDur_HC	0	-10	10	0
LogDur_PB	0	-10	10	0
LogDur_SH	0	-10	10	0
LogDur_SR	0	-10	10	0

thisES_H2	0	-10	10	0
thisEO_H2	0	-10	10	0
//thisHC_H2	0	-10	10	0
thisPB_H2	0	-10	10	0
thisSH_H2	0	-10	10	0
thisSR_H2	0	-10	10	0

thisES_H3	0	-10	10	0
thisEO_H3	0	-10	10	0
//thisHC_H3	0	-10	10	0
thisPB_H3	0	-10	10	0
thisSH_H3	0	-10	10	0
thisSR_H3	0	-10	10	0

thisH2_ES	0	-10	10	0
thisES_ES	0	-10	10	0
thisEO_ES	0	-10	10	0
thisHC_ES	0	-10	10	0
thisPB_ES	0	-10	10	0
thisSH_ES	0	-10	10	0
thisSR_ES	0	-10	10	0

thisH2_EO	0	-10	10	0
thisES_EO	0	-10	10	0
thisEO_EO	0	-10	10	0
thisHC_EO	0	-10	10	0
thisPB_EO	0	-10	10	0
thisSH_EO	0	-10	10	0
thisSR_EO	0	-10	10	0

thisH2_HC	0	-10	10	0
thisES_HC	0	-10	10	0
thisEO_HC	0	-10	10	0
thisHC_HC	0	-10	10	0
thisPB_HC	0	-10	10	0
thisSH_HC	0	-10	10	0
thisSR_HC	0	-10	10	0

thisH2_PB	0	-10	10	0
thisES_PB	0	-10	10	0
thisEO_PB	0	-10	10	0
thisHC_PB	0	-10	10	0
thisPB_PB	0	-10	10	0
thisSH_PB	0	-10	10	0
thisSR_PB	0	-10	10	0

thisH2_SH	0	-10	10	0
thisES_SH	0	-10	10	0
thisEO_SH	0	-10	10	0
thisHC_SH	0	-10	10	0
thisPB_SH	0	-10	10	0
thisSH_SH	0	-10	10	0
thisSR_SH	0	-10	10	0

thisH2_SR	0	-10	10	0
thisES_SR	0	-10	10	0
thisEO_SR	0	-10	10	0
thisHC_SR	0	-10	10	0
thisPB_SR	0	-10	10	0
thisSH_SR	0	-10	10	0
thisSR_SR	0	-10	10	0

hSIN1_H2	0	-10	10	0
hSIN1_H3	0	-10	10	0
hSIN1_ES	0	-10	10	0
hSIN1_EO	0	-10	10	0
hSIN1_HC	0	-10	10	0
hSIN1_PB	0	-10	10	0
hSIN1_SH	0	-10	10	0
hSIN1_SR	0	-10	10	0

hCOS1_H2	0	-10	10	0
hCOS1_H3	0	-10	10	0
hCOS1_ES	0	-10	10	0
hCOS1_EO	0	-10	10	0
hCOS1_HC	0	-10	10	0
hCOS1_PB	0	-10	10	0
hCOS1_SH	0	-10	10	0
hCOS1_SR	0	-10	10	0


[Utilities]
1	ST	ST_AV	ASC_ST * one
2	H2	H2_AV	ASC_H2 * one + LogDur_H2 * LogDur15
				+ thisES_H2 * thisES
				+ thisEO_H2 * thisEO
				+ thisPB_H2 * thisPB
				+ thisSH_H2 * thisSH
				+ thisSR_H2 * thisSR
				+ hSIN1_H2 * hSIN1 + hCOS1_H2 * hCOS1
				+ ZERO [ SIGMA_H2 ] * one
3	H3	H3_AV	ASC_H3 * one + LogDur_H3 * LogDur15
				+ thisES_H3 * thisES
				+ thisEO_H3 * thisEO
				+ thisPB_H3 * thisPB
				+ thisSH_H3 * thisSH
				+ thisSR_H3 * thisSR
				+ hSIN1_H3 * hSIN1 + hCOS1_H3 * hCOS1
				+ ZERO [ SIGMA_H3 ] * one
4	ES	ES_AV	ASC_ES * one + LogDur_ES * LogDur15
				+ thisH2_ES * thisH2
				+ thisES_ES * thisES
				+ thisEO_ES * thisEO
				+ thisHC_ES * thisHC
				+ thisPB_ES * thisPB
				+ thisSH_ES * thisSH
				+ thisSR_ES * thisSR
				+ hSIN1_ES * hSIN1 + hCOS1_ES * hCOS1
				+ ZERO [ SIGMA_ES ] * one
5	EO	EO_AV	ASC_EO * one + LogDur_EO * LogDur15
				+ thisH2_EO * thisH2
				+ thisES_EO * thisES
				+ thisEO_EO * thisEO
				+ thisHC_EO * thisHC
				+ thisPB_EO * thisPB
				+ thisSH_EO * thisSH
				+ thisSR_EO * thisSR
				+ hSIN1_EO * hSIN1 + hCOS1_EO * hCOS1
				+ ZERO [ SIGMA_EO ] * one
6	HC	HC_AV	ASC_HC * one + LogDur_HC * LogDur15
				+ thisH2_HC * thisH2
				+ thisES_HC * thisES
				+ thisEO_HC * thisEO
				+ thisHC_HC * thisHC
				+ thisPB_HC * thisPB
				+ thisSH_HC * thisSH
				+ thisSR_HC * thisSR
				+ hSIN1_HC * hSIN1 + hCOS1_HC * hCOS1
				+ ZERO [ SIGMA_HC ] * one
7	PB	PB_AV	ASC_PB * one + LogDur_PB * LogDur15
				+ thisH2_PB * thisH2
				+ thisES_PB * thisES
				+ thisEO_PB * thisEO
				+ thisHC_PB * thisHC
				+ thisPB_PB * thisPB
				+ thisSH_PB * thisSH
				+ thisSR_PB * thisSR
				+ hSIN1_PB * hSIN1 + hCOS1_PB * hCOS1
				+ ZERO [ SIGMA_PB ] * one
8	SH	SH_AV	ASC_SH * one + LogDur_SH * LogDur15
				+ thisH2_SH * thisH2
				+ thisES_SH * thisES
				+ thisEO_SH * thisEO
				+ thisHC_SH * thisHC
				+ thisPB_SH * thisPB
				+ thisSH_SH * thisSH
				+ thisSR_SH * thisSR
				+ hSIN1_SH * hSIN1 + hCOS1_SH * hCOS1
				+ ZERO [ SIGMA_SH ] * one
9	SR	SR_AV	ASC_SR * one + LogDur_SR * LogDur15
				+ thisH2_SR * thisH2
				+ thisES_SR * thisES
				+ thisEO_SR * thisEO
				+ thisHC_SR * thisHC
				+ thisPB_SR * thisPB
				+ thisSH_SR * thisSH
				+ thisSR_SR * thisSR
				+ hSIN1_SR * hSIN1 + hCOS1_SR * hCOS1
				+ ZERO [ SIGMA_SR ] * one

[Expressions]
one = 1
LogDur15 = log( actDur15 )

[PanelData]
perid
ZERO_SIGMA_H2
ZERO_SIGMA_H3
ZERO_SIGMA_ES
ZERO_SIGMA_EO
ZERO_SIGMA_HC
ZERO_SIGMA_PB
ZERO_SIGMA_SH
ZERO_SIGMA_SR

[Draws]
1000

[Model]
$MNL 


