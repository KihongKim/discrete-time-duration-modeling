# estimate multilevel MNL models
# by Kihong Kim


setwd("/home/kihong/Dissertation/Analysis3") # on the sapporo server

library(mlogit)

load("myRdata.15dt.RData")

# null model
ids = unique(myRdata.15dt$dtid)
alt.ids = unique(myRdata.15dt$alt)
logLik.null <- length(ids) * log(1/length(alt.ids))

# constants-only model
mnl.asc <- mlogit(chosen~alt, data=myRdata.15dt, reflevel="ST")
summary(mnl.asc)
logLik.asc <- logLik(mnl.asc)

# create additional state dummies that are located in the first place of the right-hand side of mlogit fungction
myRdata.15dt$thisH2_EO <- with(myRdata.15dt, ifelse(alt=="EO", thisH2, 0))
myRdata.15dt$thisH2_ES <- with(myRdata.15dt, ifelse(alt=="ES", thisH2, 0))
myRdata.15dt$thisH2_HC <- with(myRdata.15dt, ifelse(alt=="HC", thisH2, 0))
myRdata.15dt$thisH2_PB <- with(myRdata.15dt, ifelse(alt=="PB", thisH2, 0))
myRdata.15dt$thisH2_SH <- with(myRdata.15dt, ifelse(alt=="SH", thisH2, 0))
myRdata.15dt$thisH2_SR <- with(myRdata.15dt, ifelse(alt=="SR", thisH2, 0))

myRdata.15dt$thisHC_EO <- with(myRdata.15dt, ifelse(alt=="EO", thisHC, 0))
myRdata.15dt$thisHC_ES <- with(myRdata.15dt, ifelse(alt=="ES", thisHC, 0))
myRdata.15dt$thisHC_HC <- with(myRdata.15dt, ifelse(alt=="HC", thisHC, 0))
myRdata.15dt$thisHC_PB <- with(myRdata.15dt, ifelse(alt=="PB", thisHC, 0))
myRdata.15dt$thisHC_SH <- with(myRdata.15dt, ifelse(alt=="SH", thisHC, 0))
myRdata.15dt$thisHC_SR <- with(myRdata.15dt, ifelse(alt=="SR", thisHC, 0))

# single-level MNL
mnl.sl <- mlogit(chosen~
                  thisH2_EO+thisH2_ES+thisH2_HC+thisH2_PB+thisH2_SH+thisH2_SR+
                  thisHC_EO+thisHC_ES+thisHC_HC+thisHC_PB+thisHC_SH+thisHC_SR|
                  thisEO+thisES+thisPB+thisSH+thisSR+
                  log(actDur15)+
                  hSIN1+hCOS1|0,
                data=myRdata.15dt, reflevel="ST")
summary(mnl.sl)
save(mnl.sl, file="mnl.sl.RData")

# multilevel MNL with variance only
mnl.ml.var <- mlogit(chosen~
					thisH2_EO+thisH2_ES+thisH2_HC+thisH2_PB+thisH2_SH+thisH2_SR|
					thisEO+thisES+thisHC+thisPB+thisSH+thisSR+
					log(actDur15)+
					hSIN1+hCOS1|0,
  					data=mlogitData.15dt, reflevel="ST",
  					rpar=c("H2:(intercept)"="n",
                    		"H3:(intercept)"="n",
                        	"EO:(intercept)"="n",
                        	"ES:(intercept)"="n",
                            "HC:(intercept)"="n",
                            "PB:(intercept)"="n",
                            "SH:(intercept)"="n",
                            "SO:(intercept)"="n"),
                    R=1500, halton=NA, print.level=1, panel=TRUE)
summary(mnl.ml.var)
save(mnl.ml.var, file="mnl.ml.var.RData")

# multilevel with covariance
mnl.ml.cov <- mlogit(chosen~
					thisH2_EO+thisH2_ES+thisH2_HC+thisH2_PB+thisH2_SH+thisH2_SR+
					thisHC_EO+thisHC_ES+thisHC_HC+thisHC_PB+thisHC_SH+thisHC_SR|
					thisEO+thisES+thisPB+thisSH+thisSR+
					log(actDur15)+
					hSIN1+hCOS1|0,
  					data=myRdata.15dt, reflevel="ST",
  					rpar=c("H2:(intercept)"="n",
                    		"H3:(intercept)"="n",
                        	"ES:(intercept)"="n",
                        	"EO:(intercept)"="n",
                            "HC:(intercept)"="n",
                            "PB:(intercept)"="n",
                            "SH:(intercept)"="n",
                            "SR:(intercept)"="n"),
                    R=1000, halton=NA, print.level=1, panel=TRUE, correlation=TRUE)
summary(mnl.ml.cov)
save(mnl.ml.cov, file="mnl.ml.cov.RData")
