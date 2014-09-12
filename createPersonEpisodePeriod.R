# create a person-episode-period file to conduct a discrete-time duration analysis
# by Kihong Kim


setwd("/Users/Potenza/Documents/01_Dissertation/Analysis3") # on my macbook
setwd("/home/kihong/Dissertation/Analysis3") # on the sapporo server
options(scipen=999) # to remove any scientific notations

library(survival)
library(sqldf)
library(reshape)

# load data
activity <- read.csv("../Data/2011OTAS/ACTIVITY.csv", header=T)
household <- read.csv("../Data/2011OTAS/HH.csv", header=T)
person <- read.csv("../Data/2011OTAS/PER.csv", header=T)

# sort activity by SAMPN, PERNO, and PLANO
activity <- with(activity, activity[order(SAMPN,PERNO,PLANO),])

# select households only in Metro (AREA=11; 4799), not RTC (AREA=21; 1650)
event <- subset(activity, SAMPN>8000000)

# merge event with household and person
event <- merge(event, household, by="SAMPN", all.x=T)
event <- merge(event, person, by=c("SAMPN","PERNO"), all.x=T)

# add 'perid' (the maximum PERNO is 8)
event$perid <- with(event, SAMPN*10+PERNO) 

# add 'actNo' and 'maxAct'
event$actNo <- with(event, ave(PLANO, SAMPN,PERNO, FUN=seq))
event$maxAct <- with(event, ave(actNo, SAMPN,PERNO, FUN=length))
# tip: 'ave' is to do group averages over the combined levels of factors

# add 'arrive' and 'depart' on the 1440-minute time scale
event$arrive <- with(event, ARR_HR*60+ARR_MIN)
event$depart <- with(event, DEP_HR*60+DEP_MIN)

# correct 'arrive' and 'depart' after midnight
event$arrive <- with(event, ifelse(arrive<180, arrive+1440, arrive))
event$depart <- with(event, ifelse(depart<180, depart+1440, depart))

# define the sample
# 1. delete persons who have no trip during the day
NoTrip <- sqldf("select perid from event where actNo==1 and maxAct==1")
event <- subset(event, !(event$perid %in% NoTrip$perid))
# 2. delete persons whose first activity is not 'Home'
NotHomeAtFirst <- sqldf("select perid from event where actNo=1 and TPURP!=1 and TPURP!=2")
event <- subset(event, !(event$perid %in% NotHomeAtFirst$perid))
# 3. delete persons whose last activity is not 'Home'
NotHomeAtLast <- sqldf("select perid from event where actNo=maxAct and TPURP!=1 and TPURP!=2")
event <- subset(event, !(event$perid %in% NotHomeAtLast$perid))

# delete CM (Change Model) activities to use only linked trips
event <- subset(event, TPURP!=7)

# update actNo and maxAct
event$actNo <- with(event, ave(PLANO, SAMPN,PERNO, FUN=seq))
event$maxAct <- with(event, ave(actNo, SAMPN,PERNO, FUN=length))

# aggregate activity types (thisActG)
aggregateActivities = read.csv("../Data/aggregateActivities4.csv", stringsAsFactors=FALSE)
event$thisActG = aggregateActivities$ActG[match(event$TPURP, aggregateActivities$Code)]
event$thisActG_Name = aggregateActivities$ActG_Name[match(event$TPURP, aggregateActivities$Code)]

# disaggregate home activities into H1, H2, and H3
attach(event)
event$thisActG[thisActG=="HM" & actNo==1] <- "H1"
event$thisActG[thisActG=="HM" & actNo!=1 & actNo!=maxAct] <- "H2"
event$thisActG[thisActG=="HM" & actNo!=1 & actNo==maxAct] <- "H3"
event$thisActG_Name[thisActG=="H1"] <- "HomeBegining"
event$thisActG_Name[thisActG=="H2"] <- "HomeMiddle"
event$thisActG_Name[thisActG=="H3"] <- "HomeEnd"
detach(event)

# lastActG
event$lastActG <- c(NA, event$thisActG[-length(event$thisActG)])
event$lastActG[event$actNo==1] <- NA
# tip: '-length()' is all elements except for the last observation

# nextActG
event$nextActG <- c(event$thisActG[-1], NA)
event$nextActG[event$actNo==event$maxAct] <- NA

# delete 3 person involving a transition from H1 to H2
sqldf("select perid from event where lastActG=='H1' and thisActG=='H2'")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80043571")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80856423")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==81325631")
H1toH2 <- sqldf("select perid from event where lastActG=='H1' and thisActG=='H2'")
event <- subset(event, !(event$perid %in% H1toH2$perid))

# delete 14 persons involving a transition from H2 to H3
sqldf("select perid from event where lastActG=='H2' and thisActG=='H3'")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80252491")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80333872")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80954302")
H2toH3 <- sqldf("select perid from event where lastActG=='H2' and thisActG=='H3'")
event <- subset(event, !(event$perid %in% H2toH3$perid))

# delete 5 persons involving a transition from H1 to H3
sqldf("select perid from event where lastActG=='H1' and thisActG=='H3'")
sqldf("select perid,PLANO,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80824324")
sqldf("select perid,PLANO,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==81181904")
sqldf("select perid,PLANO,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==81204141")
H1toH3 <- sqldf("select perid from event where lastActG=='H1' and thisActG=='H3'")
event <- subset(event, !(event$perid %in% H1toH3$perid))

# add lastDepart
event$lastDepart <- c(NA, event$depart[-length(event$depart)])
event$lastDepart[event$actNo==1] <- NA

# add tripDur
event$tripDur <- with(event, arrive-lastDepart)

# add actDur
event <- rename(event, c(ACTDUR="ACTDUR_OLD"))
event$actDur <- with(event, depart-arrive)

# convert activity duration from continuous to interval
# add 'actDur15' (15-min time intervals of activity duration)
event$actDur15 <- with(event, ifelse(actDur%%15==0, actDur/15, actDur%/%15+1))  # (,]
# add 'actDur10' (10-min time intervals of activity duration)
event$actDur10 <- with(event, ifelse(actDur%%10==0, actDur/10, actDur%/%10+1))  # (,]
# add 'actDur5' (5-min time intervals of activity duration)
event$actDur5 <- with(event, ifelse(actDur%%5==0, actDur/5, actDur%/%5+1))  # (,]

# add 'dayDepart'
event$dayDepart <- with(event, ave(depart, perid, FUN=min))

# time-of-day indicators to tell when the current activity ends
event$T0304 <- with(event, ifelse(DEP_HR==3, 1, 0))
event$T0405 <- with(event, ifelse(DEP_HR==4, 1, 0))
event$T0506 <- with(event, ifelse(DEP_HR==5, 1, 0))
event$T0607 <- with(event, ifelse(DEP_HR==6, 1, 0))
event$T0708 <- with(event, ifelse(DEP_HR==7, 1, 0))
event$T0809 <- with(event, ifelse(DEP_HR==8, 1, 0))
event$T0910 <- with(event, ifelse(DEP_HR==9, 1, 0))
event$T1011 <- with(event, ifelse(DEP_HR==10, 1, 0))
event$T1112 <- with(event, ifelse(DEP_HR==11, 1, 0))
event$T1213 <- with(event, ifelse(DEP_HR==12, 1, 0))
event$T1314 <- with(event, ifelse(DEP_HR==13, 1, 0))
event$T1415 <- with(event, ifelse(DEP_HR==14, 1, 0))
event$T1516 <- with(event, ifelse(DEP_HR==15, 1, 0))
event$T1617 <- with(event, ifelse(DEP_HR==16, 1, 0))
event$T1718 <- with(event, ifelse(DEP_HR==17, 1, 0))
event$T1819 <- with(event, ifelse(DEP_HR==18, 1, 0))
event$T1920 <- with(event, ifelse(DEP_HR==19, 1, 0))
event$T2021 <- with(event, ifelse(DEP_HR==20, 1, 0))
event$T2122 <- with(event, ifelse(DEP_HR==21, 1, 0))
event$T2223 <- with(event, ifelse(DEP_HR==22, 1, 0))
event$T2324 <- with(event, ifelse(DEP_HR==23, 1, 0))
event$T2401 <- with(event, ifelse(DEP_HR==24, 1, 0))
event$T0102 <- with(event, ifelse(DEP_HR==1, 1, 0))
event$T0203 <- with(event, ifelse(DEP_HR==2, 1, 0))

# periodic or circular time-of-day effects
# hour scale
event$DEP_HR2 <- with(event, ifelse(DEP_HR<24, DEP_HR, DEP_HR-24))
event$hSIN1 <- with(event, sin((2*pi/24)*DEP_HR2))
event$hCOS1 <- with(event, cos((2*pi/24)*DEP_HR2))
event$hSIN2 <- with(event, sin((2*pi/24)*DEP_HR2*2))
event$hCOS2 <- with(event, cos((2*pi/24)*DEP_HR2*2))
event$hSIN3 <- with(event, sin((2*pi/24)*DEP_HR2*3))
event$hCOS3 <- with(event, cos((2*pi/24)*DEP_HR2*3))
event$hSIN4 <- with(event, sin((2*pi/24)*DEP_HR2*4))
event$hCOS4 <- with(event, cos((2*pi/24)*DEP_HR2*4))
# minute scale
event$depart2 <- with(event, ifelse(depart<1440, depart, depart-1440))
event$mSIN1 <- with(event, sin((2*pi/1440)*depart2))
event$mCOS1 <- with(event, cos((2*pi/1440)*depart2))
event$mSIN2 <- with(event, sin((2*pi/1440)*depart2*2))
event$mCOS2 <- with(event, cos((2*pi/1440)*depart2*2))
event$mSIN3 <- with(event, sin((2*pi/1440)*depart2*3))
event$mCOS3 <- with(event, cos((2*pi/1440)*depart2*3))
event$mSIN4 <- with(event, sin((2*pi/1440)*depart2*4))
event$mCOS4 <- with(event, cos((2*pi/1440)*depart2*4))

# current-activity indicators to tell what the current activity is
event$thisH1 <- with(event, ifelse(thisActG=="H1", 1, 0))
event$thisH2 <- with(event, ifelse(thisActG=="H2", 1, 0))
event$thisH3 <- with(event, ifelse(thisActG=="H3", 1, 0))
event$thisWK <- with(event, ifelse(thisActG=="WK", 1, 0))
event$thisWR <- with(event, ifelse(thisActG=="WR", 1, 0))
event$thisSC <- with(event, ifelse(thisActG=="SC", 1, 0))
event$thisES <- with(event, ifelse(thisActG=="ES", 1, 0))
event$thisEO <- with(event, ifelse(thisActG=="EO", 1, 0))
event$thisPB <- with(event, ifelse(thisActG=="PB", 1, 0))
event$thisHC <- with(event, ifelse(thisActG=="HC", 1, 0))
event$thisSH <- with(event, ifelse(thisActG=="SH", 1, 0))
event$thisSR <- with(event, ifelse(thisActG=="SR", 1, 0))
event$thisOT <- with(event, ifelse(thisActG=="OT", 1, 0))

# next-activity indicators to tell what the next activity is
event$nextH1 <- with(event, ifelse(nextActG=="H1", 1, 0))
event$nextH2 <- with(event, ifelse(nextActG=="H2", 1, 0))
event$nextH3 <- with(event, ifelse(nextActG=="H3", 1, 0))
event$nextWK <- with(event, ifelse(nextActG=="WK", 1, 0))
event$nextWR <- with(event, ifelse(nextActG=="WR", 1, 0))
event$nextSC <- with(event, ifelse(nextActG=="SC", 1, 0))
event$nextES <- with(event, ifelse(nextActG=="ES", 1, 0))
event$nextEO <- with(event, ifelse(nextActG=="EO", 1, 0))
event$nextPB <- with(event, ifelse(nextActG=="PB", 1, 0))
event$nextHC <- with(event, ifelse(nextActG=="HC", 1, 0))
event$nextSH <- with(event, ifelse(nextActG=="SH", 1, 0))
event$nextSR <- with(event, ifelse(nextActG=="SR", 1, 0))
event$nextOT <- with(event, ifelse(nextActG=="OT", 1, 0))

# delete 'H3' since it has no choice for the next activity
sqldf("select thisActG,count(*) from event where actNo=maxAct group by thisActG")
event <- event[-which(event$thisActG=="H3"),] 

#
save(event, file="event.RData")

####################################
### create a data set for each group
####################################
# group 1 - retired, age>=65, and no school
event.g1 <- subset(event, WKSTAT==1 & AGE>=65 & AGE<95 & STUDE==3)
# delete 2 persons with thisActG=OT
peridWithOT <- sqldf("select perid from 'event.g1' where thisActG='OT' ")
event.g1 <- subset(event.g1, !(event.g1$perid %in% peridWithOT$perid))
#
save(event.g1, file="event.g1.RData")

# compute activity duration and trip duration by thisActG
sqldf("select thisActG,count(*),round(avg(actDur),1),round(stdev(actDur),1),median(actDur),min(actDur),max(actDur) from 'event.g1' group by thisActG")
sqldf("select nextActG,count(*),round(avg(actDur),1),round(stdev(actDur),1),median(actDur),min(actDur),max(actDur) from 'event.g1' group by nextActG")
sqldf("select thisActG,count(*),round(avg(tripDur),1),round(stdev(tripDur),1),median(tripDur),min(tripDur),max(tripDur) from 'event.g1' group by thisActG")
sqldf("select perid,PNAME,actNo,maxAct,thisActG,nextActG,arrive,depart,actDur,tripDur from 'event.g1' limit 5")
sqldf("select perid,AGE,PNAME,actNo,maxAct,thisActG,nextActG,arrive,depart,actDur,tripDur from 'event.g1' where perid=80000402")

##########################################################
# convert from "person-episode" to "person-episode-period"
##########################################################
### using 5-min interavals
event.g1.5dt <- survSplit(event.g1, cut=(1:max(event.g1$actDur5)), end="actDur5", event="nextActG", start="start")
event.g1.5dt <- with(event.g1.5dt, event.g1.5dt[order(SAMPN, PERNO, actNo, start),])
# add time dummies
for (i in 1:max(event.g1.5dt$actDur5)) {
  event.g1.5dt[[paste("t",i,sep="")]] <- ifelse(event.g1.5dt$actDur5==i, 1, 0)
}
# add exposure
event.g1.5dt$exposure <- with(event.g1.5dt, ifelse(actDur5*5 <= actDur, 5, actDur%%5))
# testing
sqldf("select perid,actNo,maxAct,tripDur,thisActG,nextActG,actDur,actDur5,exposure,arrive,depart from 'event.g1.5dt' limit 126")
table(event.g1.5dt$nextActG, useNA="ifany")
#
save(event.g1.5dt, file="event.g1.5dt.RData")

### using 10-min interavals
event.g1.10dt <- survSplit(event.g1, cut=(1:max(event.g1$actDur10)), end="actDur10", event="nextActG", start="start")
event.g1.10dt <- with(event.g1.10dt, event.g1.10dt[order(SAMPN, PERNO, actNo, start),])
# add time dummies
for (i in 1:max(event.g1.10dt$actDur10)) {
  event.g1.10dt[[paste("t",i,sep="")]] <- ifelse(event.g1.10dt$actDur10==i, 1, 0)
}
# add exposure
event.g1.10dt$exposure <- with(event.g1.10dt, ifelse(actDur10*10 <= actDur, 10, actDur%%10))
# testing
sqldf("select perid,actNo,maxAct,thisActG,nextActG,actDur,actDur10,exposure,arrive,depart from 'event.g1.10dt' limit 100")
table(event.g1.10dt$nextActG, useNA="ifany")
#
save(event.g1.10dt, file="event.g1.10dt.RData")

### using 15-min interavals
event.g1.15dt <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextActG", start="start")
event.g1.15dt <- with(event.g1.15dt, event.g1.15dt[order(SAMPN, PERNO, actNo, start),])
# add time dummies
for (i in 1:max(event.g1.15dt$actDur15)) {
  event.g1.15dt[[paste("t",i,sep="")]] <- ifelse(event.g1.15dt$actDur15==i, 1, 0)
}
# add exposure
event.g1.15dt$exposure <- with(event.g1.15dt, ifelse(actDur15*15 <= actDur, 15, actDur%%15))
# viewing
sqldf("select perid,tourNo,maxTour,actNo,maxAct,thisActG,nextActG,actDur,actDur15,exposure,arrive,depart,t1,t2,t10,t11,t68,t69 from 'event.g1.15dt' limit 100")
table(event.g1.15dt$nextActG, useNA="ifany")
#
save(event.g1.15dt, file="event.g1.15dt.RData")

