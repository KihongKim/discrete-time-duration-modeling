# create data sets for mlogit in R and for Biogeme
# by Kihong Kim


setwd("/Users/Potenza/Documents/01_Dissertation/Analysis3") # on my macbook
setwd("/home/kihong/Dissertation/Analysis3") # on the sapporo server

library(mlogit)

# load data
load("event.g1.15dt.RData")
# load("event.g1.10dt.RData")
# load("event.g1.5dt.RData")

###################################
# create a data set for mlogit in R
###################################
# a function to create a long-format data set for MNL models
createLong <- function(x,t) {
  x$nextActG[x$nextActG==0] <- "ST"
  x$nextActG <- as.factor(x$nextActG)
  choiceSet <- levels(x$nextActG)
  x$aeID <- x$perid*100+x$actNo
  x$dtID <- x$aeID*1000+x[[paste0("actDur",t)]]
  alts <- list()
  for (i in choiceSet) {
    x$alt <- i
    x$chosen <- ifelse(x$nextActG==i, 1, 0)
    if (i=="H2") {x$available <- ifelse(x$thisActG=="H1"|x$thisActG=="H2", 0, 1)}
    else {if (i=="H3") {x$available <- ifelse(x$thisActG=="H1"|x$thisActG=="H2", 0, 1)}
          else {x$available <- 1}}
    alts[[i]] <- x
  }
  long <- do.call("rbind", alts)
  long <- with(long, long[order(dtID,alt),])
  long <- subset(long, available==1)
  long$numAlt <- with(long, ave(alt, dtID, FUN=length))
  return(long)
}

#
event.g1.15dt.long <- createLong(event.g1.15dt,15)
save(event.g1.15dt.long, file="event.g1.15dt.long.RData")
# event.g1.10dt.long <- createLong(event.g1.10dt,10)
# save(event.g1.10dt.long, file="event.g1.10dt.long.RData")
# event.g1.5dt.long <- createLong(event.g1.5dt,5)
# save(event.g1.5dt.long, file="event.g1.5dt.long.RData")

# create a data set to run the "mlogit" function
myRdata.15dt <- mlogit.data(event.g1.15dt.long,shape="long",choice="chosen",chid.var="dtID",alt.var="alt",id="perid")
head(index(myRdata.15dt),10)
save(myRdata.15dt, file="myRdata.15dt.RData")
#mydata.10dt <- mlogit.data(event.g1.10dt.long,shape="long",choice="chosen",chid.var="dtID",alt.var="alt",id="perid")
#head(index(mydata.10dt),10)
#save(mydata.10dt, file="mydata.10dt.RData")
#mydata.5dt <- mlogit.data(event.g1.5dt.long,shape="long",choice="chosen",chid.var="dtID",alt.var="alt",id="perid")
#head(index(mydata.5dt),10)
#save(mydata.5dt, file="mydata.5dt.RData")

###############################
# create a data set for Biogeme
###############################
# a function to create a long-format data set for MNL models
createBGdat <- function(x) {
  myvars <- c("perid","actNo","thisActG","actDur15","nextActG",
              "DEP_HR2","arrive","dayDepart","TOURDEP_HR","TOURDEP_MIN",
              "thisH1","thisH2","thisEO","thisES","thisHC","thisPB","thisSH","thisSR",
              "hSIN1","hCOS1","hSIN2","hCOS2","hSIN3","hCOS3","hSIN4","hCOS4")
  x <- x[myvars]
  x$thisActG <- factor(x$thisActG, levels=c("H1","H2","EO","ES","HC","PB","SH","SR"))
  x$thisActG <- as.numeric(x$thisActG)
  x$nextActG[x$nextActG==0] <- "ST"
  x$nextActG <- factor(x$nextActG, levels=c("ST","H2","H3","EO","ES","HC","PB","SH","SR"))
  x$nextActG <- as.numeric(x$nextActG)
  x$cumDay <- with(x, ifelse(thisActG==1|thisActG==2, 0, ((arrive-dayDepart)+(actDur15*15))))
  x$cumTour <- with(x, ifelse(thisActG==1|thisActG==2, 0, ((arrive-(TOURDEP_HR*60+TOURDEP_MIN))+(actDur15*15))))
  x$aeID <- x$perid*100+x$actNo
  x$dtID <- x$aeID*1000+x$actDur15
  alts <- lapply(1:10, function(i) i=x)
  alts <- lapply(seq_along(alts),
                 function(i,x) {
                   x[[i]]$alt <- i
                   return (x[[i]])
                 }, alts)
  long <- do.call("rbind", alts)
  long <- with(long, long[order(dtID,alt),])
  long$ST_AV <- 1
  long$H2_AV <- with(long, ifelse(thisActG==1|thisActG==2, 0, 1))
  long$H3_AV <- with(long, ifelse(thisActG==1|thisActG==2, 0, 1))
  long$EO_AV <- 1
  long$ES_AV <- 1
  long$HC_AV <- 1
  long$PB_AV <- 1
  long$SH_AV <- 1
  long$SR_AV <- 1
  return(long)
}

event.g1.15dt.long.BG <- createBGdat(event.g1.15dt)
save(event.g1.15dt.long.BG, file="event.g1.15dt.long.BG.RData")
write.table(event.g1.15dt.long.BG, "myBGdata15.dat", sep="\t", row.names=FALSE, quote=FALSE)

