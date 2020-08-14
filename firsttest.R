library(dplyr)
library(tidyr)
library(stringr)
library(survPen)
library(stringi)
getwd()

clf<-read.csv("200725_Caulfield.csv", stringsAsFactors = FALSE)
clf<-select(clf, meeting.date, track, race.number, distance, horse.name, horse.number, horse.barrier, horse.weight, horse.claim, horse.last10, horse.record, horse.record.distance, horse.record.track, horse.record.first.up, horse.record.second.up, prizemoney)
clf<-clf %>% distinct(horse.name, .keep_all=TRUE)
clf<-clf[-12,]
View(clf)

clf<-separate(data=clf, col=horse.record, into=c("starts", "placings"), sep=":")
clf$last2<-str_sub(clf$horse.last10, -2, -1)
clf<-clf[c(1,2,3,4,5,6,7,8,9,10,18,11,12,13,14,15,16,17)]
clf[,7]<-sapply(clf[,7], as.numeric)
View(clf)

clf[,3]<-sapply(clf[,3], as.numeric)

clf$prizemoney<-sub('.', '', clf$prizemoney)
clf[,18]<-sapply(clf[,18], as.numeric)
clf[,12]<-sapply(clf[,12], as.numeric)
clf$prizerat<-clf$prizemoney/clf$starts
clf$prizerat<-floor(clf$prizerat)
sumprizerat<-vector()
for(i in 1:max(clf$race.number)){sumprizerat[i]=sum(subset(clf, race.number==i) $prizerat)}
clf$prizeratprob<-clf$prizerat/sumprizerat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $prizeratprob)

clf$rev10<-stri_reverse(clf$horse.last10)
clf$rev10<- paste0(clf$rev10, "x1")
for(i in 1:nrow(clf)){clf$spellcount[i]<-instr(clf$rev10[i],"x")}
for(i in 1:nrow(clf)){
if(clf$spellcount[i]==1){
  clf$spellrat[i]="6"
 } else if (clf$spellcount[i]==2){
  clf$spellrat[i]="6"
 }else if (clf$spellcount[i]==3){
   clf$spellrat[i]="5"
 }else if (clf$spellcount[i]==4){
   clf$spellrat[i]="4"
 }else if (clf$spellcount[i]==5){
   clf$spellrat[i]="3"
 }else if (clf$spellcount==6){
   clf$spellrat="2"
 }else if (clf$spellcount[i]>=7){
   clf$spellrat[i]="1"
 }}
clf[,23]<-sapply(clf[,23], as.numeric)
sumspellrat<-vector()
for(i in 1:max(clf$race.number)){sumspellrat[i]=sum(subset(clf, race.number==i) $spellrat)}
clf$spellratprob<-clf$spellrat/sumspellrat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $spellratprob)

clf$startsrat<-max(clf$starts)+1 -clf$starts
sumstartsrat<-vector()
for(i in 1:max(clf$race.number)){sumstartsrat[i]=sum(subset(clf, race.number==i) $startsrat)}
clf$startsratprob<-clf$startsrat/sumstartsrat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $startsratprob)

clf<-separate(data=clf, col=placings, into=c("first", "second", "third"), sep="-")
clf[,13:15]<-sapply(clf[,13:15], as.numeric)
clf$winrat<-clf$first/clf$starts
sumwinrat<-vector()
for(i in 1:max(clf$race.number)){sumwinrat[i]=sum(subset(clf, race.number==i) $winrat)}
clf$winratprob<-clf$winrat/sumwinrat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $winratprob)

clf$placetotal<-clf$first + clf$second + clf$third
clf$placerat<-clf$placetotal/clf$starts
sumplacerat<-vector()
for(i in 1:max(clf$race.number)){sumplacerat[i]=sum(subset(clf, race.number==i) $placerat)}
clf$placeratprob<-clf$placerat/sumplacerat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $placeratprob)

clf$barrierrat<-(82-clf$horse.barrier)/450
sumbarrierrat<-vector()
for(i in 1:max(clf$race.number)){sumbarrierrat[i]=sum(subset(clf, race.number==i) $barrierrat)}
clf$barrierratprob<-clf$barrierrat/sumbarrierrat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $barrierratprob)

clf$l2<-str_sub(clf$horse.last10, -2, -2)
clf$l1<-str_sub(clf$horse.last10, -1, -1)
clf$l2[clf$l2==""] <- "11"
clf$l1[clf$l1==""] <- "11"
for(i in 1:nrow(clf)){if(clf$l2[i]=="x"){clf$l2[i]="10"}}
for(i in 1:nrow(clf)){if(clf$l1[i]=="x"){clf$l1[i]="10"}}
for(i in 1:nrow(clf)){if(clf$l2[i]=="0"){clf$l2[i]="10"}}
for(i in 1:nrow(clf)){if(clf$l1[i]=="0"){clf$l1[i]="10"}}
clf[,11:12]<-sapply(clf[,11:12], as.numeric)
for(i in 1:nrow(clf)){if(clf$l2[i]<=clf$l1[i]){clf$l1[i]=clf$l2[i]}}
for(i in 1:nrow(clf)){
  if(clf$l1[i]==1){
    clf$lastrat[i]="6"
  } else if (clf$l1[i]==2){
    clf$lastrat[i]="6"
  }else if (clf$l1[i]==3){
    clf$lastrat[i]="5"
  }else if (clf$l1[i]==4){
    clf$lastrat[i]="4"
  }else if (clf$l1[i]==5){
    clf$lastrat[i]="3"
  }else if (clf$l1==6){
    clf$lastrat="2"
  }else if (clf$l1[i]>=7){
    clf$lastrat[i]="1"
  }}
clf[,37]<-sapply(clf[,37], as.numeric)
sumlastrat<-vector()
for(i in 1:max(clf$race.number)){sumlastrat[i]=sum(subset(clf, race.number==i) $lastrat)}
clf$lastratprob<-clf$lastrat/sumlastrat[clf$race.number]
# Check prob: sum(subset(clf,race.number=="1") $lastratprob)






