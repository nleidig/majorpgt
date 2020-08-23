library(dplyr)
library(tidyr)
library(stringr)
library(survPen)
library(stringi)
getwd()

clf<-read.csv("200823_Sunshine Coast.csv", stringsAsFactors = FALSE)
clf<-select(clf, meeting.date, track, race.number, distance, horse.name, horse.number, horse.barrier, horse.weight, horse.claim, horse.last10, horse.record, horse.record.distance, horse.record.track, horse.record.first.up, horse.record.second.up, prizemoney)
clf<-clf %>% distinct(horse.name, .keep_all=TRUE)
clf<-clf[!(clf$track=="track"),]
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
for(i in 1:nrow(clf)){if (clf$winrat[i]==0){clf$winrat[i]=.000001}}
sumwinrat<-vector()
for(i in 1:max(clf$race.number)){sumwinrat[i]=sum(subset(clf, race.number==i) $winrat)}
clf$winratprob<-clf$winrat/sumwinrat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $winratprob)

clf$placetotal<-clf$first + clf$second + clf$third
clf$placerat<-clf$placetotal/clf$starts
for(i in 1:nrow(clf)){if (clf$placerat[i]==0){clf$placerat[i]=.000001}}
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
clf[,36:37]<-sapply(clf[,36:37], as.numeric)
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
clf[,38]<-sapply(clf[,38], as.numeric)
sumlastrat<-vector()
for(i in 1:max(clf$race.number)){sumlastrat[i]=sum(subset(clf, race.number==i) $lastrat)}
clf$lastratprob<-clf$lastrat/sumlastrat[clf$race.number]
# Check prob: sum(subset(clf,race.number=="1") $lastratprob)

clf<-separate(data=clf, col=horse.record.first.up, into=c("fupst", "fuppl"), sep=":")
clf<-separate(data=clf, col=horse.record.second.up, into=c("secst", "suppl"), sep=":")
clf<-separate(data=clf, col=horse.record.distance, into=c("distst", "distpl"), sep=":")
clf<-separate(data=clf, col=horse.record.track, into=c("trackst", "trackpl"), sep=":")

clf<-separate(data=clf, col=fuppl, into=c("fuppl1", "fuppl2", "fuppl3"), sep="-")
clf<-separate(data=clf, col=suppl, into=c("suppl1", "suppl2", "suppl3"), sep="-")
clf<-separate(data=clf, col=distpl, into=c("distpl1", "distpl2", "distpl3"), sep="-")
clf<-separate(data=clf, col=trackpl, into=c("trackpl1", "trackpl2", "trackpl3"), sep="-")
for(i in 1:nrow(clf)){if(clf$secst[i]==0){clf$secst[i]=1}}
for(i in 1:nrow(clf)){if(clf$distst[i]==0){clf$distst[i]=1}}
for(i in 1:nrow(clf)){if(clf$trackst[i]==0){clf$trackst[i]=1}}

clf[,16:31]<-sapply(clf[,16:31], as.numeric)

clf$fuptotal<-clf$fuppl1 + clf$fuppl2 + clf$fuppl3
clf$fuprat<-clf$fuptotal/clf$fupst
for(i in 1:nrow(clf)){if (clf$fuprat[i]==0){clf$fuprat[i]=.000001}}
sumfuprat<-vector()
for(i in 1:max(clf$race.number)){sumfuprat[i]=sum(subset(clf, race.number==i) $fuprat)}
clf$fupratprob<-clf$fuprat/sumfuprat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $fupratprob)

clf$suptotal<-clf$suppl1 + clf$suppl2 + clf$suppl3
clf$suprat<-clf$suptotal/clf$secst
for(i in 1:nrow(clf)){if (clf$suprat[i]==0){clf$suprat[i]=.000001}}
sumsuprat<-vector()
for(i in 1:max(clf$race.number)){sumsuprat[i]=sum(subset(clf, race.number==i) $suprat)}
clf$supratprob<-clf$suprat/sumsuprat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $supratprob)

clf$disttotal<-clf$distpl1 + clf$distpl2 + clf$distpl3
clf$distrat<-clf$disttotal/clf$distst
for(i in 1:nrow(clf)){if (clf$distrat[i]==0){clf$distrat[i]=.000001}}
sumdistrat<-vector()
for(i in 1:max(clf$race.number)){sumdistrat[i]=sum(subset(clf, race.number==i) $distrat)}
clf$distratprob<-clf$distrat/sumdistrat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $distratprob)

clf$tracktotal<-clf$trackpl1 + clf$trackpl2 + clf$trackpl3
clf$trackrat<-clf$tracktotal/clf$trackst
for(i in 1:nrow(clf)){if (clf$trackrat[i]==0){clf$trackrat[i]=.000001}}
sumtrackrat<-vector()
for(i in 1:max(clf$race.number)){sumtrackrat[i]=sum(subset(clf, race.number==i) $trackrat)}
clf$trackratprob<-clf$trackrat/sumtrackrat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $trackratprob)

clf[,8:9]<-sapply(clf[,8:9], as.numeric)

clf$wgtrat<-82-(clf$horse.weight-clf$horse.claim)
sumwgtrat<-vector()
for(i in 1:max(clf$race.number)){sumwgtrat[i]=sum(subset(clf, race.number==i) $wgtrat)}
clf$wgtratprob<-clf$wgtrat/sumwgtrat[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $wgtratprob)

clf$prob<-clf$prizeratprob+clf$spellratprob+clf$startsratprob+clf$winratprob+clf$placeratprob+clf$barrierratprob+clf$lastratprob+clf$fupratprob+clf$supratprob+clf$distratprob+clf$trackratprob+clf$wgtratprob
sumprob<-vector()
for(i in 1:max(clf$race.number)){sumprob[i]=sum(subset(clf, race.number==i) $prob)}
clf$hseprob<-clf$prob/sumprob[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $hseprob)

clf$price<-(1/clf$hseprob-1)*2
clf$price<-floor(clf$price)
clf$price<-clf$price/2
clf$rceno<-clf$race.number
clf$hse<-clf$horse.name
clf$hseno<-clf$horse.number

clf<-arrange(clf, rceno, price)
View(clf)

clf1<-clf[ , c("rceno", "hseno", "hse", "price")]

# write.csv(clf1,"/Users/nleidig/Desktop/testrce.csv")
write.csv(clf1,"/Users/nicholasleidig/Desktop/testrce.csv")
View (clf1)
