# Major meeting
# Run code to line 16
# Remove Scratchings
# Run rest of code to line 214
# Ignore any event with first starters
# Best top rating for place
# Split up into valid events
# Attach Results

library(dplyr)
library(tidyr)
library(stringr)
library(survPen)
library(stringi)
library("corrplot")
getwd()


clf<-read.csv("200912_Flemington.csv", stringsAsFactors = FALSE)
clf<-clf<-select(clf, meeting.date, track, race.number, distance, horse.name, horse.number, horse.barrier, horse.weight, horse.claim, horse.last10, horse.record, horse.record.distance, horse.record.track, horse.record.first.up, horse.record.second.up, prizemoney)
clf<-clf %>% distinct(horse.name, .keep_all=TRUE)
clf<-clf[!(clf$track=="track"),]
View(clf)
# Remove scratchings
# clf<-clf[!(clf$horse.name=="Complacent"),]

clf<-separate(data=clf, col=horse.record, into=c("starts", "placings"), sep=":")
clf$last2<-str_sub(clf$horse.last10, -2, -1)
clf<-clf[c(1,2,3,4,5,6,7,8,9,10,18,11,12,13,14,15,16,17)]
clf[,7]<-sapply(clf[,7], as.numeric)
View(clf)

# Remove scr clf<-clf[-c(1:13),] / clf<-clf[clf$horse.name != "Abated", ] for single

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
  }else if (clf$l1[i]==6){
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
# Checkrceno<-clf$race.number
clf$hse<-clf$horse.name
clf$hseno<-clf$horse.number
View(clf)
#-------------------------------------------------------

 
clf$prob<-clf$prizeratprob+clf$spellratprob+clf$startsratprob+clf$winratprob+clf$placeratprob+clf$barrierratprob+clf$lastratprob+clf$fupratprob+clf$supratprob+clf$distratprob+clf$trackratprob+clf$wgtratprob
sumprob<-vector()
for(i in 1:max(clf$race.number)){sumprob[i]=sum(subset(clf, race.number==i) $prob)}
clf$hseprob<-clf$prob/sumprob[clf$race.number]
View(clf)
# Check prob: sum(subset(clf,race.number=="1") $hseprob)

clf$price<-(1/clf$hseprob-1)*10
clf$price<-floor(clf$price)
clf$price<-clf$price/10
clf<-arrange(clf, race.number, price)
View(clf)

# clf$prob: sum(subset(clf,race.number=="1") $wgtratprob)

clf1<-clf[ , c("race.number", "hseno", "hse", "price")]
write.csv(clf1,"/Users/nleidig/Desktop/testrce.csv")
# write.csv(clf1,"/Users/nicholasleidig/Desktop/testrce.csv")
View (clf1)
#-------------------------------------------------------

# Results analysis and append to clf
dfrslt<-read.csv("200912_Flemington_R5_results.csv", stringsAsFactors = FALSE)
View(dfrslt)
t(dfrslt) # check if needed
as.data.frame(dfrslt) ### IMPORTANT # check if needed
View (dfrslt)
# Subset clf to relevant event before appending
clf5<-clf[(clf$race.number==5),]
View(clf5)
# Remove scratchings and fix format
# clf6<-clf6[!(clf6$horse.number=="19"),] Later - remove n/a
dfrslt<-separate(data=dfrslt, col=Pos, into=c("placing", "discard"), sep="-")
dfrslt[,1]<-sapply(dfrslt[,1], as.numeric)
dfrslt[,4]<-sapply(dfrslt[,4], as.numeric)
# append
clf5$finish<-dfrslt$placing[match(clf5$hseno, dfrslt$No)]
clf5$lth<-dfrslt$Margin[match(clf5$hseno, dfrslt$No)]
clf5$sp<-dfrslt$Price[match(clf5$hseno, dfrslt$No)]
clf5 <- na.omit(clf5) # Removes scratchings
for(i in 1:nrow(clf5)){if (clf5$finish[i]==1){clf5$lth[i]=0}}
View(clf5)
#-------------------------------------------------------
# Results analysis and append to clf
dfrslt<-read.csv("200912_Flemington_R6_results.csv", stringsAsFactors = FALSE)
View(dfrslt)
t(dfrslt) # check if needed
as.data.frame(dfrslt) ### IMPORTANT # check if needed
View (dfrslt)
# Subset clf to relevant event before appending
clf6<-clf[(clf$race.number==6),]
View(clf6)
# Remove scratchings and fix format
# clf6<-clf6[!(clf6$horse.number=="19"),] Later - remove n/a
dfrslt<-separate(data=dfrslt, col=Pos, into=c("placing", "discard"), sep="-")
dfrslt[,1]<-sapply(dfrslt[,1], as.numeric)
dfrslt[,4]<-sapply(dfrslt[,4], as.numeric)
# append
clf6$finish<-dfrslt$placing[match(clf6$hseno, dfrslt$No)]
clf6$lth<-dfrslt$Margin[match(clf6$hseno, dfrslt$No)]
clf6$sp<-dfrslt$Price[match(clf6$hseno, dfrslt$No)]
clf6 <- na.omit(clf6)
for(i in 1:nrow(clf6)){if (clf6$finish[i]==1){clf6$lth[i]=0}}
View(clf6)
#-------------------------------------------------------

# Results analysis and append to clf
dfrslt<-read.csv("200912_Flemington_R7_results.csv", stringsAsFactors = FALSE)
View(dfrslt)
t(dfrslt)
as.data.frame(dfrslt) ### IMPORTANT
View (dfrslt)
# Subset clf to relevant event before appending
clf7<-clf[(clf$race.number==7),]
View(clf7)
# Remove scratchings and fix format
# clf7<-clf7[!(clf7$horse.number=="19"),] Later - remove n/a
dfrslt<-separate(data=dfrslt, col=Pos, into=c("placing", "discard"), sep="-")
dfrslt[,1]<-sapply(dfrslt[,1], as.numeric)
dfrslt[,4]<-sapply(dfrslt[,4], as.numeric)
# append
clf7$finish<-dfrslt$placing[match(clf7$hseno, dfrslt$No)]
clf7$lth<-dfrslt$Margin[match(clf7$hseno, dfrslt$No)]
clf7$sp<-dfrslt$Price[match(clf7$hseno, dfrslt$No)]
for(i in 1:nrow(clf7)){if (clf7$finish[i]==1){clf7$lth[i]=0}}
clf7 <- na.omit(clf7) 
View(clf7)
#-------------------------------------------------------

# Results analysis and append to clf
dfrslt<-read.csv("200912_Flemington_R8_results.csv", stringsAsFactors = FALSE)
View(dfrslt)
t(dfrslt)
as.data.frame(dfrslt) ### IMPORTANT
View (dfrslt)
# Subset clf to relevant event before appending
clf8<-clf[(clf$race.number==8),]
View(clf8)
# Remove scratchings and fix format
# rownames(clf8) <- NULL probably not needed
# clf8<-clf8[!(clf8$horse.number=="19"),]
dfrslt<-separate(data=dfrslt, col=Pos, into=c("placing", "discard"), sep="-")
dfrslt[,1]<-sapply(dfrslt[,1], as.numeric)
dfrslt[,4]<-sapply(dfrslt[,4], as.numeric)
# append
clf8$finish<-dfrslt$placing[match(clf8$hseno, dfrslt$No)]
for(i in 1:nrow(clf8)){if (clf8$finish[i]==1){clf8$lth[i]=0}}
clf8$lth<-dfrslt$Margin[match(clf8$hseno, dfrslt$No)]
clf8$sp<-dfrslt$Price[match(clf8$hseno, dfrslt$No)]
for(i in 1:nrow(clf8)){if (clf8$finish[i]==1){clf8$lth[i]=0}}
clf8 <- na.omit(clf8)
View(clf8)
#-------------------------------------------------------
# Results analysis and append to clf
dfrslt<-read.csv("200912_Flemington_R9_results.csv", stringsAsFactors = FALSE)
View(dfrslt)
t(dfrslt) # May not need
as.data.frame(dfrslt) ### IMPORTANT but may not need
View (dfrslt)
# Subset clf to relevant event before appending
clf9<-clf[(clf$race.number==9),]
View(clf9)
# Remove scratchings and fix format
# rownames(clf9) <- NULL probably not needed
# clf9<-clf9[!(clf9$horse.number=="19"),] # may combine in next step
dfrslt<-separate(data=dfrslt, col=Pos, into=c("placing", "discard"), sep="-")
dfrslt[,1]<-sapply(dfrslt[,1], as.numeric)
dfrslt[,4]<-sapply(dfrslt[,4], as.numeric)
View (dfrslt)
# append
clf9$finish<-dfrslt$placing[match(clf9$hseno, dfrslt$No)]
clf9$lth<-dfrslt$Margin[match(clf9$hseno, dfrslt$No)]
clf9$sp<-dfrslt$Price[match(clf9$hseno, dfrslt$No)]
# clf %>% mutate_if(is.factor, as.character) ->clf
# clf<-clf[-c(125),]
for(i in 1:nrow(clf9)){if (clf9$finish[i]==1){clf9$lth[i]=0}}
clf9 <- na.omit(clf9)
View(clf9)
#-------------------------------------------------------
clf10 <- rbind(clf10, clf5, clf6, clf7, clf8, clf9) # MASTER APPEND
View(clf10)
#-------------------------------------------------------

# General reference only
# clf2<-clf[c(34,38,40,42,45,47,51,54,57,60,63,65)]
# View(clf2)
# library(corrplot)
# corrMatrix<-cor(clf2)
# corrplot(corrMatrix, method = "number")
# corrplot(corrMatrix,method = "ellipse")
#-------------------------------------------------------



