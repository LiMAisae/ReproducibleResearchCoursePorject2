
setwd("~/data_study/ReproducibleResearch/courseAssignment2/")


library(ggplot2)
library(dplyr)
library(lubridate)

if(!file.exists("repdata_data_StormData.csv.bz2")){
  Url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url =Url, "repdata_data_StormData.csv.bz2", method = "libcurl")
}
StormData<-read.csv("repdata_data_StormData.csv.bz2")

str(StormData)


StormData<-select(StormData, BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, FATALITIES, INJURIES)
StormData$BGN_DATE <- as.Date(StormData$BGN_DATE, "%m/%d/%Y")
StormData$Year<-year(StormData$BGN_DATE)
StormData<-filter(StormData, Year>=1996)
StormData <- filter(StormData, PROPDMG > 0 | CROPDMG > 0 | FATALITIES > 0 | INJURIES > 0)

table(StormData$PROPDMGEXP)
table(StormData$CROPDMGEXP)
StormData$CROPDMGCOEF[(StormData$CROPDMGEXP %in% c("","?","0"))] <-  1
StormData$CROPDMGCOEF[(StormData$CROPDMGEXP == "2")] <-  1e2
StormData$CROPDMGCOEF[(StormData$CROPDMGEXP == "K")] <-  1e3
StormData$CROPDMGCOEF[(StormData$CROPDMGEXP == "M")] <-  1e6
StormData$CROPDMGCOEF[(StormData$CROPDMGEXP == "B")] <-  1e9


StormData$PROPDMGCOEF[(StormData$PROPDMGEXP %in% c("","-","?","+","0 "))]<-1
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="1")]<-1e1
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="2")]<-1e2
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="3")]<-1e3
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="4")]<-1e4
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="5")]<-1e5
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="6")]<-1e6
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="7")]<-1e7
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="8")]<-1e8
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="H")]<-1e2
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="K")]<-1e3
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="M")]<-1e6
StormData$PROPDMGCOEF[(StormData$PROPDMGEXP=="B")]<-1e9

StormData <- mutate(StormData, Health_impact = FATALITIES + INJURIES)
StormData <- mutate(StormData, Economic_cost = PROPDMG * PROPDMGCOEF + CROPDMG * CROPDMGCOEF)
StormData$EVTYPE <- toupper(StormData$EVTYPE)

HealthImpact_event<-summarise(group_by(StormData,EVTYPE),Health_impact=sum(Health_impact))
HealthImpact_event_principal<-arrange(HealthImpact_event, desc(Health_impact))[1:10,]

HealthImpact_event_principal$EVTYPE[HealthImpact_event_principal$EVTYPE=="HURRICANE/TYPHOON"]<-"HURRICANE (TYPHOON)"
HealthImpact_event_principal$EVTYPE[HealthImpact_event_principal$EVTYPE=="TSTM WIND"]<-"THUNDERSTORM WIND"



EconomicCost_event<-summarise(group_by(StormData,EVTYPE),Economic_cost=sum(Economic_cost))
EconomicCost_event_principal<-arrange(EconomicCost_event, desc(Economic_cost))[1:10,]

EconomicCost_event_principal$EVTYPE[EconomicCost_event_principal$EVTYPE=="HURRICANE"]<-"HURRICANE (TYPHOON)"
EconomicCost_event_principal$EVTYPE[EconomicCost_event_principal$EVTYPE=="STORM SURGE"]<-"STORM SURGE/TIDE"



HealthImpact_event_final<-summarise(group_by(HealthImpact_event_principal,EVTYPE),Health_impact=sum(Health_impact))
HealthImpact_event_final<-arrange(HealthImpact_event_final,desc(Health_impact))

g1 <- ggplot(HealthImpact_event_final, aes(x=reorder(EVTYPE, Health_impact),y= Health_impact,fill=EVTYPE)) + 
  geom_bar(stat="identity") + 
  xlab("Event") + ylab("Number of fatalities and injuries") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") +
  ggtitle("Fatalities and injuries in the US caused by severe weather events")
g1
dev.copy(png, "Health_impact.png")
dev.off()


EconomicCost_event_final<-summarise(group_by(EconomicCost_event_principal,EVTYPE),Economic_cost=sum(Economic_cost))
EconomicCost_event_final<-arrange(EconomicCost_event_final,desc(Economic_cost))
g2 <- ggplot(EconomicCost_event_final, aes(x=reorder(EVTYPE,Economic_cost),y= Economic_cost,fill=EVTYPE)) + 
  geom_bar(stat="identity") + 
  xlab("Event") + ylab("Economic Cost in USD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") +
  ggtitle("Economic cost in the US caused by severe weather events")
g2
dev.copy(png, "Economic_cost.png")
dev.off()

