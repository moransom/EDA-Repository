#### Assignment 3 Data ####
#Load in the SkuMaster.csv data set
SkuMaster<-read.csv("6 SKU Master.csv")

#Only consider Cubic Feet per UOM >= 0 and < 2
dat<-SkuMaster[SkuMaster$UomCube>0 & SkuMaster$UomCube<2, ]

#Weight per UOM is >= 0 and < 50
dat1<-dat[dat$UomWeight>0 & dat$UomWeight<50, ]

#Omit levels except CA, EA, PL (pallet and pound) for UoM
dat1$Uom<-as.factor(dat1$Uom)
dat2<-dat1[dat1$Uom=="CA" | dat1$Uom=="EA" | dat1$Uom== "PL" | dat1$Uom=="LB", ]

#Omit rows with NA
dat3<-na.omit(dat2)

#Remove leftover levels
dat4<-droplevels(dat3)

#### Step 1 ####
install.packages("greybox")
library(greybox)
dat4$Flow<-as.character(dat4$Flow)
dat4$Whs<-as.character(dat4$Whs)
dat4$Uom<-as.character(dat4$Uom)
tableplot(dat4$Flow,dat4$Whs)
cramer(dat4$Flow,dat4$Whs)
tableplot(dat4$Flow,dat4$Uom)
cramer(dat4$Flow,dat4$Uom)
tableplot(dat4$Whs,dat4$Uom)
cramer(dat4$Whs,dat4$Uom)
#Findings: Flow and Whs have the greatest association, followed by Whs and Uom
#Least association: Flow and Uom
#### Step 2 ####
all<-data.frame(dat4$Flow,dat4$Whs,dat4$Uom)
colnames(all)<-c("Flow","Whs","Uom")
assoc(all)
spread(all)
#### Step 3 ####
all<-data.frame(dat4$Flow,dat4$Whs,dat4$Uom,dat4$UnitsPerCase,dat4$LeadTime,dat4$UomCube,dat4$UomWeight,dat4$ShelfLifeDays)
colnames(all)<-c("Flow","Whs","Uom","UnitsPerCase","LeadTime","UomCube","UomWeight","ShelfLifeDays")
assoc(all)
spread(all)
#Findings: There is a moderate relationship between several correlations. 
#Highest correlation: Whs & ShelfLifeDays = 0.5188
#Second Highest: UoM & ShelfLifeDays = 0.3874
#Third Highest: Whs & UoMWeight = 0.3489