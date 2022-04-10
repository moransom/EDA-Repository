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
dat5<-data.frame(dat4$UnitsPerCase,dat4$LeadTime,dat4$UomCube,dat4$UomWeight,dat4$ShelfLifeDays)
dat5
colnames(dat5)<-c("UnitsPerCase","LeadTime","UomCube","UomWeight","ShelfLifeDays")
head(dat5)
#### Step 2 ####
cor(dat5)
install.packages("corrplot")
library(corrplot)
corrplot(cor(dat5),type="upper",method="number")
#### Step 3 ####
install.packages("Hmisc")
library(Hmisc)
dat5$UnitsPerCase<-as.numeric(dat5$UnitsPerCase)
dat5$LeadTime<-as.numeric(dat5$LeadTime)
dat5$ShelfLifeDays<-as.numeric(dat5$ShelfLifeDays)
rcorr(as.matrix(dat5),type="spearman")
#### Step 4 ####
plot(dat5$UomWeight,dat5$ShelfLifeDays)
cor(dat5$UomWeight,dat5$ShelfLifeDays)
cor.test(dat5$UomWeight,dat5$ShelfLifeDays)
#### Step 5 ####
cor(dat5$UomWeight,dat5$ShelfLifeDays,method="spearman")
cor.test(dat5$UomWeight,dat5$ShelfLifeDays,method="spearman")
#### Step 6 ####
#Step 4 Findings (Pearson): cor=-0.05414; p-value= 0.0001736
#Step 5 Findings (Spearman): cor=-0.009687; p-value=0.5021
#Discussion: The Pearson relationship has a stronger correlation and significant value than the Spearman relationship
#### Step 7 ####
plot(dat5$UomWeight,dat5$UomCube)
cor(dat5$UomWeight,dat5$UomCube)
cor.test(dat5$UomWeight,dat5$UomCube)
#### Step 8 ####
cor(dat5$UomWeight,dat5$UomCube,method="spearman")
cor.test(dat5$UomWeight,dat5$UomCube,method="spearman")
#### Step 9 ####
#Step 7 Findings (Pearson): cor= 0.3365; p-value < 2.2e-16
#Step 8 Findings (Spearman): cor=0.3268; p-value < 2.2e-16
#Discussion: The correlations of both relationships are nearly equal and the significance of both are the same, which is statistically significant