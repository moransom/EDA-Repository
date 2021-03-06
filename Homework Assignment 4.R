#### Data set up ####
#Load in the SkuMaster.csv data set
dat<-read.csv("https://raw.githubusercontent.com/moransom/EDA-Repository/main/6%20SKU%20Master.csv")
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


#### Boxplot: Weight per UOM ####
#Create a boxplot on Weight per UOM
boxplot(dat4$UomWeight,
        main="Weight per UoM",
        col="chocolate",
        border="gray35",
        ylim=c(-3,55),
        outpch=21,
        cex=.5,
        outbg="darkgreen")

#### Scatterplot: Units per Case and Weight per UOM ####
#Create a scatterplot on the Units per Case and the Weight per UOM
dat4$UnitsPerCase<-as.numeric(dat4$UnitsPerCase)
plot(dat4$UnitsPerCase,dat4$UomWeight,
     main="Units per Case vs. Weight per UoM",
     xlab="Units per Case",
     ylab="Weight per UoM",
     cex=.5,
     pch=21,
     bg="springgreen")

#### Plot: Frequency of Commodity levels ####
#Create a plot showing the frequency/count with which the levels of the factor Commodity occur
dat4$Commodity<-as.factor(dat4$Commodity)
plot(dat4$Commodity,
     xlab="Commodity",
     ylab="Frequency",
     main="Frequency of Commodity Levels",
     ylim=c(0,1600),
     cex.axis=.7,cex.names=.55,las=2,
     col=rainbow(8))
x<-c("BK00","BK1505","BK20","BV00","BV05","BV0505",
     "BV0510","BV10","BV15","BV25","BV30","DM00",  
     "DR00","DR05","DR10","DR15","DR2510","DS00",  
     "DS05","DS0505","DS0515","DS0520","DS0525","DS0530",
     "DS10","DS1005","DS1010","DS1015","DS1025","DS15",  
     "GR00","GR10","GR20","GR30","GR35","GR45",  
     "GR50","GR5005","GR5010","GR5015","GR55","GR60",  
     "IC00","ME00","ME05","ME15","PD00","PD05",  
     "PD0505","PD0510","PD1005","PD1010","PD1015","PO00",  
     "PO05","PO20","SE00","SE05","SE0520")
legend("top",x,
       inset=c(.2,.0000001),
       ncol=8,
       pch=c(21),
       cex=.35,
       fill=rainbow(59))

#### Plot: Frequency of UOM levels ####
#Create a plot showing the frequency/count with which the levels of the Units of Measure occur
plot(dat4$Uom,
     xlab="UoM",
     ylab="Frequency",
     main="Frequency of UoM levels",
     ylim=c(0,3000),
     cex.axis=.6,cex.names=.7, las=1,
     col=rainbow(4))
x<-c("CA","EA","LB","PL")
legend("topright",x,
       ncol=2,
       cex=.8,
       fill=rainbow(4))

#### Side-By Side Boxplot: Cubic Feet by UOM by Flow Type ####
#Create a side-by-side boxplot of Cubic Feet by UoM by the types of Flow
dat4$Flow<-as.factor(dat4$Flow)
plot(dat4$Flow,dat4$UomCube)

#Restrict to products that pass through Direct to Store supply chain channel
dat5<-dat4[dat4$Flow=="DD", ]


#### Boxplot: Weight per UOM ####
#Create a boxplot on the Weight per UOM
boxplot(dat5$UomWeight)

#Q: Which observations/rows appear to be outliers?
#A: Observations 20, 29, and 17.8 appear to be outliers. 
boxplot.stats(dat5$UomWeight)$out

#### Histogram of Weight per UOM ####
#Create a histogram on Weight per UoM
hist(dat5$UomWeight)


#### Dotplot: Weight per UOM ####
#Create a dotplot on Weight per UoM labeled with the SKU Number. 
#Q: Which SKU Number has the highest Weight per UoM?
#A: 08992001
dotchart(dat5$UomWeight,dat5$SkuNbr)


#### Stripchart: Weight per UOM by UOM ####
#Create a stripchart for Weight per UoM by the Units of Measure
#There should only be EA and CA
dat6<-dat5[dat5$Uom=="EA" | dat5$Uom=="CA", ]
stripchart(dat6$UomWeight~dat6$Uom)


