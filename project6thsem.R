# setting the WD
setwd("C:/Users/HP/Desktop/Crab Project")

#Reading the data
crabdata = read.csv("CrabAgePrediction_ani.csv")

# View the dataset
View(crabdata)
nrow(crabdata) # counting the number of rows

ncol(crabdata) # counting the number of coloumn 
#summary 
summary(crabdata)
#Loading required libraries
library(readxl)
library(dplyr)
library(moments)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(visdat)
library(lattice)
library(caret)
library(magrittr)
library(pROC)
library(broom)
library(GGally)
# Visualise Missing values
sum(is.na(crabdata))
vis_miss(crabdata)
#barplot count data
ggplot(crabdata,aes(x=Sex,fill=Sex))+geom_bar()+labs(title="Number of Crab by Sex",x="sex",y="count")+theme_minimal()+scale_color_brewer(palette ="PuOr")
ggplot(crabdata,aes(x=Sex,fill=Sex))+geom_bar()+labs(title="Number of Crab by Sex",x="sex",y="count")+theme_minimal()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
count(crabdata,crabdata$Sex=='M') # counting the number of male crab
count(crabdata,crabdata$Sex=='F') # counting the number of female crab
count(crabdata,crabdata$Sex=='I') # counting the number of indeterminate crab
library(MASS) # calling the required library

## creating a dataset which contains age, sex, weight
bardata=data.frame(crabdata$Age,crabdata$Sex,crabdata$Weight)
View(bardata)
u=unique(crabdata$Age) ## checking for the range of the Age
sort(u)
##creating a multiple bar diagram for three different gender category with respect to age and weight
ggplot(bardata,aes(fill=crabdata.Sex,y=crabdata.Weight,x=crabdata.Age))+geom_bar(position="dodge",stat = "identity",names(1:29))+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#plotting Shucked weight against Age of crabs
plot(crabdata$Age,crabdata$Shucked_Weight,xlab="Age",ylab = "Shucked Weight" ,col="chartreuse",pch=1,main="Relationship between Shucked weight and Age") 

#adding a trend line to shucked weight and age 
library(sqldf)
##creating the Summary of average shucked weight corresponding to distinct Ages
SW_A= sqldf("select Age , min(shucked_weight),max(shucked_weight),sum(shucked_weight), count(*) from crabdata group by Age")
library(openxlsx)
#saving in excel file
write.xlsx(SW_A, "summary_crab_age.xlsx")

#plotting Shucked weight against Diameter of crabs
plot(crabdata$Diameter,crabdata$Shucked_Weight,pch=4,col="purple",main="Scatterplot between diameter and shucked weight",xlab = "Diameter",ylab = "Shucked Weight")

##creating the Summary of average shucked weight corresponding to Diameter.
SW_D= sqldf("select Diameter , min(shucked_weight),max(shucked_weight),sum(shucked_weight), count(*) from crabdata group by Diameter")
write.xlsx(SW_A, "Diameter_summary1.xlsx") ## saving it as a excel file for the trend line.
#plotting Shucked weight against Length of crabs
plot(crabdata$Length,crabdata$Shucked_Weight,col="navy",pch=7,main="Scatterplot between Shucked weight and Length",xlab = "Length",ylab = "Shucked Weight")

##creating the Summary of average shucked weight corresponding to Length.
SW_L= sqldf("select Length , min(shucked_weight),max(shucked_weight),sum(shucked_weight), count(*) from crabdata group by Length")
write.xlsx(SW_L, "Length_summary.xlsx") ## saving it as a excel file for the trend line.

#plotting Shucked weight against Height of crabs
plot(crabdata$Height,crabdata$Shucked_Weight,col="#00FF00",pch=1,main="Scatterplot between Height and Shucked Weight",xlab = "Height",ylab = "Shucked Weight") 
##creating the Summary of average shucked weight corresponding to Height .
SW_H= sqldf("select Height , min(shucked_weight),max(shucked_weight),sum(shucked_weight), count(*) from crabdata group by Height")


write.xlsx(SW_H, "Height_summary.xlsx") ## saving it as a excel file for the trend line.

install.packages("corrplot") # installing the required library
library(corrplot) # calling the required library
# we are creating boxplots of 8 variables with respect of Gender  to check the  presence of outlier
ggplot(crabdata) + geom_boxplot(aes(x=Sex, y=Length, color=Sex))+ggtitle("Boxplot between length and sex")

ggplot(crabdata) + geom_boxplot(aes(x=Sex, y=Diameter, color=Sex))+ggtitle("Boxplot between Diameter and Sex")

ggplot(crabdata) + geom_boxplot(aes(x=Sex, y=Height, color=Sex))+ggtitle("Boxplot between Height and Sex")

ggplot(crabdata) + geom_boxplot(aes(x=Sex, y=Weight, color=Sex))+ggtitle("Boxplot between Weight and Sex")

ggplot(crabdata) + geom_boxplot(aes(x=Sex, y=Shucked_Weight, color=Sex))+ggtitle("Boxplot between Shucked Weight and Sex")

ggplot(crabdata) + geom_boxplot(aes(x=Sex, y=Viscera_Weight, color=Sex))+ggtitle("Boxplot between Viscera Weight and Sex")

ggplot(crabdata) + geom_boxplot(aes(x=Sex, y=Shell_Weight, color=Sex))+ggtitle("Boxplot between Shell Weight and Sex")

ggplot(crabdata) + geom_boxplot(aes(x=Sex, y=Age, color=Sex))+ggtitle("Boxplot between age and Sex")



data_numeric = select_if(crabdata, is.numeric) ## we are selecting those colums which has numeric value

data_corr <- cor(data_numeric) ## creating  correlation between those numeric variables

data_corr1=as.matrix(data_corr) ## making it as a correlation matirix

glimpse(crabdata)# having a View
#correlation plot
corrplot(data_corr1, method = "square", type = "upper", tl.col = "black")
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(data_corr1,method="square",type= "lower",lab=TRUE, lab_col="black",lab_size= 5, outline.color="white",tl.cex=20,legend.title= "") + theme(axis.text.x=element_text(angle=90,hjust=1),panel.background = element_rect(fill="white"),panel.grid.major = element_blank(),panel.border = element_blank(),legend.key.height = unit(7,"cm"),legend.key.width = unit(1,"cm"))
ggcorrplot(data_corr1,method="square",type= "lower",lab=TRUE, lab_col="black")

#Subsetting the data in two parts with length less than and grater than median length

a = median(crabdata$Length) # the median length is 1.3625
a
# data for crabs with length less than that of the median length
crabdata_small = crabdata[crabdata$Length< a,]


#Check
max(crabdata_small$Length) ## max is coming out to be 1.35


# data for crabs with length more than that of the median length
crabdata_big = crabdata[crabdata$Length>= median(crabdata$Length),]

#Check
min(crabdata_big$Length) ## min is coming out to be 1.3625

# plotting the scatter plot of weight vs Age for the small crabs

plot(crabdata_small$Age, crabdata_small$Shucked_Weight, main = "Scatterplot of Shucked Weight vs Age for Small Crabs",xlab="Shucked Weight", ylab="Age")

library(dplyr)
library(openxlsx)

data_small_crab = crabdata_small %>% group_by(Age) %>% summarise(average = mean(Shucked_Weight))
write.xlsx(data_small_crab, "Small_Crab_age_vs_shucked_wight.xlsx")

# plotting the scatter plot of weight vs Age for the Big crabs

plot(crabdata_big$Age, crabdata_big$Shucked_Weight, main = "Scatterplot of Shucked Weight vs Age for Big Crabs",xlab=" Shucked Weight", ylab="Age")


data_big_crab = crabdata_big %>% group_by(Age) %>% summarise(average = mean(Shucked_Weight))
write.xlsx(data_big_crab, "Big_Crab_age_vs_shucked_weight.xlsx")

#Subsetting the data in two parts with diameter less than and grater than median Diameter

b = median(crabdata$Diameter) # the median length is 1.0625
b
# data for crabs with Diameter less than that of the median diameter
crabdata_small_D = crabdata[crabdata$Diameter< b,]

#Check
max(crabdata_small$Diameter) ## max is coming out to be 1.1875, so its fine


# data for crabs with Diameter more than that of the median Diameter
crabdata_big = crabdata[crabdata$Diameter>= median(crabdata$Diameter),]

#Check
min(crabdata_big$Diameter) ## min is coming out to be 1.0625, so its fine

# plotting the scatter plot of weight vs Age for the  Diameter small crabs, this will give you a simple plot

plot(crabdata_small$Age, crabdata_small$Shucked_Weight, main = "Age vs Shucked Weight for Small Crabs with respect to Diameter", xlab = "Age", ylab= "Shucked Weight")
library(dplyr)
library(openxlsx)

data_small_crab = crabdata_small %>% group_by(Age) %>% summarise(average = mean(Shucked_Weight))
write.xlsx(data_small_crab, "Diameter_Small_Crab_age_vs_shucked_wight.xlsx")
# plotting the scatter plot of weight vs Age for the Diameter Big crabs

plot(crabdata_big$Age, crabdata_big$Shucked_Weight, main = "Age vs Shucked Weight for Big Crabs",xlab="Age", ylab="Shcuked weight")


data_big_crab = crabdata_big %>% group_by(Age) %>% summarise(average = mean(Shucked_Weight))
write.xlsx(data_big_crab, "Diameter_Big_Crab_age_vs_shucked_wight.xlsx")
##correlation between length and diameter
cor(crabdata_small$Length,crabdata_small$Diameter)

                           ##****Developing Model****##
## we are fitting model with the transformed predictor accordingly.
 
summary(lm(Shucked_Weight~log(Age)-1,data=crabdata_small))

summary(lm(Shucked_Weight~(Length)^2-1,data=crabdata_small))

summary(lm(Shucked_Weight~(Diameter)^2-1,data=crabdata_small)) 

summary(lm(Shucked_Weight~(Height)^2-1,data=crabdata_small)) 

summary(lm(Shucked_Weight~log(Age)+(Length)^2-1,data=crabdata_small))

summary(lm(Shucked_Weight~log(Age)+(Diameter)^2-1,data=crabdata_small))

summary(lm(Shucked_Weight~log(Age)+(Height)^2-1,data=crabdata_small))

summary(lm(Shucked_Weight~log(Age)+(Length)^2+(Diameter)^2-1,data=crabdata_small))

summary(lm(Shucked_Weight~log(Age)+(Length)^2+(Height)^2-1,data=crabdata_small))

summary(lm(Shucked_Weight~log(Age)+(Diameter)^2+(Height)^2-1,data=crabdata_small))

################################################################################
     ## -- considering interaction in regression ---##

crabdata$size=NULL ## creating an extra column to our dataset for size

crabdata$size[crabdata$Length<median(crabdata$Length)]="S" ## considering those crabs as small one whose length is less than the median length

crabdata$size[crabdata$Length>=median(crabdata$Length)]="B" ## considering those crabs as big one whose length is graeter than the median length

crabdata$size=as.factor(crabdata$size) # making it as a factor

## let us take the combination of our predictor to see how it is affecting our response.

model_I1=lm(Shucked_Weight~log(Age)+size+log(Age):size-1,data=crabdata)

summary(model_I1)

model_I2=lm(Shucked_Weight~log(Age)+(Length)^2+size+log(Age):size-1,data=crabdata)

summary(model_I2)

model_I3=lm(Shucked_Weight~log(Age)+(Diameter)^2+size+log(Age):size-1,data=crabdata)

summary(model_I3)

summary(lm(Shucked_Weight~log(Age)+(Height)^2+size+log(Age):size-1,data=crabdata))


