#data<-MHL_19_331_121_Efficacy <- read_excel("C:/Users/edmondsonef/Desktop/MHL 19-331-121 Efficacy.xlsx", sheet = "Initial CBC")
library(ggplot2)
library(gridExtra)
library(patchwork)
library(readxl)
#library(ggpubr)
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
###Generate Data


### WBC  
my_mean = aggregate(data$WBC, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$WBC , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

### WBC Plot
WBC <- ggplot(data) + 
  scale_y_continuous(name = "White Blood Cells") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = WBC, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### Neutrophils
my_mean = aggregate(data$NE, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$NE , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0.46)
my_info$ref.hi = c(6.04)

### Nuetrophils plot
NE <- ggplot(data) + 
  scale_y_continuous(name = "Neutrophils") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = NE, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### Lymphocyes
my_mean = aggregate(data$LY, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$LY , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(1.25)
my_info$ref.hi = c(6.99)

### Lymphocyes plot
LY <- ggplot(data) + 
  scale_y_continuous(name = "Lymphocyes") +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = LY, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### Monocytes 
my_mean = aggregate(data$MO, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MO , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0)
my_info$ref.hi = c(1.04)

### Monocytes plot
MO <- ggplot(data) + 
  scale_y_continuous(name = "Monocytes") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MO, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### Eosinophils
my_mean = aggregate(data$EO, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$EO , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0)
my_info$ref.hi = c(0.25)

### Eosinophils plot
EO <- ggplot(data) + 
  scale_y_continuous(name = "Eosinophils") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = EO, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### Basophils 
my_mean = aggregate(data$BA, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$BA , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0)
my_info$ref.hi = c(0.23)

### Basophils plot
BA <- ggplot(data) + 
  scale_y_continuous(name = "Basophils") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = BA, shape = Sex, color = `Groups`), width = 0.1, height = 0.001, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())



### Platelets
my_mean = aggregate(data$PLT, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$PLT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(305.79)
my_info$ref.hi = c(1444.39)

### Platelets plot
PLT <- ggplot(data) + 
  scale_y_continuous(name = "Platelets") + 
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = PLT, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())




## RBC ###
## RBC ###
## RBC ###
## RBC ###
## RBC ###
## RBC ###
## RBC ###
## RBC ###
## RBC ###
## RBC ###
## RBC ###
## RBC ###

### Hematocrit
my_mean = aggregate(data$HCT, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$HCT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(36.85)
my_info$ref.hi = c(50.71)

### Hematocrit plot
ymxHCT = max(data$HCT, na.rm = T)
yminHCT = min(data$HCT, na.rm = T)
HCT <- ggplot(data) + 
  scale_y_continuous(name = "Hematocrit", limits=c(yminHCT*.98, ymxHCT*1.02)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = HCT, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### Reticulocytes
my_mean = aggregate(data$Retics, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Retics , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(6)
my_info$ref.hi = c(6)

### Reticulocytes plot
ymxRetics = max(data$Retics, na.rm = T)
yminRetics = min(data$Retics, na.rm = T)
Retics <- ggplot(data) + 
  scale_y_continuous(name = "Reticulocytes", limits=c(yminRetics*.98, ymxRetics*1.02)) + 
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = Retics, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### MCH
my_mean = aggregate(data$MCH, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MCH , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(14)
my_info$ref.hi = c(20)

### MCH plot
ymxMCH = max(data$MCH, na.rm = T)
yminMCH = min(data$MCH, na.rm = T)
MCH <- ggplot(data) + 
  scale_y_continuous(name = "MCH", limits=c(yminMCH*.98, ymxMCH*1.02))+#, limits = c(12, 22)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MCH, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### MCV
my_mean = aggregate(data$MCV, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MCV , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(47)
my_info$ref.hi = c(67)

### MCV plot
ymxMCV = max(data$MCV, na.rm = T)
yminMCV = min(data$MCV, na.rm = T)
MCV <- ggplot(data) + 
  scale_y_continuous(name = "MCV", limits=c(yminMCV*.98, ymxMCV*1.02))+#, limits = c(35, 70)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MCV, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### MCHC
my_mean = aggregate(data$MCHC, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MCHC, by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(28)
my_info$ref.hi = c(33)

### MCHC plot
ymxMCHC = max(data$MCHC, na.rm = T)
yminMCHC = min(data$MCHC, na.rm = T)
MCHC <- ggplot(data) + 
  scale_y_continuous(name = "MCHC", limits=c(yminMCHC*.98, ymxMCHC*1.02))+#, limits = c(27, 40)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MCHC, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### Hb
my_mean = aggregate(data$Hb, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Hb, by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(11)
my_info$ref.hi = c(15.6)

### Hb plot
ymxHb = max(data$Hb, na.rm = T)
yminHb = min(data$Hb, na.rm = T)
Hb <- ggplot(data) + 
  scale_y_continuous(name = "Hb", limits=c(yminHb*.98, ymxHb*1.02))+#, limits = c(10, 18)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = Hb, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### RBC
my_mean = aggregate(data$RBC, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$RBC, by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(6)
my_info$ref.hi = c(11)

### RBC plot
ymxRBC = max(data$RBC, na.rm = T)
yminRBC = min(data$RBC, na.rm = T)
RBC <- ggplot(data) + 
  scale_y_continuous(name = "RBC", limits=c(yminRBC*.98, ymxRBC*1.02))+#, limits = c(5.8, 11.3)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = RBC, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### RDW
my_mean = aggregate(data$RDW, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$RDW, by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(13)
my_info$ref.hi = c(20)

### RDW plot
ymxRDW = max(data$RDW, na.rm = T)
yminRDW = min(data$RDW, na.rm = T)
RDW <- ggplot(data) + 
  scale_y_continuous(name = "RDW", limits=c(yminRDW*.98, ymxRDW*1.02))+#, limits = c(11, 21)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = RDW, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())



###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS
###PLATELETS

### Platelets
my_mean = aggregate(data$PLT, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$PLT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(305.79)
my_info$ref.hi = c(1444.39)

### Platelets plot
PLT <- ggplot(data) + 
  scale_y_continuous(name = "Platelets") + 
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = PLT, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### MPV
my_mean = aggregate(data$MPV, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MPV , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(4)
my_info$ref.hi = c(10.6)

### Platelets plot
MPV <- ggplot(data) + 
  scale_y_continuous(name = "MPV", limits  = c(3.5, 10.65)) + 
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MPV, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())






### Generate Multiplots
#library(patchwork)

#tiff("01. WBC+PLT.tiff", units="in", width=11, height=5, res=300)
#grid.arrange(WBC, NE, LY, PLT, MO, EO, BA, MPV, ncol = 4, nrow = 2, top = "CBC: WBC and Platelet Counts")
#dev.off()

# tiff("01. WBC+PLT.tiff", units="in", width=13, height=10, res=300)
# WBC + NE + LY + MO + EO + BA + PLT + MPV + 
#   plot_layout(guides = "collect") + 
#   plot_annotation(title = "CBC: White Blood Cell and Platelet Counts")
# dev.off()

tiff("WBC+PLT.tiff", units="in", width=11, height=8, res=200)
(WBC | NE) / 
  (LY | MO) /
  (EO | BA) /
  (PLT | MPV) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "CBC: White Blood Cell and Platelet Counts")
dev.off()

#tiff("02. RBC.tiff", units="in", width=11, height=5, res=300)
#grid.arrange(HCT, RBC, Hb, Retics, MCH, MCHC, MCV, RDW, ncol = 4, nrow = 2, top = "CBC: Erythroid")
#dev.off()


tiff("RBC.tiff", units="in", width=11, height=8, res=200)
(HCT | RBC) / 
  (Hb | MCH) /
  (MCHC | MCV) /
  (Retics | RDW) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "CBC: Erythroid Parameters")
dev.off()
