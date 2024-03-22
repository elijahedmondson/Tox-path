
#data<-MHL_19_331_121_Efficacy <- read_excel("C:/Users/edmondsonef/Desktop/MHL 19-331-121 Efficacy.xlsx", sheet = "plot")

library(ggplot2)
library(gridExtra)
library(readxl)
library(ggpubr)
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
###Generate Data
#data <- read_excel("ADME Tox 202.xlsx", sheet = "Chemistry")
#CBC <- read_excel("ADME Tox 202.xlsx", sheet = "CBC")
#AData <- read_excel("ADME Tox 202.xlsx", sheet = "Animal Data")

### Albulmin:Globulin Ratio
data<-dplyr:: mutate(data, AGR = (ALB/(TP-ALB)))
my_mean = aggregate(data$AGR, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$AGR , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
#my_info$ref.low = c()
#my_info$ref.hi = c()

### AGR Plot
ymxAGR = max(data$AGR, na.rm = T)
yminAGR = min(data$AGR, na.rm = T)
AGR <- ggplot(data) + 
  scale_y_continuous(name = "Albumin:Globulin", limits=c(yminAGR*.98, ymxAGR*1.02)) +
  ##geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = AGR, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### BUN:Creatinine Ratio
data<-dplyr:: mutate(data, BCR = (BUN/CRE))
my_mean = aggregate(data$BCR, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$BCR , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
#my_info$ref.low = c()
#my_info$ref.hi = c()

### BCR Plot
ymxBCR = max(data$BCR, na.rm = T)
yminBCR = min(data$BCR, na.rm = T)
BCR <- ggplot(data) + 
  scale_y_continuous(name = "BUN:Creatinine", limits=c(yminBCR*.98, ymxBCR*1.02)) +
  ##geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = BCR, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### ALB  
my_mean = aggregate(data$ALB, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALB , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.77)
my_info$ref.hi = c(4.8)

### ALB Plot
ymxALB = max(data$ALB, na.rm = T)
yminALB = min(data$ALB, na.rm = T)
ALB <- ggplot(data) + 
  scale_y_continuous(name = "Albumin", limits=c(yminALB*.98, ymxALB*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = ALB, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### ALP
my_mean = aggregate(data$ALP, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALP , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(41)
my_info$ref.hi = c(190)

### ALP plot
ymxALP = max(data$ALP, na.rm = T)
yminALP = min(data$ALP, na.rm = T)
ALP <- ggplot(data) + 
  scale_y_continuous(name = "ALP", limits=c(yminALP*.98, ymxALP*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = ALP, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### ALT
my_mean = aggregate(data$ALT, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(21)
my_info$ref.hi = c(168)

### ALT plot
ymxALT = max(data$ALT, na.rm = T)
yminALT = min(data$ALT, na.rm = T)
ALT <- ggplot(data) + 
  scale_y_continuous(name = "ALT", limits=c(yminALT*.98, ymxALT*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = ALT, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### BUN 
my_mean = aggregate(data$BUN, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$BUN , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(10.7)
my_info$ref.hi = c(34.2)

### BUN plot
ymxBUN = max(data$BUN, na.rm = T)
yminBUN = min(data$BUN, na.rm = T)
BUN <- ggplot(data) + 
  scale_y_continuous(name = "BUN", limits=c(yminBUN*.98, ymxBUN*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = BUN, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### CREATININE
my_mean = aggregate(data$CRE, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$CRE , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0.2)
my_info$ref.hi = c(0.5)

### CREATININE plot
ymxCRE = max(data$CRE, na.rm = T)
yminCRE = min(data$CRE, na.rm = T)
CRE <- ggplot(data) + 
  scale_y_continuous(name = "Creatinine", limits=c(yminCRE*.98, ymxCRE*1.02)) +
  ##geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = CRE, shape = Sex, color = `Groups`), height = 0.01, width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### GLU 
my_mean = aggregate(data$GLU, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$GLU , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(115)
my_info$ref.hi = c(292)

### GLU plot
ymxGLU = max(data$GLU, na.rm = T)
yminGLU = min(data$GLU, na.rm = T)
GLU <- ggplot(data) + 
  scale_y_continuous(name = "GLU", limits=c(yminGLU*.98, ymxGLU*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = GLU, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### TP
my_mean = aggregate(data$TP, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$TP , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(4.6)
my_info$ref.hi = c(6.6)

### TP plot
ymxTP = max(data$TP, na.rm = T)
yminTP = min(data$TP, na.rm = T)
TP <- ggplot(data) + 
  scale_y_continuous(name = "Total Protein", limits=c(yminTP*.98, ymxTP*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = TP, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### GLOB
my_mean = aggregate(data$GLOB, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$GLOB , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0.5)
my_info$ref.hi = c(3.1)

### GLOB plot
ymxGLOB = max(data$GLOB, na.rm = T)
yminGLOB = min(data$GLOB, na.rm = T)
GLOB <- ggplot(data) + 
  scale_y_continuous(name = "GLOB", limits=c(yminGLOB*.98, ymxGLOB*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = GLOB, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### Ca
my_mean = aggregate(data$Ca, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Ca , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(9.77)
my_info$ref.hi = c(12.2)

### Ca plot
ymxCa = max(data$Ca, na.rm = T)
yminCa = min(data$Ca, na.rm = T)
Ca <- ggplot(data) + 
  scale_y_continuous(name = "Ca2+", limits=c(yminCa*.98, ymxCa*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = Ca, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### PHOS
my_mean = aggregate(data$PHOS, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$PHOS , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(9.1)
my_info$ref.hi = c(13.1)

### PHOS plot
ymxPHOS = max(data$PHOS, na.rm = T)
yminPHOS = min(data$PHOS, na.rm = T)
PHOS <- ggplot(data) + 
  scale_y_continuous(name = "PHOS", limits=c(yminPHOS*.98, ymxPHOS*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = PHOS, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### NaPlus
my_mean = aggregate(data$`Na Plus`, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$`Na Plus`, by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(124)
my_info$ref.hi = c(174)

### NaPlus plot
ymxNa = max(data$`Na Plus`, na.rm = T)
yminNa = min(data$`Na Plus`, na.rm = T)
NaPlus <- ggplot(data) + 
  scale_y_continuous(name = "Na+", limits=c(yminNa*.98, ymxNa*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = `Na Plus`, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### KPlus
my_mean = aggregate(data$KPlus, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$KPlus , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(6)
my_info$ref.hi = c(11.8)

### KPlus plot
ymxKPlus = max(data$KPlus, na.rm = T)
yminKPlus = min(data$KPlus, na.rm = T)
KPlus <- ggplot(data) + 
  scale_y_continuous(name = "K+", limits=c(yminKPlus*.98, ymxKPlus*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = KPlus, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### TBIL
my_mean = aggregate(data$TBIL, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$TBIL , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0.1)
my_info$ref.hi = c(0.6)

### TBIL plot
ymxTBIL = max(data$TBIL, na.rm = T)
yminTBIL = min(data$TBIL, na.rm = T)
TBIL <- ggplot(data) + 
  scale_y_continuous(name = "TBIL", limits=c(yminTBIL*.98, ymxTBIL*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = TBIL, shape = Sex, color = `Groups`), width = 0.1, height = 0.001)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### AMY
my_mean = aggregate(data$AMY, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$AMY , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(200)
my_info$ref.hi = c(1200)

### AMY plot
ymxAMY = max(data$AMY, na.rm = T)
yminAMY = min(data$AMY, na.rm = T)
AMY <- ggplot(data) + 
  scale_y_continuous(name = "AMY", limits=c(yminAMY*.98, ymxAMY*1.02)) +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = AMY, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())



tiff("CHEMISTRY.tiff", units="in", width=15, height=10, res=200)
(TP | ALB | GLOB | AGR) / 
  (BUN | CRE | BCR | GLU) /
  (Ca | PHOS | KPlus | NaPlus) /
  (ALP | ALT | TBIL | AMY) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Clinical Chemistry")
dev.off()


