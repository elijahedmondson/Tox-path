
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
AGR <- ggplot(data) + 
  scale_y_continuous(name = "Albumin:Globulin") +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
BCR <- ggplot(data) + 
  scale_y_continuous(name = "BUN:Creatinine") +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
ALB <- ggplot(data) + 
  scale_y_continuous(name = "Albumin", limits = c(2.2,5.5)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
ALP <- ggplot(data) + 
  scale_y_continuous(name = "ALP") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
ALT <- ggplot(data) + 
  scale_y_continuous(name = "ALT") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
BUN <- ggplot(data) + 
  scale_y_continuous(name = "BUN") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
CRE <- ggplot(data) + 
  scale_y_continuous(name = "Creatinine") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  ##geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = CRE, shape = Sex, color = `Groups`), height = 0.005, width = 0.1)+
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
GLU <- ggplot(data) + 
  scale_y_continuous(name = "GLU") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
TP <- ggplot(data) + 
  scale_y_continuous(name = "Total Protein", limits = c(4.3,7)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
GLOB <- ggplot(data) + 
  scale_y_continuous(name = "GLOB") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
Ca <- ggplot(data) + 
  scale_y_continuous(name = "Ca2+", limits=c(8,13.5)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
PHOS <- ggplot(data) + 
  scale_y_continuous(name = "PHOS") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
NaPlus <- ggplot(data) + 
  scale_y_continuous(name = "Na+", limits=c(110,185)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
KPlus <- ggplot(data) + 
  scale_y_continuous(name = "K+") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
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
TBIL <- ggplot(data) + 
  scale_y_continuous(name = "TBIL") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = TBIL, shape = Sex, color = `Groups`), width = 0.1)+
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
AMY <- ggplot(data) + 
  scale_y_continuous(name = "AMY") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = AMY, shape = Sex, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())



tiff("CHEMISTRY.tiff", units="in", width=14, height=12, res=200)
(TP | AL | GLOB | AGR) / 
  (BUN | CRE | BCR | GLU) /
  (Ca | PHOS | KPlus | NaPlus) /
  (ALP | ALT | TBIL | AMY) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Clinical Chemistry")
dev.off()


