
#data<-MHL_19_331_121_Efficacy <- read_excel("C:/Users/edmondsonef/Desktop/MHL 19-331-121 Efficacy.xlsx", sheet = "Initial CBC")
library(ggplot2)
library(gridExtra)
library(readxl)
library(patchwork)
#library(ggpubr)
setwd("C:/Users/edmondsonef/Desktop/R-plots/")


library(dplyr)
data <- dplyr::filter(data, Censor == 1)

#####
#####WBC
#####

my_mean = aggregate(data$WBC, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$WBC , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ymin = ymin = mean - CIdiff
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

### WBC Plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
WBC <- ggplot(data) + 
  scale_y_continuous(name = "White Blood Cells", limits=c(yminWBC*.95, ymxWBC*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = WBC, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Neutrophils
my_mean = aggregate(data$NE, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$NE , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0.46)
my_info$ref.hi = c(6.04)

### Nuetrophils plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
NE <- ggplot(data) + 
  scale_y_continuous(name = "Neutrophils", limits=c(yminNE*.95, ymxNE*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = NE, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Lymphocyes
my_mean = aggregate(data$LY, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$LY , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(1.25)
my_info$ref.hi = c(6.99)

### Lymphocyes plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
LY <- ggplot(data) + 
  scale_y_continuous(name = "Lymphocyes", limits=c(yminLY*.95, ymxLY*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = LY, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Monocytes 
my_mean = aggregate(data$MO, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MO , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0)
my_info$ref.hi = c(1.04)

### Monocytes plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
MO <- ggplot(data) + 
  scale_y_continuous(name = "Monocytes", limits=c(yminMO*.95, ymxMO*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MO, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Eosinophils
my_mean = aggregate(data$EO, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$EO , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0)
my_info$ref.hi = c(0.25)

### Eosinophils plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
EO <- ggplot(data) + 
  scale_y_continuous(name = "Eosinophils", limits=c(yminEO*.95, ymxEO*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = EO, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Basophils 
my_mean = aggregate(data$BA, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$BA , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(0)
my_info$ref.hi = c(0.23)

### Basophils plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
BA <- ggplot(data) + 
  scale_y_continuous(name = "Basophils", limits=c(yminBA*.95, ymxBA*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = BA, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


#####
#####RBC
#####

### Hematocrit
my_mean = aggregate(data$HCT, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$HCT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(36.85)
my_info$ref.hi = c(50.71)

### Hematocrit plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
HCT <- ggplot(data) + 
  scale_y_continuous(name = "Hematocrit", limits=c(yminHCT*.95, ymxHCT*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = HCT, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Reticulocytes
my_mean = aggregate(data$Retics, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Retics , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(6)
my_info$ref.hi = c(6)

### Reticulocytes plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
Retics <- ggplot(data) + 
  scale_y_continuous(name = "Reticulocytes", limits=c(yminRET*.95, ymxRET*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = Retics, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### MCH
my_mean = aggregate(data$MCH, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MCH , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(14)
my_info$ref.hi = c(20)

### MCH plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
MCH <- ggplot(data) + 
  scale_y_continuous(name = "MCH", limits=c(yminMCH*.95, ymxMCH*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MCH, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### MCV
my_mean = aggregate(data$MCV, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MCV , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(47)
my_info$ref.hi = c(67)

### MCV plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
MCV <- ggplot(data) + 
  scale_y_continuous(name = "MCV", limits=c(yminMCV*.95, ymxMCV*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MCV, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### MCHC
my_mean = aggregate(data$MCHC, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MCHC, by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(28)
my_info$ref.hi = c(33)

### MCHC plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
MCHC <- ggplot(data) + 
  scale_y_continuous(name = "MCHC", limits=c(yminMCHC*.95, ymxMCHC*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MCHC, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Hb
my_mean = aggregate(data$Hb, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Hb, by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(11)
my_info$ref.hi = c(15.6)

### Hb plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
Hb <- ggplot(data) + 
  scale_y_continuous(name = "Hb", limits=c(yminHb*.95, ymxHb*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = Hb, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### RBC
my_mean = aggregate(data$RBC, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$RBC, by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(6)
my_info$ref.hi = c(11)

### RBC plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
RBC <- ggplot(data) + 
  scale_y_continuous(name = "RBC", limits=c(yminRBC*.95, ymxRBC*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = RBC, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### RDW
my_mean = aggregate(data$RDW, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$RDW, by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(13)
my_info$ref.hi = c(20)

### RDW plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
RDW <- ggplot(data) + 
  scale_y_continuous(name = "RDW", limits=c(yminRDW*.95, ymxRDW*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = RDW, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))



#####
#####PLATELETS
#####

### Platelets
my_mean = aggregate(data$PLT, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$PLT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(305.79)
my_info$ref.hi = c(1444.39)

### Platelets plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
PLT <- ggplot(data) + 
  scale_y_continuous(name = "Platelets", limits=c(yminPLT*.95, ymxPLT*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = PLT, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### MPV
my_mean = aggregate(data$MPV, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$MPV , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(4)
my_info$ref.hi = c(10.6)

### Platelets plot
ymxWBC = max(c(data$WBC, my_info$ref.hi),na.rm=T)
yminWBC = min(c(data$WBC, my_info$ref.low),na.rm=T)
MPV <- ggplot(data) + 
  scale_y_continuous(name = "MPV", limits=c(yminMPV*.95, ymxMPV*1.05))+
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = MPV, color = `Groups`), width = 0.1, show.legend=T, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))



#####
#####PLOT CBC
#####
tiff("WBC and Platelets.tiff", units="in", width=15, height=8, res=200)
(WBC | NE |LY | MO) /
  (EO | BA |PLT | MPV )/
  plot_layout(guides = "collect") + 
  plot_annotation(title = "CBC: White Blood Cell and Platelet Counts")
dev.off()



tiff("Erythroid.tiff", units="in", width=15, height=8, res=200)
(HCT | RBC | Hb| MCH) /
  (MCHC | MCV | Retics | RD) / 
  plot_layout(guides = "collect") + 
  plot_annotation(title = "CBC: Erythroid Parameters")
dev.off()




#####
#####CHEM
#####



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
  scale_y_continuous(name = "BUN:Creatinine")+
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=8) +
  geom_jitter(aes(x = Group, y = BCR, color = `Groups`), width = 0.08, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4, size=0.7) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 14))


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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = AGR, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = BCR, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = ALB, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = ALP, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = ALT, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = BUN, color = `Groups`), width = 0.1)+
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
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = CRE, color = `Groups`), height = 0.005, width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = GLU, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = TP, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = GLOB, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = Ca, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = PHOS, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### NaPlus
my_mean = aggregate(data$NaPlus, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$NaPlus , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(124)
my_info$ref.hi = c(174)

### NaPlus plot
NaPlus <- ggplot(data) + 
  scale_y_continuous(name = "Na+", limits=c(110,185)) +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = NaPlus, color = `Groups`), width = 0.1)+
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
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7) +
  geom_jitter(aes(x = Group, y = KPlus, color = `Groups`), width = 0.1)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


library(patchwork)
tiff("03. Chem.tiff", units="in", width=14, height=10, res=300)
TP + ALB + GLOB + AGR + BUN + CRE + BCR + GLU + Ca + PHOS + KPlus + NaPlus + ALP + ALT + 
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Clinical Chemistry")
dev.off()



#tiff("03. Chem.tiff", units="in", width=11, height=11, res=300)
#grid.arrange(TP, ALB, GLOB, AGR, ALP, ALT, BUN, CRE, Ca, PHOS, KPlus, NaPlus, GLU, 
#             ncol = 4, nrow = 4, top = "Clinical Chemistry")
#dev.off()

#####
####Plot CHEM
#####

# 
# tiff("Chemistry.tiff", units="in", width=15, height=8, res=200)
# (TP | ALB | GLOB | AGR | GLU ) /
#   (ALP | ALT |BUN | CRE | BCR) /
#   (Ca | PHOS | KPlus| NaPlus | TBIL) /
#   plot_layout(guides = "collect") +
#   plot_annotation(title = "Clinical Chemistry")
# dev.off()
