#data<-MHL_19_331_121_Efficacy <- read_excel("C:/Users/edmondsonef/Desktop/MHL 19-331-121 Efficacy.xlsx", sheet = "Initial CBC")
library(ggplot2)
library(gridExtra)
library(patchwork)
library(readxl)
#library(ggpubr)
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
###Generate Data


### CHOL
my_mean = aggregate(data$CHOL, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$CHOL , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(36)
my_info$ref.hi = c(96)

CHOL <- ggplot(data) + 
  scale_y_continuous(name = "CHOL") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=5) +
  geom_jitter(aes(x = Group, y = CHOL, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### TRIG
my_mean = aggregate(data$TRIG, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$TRIG , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(55)
my_info$ref.hi = c(144)

TRIG <- ggplot(data) + 
  scale_y_continuous(name = "TRIG") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=5) +
  geom_jitter(aes(x = Group, y = TRIG, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())


### ALT
my_mean = aggregate(data$ALT, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$ALT , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(28)
my_info$ref.hi = c(132)

ALT <- ggplot(data) + 
  scale_y_continuous(name = "ALT") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=5) +
  geom_jitter(aes(x = Group, y = ALT, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### AST 
my_mean = aggregate(data$AST, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$AST , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(59)
my_info$ref.hi = c(247)

AST <- ggplot(data) + 
  scale_y_continuous(name = "AST") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=5) +
  geom_jitter(aes(x = Group, y = AST, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### GLU
my_mean = aggregate(data$GLU, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$GLU , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(90)
my_info$ref.hi = c(192)

GLU <- ggplot(data) + 
  scale_y_continuous(name = "GLU") +
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=5) +
  geom_jitter(aes(x = Group, y = GLU, shape = Sex, color = `Groups`), width = 0.1, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())

### VLDL 
my_mean = aggregate(data$`*VLDL`, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$`*VLDL` , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)


VLDL <- ggplot(data) + 
  scale_y_continuous(name = "VLDL") +
  #geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  geom_jitter(aes(x = Group, y = `*VLDL`, shape = Sex, color = `Groups`), width = 0.1, height = 0.001, show.legend=T)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 1.5) +
  #geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2, size=0.7)  +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank())



tiff("LIV.tiff", units="in", width=8, height=8, res=300)
(CHOL | TRIG) / 
  (ALT | AST) /
  (GLU | VLDL) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Chemistry: Liver Panel")
dev.off()

#tiff("02. RBC.tiff", units="in", width=11, height=5, res=300)
#grid.arrange(HCT, RBC, Hb, Retics, MCH, MCHC, MCV, RDW, ncol = 4, nrow = 2, top = "CBC: Erythroid")
#dev.off()
