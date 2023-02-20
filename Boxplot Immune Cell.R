
library(ggplot2)
library(gridExtra)
library(patchwork)
library(readxl)
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
### PDL1  
my_mean = aggregate(data$PDL1, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$PDL1 , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

PDL1 <- ggplot(data) + 
  scale_y_continuous(name = "PDL1") +
  geom_jitter(aes(x = Strain, y = PDL1, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))  

### CD45
my_mean = aggregate(data$CD45, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$CD45 , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

CD45 <- ggplot(data) + 
  scale_y_continuous(name = "CD45") +
  geom_jitter(aes(x = Strain, y = CD45, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))

###HLA
my_mean = aggregate(data$HLA, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$HLA , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

HLA <- ggplot(data) + 
  scale_y_continuous(name = "HLA") +
  geom_jitter(aes(x = Strain, y = HLA, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))

###PD1
my_mean = aggregate(data$PD1, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$PD1 , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

PD1 <- ggplot(data) + 
  scale_y_continuous(name = "PD1") +
  geom_jitter(aes(x = Strain, y = PD1, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))

###CD20
my_mean = aggregate(data$CD20, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$CD20 , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

CD20 <- ggplot(data) + 
  scale_y_continuous(name = "CD20") +
  geom_jitter(aes(x = Strain, y = CD20, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))

###CD57
my_mean = aggregate(data$CD57, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$CD57 , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

CD57 <- ggplot(data) + 
  scale_y_continuous(name = "CD57") +
  geom_jitter(aes(x = Strain, y = CD57, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))

###CD68
my_mean = aggregate(data$CD68, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$CD68 , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

CD68 <- ggplot(data) + 
  scale_y_continuous(name = "CD68") +
  geom_jitter(aes(x = Strain, y = CD68, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))

###FOXP3
my_mean = aggregate(data$FOXP3, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$FOXP3 , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

FOXP3 <- ggplot(data) + 
  scale_y_continuous(name = "FOXP3") +
  geom_jitter(aes(x = Strain, y = FOXP3, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))
##CD8
my_mean = aggregate(data$CD8, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$CD8 , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

CD8 <- ggplot(data) + 
  scale_y_continuous(name = "CD8") +
  geom_jitter(aes(x = Strain, y = CD8, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))
##CD4
my_mean = aggregate(data$CD4, by=list(data$Strain), mean, na.rm = T) ; colnames(my_mean)=c("Strain" , "mean")
my_CI = aggregate(data$CD4 , by=list(data$Strain) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Strain" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info$ref.low = c(2.71)
my_info$ref.hi = c(12.33)

CD4 <- ggplot(data) + 
  scale_y_continuous(name = "CD4") +
  geom_jitter(aes(x = Strain, y = CD4, color = `Strain`), width = 0.1, show.legend=F, size = 3)+
  geom_point(data = my_info, aes(x = Strain , y = mean), color = "#a9a9a9", size = 5) +
  geom_errorbar(data = my_info, aes(x = Strain, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=1)  +
  theme_bw() +
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size = 18))


tiff("Spleen 123.tiff", units="in", width=9, height=8, res=300)
(CD45) / 
  (CD20 | PD1 ) /
  (CD4 | CD8) /
  (FOXP3 | HLA) /
  plot_layout(guides = "collect") #+ 
  #plot_annotation(title = "Tumor")
dev.off()

