
#data <- read_excel("C:/Users/edmondsonef/Desktop/MHL 19-331-121 Efficacy.xlsx", sheet = "19-331-121 Chem")
library(ggplot2)
library(gridExtra)
library(readxl)
library(patchwork)

setwd("C:/Users/edmondsonef/Desktop/R-plots/")

#####
#####Absolute Weights
#####


### Body Weight  
my_mean = aggregate(data$Weight, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Weight , by=list(data$Group), FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### BW Plot
BW <- ggplot(data) + 
  scale_y_continuous(name = "Body Weight (95% CI)", limits=c(20, 35)) +
  geom_jitter(aes(x = Group, y = Weight, color = `Groups`), width = 0.1, size = 2)+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.5) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Brain Weight
my_mean = aggregate(data$'Brain Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Brain Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Brain plot
Brain <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$"Brain Weight", color = `Groups`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Brain (grams)")+#, limits=c(0.4, 0.6)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Heart Weight
my_mean = aggregate(data$'Heart Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Heart Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Heart plot
Heart <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Heart Weight', color = `Groups`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Heart (grams)", limits=c(0.09, 0.2)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Liver Weight
my_mean = aggregate(data$'Liver Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Liver Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Liver plot
Liver <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Liver Weight', color = `Groups`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Liver (grams)", limits=c(1, 1.7)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Lung Weight
my_mean = aggregate(data$'Lung Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Lung Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Lung plot
Lung <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Lung Weight', color = `Groups`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Lung (grams)", limits=c(0.15, 0.25)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Spleen Weight
my_mean = aggregate(data$'Spleen Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Spleen Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Spleen plot
Spleen <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Spleen (grams)", limits=c(0.05, 0.1)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  geom_jitter(aes(x = Group, y = data$'Spleen Weight', color = `Groups`), width = 0.1, size = 2) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Kidney Weight
my_mean = aggregate(data$'Kidney Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Kidney Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Kidney plot
Kidney <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Kidney Weight', color = `Groups`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Kidney (grams)", limits=c(0.2, 0.4)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Graft Weight
my_mean = aggregate(data$'Graft Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Graft Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Graft plot
Graft <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Graft Weight', color = `Groups`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Graft")+#)+#, limits=c(0, 10)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


#####
#####Plot Absolute Weights
#####

tiff("04. Absolute Organ Weights.tiff", units="in", width=9, height=10, res=300)
(BW) /
  (Graft | Heart) /
#  (Brain | Heart) /
  (Lung | Kidney) /
  (Spleen | Liver) /
  plot_layout(guides = "collect") +
  plot_annotation(title = "Absolute Organ Weights (grams)")
dev.off()

# tiff("04. Absolute Organ Weights.tiff", units="in", width=9, height=10, res=300)
# (BW | Graft) /
#  (Brain | Heart) /
#  (Lung | Kidney) /
#  (Spleen | Liver) /
#  plot_layout(guides = "collect") +
#  plot_annotation(title = "Absolute Organ Weights (grams)")
# dev.off()





















#####
#####Relative Organ weights
#####

### Body Weight  
my_mean = aggregate(data$Weight, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Weight , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### BW Plot

BW <- ggplot(data) + 
  scale_y_continuous(name = "Body Weight (95% CI)")+#, limits=c(18, 28.5)) +
  geom_jitter(aes(x = Group, y = Weight, color = `Groups`), width = 0.1, size = 2)+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.5) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Brain Weight
my_mean = aggregate(data$'Brain % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Brain % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Brain plot
Brain <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$"Brain % BW", color = `Groups`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Brain (% BW)")+#, limits=c(1.5, 2.5)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Heart Weight
my_mean = aggregate(data$'Heart % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Heart % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Heart plot
Heart <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Heart % BW', color = `Groups`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Heart (% BW)", limits=c(0.3, 0.6)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Liver Weight
my_mean = aggregate(data$'Liver % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Liver % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Liver plot
Liver <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Liver % BW', color = `Groups`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Liver (% BW)", limits=c(3.5, 5.3)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Lung Weight
my_mean = aggregate(data$'Lung % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Lung % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Lung plot
Lung <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Lung % BW', color = `Groups`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Lung (% BW)", limits=c(0.5, 0.9)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Spleen Weight
my_mean = aggregate(data$'Spleen % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Spleen % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Spleen plot
Spleen <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Spleen % BW', color = `Groups`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Spleen (% BW)", limits=c(0.2, 0.35)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Kidney Weight
my_mean = aggregate(data$'Kidney % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Kidney % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Kidney plot
Kidney <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Kidney % BW', color = `Groups`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Kidney (% BW)", limits=c(0.8, 1.3)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Graft Weight
my_mean = aggregate(data$'Graft % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Graft % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Graft plot
Graft <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Graft % BW', color = `Groups`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group , y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Graft % BW", limits=c(1, 3.5)) +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))



#####
##### Plot Relative Organ weights
#####

### Make multiple plots
tiff("05. Relative Organ Weights.tiff", units="in", width=9, height=10, res=300)
(BW) / 
  (Graft | Heart) /
  (Lung | Kidney) /
  (Spleen | Liver) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Relative Organ Weights (% of body weight)")
dev.off()

#tiff("05. Relative Organ Weights.tiff", units="in", width=8, height=10, res=300)
#(BW) / 
#  (Graft | Heart) /
#  (Lung | Kidney) /
#  (Spleen | Liver) /
#  plot_layout(guides = "collect") + 
#  plot_annotation(title = "Absolute Organ Weights (grams)")
#dev.off()
