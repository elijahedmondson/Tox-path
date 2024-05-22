
#data <- read_excel("C:/Users/edmondsonef/Desktop/MHL 19-331-121 Efficacy.xlsx", sheet = "19-331-121 Chem")
library(ggplot2)
library(gridExtra)
library(readxl)
library(patchwork)

setwd("C:/Users/edmondsonef/Desktop/R-plots/")

#####
#####Absolute Weights
#####
library(dplyr)
#data <- dplyr::filter(data, Censor == 1)
#data <- dplyr::filter(data, Group != "F08")

### Body Weight  
my_mean = aggregate(data$Weight, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Weight , by=list(data$Group), FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### BW Plot   , limits=c(10, 30)) +
ymx = max(data$Weight, na.rm = T)
ymin = min(data$Weight, na.rm = T)
BW <- ggplot(data) + 
  scale_y_continuous(name = "Body Weight (95% CI)", limits=c(ymin*.98, ymx*1.02))+
  geom_jitter(aes(x = Group, y = Weight, color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.5) +
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Brain Weight
my_mean = aggregate(data$'Brain Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Brain Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Brain plot
ymx_br = max(data$'Brain Weight')
ymin_br = min(data$'Brain Weight')
Brain <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$"Brain Weight", color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Brain (grams)", limits=c(ymin_br*.98, ymx_br*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Heart Weight
my_mean = aggregate(data$'Heart Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Heart Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Heart plot
ymx_ht = max(data$'Heart Weight')
ymin_ht = min(data$'Heart Weight')
Heart <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Heart Weight', color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Heart (grams)", limits=c(ymin_ht*.98, ymx_ht*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Liver Weight
my_mean = aggregate(data$'Liver Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Liver Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Liver plot
ymx_lv = max(data$'Liver Weight')
ymin_lv = min(data$'Liver Weight')
Liver <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Liver Weight', color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Liver (grams)", limits=c(ymin_lv*.98, ymx_lv*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Lung Weight
my_mean = aggregate(data$'Lung Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Lung Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Lung plot
ymx_lu = max(data$'Lung Weight')
ymin_lu = min(data$'Lung Weight')
Lung <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Lung Weight', color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Lung (grams)", limits=c(ymin_lu*.98, ymx_lu*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Spleen Weight
my_mean = aggregate(data$'Spleen Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Spleen Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Spleen plot
ymx_sp = max(data$'Spleen Weight')
ymin_sp = min(data$'Spleen Weight')
Spleen <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Spleen (grams)", limits=c(ymin_sp*.98, ymx_sp*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  geom_jitter(aes(x = Group, y = data$'Spleen Weight', color = Groups, shape = `Sex`), width = 0.1, size = 2) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Kidney Weight
my_mean = aggregate(data$'Kidney Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Kidney Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Kidney plot
ymx_kd = max(data$'Kidney Weight')
ymin_kd = min(data$'Kidney Weight')
Kidney <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Kidney Weight', color = Groups, shape = `Sex`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Kidney (grams)", limits=c(ymin_kd*.98, ymx_kd*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


## Thymus Weight
my_mean = aggregate(data$'Thymus Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Thymus Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

## Thymus plot
ymx_thy = max(data$'Thymus Weight')
ymin_thy = min(data$'Thymus Weight')
Thymus <- ggplot(data) +
  geom_jitter(aes(x = Group, y = data$'Thymus Weight', color = Groups, shape = `Sex`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Thymus", limits=c(ymin_thy*.98, ymx_thy*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Graft Weight
# my_mean = aggregate(data$'Graft Weight', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
# my_CI = aggregate(data$'Graft Weight' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
# my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
# my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
# 
# ### Graft plot
# ymx_gr = max(data$'Graft Weight')
# ymin_gr = min(data$'Graft Weight')
# Graft <- ggplot(data) + 
#   geom_jitter(aes(x = Group, y = data$'Graft Weight', color = Groups, shape = `Sex`), width = 0.1, size = 2) +
#   geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
#   scale_y_continuous(name = "Graft", limits=c(ymin_gr*.98, ymx_gr*1.02))+
#   geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
#   theme_bw() +
#   #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
#   #theme(axis.text.x=element_blank()) +
#   theme(axis.title.x=element_blank(), text = element_text(size = 12))


#####
#####Plot Absolute Weights
#####

# tiff("Absolute Organ Weights.tiff", units="in", width=12, height=10, res=200)
# (BW) /
# #  (Graft | Heart) /
#   (Brain | Heart | Liver) /
#   (Spleen | Kidney| Lung) /
#   plot_layout(guides = "collect") +
#   plot_annotation(title = "Absolute Organ Weights (grams)")
# dev.off()

tiff("Absolute Organ Weights.tiff", units="in", width=10, height=10, res=300)
#(BW | Thymus) /
# (BW | Graft) /
(BW) /
 (Brain | Heart) /
 (Lung | Kidney) /
 (Spleen | Liver) /
 plot_layout(guides = "collect") +
 plot_annotation(title = "Absolute Organ Weights (grams)")
dev.off()

# tiff("Absolute Organ Weights.tiff", units="in", width=8, height=8, res=200)
#   (BW | Heart) /
#   (Spleen | Liver) /
#   (Kidney | Lung) /
#     plot_layout(guides = "collect") +
#   plot_annotation(title = "Absolute Organ Weights (grams)")
# dev.off()
# 




















#####
#####Relative Organ weights
#####

### Body Weight  
my_mean = aggregate(data$Weight, by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$Weight , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### BW Plot
ymx = max(data$'Weight')
ymin = min(data$'Weight')
BW <- ggplot(data) + 
  scale_y_continuous(name = "Body Weight (95% CI)", limits=c(ymin*.98, ymx*1.02))+
  geom_jitter(aes(x = Group, y = Weight, color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.2 , size=0.5) +
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Brain Weight
ymx_br = max(data$'Brain % BW')
ymin_br = min(data$'Brain % BW')
my_mean = aggregate(data$'Brain % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Brain % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Brain plot
Brain <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$"Brain % BW", color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Brain (% BW)", limits=c(ymin_br*.98, ymx_br*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))


### Heart Weight
my_mean = aggregate(data$'Heart % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Heart % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Heart plot
ymx_ht = max(data$'Heart % BW')
ymin_ht = min(data$'Heart % BW')
Heart <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Heart % BW', color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Heart (% BW)", limits=c(ymin_ht*.98, ymx_ht*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Liver Weight
my_mean = aggregate(data$'Liver % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Liver % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Liver plot
ymx_lv = max(data$'Liver % BW')
ymin_lv = min(data$'Liver % BW')
Liver <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Liver % BW', color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Liver (% BW)", limits=c(ymin_lv*.98, ymx_lv*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Lung Weight
my_mean = aggregate(data$'Lung % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Lung % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Lung plot
ymx_lu = max(data$'Lung % BW')
ymin_lu = min(data$'Lung % BW')
Lung <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Lung % BW', color = Groups, shape = `Sex`), width = 0.1, size = 2)+
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Lung (% BW)", limits=c(ymin_lu*.98, ymx_lu*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Spleen Weight
my_mean = aggregate(data$'Spleen % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Spleen % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Spleen plot
ymx_sp = max(data$'Spleen % BW')
ymin_sp = min(data$'Spleen % BW')
Spleen <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Spleen % BW', color = Groups, shape = `Sex`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Spleen (% BW)", limits=c(ymin_sp*.98, ymx_sp*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Kidney Weight
my_mean = aggregate(data$'Kidney % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Kidney % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Kidney plot
ymx_kd = max(data$'Kidney % BW')
ymin_kd = min(data$'Kidney % BW')
Kidney <- ggplot(data) + 
  geom_jitter(aes(x = Group, y = data$'Kidney % BW', color = Groups, shape = `Sex`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Kidney (% BW)", limits=c(ymin_kd*.98, ymx_kd*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

## Thymus Weight
my_mean = aggregate(data$'Thymus % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Thymus % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Thymus plot
ymx_thy = max(data$'Thymus % BW')
ymin_thy = min(data$'Thymus % BW')
Thymus <- ggplot(data) +
  geom_jitter(aes(x = Group, y = data$'Thymus % BW', color = Groups, shape = `Sex`), width = 0.1, size = 2) +
  geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
  scale_y_continuous(name = "Thymus (% BW)", limits=c(ymin_thy*.98, ymx_thy*1.02))+
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
  theme_bw() +
  #theme(axis.text.x=element_text(angle=25,hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 12))

### Graft Weight
my_mean = aggregate(data$'Graft % BW', by=list(data$Group), mean, na.rm = T) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Graft % BW' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

### Graft plot
# ymx_gr = max(data$'Graft % BW')
# ymin_gr = min(data$'Graft % BW')
# Graft <- ggplot(data) + 
#   geom_jitter(aes(x = Group, y = data$'Graft % BW', color = Groups, shape = `Sex`), width = 0.1, size = 2) +
#   geom_point(data = my_info, aes(x = Group, y = mean), color = "#a9a9a9", size = 3) +
#   scale_y_continuous(name = "Graft % BW", limits=c(ymin_gr*.98, ymx_gr*1.02))+
#   geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "#a9a9a9", width = 0.4 , size=0.5) +
#   theme_bw() +
#   #theme(axis.text.x=element_text(angle=25,hjust=1)) +   
#   #theme(axis.text.x=element_blank()) +
#   theme(axis.title.x=element_blank(), text = element_text(size = 12))



#####
##### Plot Relative Organ weights
#####

### Make multiple plots
# tiff("Relative Organ Weights.tiff", units="in", width=12, height=10, res=200)
# (BW) / 
#   #(Graft | Heart) /
#   (Brain | Heart | Liver) /
#   (Spleen | Kidney| Lung) /
#   plot_layout(guides = "collect") + 
#   plot_annotation(title = "Relative Organ Weights (% of body weight)")
# dev.off()


tiff("Relative Organ Weights.tiff", units="in", width=10, height=10, res=300)
#(BW | Thymus) /
#(BW | Graft) /
(BW) /
  (Brain | Heart) /
  (Lung | Kidney) /
  (Spleen | Liver) /
  plot_layout(guides = "collect") +
  plot_annotation(title = "Relative Organ Weights (% of body weight)")
dev.off()


# 
# tiff("Relative Organ Weights.tiff", units="in", width=8, height=8, res=200)
# (BW | Heart) /
#   (Spleen | Liver) /
#   (Kidney | Lung) /
#   plot_layout(guides = "collect") +
#   plot_annotation(title = "Relative Organ Weights (% BW)")
# dev.off()
# 
