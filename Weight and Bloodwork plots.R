library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(patchwork)

data <- read_excel("C:/Users/edmondsonef/Desktop/MHL 22-331-17 PK.xlsx")
data <- dplyr::filter(data, Censor == 1)

#####Organ Weight
#####
dataW <- dplyr::select(data, PHLNbr,Group, Groups,
                       Weight, 
                       `Brain Weight`, 
                       `Heart Weight`, 
                       `Kidney Weight`, 
                       `Liver Weight`, 
                       `Lung Weight`, 
                       `Spleen Weight`)

body_weight <- dataW %>% gather(key= "Organ", grams, 4) %>% 
  ggplot(aes(x = factor(Group), y = grams))+
  geom_boxplot(outlier.shape = NA, colour = "lightgray")+  
  theme_bw() +
  theme(axis.title.x=element_blank(), text = element_text(size = 16))+
  geom_jitter(aes(x = Group, y = grams, color = `Groups`), width = 0.1, size = 4)+
  facet_wrap(~Organ, scales = "free")
body_weight

organ_weight <- dataW %>% 
  gather(key= "Organ", grams, 5:10) %>% 
  ggplot(aes(x = factor(Group), y = grams))+
  geom_boxplot(outlier.shape = NA, colour = "lightgray")+  
  theme_bw() +
  theme(axis.title.x=element_blank(), text = element_text(size = 16))+
  geom_jitter(aes(x = Group, y = grams, color = `Groups`), width = 0.1, size = 4)+
  facet_wrap(~Organ, scales = "free", nrow=2)
organ_weight


tiff("Absolute Organ Weights.tiff", units="in", width=22, height=8, res=100)
(body_weight | organ_weight)+
  plot_layout(guides = "collect") +
  plot_layout(widths = c(2, 3))+
  plot_annotation(title = "Absolute Organ Weights (grams)")
dev.off()



#####
#####Relative Organ Weight
#####

dataWPerc <- dplyr::select(data, PHLNbr,Group, Groups,
                       Weight, 
                       `Brain % BW`, 
                       `Heart % BW`,
                       `Kidney % BW`, 
                       `Liver % BW`, 
                       `Lung % BW`,
                       `Spleen % BW`)


relative_weight <- dataWPerc %>% 
  gather(key= "Organ", value, 5:10) %>% 
  ggplot(aes(x = factor(Group), y = value))+
  geom_boxplot(outlier.shape = NA, colour = "lightgray")+  
  theme_bw() +
  theme(axis.title.x=element_blank(), text = element_text(size = 16))+
  geom_jitter(aes(x = Group, y = value, color = `Groups`), width = 0.1, size = 4)+
  facet_wrap(~Organ, scales = "free", nrow=2)
relative_weight

tiff("Relative Organ Weights.tiff", units="in", width=22, height=8, res=100)
(body_weight | relative_weight)+
  plot_layout(guides = "collect") +
  plot_layout(widths = c(2, 3))+
  plot_annotation(title = "Relative Organ Weights")
dev.off()



#####
#####CBC
#####




### WBC Plot

dataCBC <- dplyr::select(data, PHLNbr,Group, Groups,
                         WBC,NE,LY,MO,EO,BA,RBC,Hb,HCT,MCH,MCH,MCHC,RDW,Retics,PLT,MPV)

WBC <- dataCBC %>% gather(key= "Value", value, 4) %>% 
  geom_errorbar(data = my_info, aes(x = Group, ymin = ref.low, ymax = ref.hi), color = "#f5f5f5", width = 0, size=10) +
  ggplot(aes(x = factor(Group), y = value))+
  geom_boxplot(outlier.shape = NA, colour = "lightgray")+  
  theme_bw() +
  theme(axis.title.x=element_blank(), text = element_text(size = 16))+
  geom_jitter(aes(x = Group, y = value, color = `Groups`), width = 0.1, size = 4)+
  facet_wrap(~Value, scales = "free")
WBC

CBC <- dataCBC %>% 
  gather(key= "Value", value, 4:9) %>% 
  ggplot(aes(x = factor(Group), y = value))+
  geom_boxplot(outlier.shape = NA, colour = "lightgray")+  
  theme_bw() +
  theme(axis.title.x=element_blank(), text = element_text(size = 16))+
  geom_jitter(aes(x = Group, y = value, color = `Groups`), width = 0.1, size = 4)+
  facet_wrap(~Value, scales = "free", nrow=2)
CBC

tiff("Relative Organ Weights.tiff", units="in", width=22, height=8, res=100)
(body_weight | relative_weight)+
  plot_layout(guides = "collect") +
  plot_layout(widths = c(2, 3))+
  plot_annotation(title = "CBC")
dev.off()



#####
#####Chemistry
#####
dataCHEM <- dplyr::select(data, PHLNbr,Group, Groups,
                         ALB, ALP, ALT, AMY, TBIL, BUN, Ca, PHOS, CRE, GLU, NaPlus,KPlus,TP, GLOB)
                         
CHEM <- dataCHEM %>% 
  gather(key= "Organ", value, 4:16) %>% 
  ggplot(aes(x = factor(Group), y = value))+
  geom_boxplot(outlier.shape = NA, colour = "lightgray")+  
  theme_bw() +
  theme(axis.title.x=element_blank(), text = element_text(size = 16))+
  geom_jitter(aes(x = Group, y = value, color = `Groups`), width = 0.1, size = 4)+
  facet_wrap(~Organ, scales = "free", nrow=2)
CHEM

tiff("Chemistry.tiff", units="in", width=22, height=8, res=100)
(CHEM)+
  plot_layout(guides = "collect") +
  plot_layout(widths = c(2, 3))+
  plot_annotation(title = "Relative Organ Weights (grams)")
dev.off()

