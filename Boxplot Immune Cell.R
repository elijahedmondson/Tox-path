
library(ggplot2)
library(ggfortify)
library(readxl)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(Rmisc)
library(tidyverse)
library(plyr)
library(GGally)
library(ggplot2)
library(gapminder)
library(ggsignif)
library(patchwork)
setwd("C:/Users/edmondsonef/Desktop/R-plots/")


###
###
### TUMOR 122
###
###

data <- read_excel("C:/Users/edmondsonef/Desktop/Humanized/MHL Humanized Tissue Counts.xlsx", 
                   sheet = "122 Tumor")
group = data$`Strain`

PDL1variable = data$`PDL1`
PDL1 <- ggplot(data, aes(group, PDL1variable))+
  geom_boxplot()+
  scale_y_continuous(name = "PD-L1", limits = c(0, 60)) + #ylim=c(0, ymx*1.02)
  geom_jitter(aes(x = group, y = PDL1variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("NSG", "NSG-IL-15"),
                                 c("NSG", "NSG-SGM3"),
                                 c("NSG-IL-15", "NSG-SGM3")),
              y_position = c(25, 25.33,25.66, 26, 26.33, 26.66),
              map_signif_level = F, textsize = 4, vjust=0.4,test = "wilcox.test")+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
PDL1

CD45variable = data$`CD45`
CD45 <- ggplot(data, aes(group, CD45variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD45", limits = c(0, 5)) +
  geom_jitter(aes(x = group, y = CD45variable, color = group), width = 0.2, height = 0.01, size = 5) +
  # geom_signif(comparisons = list(c("NSG", "NSG-IL-15"), 
  #                                c("NSG", "NSG-SGM3"), 
  #                                c("NSG-IL-15", "NSG-SGM3")),
  #             y_position = c(4,4.5,5),
  #             map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

 
HLAvariable = data$`HLA`
HLA <- ggplot(data, aes(group, HLAvariable))+
  geom_boxplot()+
  scale_y_continuous(name = "HLA") +
  geom_jitter(aes(x = group, y = HLAvariable, color = group), width = 0.2, height = 0.01, size = 5) +
  # geom_signif(comparisons = list(c("NSG", "NSG-IL-15"),
  #                                c("NSG", "NSG-SGM3"),
  #                                c("NSG-IL-15", "NSG-SGM3")),
  #             y_position = c(21, 22, 23),
  #             map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

PD1variable = data$`PD1`
PD1 <- ggplot(data, aes(group, PD1variable))+
  geom_boxplot()+
  scale_y_continuous(name = "PD1") +
  geom_jitter(aes(x = group, y = PD1variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(#c("NSG", "NSG-IL-15"), 
                                 c("NSG", "NSG-SGM3")), 
                                 #c("NSG-IL-15", "NSG-SGM3")),
              y_position = c(4.4, 5),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

CD20variable = data$`CD20`
CD20 <- ggplot(data, aes(group, CD20variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD20") +
  geom_jitter(aes(x = group, y = CD20variable, color = group), width = 0.2, height = 0.01, size = 5) +
  # geom_signif(comparisons = list(c("NSG", "NSG-IL-15"), 
  #                                c("NSG", "NSG-SGM3"), 
  #                                c("NSG-IL-15", "NSG-SGM3")),
  #             y_position = c(.33,.66,1),
  #             map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

CD57variable = data$`CD57`
CD57 <- ggplot(data, aes(group, CD57variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD57") +
  geom_jitter(aes(x = group, y = CD57variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(#c("NSG", "NSG-IL-15"), 
                                 c("NSG", "NSG-SGM3"), 
                                 c("NSG-IL-15", "NSG-SGM3")),
              y_position = c(.2,.3),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

CD68variable = data$`CD68`
CD68 <- ggplot(data, aes(group, CD68variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD68") +
  geom_jitter(aes(x = group, y = CD68variable, color = group), width = 0.2, height = 0.01, size = 5) +
  # geom_signif(comparisons = list(c("NSG", "NSG-IL-15"), 
  #                                c("NSG", "NSG-SGM3"), 
  #                                c("NSG-IL-15", "NSG-SGM3")),
  #             y_position = c(2,2.5,3),
  #             map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

FOXP3variable = data$`FOXP3`
FOXP3 <- ggplot(data, aes(group, FOXP3variable))+
  geom_boxplot()+
  scale_y_continuous(name = "FOXP3") +
  geom_jitter(aes(x = group, y = FOXP3variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(#c("NSG", "NSG-IL-15"), 
                                 c("NSG", "NSG-SGM3")), 
                                 #c("NSG-IL-15", "NSG-SGM3")),
              y_position = c(2,2.5,3),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

CD8variable = data$`CD8`
CD8 <- ggplot(data, aes(group, CD8variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD8") +
  geom_jitter(aes(x = group, y = CD8variable, color = group), width = 0.2, height = 0.01, size = 5) +
  # geom_signif(comparisons = list(c("NSG", "NSG-IL-15"), 
  #                                c("NSG", "NSG-SGM3"), 
  #                                c("NSG-IL-15", "NSG-SGM3")),
  #             y_position = c(2,2.5,3),
  #             map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

CD4variable = data$`CD4`
CD4 <- ggplot(data, aes(group, CD4variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD4") +
  geom_jitter(aes(x = group, y = CD4variable, color = group), width = 0.2, height = 0.01, size = 5) +
  # geom_signif(comparisons = list(c("NSG", "NSG-IL-15"), 
  #                                c("NSG", "NSG-SGM3"), 
  #                                c("NSG-IL-15", "NSG-SGM3")),
  #             y_position = c(1.5, 2, 2.5),
  #             map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())




tiff("Tumor 122.tiff", units="in", width=12, height=18, res=300)
(CD45 | CD68) / 
  (PDL1 | PD1 ) /
  (CD4 | CD8) /
  (FOXP3 | HLA) /
  (CD20 | CD57) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = 'Tumor: 19-331-122', 
                  tag_levels = 'A',
                  theme = theme(plot.title = element_text(size = 18))) 
dev.off()


###
###
### Spleen 122
###
###

data <- read_excel("C:/Users/edmondsonef/Desktop/Humanized/MHL Humanized Tissue Counts.xlsx", 
                   sheet = "122 spleen comp")

group = data$`Strain`

PDL1variable = data$`PDL1`
PDL1 <- ggplot(data, aes(group, PDL1variable))+
  geom_boxplot()+
  scale_y_continuous(name = "PD-L1") +#, limits = c(0, 60)) + #ylim=c(0, ymx*1.02)
  geom_jitter(aes(x = group, y = PDL1variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("01 huNSG", "02 huNSG & graft"),
                                 c("03 huNSG-IL-15", "04 huNSG-IL-15 & graft")),
              #y_position = c(32, 33,34),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

CD45variable = data$`CD45`
CD45 <- ggplot(data, aes(group, CD45variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD45")+#, limits = c(0, 5)) +
  geom_jitter(aes(x = group, y = CD45variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("01 huNSG", "02 huNSG & graft"),
                                 c("03 huNSG-IL-15", "04 huNSG-IL-15 & graft")),
              #y_position = c(80,85,90),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD45

HLAvariable = data$`HLA`
HLA <- ggplot(data, aes(group, HLAvariable))+
  geom_boxplot()+
  scale_y_continuous(name = "HLA") +
  geom_jitter(aes(x = group, y = HLAvariable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("01 huNSG", "02 huNSG & graft"),
                                 c("03 huNSG-IL-15", "04 huNSG-IL-15 & graft")),
              #y_position = c(100, 103, 106),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
HLA

PD1variable = data$`PD1`
PD1 <- ggplot(data, aes(group, PD1variable))+
  geom_boxplot()+
  scale_y_continuous(name = "PD1") +
  geom_jitter(aes(x = group, y = PD1variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("01 huNSG", "02 huNSG & graft"),
                                 c("03 huNSG-IL-15", "04 huNSG-IL-15 & graft")),
    #y_position = c(55, 65, 60),
    map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
PD1


CD20variable = data$`CD20`
CD20 <- ggplot(data, aes(group, CD20variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD20") +
  geom_jitter(aes(x = group, y = CD20variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("01 huNSG", "02 huNSG & graft"),
                                 c("03 huNSG-IL-15", "04 huNSG-IL-15 & graft")),
              #y_position = c(90,95,100),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD20

CD57variable = data$`CD57`
CD57 <- ggplot(data, aes(group, CD57variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD57") +
  geom_jitter(aes(x = group, y = CD57variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("01 huNSG", "02 huNSG & graft"),
                                 c("03 huNSG-IL-15", "04 huNSG-IL-15 & graft")),
    #y_position = c(8,10,12),
    map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD57

# CD68variable = data$`CD68`
# CD68 <- ggplot(data, aes(group, CD68variable))+
#   geom_boxplot()+
#   scale_y_continuous(name = "CD68: % positive") +
#   geom_jitter(aes(x = group, y = CD68variable, color = group), width = 0.2, height = 0.01, size = 5) +
#   geom_signif(comparisons = list(c("NSG", "NSG-IL-15"),
#                                  c("NSG", "NSG-SGM3"),
#                                  c("NSG-IL-15", "NSG-SGM3")),
#               y_position = c(2,2.5,3),
#               map_signif_level = T, textsize = 4, vjust=0.4)+
#   theme_bw(base_size = 18) +
#   theme(legend.position = "none") +
#   theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
# CD68

FOXP3variable = data$`FOXP3`
FOXP3 <- ggplot(data, aes(group, FOXP3variable))+
  geom_boxplot()+
  scale_y_continuous(name = "FOXP3") +
  geom_jitter(aes(x = group, y = FOXP3variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("01 huNSG", "02 huNSG & graft"),
                                 c("03 huNSG-IL-15", "04 huNSG-IL-15 & graft")),
    #y_position = c(10,11,12),
    map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
FOXP3

CD8variable = data$`CD8`
CD8 <- ggplot(data, aes(group, CD8variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD8") +
  geom_jitter(aes(x = group, y = CD8variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("01 huNSG", "02 huNSG & graft"),
                                 c("03 huNSG-IL-15", "04 huNSG-IL-15 & graft")),
              #y_position = c(17,19,21),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD8

CD4variable = data$`CD4`
CD4 <- ggplot(data, aes(group, CD4variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD4") +
  geom_jitter(aes(x = group, y = CD4variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("01 huNSG", "02 huNSG & graft"),
                                 c("03 huNSG-IL-15", "04 huNSG-IL-15 & graft")),
              #y_position = c(60, 85, 82),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD4



tiff("Spleen 122 comps.tiff", units="in", width=12, height=14, res=300)
(CD45 | PD1) / 
  #(PDL1 | PD1 ) /
  (CD4 | CD8) /
  (FOXP3 | HLA) /
  (CD20 | CD57) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = 'Spleen: 19-331-122', 
                  tag_levels = 'A',
                  theme = theme(plot.title = element_text(size = 18))) 
dev.off()


###
###
### Spleen 123
###
###

data <- read_excel("C:/Users/edmondsonef/Desktop/Humanized/MHL Humanized Tissue Counts.xlsx", 
                   sheet = "123 Spleen")

group = data$`Strain`

# PDL1variable = data$`PDL1`
# PDL1 <- ggplot(data, aes(group, PDL1variable))+
#   geom_boxplot()+
#   scale_y_continuous(name = "PD-L1: H-Score") +#, limits = c(0, 60)) + #ylim=c(0, ymx*1.02)
#   geom_jitter(aes(x = group, y = PDL1variable, color = group), width = 0.2, height = 0.01, size = 5) +
#   geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
#               y_position = c(32),
#               map_signif_level = T, textsize = 4, vjust=0.4)+
#   theme_bw(base_size = 18) +
#   theme(legend.position = "none") +
#   theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
# PDL1

CD45variable = data$`CD45`
CD45 <- ggplot(data, aes(group, CD45variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD45")+#, limits = c(0, 5)) +
  geom_jitter(aes(x = group, y = CD45variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD45

HLAvariable = data$`HLA`
HLA <- ggplot(data, aes(group, HLAvariable))+
  geom_boxplot()+
  scale_y_continuous(name = "HLA") +
  geom_jitter(aes(x = group, y = HLAvariable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
HLA

PD1variable = data$`PD1`
PD1 <- ggplot(data, aes(group, PD1variable))+
  geom_boxplot()+
  scale_y_continuous(name = "PD1") +
  geom_jitter(aes(x = group, y = PD1variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
               map_signif_level = T, textsize = 4, vjust=0.4,test = "wilcox.test")+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
PD1


CD20variable = data$`CD20`
CD20 <- ggplot(data, aes(group, CD20variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD20") +
  geom_jitter(aes(x = group, y = CD20variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD20

CD57variable = data$`CD57`
CD57 <- ggplot(data, aes(group, CD57variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD57") +
  geom_jitter(aes(x = group, y = CD57variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD57

CD68variable = data$`CD68`
CD68 <- ggplot(data, aes(group, CD68variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD68") +
  geom_jitter(aes(x = group, y = CD68variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")), map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD68

FOXP3variable = data$`FOXP3`
FOXP3 <- ggplot(data, aes(group, FOXP3variable))+
  geom_boxplot()+
  scale_y_continuous(name = "FOXP3") +
  geom_jitter(aes(x = group, y = FOXP3variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
FOXP3

CD8variable = data$`CD8`
CD8 <- ggplot(data, aes(group, CD8variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD8") +
  geom_jitter(aes(x = group, y = CD8variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD8

CD4variable = data$`CD4`
CD4 <- ggplot(data, aes(group, CD4variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD4") +
  geom_jitter(aes(x = group, y = CD4variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD4



tiff("Spleen 123.tiff", units="in", width=12, height=14, res=300)
(CD45 | PD1) / 
  #(PDL1 | PD1 ) /
  (CD4 | CD8) /
  (FOXP3 | HLA) /
  (CD20 | CD57) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = 'Spleen: 19-331-123', 
                  tag_levels = 'A',
                  theme = theme(plot.title = element_text(size = 18))) 
dev.off()





###
###
### Spleen 123
###
###

data <- read_excel("C:/Users/edmondsonef/Desktop/Humanized/MHL Humanized Tissue Counts.xlsx", 
                   sheet = "123 Tumor")

group = data$`Strain`

PDL1variable = data$`PDL1`
PDL1 <- ggplot(data, aes(group, PDL1variable))+
  geom_boxplot()+
  scale_y_continuous(name = "PD-L1") +#, limits = c(0, 60)) + #ylim=c(0, ymx*1.02)
  geom_jitter(aes(x = group, y = PDL1variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
PDL1

CD45variable = data$`CD45`
CD45 <- ggplot(data, aes(group, CD45variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD45")+#, limits = c(0, 5)) +
  geom_jitter(aes(x = group, y = CD45variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD45

HLAvariable = data$`HLA`
HLA <- ggplot(data, aes(group, HLAvariable))+
  geom_boxplot()+
  scale_y_continuous(name = "HLA") +
  geom_jitter(aes(x = group, y = HLAvariable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
HLA

PD1variable = data$`PD1`
PD1 <- ggplot(data, aes(group, PD1variable))+
  geom_boxplot()+
  scale_y_continuous(name = "PD1") +
  geom_jitter(aes(x = group, y = PD1variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
PD1


CD20variable = data$`CD20`
CD20 <- ggplot(data, aes(group, CD20variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD20") +
  geom_jitter(aes(x = group, y = CD20variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD20

CD57variable = data$`CD57`
CD57 <- ggplot(data, aes(group, CD57variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD57") +
  geom_jitter(aes(x = group, y = CD57variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD57

CD68variable = data$`CD68`
CD68 <- ggplot(data, aes(group, CD68variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD68") +
  geom_jitter(aes(x = group, y = CD68variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")), map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD68

FOXP3variable = data$`FOXP3`
FOXP3 <- ggplot(data, aes(group, FOXP3variable))+
  geom_boxplot()+
  scale_y_continuous(name = "FOXP3") +
  geom_jitter(aes(x = group, y = FOXP3variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
FOXP3

CD8variable = data$`CD8`
CD8 <- ggplot(data, aes(group, CD8variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD8") +
  geom_jitter(aes(x = group, y = CD8variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD8

CD4variable = data$`CD4`
CD4 <- ggplot(data, aes(group, CD4variable))+
  geom_boxplot()+
  scale_y_continuous(name = "CD4") +
  geom_jitter(aes(x = group, y = CD4variable, color = group), width = 0.2, height = 0.01, size = 5) +
  geom_signif(comparisons = list(c("Nivolumab", "Vehicle")),
              map_signif_level = T, textsize = 4, vjust=0.4)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())
CD4



tiff("Tumor 123.tiff", units="in", width=12, height=18, res=300)
(CD45 | CD68) / 
  (PDL1 | PD1 ) /
  (CD4 | CD8) /
  (FOXP3 | HLA) /
  (CD20 | CD57) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = 'Tumor: 19-331-123', 
                  tag_levels = 'A',
                  theme = theme(plot.title = element_text(size = 18))) 
dev.off()
