       #Lipid 4 Plots with table means

#####LIPIDS LINEGRAPH###
#make the table will the means of each treatment so we have only 1 row per 
#treatment per species  (so 6 rows)
#AG
library(dplyr)
Ag_Lipidm <- AG_Lipid %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(mean0.75h = mean(Zero.seventyfive),
            mean1.5h = mean(One.five),
            mean3h = mean(Three),
            mean6h = mean(Six),
            mean12h = mean(Twelve),
            mean24h = mean(Twentyfour),
            mean48h = mean(Fourtyeight))
#AN LIPIDS
library(dplyr)
An_Lipidm <- AN_Lipid %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(mean0.75h = mean(Zero.seventyfive),
            mean1.5h = mean(One.five),
            mean3h = mean(Three),
            mean6h = mean(Six),
            mean12h = mean(Twelve),
            mean24h = mean(Twentyfour),
            mean48h = mean(Fourtyeight))



####long tables lipid for boxplot
##AG
lAG_Lipid <- pivot_longer(AG_Lipid, 2:8, "time")
lAG_Lipid$Trt <- factor(lAG_Lipid$Trt,
                        levels = 1:5,
                        labels = c("Control", "Cu", "Cu+10DOM", "Cu+100DOM", "100DOM"))#to order

lAG_Lipid$time <- factor(lAG_Lipid$time,
                         levels = c ("Zero.seventyfive", "One.five", "Three", 
                                     "Six", "Twelve", "Twentyfour", "Fourtyeight"))
                         
##AN
lAN_Lipid <- pivot_longer(AN_Lipid, 2:8, "time")
lAN_Lipid$Trt <- factor(lAN_Lipid$Trt,
                        levels = 1:5,
                        labels = c("Control", "Cu", "Cu+10DOM", "Cu+100DOM", "100DOM"))#to order

lAN_Lipid$time <-  factor(lAN_Lipid$time,
                          levels = c("Zero.seventyfive", "One.five", "Three", 
                                     "Six", "Twelve", "Twentyfour", "Fourtyeight"))
                         
#long table for Cu AN for line plots
#AG

lAG_Lipidm <- pivot_longer(Ag_Lipidm, 3:9, "time") #for AG

lAG_Lipidm$time <- factor(lAG_Lipidm$time,
                          levels = c("mean0.75h", "mean1.5h", "mean3h",
                                     "mean6h", "mean12h", "mean24h",
                                     "mean48h"),
                          labels = c("0.75h", "1.5h", "3h",
                                     "6h", "12h", "24h", "48h"))
lAG_Lipidm$Trt <- factor(lAG_Lipidm$Trt,
                         levels = 1:5,
                         labels = c("Control", "Cu", "Cu+10DOM", "Cu+100DOM", "100DOM"))#to order
#AN
lAN_Lipidm <- pivot_longer(An_Lipidm, 3:9, "time") #for AN


lAN_Lipidm$time <- factor(lAN_Lipidm$time,
                          levels = c("mean0.75h", "mean1.5h", "mean3h",
                                     "mean6h", "mean12h", "mean24h",
                                     "mean48h"),
                          labels = c("0.75h", "1.5h", "3h",
                                     "6h", "12h", "24h", "48h"))

lAN_Lipidm$Trt <- factor(lAN_Lipidm$Trt,
                         levels = 1:5,
                         labels = c("Control", "Cu", "Cu+10DOM", "Cu+100DOM", "100DOM"))#to order


###FUSING THE 4 GRAPHS OF LIPIDS (Boxplot x Lineplot)
library(ggplot2)
library(cowplot)
p1 <- ggplot(lAG_Lipid, aes(x = time, y = value,fill = Trt, color = Trt, )) +
  geom_boxplot()+
  labs(y = "MDA(% of the control)",  x = NULL, fill = "Treatment", color = "Treatment") +
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  theme(legend.position = "none") #remove the legend

p2 <- ggplot(lAN_Lipid, aes(x = time, y = value,fill = Trt, color = Trt, )) +
  geom_boxplot()+
  labs(y = NULL, fill = "Treatment", color = "Treatment", x = NULL) +
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  theme(legend.position = "none")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


p3 <- ggplot(lAG_Lipidm,aes(x = Time, y = value, group = Trt,)) +
  geom_line(aes(color = Trt)) + 
  geom_line(linetype = "dashed")+
  geom_point()+
  labs(y = "MDA(% of the control)", fill = "Treatment", color = "Treatment") +
  theme_bw()+
  theme(legend.position = "none") #remove the legend
  
p4 <- ggplot(lAN_Lipidm,aes(x = Time, y = value, group = Trt,)) +
  geom_line(aes(color = Trt)) + 
  geom_line(linetype = "dashed")+
  labs(y = NULL, color = "Treatment") +
  geom_point()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

  labs(title = "Lipid peroxidation expressed as MDA concentration in L.minor 
    after treated with Copper and Leaf Litter extracts")+
  theme(plot.title = element_text(lineheight = 0.9))

#putting legend one side
legend <- get_legend(
  p1 + theme(legend.position = "right")) 

plot_grid(plots,legend, rel_widths = c(3, .4),
labels = c('AG', 'AN', '', ''),
align="hv"
)
