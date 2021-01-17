###H2O2 4 Plots with table means

#####H2O2 LINEGRAPH###
#make the table will the means of each treatment so we have only 1 row per 
#treatment per species  (so 6 rows)
#AG
library(dplyr)
H2O2_AG2 <- AG_H2O2 %>% 
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
H2O2_AN2 <- AN_H2O2 %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(mean0.75h = mean(Zero.seventyfive),
            mean1.5h = mean(One.five),
            mean3h = mean(Three),
            mean6h = mean(Six),
            mean12h = mean(Twelve),
            mean24h = mean(Twentyfour),
            mean48h = mean(Fourtyeight))



####long tables H2O2 for boxplot
##AG
lAG_H2O2 <- pivot_longer(AG_H2O2, 2:8, "Time")
lAG_H2O2$Trt <- factor(lAG_H2O2$Trt,
                        levels = 1:5,
                        labels = c("Control", "Cu", "Cu+10DOM", "Cu+100DOM", "100DOM"))#to order

lAG_H2O2$Time <- factor(lAG_H2O2$Time,
                         levels = c ("Zero.seventyfive", "One.five", "Three", 
                                     "Six", "Twelve", "Twentyfour", "Fourtyeight"))
                         
##AN
lAN_H2O2 <- pivot_longer(AN_H2O2, 2:8, "Time")
lAN_H2O2$Trt <- factor(lAN_H2O2$Trt,
                       levels = 1:5,
                       labels = c("Control", "Cu", "Cu+10DOM", "Cu+100DOM", "100DOM"))#to order


lAN_H2O2$Time <-  factor(lAN_H2O2$Time,
                          levels = c("Zero.seventyfive", "One.five", "Three", 
                                     "Six", "Twelve", "Twentyfour", "Fourtyeight"))
                         
#long table for line plots
#AG

lH2O2_AG2 <- pivot_longer(H2O2_AG2, 3:9, "Time") #for AG

lH2O2_AG2$Time <- factor(lH2O2_AG2$Time,
                          levels = c("mean0.75h", "mean1.5h", "mean3h",
                                     "mean6h", "mean12h", "mean24h",
                                     "mean48h"),
                          labels = c("0.75h", "1.5h", "3h",
                                     "6h", "12h", "24h", "48h"))
lH2O2_AG2$Trt <- factor(lH2O2_AG2$Trt,
                         levels = 1:5,
                         labels = c("Control", "Cu", "Cu+10DOM", "Cu+100DOM", "100DOM"))#to order
#AN
lH2O2_AN2 <- pivot_longer(H2O2_AN2, 3:9, "Time") #for AN


lH2O2_AN2$Time <- factor(lH2O2_AN2$Time,
                          levels = c("mean0.75h", "mean1.5h", "mean3h",
                                     "mean6h", "mean12h", "mean24h",
                                     "mean48h"),
                          labels = c("0.75h", "1.5h", "3h",
                                     "6h", "12h", "24h", "48h"))

###FUSING THE 4 GRAPHS OF H2O2 (Boxplot x Lineplot)
library(ggplot2)
library(cowplot)

p1 <- ggplot(lAG_H2O2, aes(x = Time, y = value, fill = Trt, color = Trt)) +
  geom_boxplot()+
  labs(y = "H2O2 concentration (% of the control)",  x = NULL, 
       fill = "Treatment", color = "Treatment") +
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  theme(legend.position = "none") #remove the legend

p2 <- ggplot(lAN_H2O2, aes(x = Time, y = value, fill = Trt, color = Trt)) +
  geom_boxplot()+
  labs(y = NULL, fill = "Treatment", color = "Treatment", x = NULL) +
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  theme(legend.position = "none")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

p3 <- ggplot(lH2O2_AG2,aes(x = Time, y = value, group = Trt,)) +
  geom_line(aes(color = Trt)) + 
  geom_line(linetype = "dashed")+
  geom_point()+
  labs(y = "H2O2 concentration (% of the control)", fill = "Treatment", color = "Treatment") +
  theme_bw()+
  theme(legend.position = "none") #remove the legend
  
p4 <- ggplot(lH2O2_AN2,aes(x = Time, y = value, group = Trt,)) +
  geom_line(aes(color = Trt)) + 
  geom_line(linetype = "dashed")+
  labs(y = NULL, color = "Treatment") +
  geom_point()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

plots <- plot_grid(p1, p2, p3, p4)+ 
  labs(title = "H2O2 concentration in L.minor 
    after treated with Copper and Leaf Litter extracts")+
  theme(plot.title = element_text(lineheight = 0.9))
plots

#putting legend one side
legend <- get_legend(
  p1 + theme(legend.position = "right")) 

plot_grid(plots,legend, rel_widths = c(3, .4),
labels = c('AG', 'AN', '', ''),
align="hv"
)

