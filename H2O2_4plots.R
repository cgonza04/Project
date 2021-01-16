#Label Trt and set as categorical factors
AN_H2O2$Trt <- as.factor((AN_H2O2$Trt))
AN_H2O2$Trt = factor(AN_H2O2$Trt, labels = c("control", "Cu", "Cu+10AN", "Cu+100AN", "100AN"))

class(AN_H2O2$Trt)

#4 plots:


#For Boxplot
#For AG_H2O2
library(tidyr)
lAG_H2O2 <- pivot_longer(AG_H2O2, 2:8, "time")
lAG_H2O2$Trt <- factor(lAG_H2O2$Trt,
                       levels = 1:5,
                       labels = c("control", "Cu", "Cu+10AG", "Cu+100AG", "100AG"))#to order

lAG_H2O2$time <- factor(lAG_H2O2$time,
                        levels = c("Zero.seventyfive", "One.five", "Three", 
                                   "Six", "Twelve", "Twentyfour", "Fourtyeight"))

library(ggplot2)
p <- ggplot(lAG_H2O2, aes(x = time, y = value, fill = Trt, color = Trt)) +
  geom_boxplot() +
  labs(y = "H2O2 concentration", fill = "Treatment", color = "Treatment") +
  theme_bw()
p 

#The highest changes in H2O2 levels in plants were induced at early stages of the treatments.


#For line graph
#For AG_H2O2
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
lH2O2_AG2 <- pivot_longer(H2O2_AG2, 3:9, "time")

lH2O2_AG2$time <- factor(lH2O2_AG2$time,
                         levels = c("mean0.75h", "mean1.5h", "mean3h",
                                    "mean6h", "mean12h", "mean24h",
                                    "mean48h"),
                         labels = c("0.75h", "1.5h", "3h",
                                    "6h", "12h", "24h", "48h"))
lH2O2_AG2$Trt <- factor(lH2O2_AG2$Trt,
                        levels = 1:5,
                        labels = c("Control", "Cu", "Cu+10AG", "Cu+100AG", "100AG"))#to order


j <- ggplot(lH2O2_AG2, aes(x = time, y = value,group = Trt, )) +
  geom_line(aes(color = Trt)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  ggtitle("H2O2 concentration in Lemna minor 
        after treated with Copper and A.glutinosa extracts")+
  labs(y = "H2O2(% of the control)", fill = "Treatment", color = "Treatment") +
  theme_bw()
j

#The highest changes in H2O2 levels in plants were observed at early stages of treatments.

#For both box plot and line plots in a single
library(ggplot2)
library(cowplot)
p1 <- ggplot(lAN_H2O2, aes(x = time, y = value,fill = Trt, color = Trt, )) +
  geom_boxplot() 
p2 <- ggplot(lAG_H2O2, aes(x = time, y = value,fill = Trt, color = Trt, )) +
  geom_boxplot() 
p3 <- ggplot(lH2O2_AN2,aes(x = time, y = value, group = Trt,)) +
  geom_line(aes(color = Trt)) + 
  geom_line(linetype = "dashed")+
  geom_point()
p4 <- ggplot(lH2O2_AG2,aes(x = time, y = value, group = Trt,)) +
  geom_line(aes(color = Trt)) + 
  geom_line(linetype = "dashed")+
  geom_point()
plot_grid(p1, p2, p3, p4)

# now add the title
title <- ggdraw() + 
  draw_label(
    "H2O2 concentration in Lemna minor 
        after treated with Copper and leaf litter extracts",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
