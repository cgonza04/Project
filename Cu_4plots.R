#line plot for Cu AG

#long tables for boxplot for Cu AG
lCu_AG <- pivot_longer(Cu_AG, 2:8, "time")
lCu_AG$Trt <- factor(lCu_AG$Trt,
                     levels = 1:4,
                     labels = c("control", "Cu+10_DOM", "Cu+100_DOM", 
                                "100_DOM"))
namex1 <- factor(lCu_AG$time, levels = c("Zero.seventyfive", "One.five", 
                                         "Three","Six", "Twelve", "Twentyfour",
                                         "Fourtyeight"))

#long tables for boxplot for Cu AN
lCu_AN <- pivot_longer(Cu_AN, 2:8, "time")
lCu_AN$Trt <- factor(lCu_AN$Trt,
                     levels = 1:4,
                     labels = c("control", "Cu+10_DOM", "Cu+100_DOM", 
                                "100_DOM"))

#long table for Cu AG for line plots
lCu_AGmeans <- pivot_longer(Cu_AGmeans, 3:9, "time")

lCu_AGmeans$Trt <- factor(lCu_AGmeans$Trt,
                          levels = 1:4,
                          labels = c("control", "Cu+10_DOM", "Cu+100_DOM", 
                                     "100_DOM"))#to order
#to put the x names in the good order
name <- factor(lCu_AGmeans$time, levels = c("mean0.75", "mean1.5", "mean3",
                                              "mean6", "mean12", "mean24", 
                                              "mean48") )

#long table for Cu AN for line plots
lCu_ANmeans <- pivot_longer(Cu_ANmeans, 3:9, "time") #for AN

lCu_ANmeans$Trt <- factor(lCu_ANmeans$Trt,
                          levels = 1:4,
                          labels = c("control", "Cu+10_DOM", "Cu+100_DOM", 
                                     "100_DOM"))


library('ggplot2')

Cu_AN

library(ggplot2)
library(cowplot)
p1 <- ggplot(lCu_AN, aes(x = namex1, y = value, fill = Trt, 
                         color = Trt, scale.default())) +
  geom_boxplot()+
  labs(y = "Cu concentration", fill = "Treatment", color = "Treatment") +
  theme_bw()
p2 <- ggplot(lCu_AG, aes(x = namex1, y = value ,fill = Trt, 
                        color = Trt, scale.default())) +
  geom_boxplot()+
  theme_bw()
p3 <- ggplot(lCu_ANmeans, aes(x = name, y = value,group = Trt, )) +
  geom_line(aes(color = Trt)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  labs(y = "Cu concentration", fill = "Treatment", color = "Treatment") +
  theme_bw()
p4 <- ggplot(lCu_AGmeans, aes(x = name, y = value,group = Trt, )) +
  geom_line(aes(color = Trt)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  labs(y = "Cu concentration", fill = "Treatment", color = "Treatment") +
  theme_bw()
plot_grid(p1, p2, p3, p4)
#Putting labels
plot_grid(
  p1, p2, p3, p4,
  labels = c('AN', '', 'AG', ''),
  align="hv"
  )

