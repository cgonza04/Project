##MAKE THE TABLE WITH MEANS FOR EACH TRT (=ONLY ONE LINE PER TRT PER SPECIES)
library(dplyr)

Cu_AGmeans <- Cu_AG %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(mean0.75 = mean(Zero.seventyfive),
            mean1.5 = mean(One.five),
            mean3 = mean(Three),
            mean6 =mean(Six),
            mean12 = mean(Twelve),
            mean24 = mean(Twentyfour),
            mean48 = mean(Fourtyeight))
#colnames(Cu2)[colnames(Cu2) == "mean"] <- "meanFourtyeight"

#Cu_ANmeans
Cu_ANmeans <- Cu_AN %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(mean0.75 = mean(Zero.seventyfive),
            mean1.5 = mean(One.five),
            mean3 = mean(Three),
            mean6 =mean(Six),
            mean12 = mean(Twelve),
            mean24 = mean(Twentyfour),
            mean48 = mean(Fourtyeight))


#long tables for boxplot for Cu AG
lCu_AG <- pivot_longer(Cu_AG, 2:8, "time")
lCu_AG$Trt <- factor(lCu_AG$Trt,
                     levels = 1:4,
                     labels = c("Control", "Cu","Cu+10_DOM", "Cu+100_DOM"))
namex1 <- factor(lCu_AG$time, levels = c("Zero.seventyfive", "One.five", 
                                         "Three","Six", "Twelve", "Twentyfour",
                                         "Fourtyeight"))

#long tables for boxplot for Cu AN
lCu_AN <- pivot_longer(Cu_AN, 2:8, "time")
lCu_AN$Trt <- factor(lCu_AN$Trt,
                     levels = 1:4,
                     labels = c("Control", "Cu","Cu+10_DOM", "Cu+100_DOM"))

#long table for Cu AG for line plots
lCu_AGmeans <- pivot_longer(Cu_AGmeans, 3:9, "time")

lCu_AGmeans$Trt <- factor(lCu_AGmeans$Trt,
                          levels = 1:4,
                          labels = c("Control", "Cu","Cu+10_DOM", "Cu+100_DOM"))
                          #to order
#to put the x names in the good order
name <- factor(lCu_AGmeans$time, levels = c("mean0.75", "mean1.5", "mean3",
                                            "mean6", "mean12", "mean24",
                                            "mean48"),
               labels = c("0.75", "1.5", "3","6", "12", "24","48"))

#long table for Cu AN for line plots
lCu_ANmeans <- pivot_longer(Cu_ANmeans, 3:9, "time") #for AN

lCu_ANmeans$Trt <- factor(lCu_ANmeans$Trt,
                          levels = 1:4,
                          labels = c("Control", "Cu","Cu+10_DOM", "Cu+100_DOM"))

#make the plots
library(ggplot2)
library(cowplot)

p1 <- ggplot(lCu_AN, aes(x = namex1, y = value, fill = Trt, 
                         color = Trt, scale.default())) +
  geom_boxplot()+
  labs(y = "Cu concentration", fill = "Treatment", color = "Treatment", x = NULL) +
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  theme(legend.position = "none") #remove the legend

p2 <- ggplot(lCu_AG, aes(x = namex1, y = value ,fill = Trt, 
                        color = Trt, scale.default())) +
  geom_boxplot()+
  labs(y = NULL, fill = "Treatment", color = "Treatment", x = NULL) +
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  theme(legend.position = "none")+ 
theme(axis.ticks.y = element_blank(),
      axis.text.y = element_blank())

p3 <- ggplot(lCu_ANmeans, aes(x = name, y = value,group = Trt, )) +
  geom_line(aes(color = Trt)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  labs(y = "Cu concentration", fill = "Treatment", color = "Treatment", x = "Time") +
  theme_bw()+
  theme(legend.position = "none")

p4 <- ggplot(lCu_AGmeans, aes(x = name, y = value,group = Trt, )) +
  geom_line(aes(color = Trt)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  labs(y = NULL, fill = "Treatment", color = "Treatment", x = "Time") +
  theme_bw()+
  theme(legend.position = "none")+ 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

#add title
plots <- plot_grid(p1, p2, p3, p4)+ 
  labs(title = "Copper concentration in Lemna minor
after treated with Copper and Leaf Litter extracts")+
  theme(plot.title = element_text(lineheight = 0.9))

#put once the legend on right side
legend <- get_legend(p1 + theme(legend.position = "right"))
plot_grid(plots,legend, rel_widths = c(3, .4),
          labels = c('AN', 'AG', '', ''),
          align="hv")
