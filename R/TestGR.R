###Frond Growth AG

#Subset of 1,2, 3, 4 and 5 treatments.
GR2 <- subset(GR, Variant == 1 | Variant == 2 | Variant == 3 | Variant == 4| Variant == 5)

# Build the linear model
model  <- lm(FrondGR~Variant, data = GR2)

#Shapiro test
shap <- shapiro.test(model[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
##install.packages("agricolae")
library(agricolae)

KrusFG <- agricolae::kruskal(GR2$FrondGR, GR2$Variant)


### Root Growth AG

# Build the linear model
model2  <- lm(RootGR~Variant, data = GR2)

#Shapiro test
shap2 <- shapiro.test(model2[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
KrusRG <- agricolae::kruskal(GR2$RootGR, GR2$Variant)


### Frond Growth AN

#Subset of 1,2, 6, 7 and 8 treatments.
GR3 <- subset(GR, Variant == 1 | Variant == 2 | Variant == 6 | Variant == 7
              | Variant == 8)

# Build the linear model
model3  <- lm(FrondGR~Variant, data = GR3)

#Shapiro test
shap3 <- shapiro.test(model3[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
KrusFG3 <- agricolae::kruskal(GR3$FrondGR, GR3$Variant)

### Root Growth AN

# Build the linear model
model4  <- lm(RootGR~Variant, data = GR3)

#Shapiro test
shap4 <- shapiro.test(model4[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
KrusRG4 <- agricolae::kruskal(GR3$RootGR, GR3$Variant)

#Make combined boxplots that show the effect of both AG and AN in the presence of Cu.

par(mfrow=c(2,2),mar=c(3,4,2,0),oma=c(6,6,5,3))

boxplot(FrondGR~Variant, data = GR2,
        ylim= c(-0.4,0.3),
        ylab="Frond", cex.lab=1.4,
        xlab= NULL, xaxt='n',
        main = "AG",
        border=c("2","3","4","5","8"))
text(x=(1:5),y=c(0.23, -0.2, 0.05, 0.09, 0.16) , c("a","e", "d","c","b"))
boxplot(FrondGR~Variant, data = GR3,
        ylim= c(-0.4,0.3),
        ylab=NULL, yaxt='n',
        xlab=NULL, xaxt='n',
        main = "AN"
        ,border=c("2","3","6","7","darkgreen"))
text(x=(1:5),y=c(0.23, -0.2, 0.04, 0.02, 0.22) , c("a","e","c","d","b"))
boxplot(RootGR~Variant, data = GR2,
        ylim= c(0,0.9),
        ylab="Root",cex.lab=1.4,
        xlab=NULL, names = c("Control", "Cu", 
                             "Cu+10AG","Cu+100AG", 
                             "100AG"),
        main = NULL
        ,border=c("2","3","4","5","8"))
text(x=(1:5),y=c(0.82, 0.05, 0.05, 0.22, 0.43) , c("a","d","d","c","b"))
boxplot(RootGR~Variant, data = GR3,
        ylim= c(0,0.9),
        ylab=NULL, yaxt='n',
        xlab=NULL, names = c("Control", "Cu", 
                             "Cu+10AN","Cu+100AN", 
                             "100AN"), 
        main = NULL
        ,border=c("2","3","6","7","darkgreen"))
text(x=(1:5),y=c(0.82, 0.04, 0.15, 0.44, 0.57) , c("a","e","d","c","b"))
mtext("Effect of Cu, Ag and AN on Growth Rate", side = 3, line = 2, 
      outer = TRUE, cex = 1.5)
mtext("Treatments", side = 1, line = 2, 
      outer = TRUE, cex = 1.2)
mtext("Growth Rate (mm d^-1)", side = 2, line = 3, 
      outer = TRUE, cex = 1.2)


#Check relationship between Frond and Growth rate. 

#Test normality with linear model and Shapiro test.
model5  <- lm(FrondGR~RootGR, data = GR)
shap5 <- shapiro.test(model5[["residuals"]]) # => p = 8.075e-07
shap5

#The relation between Frond and Root Growth is non-parametric. 
#This is why we use the Spearman correlation formula. 


#Because the value of the correlation is 0.84, here is a very strong correlation. 
#https://www.statstutor.ac.uk/resources/uploaded/spearmans.pdf

library("ggpubr")
ggscatter(GR, x = "RootGR", y = "FrondGR", 
          add = "reg.line", conf.int = TRUE, color = 4, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Root Growth (mm d^-1)", ylab = "Frond Growth (mm d^-1)",
          title = "Correlation between Frond and Root Growth")+
          theme(plot.title = element_text(hjust = 0.5))


