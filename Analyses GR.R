#Adding CU, ten and hun columns to Growth data set

#Adding a column with Cu presence 
GR$Copper <- GR$Variant !=1 & GR$Variant !=5

#Adding a column with 10 mg/L 
GR$Ten.mg.DOC <- GR$Variant ==3 | GR$Variant ==6

#Adding a column with 100 mg/L 
GR$Hundred.mg.DOC <- GR$Variant ==4 | GR$Variant ==5 | GR$Variant ==7 | GR$Variant ==8




###Frond Growth 1

#Subset of 1,2, 3, 4 and 5 treatments.
GR2 <- subset(GR, Variant == 1 | Variant == 2 | Variant == 3 | Variant == 4| Variant == 5)

#Testing normality

# Build the linear model
model  <- lm(FrondGR~Variant, data = GR2)

# Create a QQ plot of residuals
qqnorm(model[["residuals"]], main = "QQ plot for model")
qqline(model[["residuals"]])

#Shapiro test
shap <- shapiro.test(model[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
##install.packages("agricolae")
library(agricolae)

KrusFG <- agricolae::kruskal(GR2$FrondGR, GR2$Variant)

boxplot(FrondGR~Variant, data = GR2,
        ylim= c(-0.4,0.3),
        ylab="Frond Growth rate (mm d^-1)", xlab="Treatments", 
        main = "Effect of Cu and AG on Frond Growth rate"
        ,border=c("2","3","4","5","8"))
legend("topright", legend=c("1: Control", "2: Cu", "3: Cu+10-DOC_AG ", 
                            "4: Cu+100-DOC_AG ", "5: 100-DOC_AG"), cex = 0.8,
       bty = "n", inset=c(0.03,0.7))
text(x=(1:5),y=c(0.23, -0.2, 0.05, 0.09, 0.16) , c("a","e", "d","c","b"))

#Interpretation of plot: 
#The control had the greater FGR (0.19 mm d^-1), followed by the treatment with 
#100 mg/L DOC derived from A. glutinosa (T5) (0.10mm d^-1). The next highest 
#value is for the treatment with 100 mg/L DOC derived from A. glutinosa (T4) 
# (0.04mm d^-1).The treatment with 6.4 mg/L of Cu (T2) (-0.3 mm d^-1) and the
#one with 10 mg/L DOC derived from A. glutinosa (T3) both showed negative FGR values: 
#-0.02 and -0.3mm d^-1 respectively.



### Root Growth 1

#Testing normality

# Build the linear model
model2  <- lm(RootGR~Variant, data = GR2)

# Create a QQ plot of residuals
qqnorm(model2[["residuals"]], main = "QQ plot for model")
qqline(model2[["residuals"]])

#Shapiro test
shap2 <- shapiro.test(model2[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
KrusRG <- agricolae::kruskal(GR2$RootGR, GR2$Variant)

boxplot(RootGR~Variant, data = GR2,
        ylim= c(0,0.9),
        ylab="Root Growth rate (mm d^-1)", xlab="Treatments", 
        main = "Effect of Cu and AG on Root Growth rate"
        ,border=c("2","3","4","5","8"))
legend("topright", legend=c("1: Control", "2: Cu", "3: Cu+10-DOC_AG ", 
                            "4: Cu+100-DOC_AG ", "5: 100-DOC_AG"), cex = 0.8,
       bty = "n", inset=c(0.03,0.03))
text(x=(1:5),y=c(0.82, 0.05, 0.05, 0.22, 0.43) , c("a","d","d","c","b"))

#Interpretation of plot:  
#The control had the highest RGR (0.72 mm d^-1). The treatment with 100 mg/L DOC 
#derived from A. glutinosa (T5) showed the second highest value (0.35 mm d^-1), 
#followed by the treatment with Cu+ 100 mg/L DOC derived from A. glutinosa (T4) 
#(0.15mm d^-1). The treatment with 6.4 mg/L of Cu (T2) an the one with 
#10 mg/L DOC derived from A. glutinosa (T3) showed no RGR whatsoever. 


### Frond Growth 2

#Subset of 1,2, 6, 7 and 8 treatments.
GR3 <- subset(GR, Variant == 1 | Variant == 2 | Variant == 6 | Variant == 7
              | Variant == 8)

#Testing normality

# Build the linear model
model3  <- lm(FrondGR~Variant, data = GR3)

# Create a QQ plot of residuals
qqnorm(model3[["residuals"]], main = "QQ plot for model")
qqline(model3[["residuals"]])

#Shapiro test
shap3 <- shapiro.test(model3[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
KrusFG3 <- agricolae::kruskal(GR3$FrondGR, GR3$Variant)

boxplot(FrondGR~Variant, data = GR3,
        ylim= c(-0.4,0.3),
        ylab="Frond Growth rate (mm d^-1)", xlab="Treatments", 
        main = "Effect of Cu and AN on Frond Growth rate"
        ,border=c("2","3","6","7","darkgreen"))
legend("topright", legend=c("1: Control", "2: Cu", "6: Cu+10-DOC_AN ", 
                            "7: Cu+100-DOC_AN ", "8: 100-DOC_AN"), cex = 0.8,
       bty = "n", inset=c(0.03,0.7))
text(x=(1:5),y=c(0.23, -0.2, 0.04, 0.02, 0.22) , c("a","e","c","d","b"))

#Interpretation of plot: 
#The control had the greater FGR (0.19mm d^-1). The treatment with 100 mg/L DOC 
#derived from A. negundo (T8) had the next highest value (0.18mm d^-1). The 
#treatments with 10 mg/L DOC (T6) and 100 mg/L DOC derived from A. negundo (T7) 
#bot had negative values, as well as the treatment with 6.4 mg/L of Cu (T2). 
#The values were -0.003, -0.02 and -0.3 respectively.

### Root Growth 2

#Testing normality

# Build the linear model
model4  <- lm(RootGR~Variant, data = GR3)

# Create a QQ plot of residuals
qqnorm(model4[["residuals"]], main = "QQ plot for model")
qqline(model4[["residuals"]])

#Shapiro test
shap4 <- shapiro.test(model4[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
KrusRG4 <- agricolae::kruskal(GR3$RootGR, GR3$Variant)

boxplot(RootGR~Variant, data = GR3,
        ylim= c(0,0.9),
        ylab="Root Growth rate (mm d^-1)", xlab="Treatments", 
        main = "Effect of Cu and AN on Root Growth rate"
        ,border=c("2","3","6","7","darkgreen"))
legend("topright", legend=c("1: Control", "2: Cu", "6: Cu+10-DOC_AN ", 
                            "7: Cu+100-DOC_AN ", "8: 100-DOC_AN"), cex = 0.8,
       bty = "n", inset=c(0.03,0.03))
text(x=(1:5),y=c(0.82, 0.04, 0.15, 0.44, 0.57) , c("a","e","d","c","b"))
                        
#Interpretation of plot:  
#The control had the highest RGR (0.72 mm d^-1). The treatment with 100 mg/L DOC 
#derived from A. negundo (T8) showed the second highest value (0.45 mm d^-1), 
#followed by the treatment with Cu+ 100 mg/L DOC derived from A. negundo (T7) 
#(0.34mm d^-1). The treatment with Cu+ 10 mg/L DOC derived from A. negundo (T6)
#goes next, with a RGR of 0.09mm d^-1. The treatment with 6.4 mg/L of Cu (T2) 
# showed no RGR whatsoever. 


### Frond Growth 3

#Subset of 1,2, 3, 4, 6 and 7 treatments.
GR5 <- subset(GR, Variant == 1 | Variant == 2 | Variant == 3 | Variant == 4| Variant == 6| Variant == 7)

#Testing normality

# Build the linear model
model7  <- lm(FrondGR~Variant, data = GR5)

# Create a QQ plot of residuals
qqnorm(model7[["residuals"]], main = "QQ plot for model")
qqline(model7[["residuals"]])

#Shapiro test
shap7 <- shapiro.test(model7[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
KrusFG7 <- agricolae::kruskal(GR5$FrondGR, GR5$Variant)

boxplot(FrondGR~Variant, data = GR5,
        ylim= c(-0.4,0.3),
        ylab="Frond Growth rate (mm d^-1)", xlab="Treatments", 
        main = "Effect of AG and AN in the presence 
        of Cu on Frond Growth rate"
        ,border=c(2:7))
legend("topright", legend=c("1: Control", "2: Cu", "3: Cu+10-DOC_AG", 
                            "4: Cu+100-DOC_AG", "6: Cu+10-DOC_AN", "7: Cu+100-DOC_AN"), cex = 0.8,
       bty = "n", inset=c(0.03,0.03))
text(x=(1:6),y=c(0.22, -0.2, 0.04, 0.09, 0.05, 0.02) , c("a","e","d","b","c","d"))

#Interpretation of plot: 
#The control showed the highest FGR (0.19mm d^-1), followed by the treatment with 
#Cu + 100 mg/L DOC derived from A. glutinosa (T4)(0.04mm d^-1). The third highest 
#was shown in the treatment with Cu + 10 mg/L DOC derived from A. negundo (T6) 
#The treatments with Cu + 10 mg/L DOC derived from A. glutinosa (T3) and the 
#one with Cu + 100 mg/L DOC derived from A. negundo (T7) were next, with no significant
#differences between them. The treatment with 6.4 mg/L of Cu (T2) showed the lowest
#value (-0.3mm d^-1)



### Root Growth 3

#Testing normality

# Build the linear model
model8  <- lm(RootGR~Variant, data = GR5)

# Create a QQ plot of residuals
qqnorm(model8[["residuals"]], main = "QQ plot for model")
qqline(model8[["residuals"]])

#Shapiro test
shap8 <- shapiro.test(model8[["residuals"]])

#We will use Kruskal wallis test because the data is not normal.
KrusRG8 <- agricolae::kruskal(GR5$RootGR, GR5$Variant)

boxplot(RootGR~Variant, data = GR5,
        ylim= c(0,0.9),
        ylab="Root Growth rate (mm d^-1)", xlab="Treatments", 
        main = "Effect of AG and AN in the presence 
        of Cu on Root Growth rate"
        ,border=c(2:7))
legend("topright", legend=c("1: Control", "2: Cu", "3: Cu+10-DOC_AG", 
                            "4: Cu+100-DOC_AG", "6: Cu+10-DOC_AN", "7: Cu+100-DOC_AN"), cex = 0.8,
       bty = "n", inset=c(0.03,0.03))
text(x=(1:6),y=c(0.82, 0.04, 0.04, 0.22, 0.15, 0.43) , 
     c("a","e","e","c","d","b"))


#Interpretation of plot:  
#The control had the highest RGR (0.72mm d^-1), followed by The treatment with 
#100 mg/L DOC derived from A. negundo (T7)(0.34mm d^-1). Next, we have the 
#treatment with with 100 mg/L DOC derived from A. glutinosa (T4) and the one 
#with 10 mg/L DOC derived from A. negundo (T6), each with 0.15 and 0.09mm d^-1
#respectively. The treatment with with 10 mg/L DOC derived from 
#A. glutinosa (T3) and the treatment with 6.4 mg/L of Cu (T2) showed no RGR.


