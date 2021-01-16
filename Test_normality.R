#ALL TESTS fro cu, lipid, h2o2 
#WITHOUT SOURCING
#install.packages("agricolae")

#Testing normality
#This is the same as doing the anova and checking the normality of residuals.

#LIPIDS

#AG

# Build the linear model
model_AG_Lipid  <- lm(Fourtyeight~Trt, data = AG_Lipid)
#shapiro test
shap_AG_Lipid <- shapiro.test(model_AG_Lipid[["residuals"]])
#Kruskall wallis test (to get group letters for the boxplot)
library(agricolae)
Krus_AG_Lipid <- agricolae::kruskal(AG_Lipid$Fourtyeight, AG_Lipid$Trt)
#boxplot:
boxplot(Fourtyeight~Trt, data = AG_Lipid,
        ylim = c(0,300),
        ylab="MDA (% of the control)",
        xlab="Treatments", names = c("Control", "Cu", "Cu+10_AN", 
                                     "Cu+100_AN", "100_AN"),
        main = "Lipid peroxidation expressed as MDA concentration in L. minor 
        after treated with Copper and A.negundo extracts",
        border=c("2","3","6","7","darkgreen"))
text( c("d","a","b","a", "c"),x=(1:5) ,y = c(130,230,170,230,140))

#AN

model_AN_Lipid  <- lm(Fourtyeight~Trt, data = AN_Lipid)#linear model
shap_AN_Lipid <- shapiro.test(model_AN_Lipid[["residuals"]])#Shapiro test
library(agricolae)#Kruskall wallis
Krus_AN_Lipid <- agricolae::kruskal(AN_Lipid$Fourtyeight, AN_Lipid$Trt)

#add the group letters to the boxplot
boxplot(Fourtyeight~Trt, data = AN_Lipid,
        ylim = c(0,300),
        ylab="MDA (% of the control)",
        xlab="Treatments", names = c("Control", "Cu", "Cu+10_AN", 
                                     "Cu+100_AN", "100_AN"),
        main = "Lipid peroxidation expressed as MDA concentration in L. minor 
        after treated with Copper and A.negundo extracts",
        border=c("2","3","6","7","darkgreen"))
text( c("d","b","b","a", "c"),x=(1:5) ,y = c(130,230,230,230,140))


#Cu CONCENTRATION

#AG GOOD
model_Cu_AG  <- lm(Fourtyeight~Trt, data = Cu_AG)#linear model
shap_Cu_AG <- shapiro.test(model_Cu_AG[["residuals"]])#Shapiro test
library(agricolae)
Krus_Cu_AG <- agricolae::kruskal(Cu_AG$Fourtyeight, Cu_AG$Trt)#Kruskall wallis
Krus_Cu_AG
#boxplot AG
boxplot(Fourtyeight~Trt, data = Cu_AG,
        ylim = c(0,460),
        ylab="Cu concentration in µg g^(−1)FW", 
        xlab="Treatments", names = c("Control", "Cu", "Cu+10-DOC_AG", 
                                     "Cu+100-DOC_AG"),
        main = "Copper concentration in Lemna minor 48h after treatments 
        with Copper and Alnus glutinosa extracts",
        border=c("2","3","6","7","darkgreen"))
text(x=(1:4) , c("c","a","b","a"), pos = 3, y = c(30,410,260,420))


#AN
model_Cu_AN  <- lm(Fourtyeight~Trt, data = Cu_AN)#linear model
shap_Cu_AN <- shapiro.test(model_Cu_AN[["residuals"]])#Shapiro test
library(agricolae)
Krus_Cu_AN <- agricolae::kruskal(Cu_AN$Fourtyeight, Cu_AN$Trt)#Kruskall wallis
Krus_Cu_AN
#boxplot AN
boxplot(Fourtyeight~Trt, data = Cu_AN,
        ylim = c(0,460),
        ylab="Cu concentration in µg g^(−1)FW",
        xlab="Treatments", names = c("Control", "Cu", "Cu+10-DOC_AG", 
                                     "Cu+100-DOC_AG"),
         main = "Copper concentration in Lemna minor48h after treatments 
    with Copper and Acer negundo extracts",
        border=c("2","3","6","7","darkgreen"))
text(x=(1:4) , c("d","a","c","b"), pos = 3, y = c(30,415,210,390))


#H2O2

#AG
model_H2O2_AG  <- lm(Fourtyeight~Trt, data = AG_H2O2)#linear model
shap_H2O2_AG <- shapiro.test(model_H2O2_AG[["residuals"]])#Shapiro test
library(agricolae)#Kruskall wallis
Krus_H2O2_AG <- agricolae::kruskal(AG_H2O2$Fourtyeight, AG_H2O2$Trt)
boxplot(Fourtyeight~Trt, data = AG_H2O2,
        ylim = c(0,300),
        ylab="H2O2 content (% of the control)",
        xlab="Treatments", names = c("Control", "Cu", "Cu+10_AG", 
                                     "Cu+100_AG", "100_AG"), 
        main = "H2O2 content in Lemna minor after treatments with Copper 
  and Acer glutinosa extracts on Hydrogen Perodixation (H2O2)",
        border=c("2","3","6","7","darkgreen"))
text( c("c","b","b","a", "d"),x=(1:5) ,y = c(130,170,170,190,110))

# we get letters for the groups: there are 3 different groups (a,b,c)
#add the group letters to the boxplot
#From the kruskal wallis test and box plot, significant difference between H2O2 concentration 
#produced by Control, Cu+100_AG and 100Ag only. The H2O2 concentration produced by CU alone and
#Cu+10_Ag were not significantly different.

#Description
#1. Control (only growth medium):100% H2O2 (it is our reference)
#2. Cu 6.4mg/L: 140%
#3. Cu 6.4mg/L +10-DOC_AN: 140 %, same group as only Cu
#4. Cu 6.4mg/L +100-DOC_AN: 170%
#5. 100-DOC_AN: a little inferior to 100%, in a
#significantly different group from the control

#Interpretation
#Transition metals including Cu, stimulate the formation of hydroxyl radicals (OH) from the 
#non-enzymatic chemical reaction between superoxide and H2O2.
#H2O2 is a sign of stress, so we see that only Alnus glutinosa extract don't
#give any stress, while Cu alone or Cu + Alnus glutinosa extract are causing
#some stress.

#We noticed that Alnus glutinosa and Acer negundo give quite the same
#distribution, with Acer negundo being slightly more stressful to Lemna minor 
#than Alnus glutinosa.




#AN
model_H2O2_AN  <- lm(Fourtyeight~Trt, data = AN_H2O2)#linear model
shap_H2O2_AN <- shapiro.test(model_H2O2_AN[["residuals"]])#Shapiro test
#we get a p-value = 0.1731067 which is higher than 0.05, so the data 
#distribution is normal. 

#H2O2 is normally distributed so we can make Anova and Tukey test

# Compute the analysis of variance
res.aov <- aov(Fourtyeight ~ Trt, data = AN_H2O2)
# Summary of the analysis
summary(res.aov)
#We get the p-value=2e-16, which is much less than 0.05. This means the 
#treatments are significantly different.

#Post-hoc test TukeyHSD-Test 
TukeyHSD(aov(res.aov))


library(agricolae)#Kruskall wallis
Krus_H2O2_AN <- agricolae::kruskal(AN_H2O2$Fourtyeight, AN_H2O2$Trt)
boxplot(Fourtyeight~Trt, data = AN_H2O2,
        ylim = c(0,300),
        ylab="H2O2 content (% of the control)",
        xlab="Treatments", names = c("Control", "Cu", "Cu+10_AN", 
                                     "Cu+100_AN", "100_AN"), 
        main = "H2O2 content in Lemna minor after treatments with Copper 
  and Acer negundo extracts on Hydrogen Perodixation (H2O2)",
        border=c("2","3","6","7","darkgreen"))
text( c("c","b","b","a", "c"),x=(1:5) ,y = c(130,170,165,200,125))


#The highest changes in H2O2 levels in plants were observed at early stages of treatments.


