#FOR LIPIDS AN

colnames(AN_Lipid)[colnames(AN_Lipid) == "48h"] <- "Fourtyeight"

#Testing normality
#This is the same as doing the anova and cheking the normality of residuals.

# Build the linear model
model_AN_Lipid  <- lm(Fourtyeight~Trt, data = AN_Lipid)

# Create a QQ plot of residuals
qqnorm(model_AN_Lipid[["residuals"]], main = "QQ plot for model_AN_Lipid")
qqline(model_AN_Lipid[["residuals"]])
#the data does not seem to follow the linear model

#Shapiro test
shap_AN_Lipid <- shapiro.test(model_AN_Lipid[["residuals"]])
#we get a p-value = 0.00000008778 which is lower than 0.05, so the data 
#distribution is not normal.

#We use Kruskal wallis test because the data is not normal.
#Test for differences between treatments using agricolae::kruskal test.
#install.packages("agricolae")
library(agricolae)
Krus_AN_Lipid<- agricolae::kruskal(AN_Lipid$Fourtyeight, AN_Lipid$Trt)
Krus_AN_Lipid
# we get letters for the groups: there are 3 different groups (a,b,c)

#add the group letters to the boxplot
boxplot(Fourtyeight~Trt, data = AN_Lipid,
        ylim = c(0,300),
        ylab="MDA (% of the control)",
        xlab="Treatments", 
        main = "Lipid peroxidation expressed as MDA concentration in Lemna minor 
        after treatments with Copper and Acer negundo extracts",
        border=c("2","3","6","7","darkgreen"))
legend(x = 0.4, y = 305, legend = c("Legend:"
                                    ,"1: Control", "2: Cu", "3: Cu+10-DOC_AN ", 
                                    "4: Cu+100-DOC_AN ", "5: 100-DOC_AN"), 
       cex = 0.8)
text( c("d","b","b","a", "c"),x=(1:5) ,y = c(130,230,230,230,140))

#Description
#1. Control (only growth medium):100% H2O2 (it is our reference)
#2. Cu 6.4mg/L: 140%
#3. Cu 6.4mg/L +10-DOC_AN: 140 %, same group as only Cu
#4. Cu 6.4mg/L +100-DOC_AN: 170%
#5. 100-DOC_AN: 100%, same group as control

#Putting pure Cu or putting a mix Cu +10 AN leaf extract have the same
#effect, they increase lipid peroxidation in Lemna minor  (Trt 2 and 3).
# And as the concentarion of extract increases into 100 NA + Cu, 
#it increases the MDA concentration as compare to the control: 
# a lot of leaf extracts have a big impact on lipid peroxidation, which means 
#Adding only leaf extracts doesn't makes a significant difference from the 
#control (they are in the same group)
#
#Interpretation:
##Lipid peroxidation is a sign of stress, it is oxidative damage that affects 
#cellular membranes, lipoproteins, and other molecules that contain lipids 
#in conditions with oxidative stress



#♠FOR Lipid AG

colnames(AG_Lipid)[colnames(AG_Lipid) == "48h"] <- "Fourtyeight"#to avoid error 
#messages due to the presence of a number (48)


#Testing normality
#This is the same as doing the anova and cheking the normality of residuals.

# Build the linear model
model_AG_Lipid  <- lm(Fourtyeight~Trt, data = AG_Lipid)

# Create a QQ plot of residuals
qqnorm(model_AG_Lipid[["residuals"]], main = "QQ plot for model_AG_Lipid")
qqline(model_AG_Lipid[["residuals"]])
#the data does not seem to follow the linear model

#Shapiro test
shap_AG_Lipid <- shapiro.test(model_AG_Lipid[["residuals"]])
#we get a p-value = 0.00003537 which is lower than 0.05, 
#so the data distribution is not normal.

#We use Kruskal wallis test because the data is not normal.
#Test for differences between treatments using agricolae::kruskal test.
#install.packages("agricolae")
library(agricolae)
Krus_AG_Lipid <- agricolae::kruskal(AG_Lipid$Fourtyeight, AG_Lipid$Trt)
Krus_AG_Lipid
# we get letters for the groups: there are 4 different groups (a,b,c,d)

#add the group letters to the boxplot
boxplot(Fourtyeight~Trt, data = AG_Lipid,
        ylim = c(0,300),
        ylab="Lipid content (% of the control)",
        xlab="Treatments", 
        main = "Lipid content in Lemna minor after treatments with Copper 
  and Alnus glutinosa extracts on Hydrogen Perodixation (H2O2)",
        border=c("2","3","6","7","darkgreen"))
legend(x = 0.4, y = 300, legend = c("Legend:"
                                    ,"1: Control", "2: Cu", "3: Cu+10-AG ", 
                                    "4: Cu+100-DOC_AG ", "5: 100-DOC_AG"), 
       cex = 0.8)
text( c("d","a","b","a", "c"),x=(1:5) ,y = c(130,220,160,220,140))

#Description
#1. Control (only growth medium):100% Lipid (it is our reference)
#2. Cu 6.4mg/L: 200%
#3. Cu 6.4mg/L +10-DOC_AG: 140 %
#4. Cu 6.4mg/L +100-DOC_AG: 200%
#5. 100-DOC_AG: 120% a little inferior to 100%, in a
#significantly different group from the control

#Interpretation
#Putting pure Cu increases the lipid peroxidation significantly i.e. by 100%.
#Increase in concentration of AG in the mixture of Cu and AG increases the 
#lipid peroxidation.
#Lipid peroxidation is 60% more in Cu + 100 Ag than in Cu + 10 Ag.
#Adding only leaf extracts doesn't make a significant difference from the 
#control (Trt 1 and 5)




