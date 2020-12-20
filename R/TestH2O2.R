#♠FOR H2O2 AN

colnames(AN_H2O2)[colnames(AN_H2O2) == "48h"] <- "Fourtyeight"

#Testing normality
#This is the same as doing the anova and cheking the normality of residuals.

# Build the linear model
model_AN_H2O2  <- lm(Fourtyeight~Trt, data = AN_H2O2)

# Create a QQ plot of residuals
qqnorm(model_AN_H2O2[["residuals"]], main = "QQ plot for model_AN_H2O2")
qqline(model_AN_H2O2[["residuals"]])
#the data does not seem to follow the linear model

#Shapiro test
shap_AN_H2O2 <- shapiro.test(model_AN_H2O2[["residuals"]])
#we get a p-value = 0.001199523 which is lower than 0.05, so the data distribution is not normal.

#We use Kruskal wallis test because the data is not normal.
#Test for differences between treatments using agricolae::kruskal test.
#install.packages("agricolae")
library(agricolae)
Krus_AN_H2O2<- agricolae::kruskal(AN_H2O2$Fourtyeight, AN_H2O2$Trt)
Krus_AN_H2O2
# we get letters for the groups: there are 3 different groups (a,b,c)

#add the group letters to the boxplot
boxplot(Fourtyeight~Trt, data = AN_H2O2,
        ylim = c(0,300),
        ylab="H2O2 content (% of the control)",
        xlab="Treatments", 
        main = "H2O2 content in Lemna minor after treatments with Copper 
  and Acer negundo extracts on Hydrogen Perodixation (H2O2)",
        border=c("2","3","6","7","darkgreen"))
legend(x = 0.4, y = 305, legend = c("Legend:"
                                      ,"1: Control", "2: Cu", "3: Cu+10-DOC_AN ", 
                                 "4: Cu+100-DOC_AN ", "5: 100-DOC_AN"), 
       cex = 0.8)
text( c("c","b","b","a", "c"),x=(1:5) ,y = c(140,170,170,220,130))

#Description
#1. Control (only growth medium):100% H2O2 (it is our reference)
#2. Cu 6.4mg/L: 140%
#3. Cu 6.4mg/L +10-DOC_AN: 140 %, same group as only Cu
#4. Cu 6.4mg/L +100-DOC_AN: 170%
#5. 100-DOC_AN: 100%, same group as control
#Putting pure Cu or putting a mix Cu +10 AN leaf extract have the same
#effect, they increase hydrogen peroxidation in Lemna minor by 40 % (Trt 2 and 3).
#Putting Cu + 100 NA increases H2O2 by 70% referring to the control: 
# a lot of leaf extracts have a big impact on hydrogen peroxidation, which means 
#Adding only leaf extracts doesn't makes a significant difference from the 
#control (they are in the same group)
#
#Interpretation:
##H2O2 is a sign of stress, so we see that only Acer negundo extract don't
#give any stress, while Cu alone or Cu + Acer negundo extract are causing
#some stress.


#♠FOR H2O2 AG

colnames(AG_H2O2)[colnames(AG_H2O2) == "48h"] <- "Fourtyeight"#to avoid error 
#messages due to the presence of a number (48)


#Testing normality
#This is the same as doing the anova and cheking the normality of residuals.

# Build the linear model
model_AG_H2O2  <- lm(Fourtyeight~Trt, data = AG_H2O2)

# Create a QQ plot of residuals
qqnorm(model_AG_H2O2[["residuals"]], main = "QQ plot for model_AG_H2O2")
qqline(model_AG_H2O2[["residuals"]])
#the data does not seem to follow the linear model

#Shapiro test
shap_AG_H2O2 <- shapiro.test(model_AG_H2O2[["residuals"]])
#we get a p-value = 0.0007190274 which is lower than 0.05, 
#so the data distribution is not normal.

#We use Kruskal wallis test because the data is not normal.
#Test for differences between treatments using agricolae::kruskal test.
#install.packages("agricolae")
library(agricolae)
Krus_AG_H2O2<- agricolae::kruskal(AG_H2O2$Fourtyeight, AN_H2O2$Trt)
Krus_AG_H2O2
# we get letters for the groups: there are 4 different groups (a,b,c,d)

#add the group letters to the boxplot
boxplot(Fourtyeight~Trt, data = AG_H2O2,
        ylim = c(0,300),
        ylab="H2O2 content (% of the control)",
        xlab="Treatments", 
        main = "H2O2 content in Lemna minor after treatments with Copper 
  and Alnus glutinosa extracts on Hydrogen Perodixation (H2O2)",
        border=c("2","3","6","7","darkgreen"))
legend(x = 0.4, y = 305, legend = c("Legend:"
                                    ,"1: Control", "2: Cu", "3: Cu+10-AG ", 
                                    "4: Cu+100-DOC_AG ", "5: 100-DOC_AG"), 
       cex = 0.8)
text( c("c","b","b","a", "d"),x=(1:5) ,y = c(130,170,170,190,110))

#Description
#1. Control (only growth medium):100% H2O2 (it is our reference)
#2. Cu 6.4mg/L: 140%
#3. Cu 6.4mg/L +10-DOC_AN: 140 %, same group as only Cu
#4. Cu 6.4mg/L +100-DOC_AN: 170%
#5. 100-DOC_AN: a little inferior to 100%, in a
#significantly different group from the control

#Interpretation
#H2O2 is a sign of stress, so we see that only Alnus glutinosa extract don't
#give any stress, while Cu alone or Cu + Alnus glutinosa extract are causing
#some stress.

#We notice that Alnus glutinosa and Acer negundo give quite the same
#distribution, with Acer negundo being slightly more stressful to Lemna minor 
#than Alnus glutinosa.

