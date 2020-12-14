#tests Cu

#♠FOR Cu_an

#Testing normality
#This is the same as doing the anova and cheking the normality of residuals.

# Build the linear model
model_Cu_an  <- lm(Fourtyeight~Trt, data = Cu_an)

# Create a QQ plot of residuals
qqnorm(model_Cu_an[["residuals"]], main = "QQ plot for model_Cu_an")
qqline(model_Cu_an[["residuals"]])
#the data does not seem to follow the linear model

#Shapiro test
shap_Cu_an <- shapiro.test(model_Cu_an[["residuals"]])
#we get a p-value = 2.7837e-05, so the data distribution is not normal.

#We use Kruskal wallis test because the data is not normal.
#Test for differences between treatments using agricolae::kruskal test.
#install.packages("agricolae")
library(agricolae)
Krus_Cu_an<- agricolae::kruskal(Cu_an$Fourtyeight, Cu_an$Trt)
Krus_Cu_an
# we get letters for the groups: the four treatments have each a different 
#group (d,a,c,b)

#add the group letters to the boxplot
boxplot(Fourtyeight~Trt, data = Cu_an,
        ylim = c(0,460),
        ylab="Cu concentration in µg g^(−1)FW",
        xlab="Treatment", 
    main = "Effect on Lemna minor after treatments with Copper 
  and Acer negundo extracts on Cu concentration")
text(x=(1:4) ,y = 425, c("d","a","c","b"), pos = 3)
legend(Trt)

#description of the plot:
#1. Control: L. minor absorbed almost no Cu
#2. Cu 6.4mg/L : L. minor absorbed around 380 micrograms of Cu
#3. Cu 6.4mg/L + 10 mg/L organic matter from _Acer negundo_
    #  L. minor absorbed around 200 micrograms of Cu
#4. Cu 6.4mg/L + 100 mg/L organic matter from _Acer negundo_
    #L. minor absorbed around 320 micrograms of Cu

#interpretation:
#When given pure Cu, L. minor absorbs a lot of it
#A. negundo contains some Cu, and if you put higher concentration of Cu, 
#L. minor absorbs more (for these concentrations).
#Here the Leaf extracts were mixed to Cu 
#(there is not experiment with the leaf extract alone)
#Treatment 2,3,4 have the same amount of pure Cu , 
#only the concentration of leaf extract added changes.
#With the leaf extract, the Cu in L. minor is lower than control with only Cu.
#So adding leaf extracts seems to lower the Cu absorption by L. minor 
#(it seems to mitigate the effect of Copper pollution)


#FOR Cu_ag

#Testing normality
#This is the same as doing the anova and cheking the normality of residuals.

# Build the linear model
model_Cu_ag  <- lm(Fourtyeight~Trt, data = Cu_ag)

# Create a QQ plot of residuals
qqnorm(model_Cu_ag[["residuals"]], main = "QQ plot for model_Cu_ag")
qqline(model_Cu_ag[["residuals"]])

#Shapiro test
shap_Cu_ag <- shapiro.test(model_Cu_ag[["residuals"]])
#we get a p-value = 2.7837e-05. So the data distribution is not normal.

#We use Kruskal wallis test because the data is not normal.
#Test for differences between treatments using agricolae::kruskal test.
#install.packages("agricolae")
library(agricolae)
Krus_Cu_ag<- agricolae::kruskal(Cu_ag$Fourtyeight, Cu_ag$Trt)
Krus_Cu_ag

boxplot(Fourtyeight~Trt, data = Cu_ag,
        ylim = c(0,460),
        ylab="Cu concentration with Alnus glutinosa after 48h", xlab="Treatment", 
        main = "Effect of treatments with Alnus glutinosa
on Cu concentration in Lemna minor")
text(x=(1:4) ,y = 425, c("c","a","b","a"), pos = 3)
#♣treatments 2 and 4 are in the same group: they are not significantly different

#description and interpretation are the same as Acer negundo
#description of the plot:
#1. Control: L. minor absorbed almost no Cu
#2. Cu 6.4mg/L : L. minor absorbed around 390 micrograms of Cu
#3. Cu 6.4mg/L + 10 mg/L organic matter from _Alnus glutinosa_
#  L. minor absorbed around 250 micrograms of Cu
#4. Cu 6.4mg/L + 100 mg/L organic matter from _Alnus glutinosa_
#L. minor absorbed around 400 micrograms of Cu

#interpretation:
#As for the last plot, when given pure Cu, L. minor absorbs a lot of it
#Alnus glutinosa contains some Cu, and if you put higher concentration of Cu, 
#L. minor absorbs more (for these concentrations, Treatments 3 and 4).
#Here the Leaf extracts were mixed to Cu 
#(there is not experiment with the leaf extract alone)
#Treatment 2,3,4 have the same amount of pure Cu , 
#only the concentration of leaf extract added changes.
#With the leaf extract, the Cu in L. minor is lower than control with only Cu.
#Adding 10 mg of leaf extracts seems to lower the Cu absorption by L. minor.
#(it seems to mitigate the effect of Copper pollution)
#When they add 100 mg leaf extract of Alnus glutinosa, L. minor absorbs as 
#much as when only Cu.



#Dunnett test EXAMPLE from https://www.rdocumentation.org/packages/DescTools/versions/0.99.38/topics/DunnettTest
x <- Cu_an$Trt == 1 # control
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis

DunnettTest(list(x, y, z))

## Equivalently,
x <- c(x, y, z)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("Normal subjects",
                       "Subjects with obstructive airway disease",
                       "Subjects with asbestosis"))

DunnettTest(x, g)







