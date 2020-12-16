#tests Cu

#♠FOR Cu_an

#Testing normality
#This is the same as doing the anova and cheking the normality of residuals.

# Build the linear model
colnames(Cu_AN)[colnames(Cu_AN) == "48h"] <- "Fourtyeight"
model_Cu_an  <- lm(Fourtyeight~Trt, data = Cu_AN)

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
Krus_Cu_an<- agricolae::kruskal(Cu_AN$Fourtyeight, Cu_AN$Trt)
Krus_Cu_an
# we get letters for the groups: the four treatments have each a different 
#group (d,a,c,b)

#add the group letters to the boxplot
boxplot(Fourtyeight~Trt, data = Cu_AN,
        ylim = c(0,460),
        ylab="Cu concentration in µg g^(−1)FW",
        xlab="Treatments", 
    main = "Copper concentration in Lemna minor48h after treatments 
    with Copper and Acer negundo extracts",
   border=c("2","3","6","7","darkgreen"))
legend(x = 0.4, y = 330, 
       legend = c("Legend:","1: Control", "2: Cu", "3: Cu+10-DOC_AN ", 
                  "4: Cu+100-DOC_AN "), 
       cex = 0.8)
text(x=(1:4) , c("d","a","c","b"), pos = 3, y = c(30,415,210,390))



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
colnames(Cu_AG)[colnames(Cu_AG) == "48h"] <- "Fourtyeight"
model_Cu_ag  <- lm(Fourtyeight~Trt, data = Cu_AG)

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
Krus_Cu_ag<- agricolae::kruskal(Cu_AG$Fourtyeight, Cu_AG$Trt)
Krus_Cu_ag

boxplot(Fourtyeight~Trt, data = Cu_AG,
        ylim = c(0,460),
        ylab="Cu concentration in µg g^(−1)FW", xlab="Treatments", 
        main = "Copper concentration in Lemna minor 48h after treatments 
        with Copper and Alnus glutinosa extracts",
border=c("2","3","6","7","darkgreen"))
legend(x = 0.4, y = 330, 
       legend = c("Legend:","1: Control", "2: Cu", "3: Cu+10-DOC_AG ", 
                  "4: Cu+100-DOC_AG "), 
       cex = 0.8)
text(x=(1:4) , c("c","a","b","a"), pos = 3, y = c(30,410,260,420))
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

