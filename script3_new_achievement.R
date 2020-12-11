#FOR CU CONCENTRATION

# Change colname of one column so we give the same name to Cu_µg_gFW_AN and Cu_µg_gFW_AG in both tables
colnames(Cu_AG)[colnames(Cu_AG) == "Cu_µg_gFW_AG"] <- "Trt"
colnames(Cu_AN)[colnames(Cu_AN) == "Cu_µg_gFW_AN"] <- "Trt"
#Trt is for the Treatment

AN <- "AN"#create vector with specie "AN"
AG <- "AG"#create vector with specie "AG"
Cu_AN <- dplyr::mutate(Cu_AN, Tree_sp = AN)#to add the column for the tree specie AN in the table of AN
Cu_AG <- dplyr::mutate(Cu_AG, Tree_sp = AG)#to add the column for the tree specie AG in the table of AG

#install.packages("tidyr")
library(tidyr)
Cu <- dplyr::bind_rows(Cu_AG, Cu_AN)
View(Cu)#we have table with the species and only one column of Trt(!!!!)

#(we can get the same table with this:)
library(dplyr)
Cu6 <- dplyr::union(Cu_AG, Cu_AN)
Cu6

#rename the column 48h as Fourtyeight because having a number in the name always gives error message
colnames(Cu)[colnames(Cu) == "48h"] <- "Fourtyeight"
colnames(Cu)[colnames(Cu) == "0.75h"] <- "Zero.seventyfive"
colnames(Cu)[colnames(Cu) == "1.5h"] <- "One.five"
colnames(Cu)[colnames(Cu) == "3h"] <- "Three"
colnames(Cu)[colnames(Cu) == "6h"] <- "Six"
colnames(Cu)[colnames(Cu) == "12h"] <- "Twelve"
colnames(Cu)[colnames(Cu) == "24h"] <- "Twentyfour"

#○make the table will the means of each treatment so we have only 1 row per treatment per species  (so 6 rows)
Cu2 <- Cu %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(mean = mean(Fourtyeight))

colnames(Cu2)[colnames(Cu2) == "mean"] <- "meanFourtyeight"#Fourtyeight in the name
View(Cu2)

#FOR GROWTH RATE
#make the table with means of each treatment = 1 row per treatment per species
library(dplyr)
GR1 <- GR %>% 
  group_by(Variant) %>% 
  summarise(meanF = mean(FrondGR), meanR = mean(RootGR))

#FOR H2O2

# Change colname of one column so we give the same name to AG_H2O2 and AN_H2O2 in both tables
colnames(AG_H2O2)[colnames(AG_H2O2) == "H2O2_AG"] <- "Trt"
colnames(AN_H2O2)[colnames(AN_H2O2) == "H2O2_AN"] <- "Trt"
#Trt is for the Treatment

#install.packages("tidyr")
AN_H2O2 <- dplyr::mutate(AN_H2O2, Tree_sp = AG)
AG_H2O2 <- dplyr::mutate(AG_H2O2, Tree_sp = AN)
H2O2 <- dplyr::bind_rows(AG_H2O2, AN_H2O2)
View(H2O2)#we have table with the species and only one column of Trt(!!!!)

#rename the column 48h as Fourtyeight because having a number in the name always gives error message
colnames(H2O2)[colnames(H2O2) == "48h"] <- "Fourtyeight"
colnames(H2O2)[colnames(H2O2) == "0.75h"] <- "Zero.seventyfive"
colnames(H2O2)[colnames(H2O2) == "1.5h"] <- "One.five"
colnames(H2O2)[colnames(H2O2) == "3h"] <- "Three"
colnames(H2O2)[colnames(H2O2) == "6h"] <- "Six"
colnames(H2O2)[colnames(H2O2) == "12h"] <- "Twelve"
colnames(H2O2)[colnames(H2O2) == "24h"] <- "Twentyfour"

#○make the table will the means of each treatment so we have only 1 row per treatment per species  (so 6 rows)
library(dplyr)
H2O2_means <- H2O2 %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(meanH2O2 = mean(Fourtyeight))
View(H2O2_means)


# FOR LIPID 

# Change colname of one column so we give the same name to Lipid_AG and Lipid_AN in both tables
colnames(AG_Lipid)[colnames(AG_Lipid) == "MDA_AG"] <- "Trt"
colnames(AN_Lipid)[colnames(AN_Lipid) == "MDA_AN"] <- "Trt"
#Trt is for the Treatment

AN <- "AN"#create vector with specie "AN"
AG <- "AG"#create vector with specie "AG"
AN_Lipid <- dplyr::mutate(AN_Lipid, Tree_sp = AN)#to add the column for the tree specie AN in the table of AN
AG_Lipid <- dplyr::mutate(AG_Lipid, Tree_sp = AG)#to add the column for the tree specie AG in the table of AG

#install.packages("tidyr")
library(tidyr)
Lipid <- dplyr::bind_rows(AN_Lipid, AG_Lipid)
View(Lipid)#we have table with the species and only one column of Trt(!!!!)

#rename the column 48h as Fourtyeight because having a number in the name always gives error message
colnames(Lipid)[colnames(Lipid) == "48h"] <- "Fourtyeight"
colnames(Lipid)[colnames(Lipid) == "0.75h"] <- "Zero.seventyfive"
colnames(Lipid)[colnames(Lipid) == "1.5h"] <- "One.five"
colnames(Lipid)[colnames(Lipid) == "3h"] <- "Three"
colnames(Lipid)[colnames(Lipid) == "6h"] <- "Six"
colnames(Lipid)[colnames(Lipid) == "12h"] <- "Twelve"
colnames(Lipid)[colnames(Lipid) == "24h"] <- "Twentyfour"

#make the table will the means of each treatment so we have only 1 row per treatment per species  (so 6 rows)
library(dplyr)
Lipid_2 <- Lipid %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(mean = mean(Fourtyeight))
View(Lipid_2)

#to add the variable of copper: presence/absence (long way, see next for short way)
#Cu3 <- Cu2$Trt > 1  #create a vector true/false: false if Trt == 1 (= control without Copper)
#Cu2$Copper <- Cu3 #create the column Copper and put the vector Cu3 who has True/False

#we can combine both lines (same result as just before):
Cu2$Copper <- Cu2$Trt > 1

#Add column for concentration 10 mg/L DOC: Trt == 3 is TRUE
Cu2$Ten.mg.DOC <- Cu2$Trt == 3

#Add column for concentration 100 mg/L DOC: Trt == 4 is TRUE
Cu2$Hundred.mg.DOC <- Cu2$Trt == 4

