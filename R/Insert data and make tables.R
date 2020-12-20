#install.packages(readxl)

# !!! Note: "Cu_µg_gFW_AN" often is saved in a strange way, so please check
#it is written "microgram" with the greek letter (line 19 of this script).

# Data for growth rates of Frond and Roots in L.minor under 8 treatments. 
#put Growth rates as GR
library(readxl)
GR <- read_excel("0.Data/Raw_Data_Fig_1_Growth_rates.xlsx", 
                 range = "K13:M77")

#Data for Cu accumulation in L.minor treated by Cu, and combinations of Cu and 
#LLE from A. glutinosa.
#import Cu concentrations for AG and AN
library(readxl)
Cu_AG <- read_excel("0.Data/Raw_Data_Fig_2_Cu_conc.xlsx", 
                    range = "B10:i26", col_names = TRUE)
#Data for Cu accumulation in L.minor treated by Cu, and combinations of Cu and 
#LLE from A. negundo.
Cu_AN <- read_excel("0.Data/Raw_Data_Fig_2_Cu_conc.xlsx", 
                    sheet = "Cu_µg_gFW_AN", range = "B10:I26")

#Concentration of hydrogen peroxide in L. minor treated by Cu, A. glutinosa 
#LLE and Cu + A. glutinosa LLE.
#import H2O2 for AG and AN
library(readxl)
AG_H2O2 <- read_excel("0.Data/Raw_Data_Fig_3_H2O2_conc.xlsx", 
                      sheet = "H2O2_AG", range = "B10:I50")
#Concentration of hydrogen peroxide in L. minor treated by Cu, A. negundo LLE 
#and Cu + A. negundo LLE.
AN_H2O2 <- read_excel("0.Data/Raw_Data_Fig_3_H2O2_conc.xlsx", 
                      sheet = "H2O2_AN", range = "B10:I50")

#Lipid peroxidation expressed as MDA concentration in L. minor treated by Cu, 
#A. glutinosa LLE and Cu + A. glutinosa LLE.
#import Lipid peroxidation
library(readxl)
AG_Lipid <- read_excel("0.Data/Raw_Data_Fig_4_Lipid_peroxidation.xlsx", 
                       sheet = "MDA_AG", range = "B10:I50")
#Lipid peroxidation expressed as MDA concentration in L. minor treated by Cu, 
#A. negundo LLE and Cu + A. negundo LLE.
AN_Lipid <- read_excel("0.Data/Raw_Data_Fig_4_Lipid_peroxidation.xlsx", 
                       sheet = "MDA_AN", range = "B10:I50")


#FOR CU CONCENTRATION

# Change colname of one column so we give the same name to Cu_µg_gFW_AN and 
#Cu_µg_gFW_AG in both tables
colnames(Cu_AG)[colnames(Cu_AG) == "Cu_µg_gFW_AG"] <- "Trt"
colnames(Cu_AN)[colnames(Cu_AN) == "Cu_µg_gFW_AN"] <- "Trt"
#Trt is for the Treatment

AN <- "AN"#create vector with specie "AN"
AG <- "AG"#create vector with specie "AG"
Cu_AN <- dplyr::mutate(Cu_AN, Tree_sp = AN)
#to add the column for the tree specie AN in the table of AN
Cu_AG <- dplyr::mutate(Cu_AG, Tree_sp = AG)
#to add the column for the tree specie AG in the table of AG

#install.packages("tidyr")
library(tidyr)
Cu <- dplyr::bind_rows(Cu_AG, Cu_AN)
#we have table with the species and only one column of Trt

#(we can get the same table with this:)
#library(dplyr)
#Cu6 <- dplyr::union(Cu_AG, Cu_AN)
#Cu6

#rename the column 48h as Fourtyeight because having a number in the name 
#always gives error message
colnames(Cu)[colnames(Cu) == "48h"] <- "Fourtyeight"
colnames(Cu)[colnames(Cu) == "0.75h"] <- "Zero.seventyfive"
colnames(Cu)[colnames(Cu) == "1.5h"] <- "One.five"
colnames(Cu)[colnames(Cu) == "3h"] <- "Three"
colnames(Cu)[colnames(Cu) == "6h"] <- "Six"
colnames(Cu)[colnames(Cu) == "12h"] <- "Twelve"
colnames(Cu)[colnames(Cu) == "24h"] <- "Twentyfour"

#make the table will the means of each treatment so we have only 1 row per 
#treatment per species  (so 6 rows)
library(dplyr)
Cu2 <- Cu %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(mean = mean(Fourtyeight))

colnames(Cu2)[colnames(Cu2) == "mean"] <- "meanFourtyeight"
#Fourtyeight in the name

#FOR GROWTH RATE
#make the table with means of each treatment = 1 row per treatment per species
library(dplyr)
GR1 <- GR %>% 
  group_by(Variant) %>% 
  summarise(meanF = mean(FrondGR), meanR = mean(RootGR))

#FOR H2O2

# Change colname of one column so we give the same name to AG_H2O2 and AN_H2O2 
#in both tables
colnames(AG_H2O2)[colnames(AG_H2O2) == "H2O2_AG"] <- "Trt"
colnames(AN_H2O2)[colnames(AN_H2O2) == "H2O2_AN"] <- "Trt"
#Trt is for the Treatment

#install.packages("tidyr")
AN_H2O2 <- dplyr::mutate(AN_H2O2, Tree_sp = AG)
AG_H2O2 <- dplyr::mutate(AG_H2O2, Tree_sp = AN)
H2O2 <- dplyr::bind_rows(AG_H2O2, AN_H2O2)
#we have table with the species and only one column of Trt(!!!!)

#rename the column 48h as Fourtyeight because having a number in the name 
# gives error message
colnames(H2O2)[colnames(H2O2) == "48h"] <- "Fourtyeight"
colnames(H2O2)[colnames(H2O2) == "0.75h"] <- "Zero.seventyfive"
colnames(H2O2)[colnames(H2O2) == "1.5h"] <- "One.five"
colnames(H2O2)[colnames(H2O2) == "3h"] <- "Three"
colnames(H2O2)[colnames(H2O2) == "6h"] <- "Six"
colnames(H2O2)[colnames(H2O2) == "12h"] <- "Twelve"
colnames(H2O2)[colnames(H2O2) == "24h"] <- "Twentyfour"

#○make the table will the means of each treatment so we have only 1 row per 
#treatment per species
library(dplyr)
H2O2_means <- H2O2 %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(meanH2O2 = mean(Fourtyeight))


# FOR LIPID 

# Change colname of one column so we give the same name to Lipid_AG and 
#Lipid_AN in both tables
colnames(AG_Lipid)[colnames(AG_Lipid) == "MDA_AG"] <- "Trt"
colnames(AN_Lipid)[colnames(AN_Lipid) == "MDA_AN"] <- "Trt"
#Trt is for the Treatment

AN <- "AN"#create vector with specie "AN"
AG <- "AG"#create vector with specie "AG"
AN_Lipid <- dplyr::mutate(AN_Lipid, Tree_sp = AN)
#to add the column for the tree specie AN in the table of AN
AG_Lipid <- dplyr::mutate(AG_Lipid, Tree_sp = AG)
#to add the column for the tree specie AG in the table of AG

#install.packages("tidyr")
library(tidyr)
Lipid <- dplyr::bind_rows(AN_Lipid, AG_Lipid)
#we have table with the species and only one column of Trt(!!!!)

#rename the column 48h as Fourtyeight because having a number in the name 
#gives error message
colnames(Lipid)[colnames(Lipid) == "48h"] <- "Fourtyeight"
colnames(Lipid)[colnames(Lipid) == "0.75h"] <- "Zero.seventyfive"
colnames(Lipid)[colnames(Lipid) == "1.5h"] <- "One.five"
colnames(Lipid)[colnames(Lipid) == "3h"] <- "Three"
colnames(Lipid)[colnames(Lipid) == "6h"] <- "Six"
colnames(Lipid)[colnames(Lipid) == "12h"] <- "Twelve"
colnames(Lipid)[colnames(Lipid) == "24h"] <- "Twentyfour"

#make the table will the means of each treatment so we have only 1 row per 
#treatment per species
library(dplyr)
Lipid_2 <- Lipid %>% 
  group_by(Trt, Tree_sp) %>% 
  summarise(mean = mean(Fourtyeight))

#add the variable of copper: presence/absence (long way, see next for short 
#way):
#Cu3 <- Cu2$Trt > 1  
#create a vector true/false: false if Trt == 1 (= control without Copper)
#Cu2$Copper <- Cu3 
#create the column Copper and put the vector Cu3 who has True/False

#we can combine both lines (same result as just before):
Cu2$Copper <- Cu2$Trt > 1

#to add the variable of copper: presence/absence
Cu3 <- Cu2$Trt > 1 
#create a vector true/false: false if Trt == 1 (= control without Copper)
Cu2$Copper <- Cu3 
#create the column Copper and put the vector Cu3 who has True/False

#we can combine both lines (same result as just before):
#Cu2$Copper <- Cu2$Trt > 1

#Add column for concentration 10 mg/L DOC: Trt == 3 is TRUE
Cu2$Ten.mg.DOC <- Cu2$Trt == 3

#Add column for concentration 100 mg/L DOC: Trt == 4 is TRUE
Cu2$Hundred.mg.DOC <- Cu2$Trt == 4


#FOR H2O2

#to add the variable of copper: presence/absence
H2O2N <- H2O2_means$Trt > 1 & H2O2_means$Trt < 5 
#create a vector true/false: false if Trt == 1 😊 control without Copper)
H2O2_means$Copper <- H2O2N

#Add column for concentration 10 mg/L DOC: Trt == 3 is TRUE
H2O2_means$Ten.mg.DOC <- H2O2_means$Trt == 3

#Add column for concentration 100 mg/L DOC: Trt == 4 is TRUE
H2O2_means$Hundred.mg.DOC <- H2O2_means$Trt > 3


#LIPIDS
#Adding a column with Cu presence 
Lipid_2$Copper <- Lipid_2$Trt >1 & Lipid_2$Trt <5 

#Adding a column with 10 mg/L 
Lipid_2$Ten.mg.DOC <- Lipid_2$Trt == 3

#Adding a column with 100 mg/L 
Lipid_2$Hundred.mg.DOC <- Lipid_2$Trt >3


#GROWTH

#Adding a column with Cu presence 
GR1$Copper <- GR1$Variant !=1 & GR1$Variant !=5

#Adding a column with 10 mg/L 
GR1$Ten.mg.DOC <- GR1$Variant ==3 | GR1$Variant ==6

#Adding a column with 100 mg/L 
GR1$Hundred.mg.DOC <- GR1$Variant ==4 | GR1$Variant ==5 | 
  GR1$Variant ==7 | GR1$Variant ==8

