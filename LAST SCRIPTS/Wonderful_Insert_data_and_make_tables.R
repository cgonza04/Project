#INSERT THE DATA

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

#change the columns names to have Trt column in all the tables
colnames(Cu_AG)[colnames(Cu_AG) == "Cu_µg_gFW_AG"] <- "Trt"
colnames(Cu_AN)[colnames(Cu_AN) == "Cu_µg_gFW_AN"] <- "Trt"
colnames(AG_H2O2)[colnames(AG_H2O2) == "H2O2_AG"] <- "Trt"
colnames(AN_H2O2)[colnames(AN_H2O2) == "H2O2_AN"] <- "Trt"
colnames(AG_Lipid)[colnames(AG_Lipid) == "MDA_AG"] <- "Trt"
colnames(AN_Lipid)[colnames(AN_Lipid) == "MDA_AN"] <- "Trt"


#ADD TREE SPECIES COLUMN AND MAKE 1 TABLE FOR EACH PARAMETER (COMBINE AN AND AG)
#install.packages("tidyr")
library(tidyr)

#For Cu

AN <- "AN"#create vector with specie "AN"
AG <- "AG"#create vector with specie "AG"
Cu_AN <- dplyr::mutate(Cu_AN, Tree_sp = AN)
#to add the column for the tree specie AN in the table of AN
Cu_AG <- dplyr::mutate(Cu_AG, Tree_sp = AG)
#to add the column for the tree specie AG in the table of AG
Cu <- dplyr::bind_rows(Cu_AG, Cu_AN)
#we have table with the species and only one column of Trt


#For H2O2

AN_H2O2 <- dplyr::mutate(AN_H2O2, Tree_sp = AG)
AG_H2O2 <- dplyr::mutate(AG_H2O2, Tree_sp = AN)
H2O2 <- dplyr::bind_rows(AG_H2O2, AN_H2O2)

#For Lipids

AN_Lipid <- dplyr::mutate(AN_Lipid, Tree_sp = AN)
AG_Lipid <- dplyr::mutate(AG_Lipid, Tree_sp = AG)
Lipid <- dplyr::bind_rows(AN_Lipid, AG_Lipid)


#RENAME COLUMNS FOR TRT AND TIME

#rename the column of hours as numbers because having a number in the name 
#always gives error message

D1 <- list(Cu=Cu, Cu_AG=Cu_AG, Cu_AN=Cu_AN,AN_H2O2=AN_H2O2,AG_H2O2=AG_H2O2, H2O2=H2O2,
           AG_Lipid=AG_Lipid, AN_Lipid=AN_Lipid, Lipid=Lipid)

#i <- names(D1)[1]#to try the 1st
for (i in names(D1)){
  colnames(D1[[i]])[colnames(D1[[i]])=="0.75h"] <- "Zero.seventyfive"
  colnames(D1[[i]])[ colnames(D1[[i]]) == "1.5h"] <- "One.five"
  colnames(D1[[i]])[ colnames(D1[[i]]) == "3h"] <- "Three"
  colnames(D1[[i]])[ colnames(D1[[i]]) == "6h"] <- "Six"
  colnames(D1[[i]])[ colnames(D1[[i]]) == "12h"] <- "Twelve"
  colnames(D1[[i]])[ colnames(D1[[i]]) == "24h"] <- "Twentyfour"
  colnames(D1[[i]])[ colnames(D1[[i]]) == "48h"] <- "Fourtyeight"
}

#give again their names to the tables:
Cu <- D1$Cu
Cu_AG <- D1$Cu_AG
Cu_AN <- D1$Cu_AN
AN_H2O2 <- D1$AN_H2O2
AG_H2O2 <- D1$AG_H2O2
H2O2 <- D1$H2O2
AG_Lipid <- D1$AG_Lipid
AN_Lipid <- D1$AN_Lipid
Lipid <- D1$Lipid



