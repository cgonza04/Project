# Data for growth rates of Frond and Roots in L.minor under 8 treatments. 
#put Growth rates as GR
library(readxl)
GR <- read_excel("0.Data/Raw_Data_Fig_1_Growth_rates.xlsx", 
                 range = "K13:M77")
View(GR)

#Data for Cu accumulation in L.minor treated by Cu, and combinations of Cu and LLE from A. glutinosa.
#import Cu concentrations for AG and AN
library(readxl)
Cu_AG <- read_excel("0.Data/Raw_Data_Fig_2_Cu_conc.xlsx", 
                    range = "B10:i26", col_names = FALSE)
#Data for Cu accumulation in L.minor treated by Cu, and combinations of Cu and LLE from A. negundo.
Cu_AN <- read_excel("0.Data/Raw_Data_Fig_2_Cu_conc.xlsx", 
                    sheet = "Cu_µg_gFW_AN", range = "B10:I26")
View(Cu_AG)

#Concentration of hydrogen peroxide in L. minor treated by Cu, A. glutinosa LLE and Cu + A. glutinosa LLE.
#import H2O2 for AG and AN
library(readxl)
AG_H2O2 <- read_excel("0.Data/Raw_Data_Fig_3_H2O2_conc.xlsx", 
                      sheet = "H2O2_AG", range = "B10:I50")
#Concentration of hydrogen peroxide in L. minor treated by Cu, A. negundo LLE and Cu + A. negundo LLE.
AN_H2O2 <- read_excel("0.Data/Raw_Data_Fig_3_H2O2_conc.xlsx", 
                      sheet = "H2O2_AN", range = "B10:I50")
print(AN_H2O2)

#Lipid peroxidation expressed as MDA concentration in L. minor treated by Cu, A. glutinosa LLE and Cu + A. glutinosa LLE.
#import Lipid peroxidation
library(readxl)
AG_Lipid <- read_excel("0.Data/Raw_Data_Fig_4_Lipid_peroxidation.xlsx", 
                       sheet = "MDA_AG", range = "B10:I50")
#Lipid peroxidation expressed as MDA concentration in L. minor treated by Cu, A. negundo LLE and Cu + A. negundo LLE.
AN_Lipid <- read_excel("0.Data/Raw_Data_Fig_4_Lipid_peroxidation.xlsx", 
                       sheet = "MDA_AN", range = "B10:I50")