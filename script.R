#put Growth rates as GR
library(readxl)
GR <- read_excel("Project/0.Data/Raw_Data_Fig_1_Growth_rates.xlsx", 
                                          range = "K13:M77")
View(GR)

#import Cu concentrations for AG and AN
library(readxl)
Cu_AG <- read_excel("Project/0.Data/Raw_Data_Fig_2_Cu_conc.xlsx", 
                                     range = "B10:i26", col_names = FALSE)
Cu_AN <- read_excel("Project/0.Data/Raw_Data_Fig_2_Cu_conc.xlsx", 
                                     sheet = "Cu_µg_gFW_AN", range = "B10:I26")
#???import H2O2 for AG and AN
library(readxl)
AG_H2O2 <- read_excel("Project/0.Data/Raw_Data_Fig_3_H2O2_conc.xlsx", 
                                       sheet = "H2O2_AG", range = "B10:I50")
AN_H2O2 <- read_excel("Project/0.Data/Raw_Data_Fig_3_H2O2_conc.xlsx", 
                                       sheet = "H2O2_AN", range = "B10:I50")
print(AN_H2O2)

#import Lipid peroxidation
library(readxl)
AG_Lipid <- read_excel("Project/0.Data/Raw_Data_Fig_4_Lipid_peroxidation.xlsx", 
                                                sheet = "MDA_AG", range = "B10:I50")
AN_Lipid <- read_excel("Project/0.Data/Raw_Data_Fig_4_Lipid_peroxidation.xlsx", 
                       sheet = "MDA_AN", range = "B10:I50")


#create more variables from the 8 treatments of Cu doc:
#???Cu: TRUE or FALSE
#DOC: AN or AG
#Cu: concentration


