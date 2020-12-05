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
Cu8 <- dplyr::bind_rows(Cu_AG, Cu_AN)
View(Cu)#we have table with the species and only one column of Trt(!!!!)

#(we can get the same table with this:)
library(dplyr)
Cu6 <- dplyr::union(Cu_AG, Cu_AN)
Cu6
