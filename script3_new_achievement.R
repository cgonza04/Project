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
View(Cu2)
