mergedcu <- merge(Cu_AG,Cu_AN,
                  all.x = TRUE,
                  sort(Cu_AG$Cu_µg_gFW_AG, decreasing = FALSE)
                 )#all.x = true == keep all the rows of both data frames
rm(mergedcu)#bcs not working well

cbind()#bind by column with base R (don't use it)
rbind()#by row (don't use it)

# merge the 2 tables with tidyr package

# Change colname of one column so we give the same name to Cu_µg_gFW_AN and Cu_µg_gFW_AG
colnames(Cu_AG)[colnames(Cu_AG) == "Cu_µg_gFW_AG"] <- "Trt"
colnames(Cu_AN)[colnames(Cu_AN) == "Cu_µg_gFW_AN"] <- "Trt"
#Trt for the Treatment

AN <- "AN"#create vector with specie "AN"
AG <- "AG"
Cu_AN <- dplyr::mutate(Cu_AN, Tree_sp = AN)#to add the column for the tree specie AN
Cu_AG <- dplyr::mutate(Cu_AG, Tree_sp = AG)

#add a column with tree name:
library(tidyr)
#install.packages("tidyr")
Cu <- dplyr::bind_rows(Cu_AG, Cu_AN)
View(Cu)#we have table with the species

#to fuse the columns with treatment
Cu2 <- dplyr::full_join(Cu_AG, Cu_AN, by = "Tree_sp")#not good
Cu3 <- dplyr::inner_join(Cu_AG, Cu_AN, by = "Tree_sp")#worse
Cu4 <- dplyr::right_join(Cu_AG, Cu_AN, by = "Tree_sp")#worse
Cu5 <- dplyr::left_join(Cu_AG, Cu_AN, by = "Tree_sp")#worse

#we want to create new vector for column
#try1
for (i in levels(Cu$Cu_µg_gFW_AN)){
  Treatment <- c("Treatment", i)
}#nope
  
#try2
Cu$Treatment <- paste(Cu$Cu_µg_gFW_AN,Cu$Cu_µg_gFW_AG, sep = "_")
#makes NA_1, NA_2,..., 1_NA, 2_NA...

install.packages("dplyr")
library(dplyr)
add_row(Cu, eruptions = 1, waiting = 1)
rlang::last_error()
rlang::last_trace()

library(dplyr)
library(tidyr)
cols_merge(
  Cu,
  Cu$Cu_µg_gFW_AN,
  Cu$Cu_µg_gFW_AG,
  pattern = paste0("{", seq_along(columns), "}", collapse = " ")
)

cols_merge(
  Cu,
  columns = Trt, Trt
  )
#doesn't work. Error: function cols_merge doesn't exist

Cu6 <- dplyr::union(Cu_AG, Cu_AN)
Cu6
#{<error/rlang_error> not compatible: 
# - Cols in y but not x: `Cu_µg_gFW_AN`.
# - Cols in x but not y: `Cu_µg_gFW_AG`.
#when we changes the columns names it works(!!!!!)

#to remove duplicate value (control values):
distinct(.data, ..., .keep_all = FALSE) Remove
rows with duplicate values.
distinct(iris, Species)

#to select only the columns we want:
#first rename the columns with numbers in the title because gives error messages
colnames(Cu)[colnames(Cu) == "48h"] <- "fourtyeightH"
library(tidyr)
Cu7 <- select(Cu$Trt, Cu$Tree_sp, Cu$"48h")
