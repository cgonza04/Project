mergedcu <- merge(Cu_AG,Cu_AN,
                  all.x = TRUE,
                  
                  sort(Cu_AG$Cu_µg_gFW_AG, decreasing = FALSE)
                 
                   )#all.x = true == keep all the rows of both data frames


View(mergedcu)

cbind()#bind by column with base R (don't use it)
rbind()#by row


# merge the 2 tables with tidyr package

#add a column with tree name:
dplyr::mutate(Cu, Tree_name = Sepal.Length + Sepal. Width)

library(tidyr)
#install.packages("tidyr")
Cu <- dplyr::bind_rows(Cu_AG, Cu_AN)
View(Cu)

