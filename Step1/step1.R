#install.packages("readr") to imporgt .csv files

### 1.a. import all the data in one or more data frames, from links

#import growth rates of L. minor exposed to copper, leaf litter extracts (LLE) 
#of Alnus glutinosa or Acer negundo, and combinations of Cu and LLE.
library(readr)
Growth_rates <- read_delim("0.Data/csv/Growth_rates.csv", 
                           "\t", escape_double = FALSE, na = "empty", 
                           trim_ws = TRUE, header = TRUE)
View(Growth_rates)
head(Growth_rates)

#import cu concentration in Lemna after time
library(readr)
Cu_conc <- read_delim("0.Data/csv/Cu_conc.csv", 
                        +     "\t", escape_double = FALSE, trim_ws = TRUE)