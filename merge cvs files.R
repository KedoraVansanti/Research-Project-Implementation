install.packages(dplyr)
install.packages(plyr)
install.packages(readr)

library(dplyr)
library(plyr)
library(readr)

data_all <- list.files(path="F:/Studium/MSc. Paleobiology/2.Semester/Research Project Implementation/MergeCSV", pattern ="*.csv", full.names =TRUE)%>%
  lapply(read_csv)%>%
  bind_rows
data_all_df <- as.data.frame(data_all)

setwd("F:/Studium/MSc. Paleobiology/2.Semester/Research Project Implementation/MergeCSV")
write.csv(data_all,"AllData.csv")

