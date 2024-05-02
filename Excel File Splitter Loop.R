# Excel File Spliter Loop 


library(dplyr)
library(readxl)
library(openxlsx)


setwd("C:/current/file/path/")

data <- read.xlsx("./path/to/excel/file.xlsx")

data <- data |> filter(data == catagories)

for i in unique(data$`character vector`)) {
  subset_data <- data |> filter(data = condition)
  write.xlsx(subset_data, paste0("../path/to/excel/", i, "_file.xlsx"))
}
