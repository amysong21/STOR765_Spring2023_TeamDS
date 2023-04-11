install.packages("readxl")
library(readxl)
df = readxl::read_excel("RData/cognit_stor765_data.xlsx",
                        sheet = "ROI_Nback_2vs0_block4")
