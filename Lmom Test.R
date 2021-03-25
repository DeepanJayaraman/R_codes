# Test Lmom
# Siemens - Turbine disk problem

library(readxl)
data <- read_excel("D:/Seimens - Turbine disk problem/data.xlsx", 
                   range = "A4:W203", col_names = FALSE)

# Z = read_excel("D:\\Seimens - Turbine disk problem\\data.xlsx")

T <- read_excel("D:/Seimens - Turbine disk problem/data.xlsx", 
                   range = "A4:Q4", col_names = FALSE)

X = data[,1:16] # Input variables(10+7)
Y1 = data[,17] # 



Z = kriging(X,Y1,matrix(T))