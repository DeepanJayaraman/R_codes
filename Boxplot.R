## OPTIMISATION - Standard deviation case - Outliers not included
# Set working directory 
setwd("D:\\R\\Seimens")
N=10
load(file=(paste(N,"_samples_optimum",".Rdata",sep = "")))
load(file=(paste(N,"_samples_optimum_out",".Rdata",sep = "")))
# K=100  # iterations

# Preallocation
vonmises_SD = matrix(nrow=100, ncol=1)
vonmises_lmom =  matrix(nrow=100, ncol=1)
vonmises_SD_out =  matrix(nrow=100, ncol=1)
vonmises_lmom_out =  matrix(nrow=100, ncol=1)

# Boxplot

for (k in 1:100){
  vonmises_SD[k] = optimum_SD[[k]][17]
  vonmises_lmom[k] = optimum_lmom[[k]][17]
  vonmises_SD_out[k] = optimum_SD_out[[k]][17]
  vonmises_lmom_out[k] = optimum_lmom_out[[k]][17]
}

mat <- cbind(vonmises_SD = as.numeric(vonmises_SD), vonmises_lmom = as.numeric(vonmises_lmom),
             vonmises_SD_out = as.numeric(vonmises_SD_out), vonmises_lmom_out = as.numeric(vonmises_lmom_out))

boxplot(mat, ylab =" Vonmises(mean+3Std.dev/3Lmom)")