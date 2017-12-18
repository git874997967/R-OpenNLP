library(rmmseg4j)
library(Rwordseg)
install.packages("Rwordseg", repos="http://R-Forge.R-project.org")  
result=mmseg4j('李彦宏会对马云造成威胁吗')
summary(result)
result
