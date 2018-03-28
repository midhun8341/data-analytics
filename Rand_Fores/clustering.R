library(gdata)
df = read.xls ("Cust_seg.xls")
colSums(is.na((dfNorm)))
d <- dist(as.matrix(df))
hc <- hclust(d)
plot(hc)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfNorm <- as.data.frame(lapply(df, normalize))

library(NbClust)
nc <- NbClust(dfNorm, distance="euclidean", min.nc=3, max.nc=10, method="complete")

library(xlsxjars)
library(WriteXLS)
write.xlsx(dfNorm, file, sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)
write.csv(dfNorm, file = "Customer_seg")

WriteXLS(dfNorm, ExcelFileName = "R.xls")
