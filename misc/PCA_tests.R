rm(list = ls())
library(popa)

setwd("//dmawi.de/potsdam/data/bioing/user/lschild/surrogate/output")

files <- list.files()[-1]
IDs <- as.numeric(sub("_0.1.csv","",files))
missing <- unique(P$Dataset_ID)[!(unique(P$Dataset_ID) %in% IDs)]

for(ID in missing[-1]){
  surrogate(ID,
            0.1,
            0.5,
            0,
            1)
}

library(vegan)
x <- P[P$Dataset_ID == 7,6:ncol(P)]
x <- x[,colSums(x)!=0]
biplot(prcomp(sqrt(x)))

y <- P[P$Dataset_ID == 4484,6:ncol(P)]
y <- y[,colSums(y)!= 0]
biplot(prcomp(sqrt(y)))

A <- matrix(data = c(0.8,0.2,0,
                     0.4,0.5,0.1,
                     0.8,0.2,0,
                     0.4,0.5,0.1,
                     0.8,0.2,0,
                     0.4,0.2,0,1),
            byrow = TRUE,
            ncol = 3)
A
B <- matrix(data = c(0.8,0.1,0.1,
                     0.4,0.5,0.1,
                     0.8,0.1,0.1,
                     0.4,0.5,0.1,
                     0.8,0.1,0.1,
                     0.4,0.5,0.1),
            byrow = TRUE,
            ncol = 3)
biplot(princomp(sqrt(B)))
biplot(princomp(sqrt(A)))
pca_A <- rda(sqrt(A))
pca_B <- rda(sqrt(B))
summary(pca_A)
summary(pca_B)
