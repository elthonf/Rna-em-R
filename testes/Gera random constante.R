a = runif(30000, min = -1, max = 1) #1.000 Linhas x 30 colunas
write.csv(a, file = "dados/random.csv",row.names=FALSE )


b = read.csv("dados/random.csv", header = TRUE)
colnames(b) = NULL
b = unlist(b)
#b = as.vector(b)
#load("dados/randData.RData")


