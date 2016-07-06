rm(list=ls())

VARIACOES = read.csv("dados/variacoes.indices.csv", header = TRUE, sep = ";", dec = ",");
VARIACOES$Data = as.Date(VARIACOES$Data, format="%d/%m/%y")
VARIACOES[,3:19] = tanh(VARIACOES[,3:19]/5) #Normalizando . inverso seria log(sqrt( (1 + y) / (1 - y)) *5, base = exp(1))


#1 - Define rede para americas entre dias 01-abr-2016 e 31-mai-2016!
rna.americas = emf.rna.read.csv.files(
    qtIn = 4, qtHid = 4, qtOut = 1,
    X = as.matrix( VARIACOES[1:41, c("SPX", "MXX", "IPSA", "MERV")] ),
    YD = as.matrix( VARIACOES[2:42, c("BVSP")] ),
    func1 = emf.rna.func.tanh, func1der = emf.rna.func.tanh.der,
    func2 = emf.rna.func.tanh, func2der = emf.rna.func.tanh.der
)

rna.americas = emf.rna.forward(rna.americas)
corretos = which(emf.rna.var.to.label(cbind(rna.americas$YD)) == emf.rna.var.to.label(cbind(rna.americas$Y)))
cat("Erro inicial: ", rna.americas$ET, ", Acurácia: ", length(corretos) / length(rna.americas$Y) * 100.0, "%", "\n")

#2 - Treina rede para américas
for(i in 1:100000){
    rna.americas = emf.rna.backward(rna = rna.americas, alpha = runif(1, min=0.1, max=0.9))
    corretos = which(emf.rna.var.to.label(cbind(rna.americas$YD)) == emf.rna.var.to.label(cbind(rna.americas$Y)))
    cat(i, "Erro:", rna.americas$ET, ", Acurácia: ", length(corretos) / length(rna.americas$Y) * 100.0, "%", "\n")
}
cat("[TREINAMENTO] Erro: ", rna.americas$ET, ", Acurácia: ", length(corretos) / length(rna.americas$Y) * 100.0, "%", "\n")


#3 - Define rede para americas entre dias 01-jun-2016 e 30-jun-2016 para VALIDAÇÃO!
rna.americas2 = emf.rna.read.csv.files(
    qtIn = 4, qtHid = 4, qtOut = 1,
    X = as.matrix( VARIACOES[42:63, c("SPX", "MXX", "IPSA", "MERV")] ),
    YD = as.matrix( VARIACOES[43:64, c("BVSP")] ),
    func1 = emf.rna.func.tanh, func1der = emf.rna.func.tanh.der,
    func2 = emf.rna.func.tanh, func2der = emf.rna.func.tanh.der,
    A = cbind( rna.americas$A0, rna.americas$A ),
    B = cbind( rna.americas$B0, rna.americas$B )
)
rna.americas2 = emf.rna.forward(rna.americas2)
corretos = which(emf.rna.var.to.label(cbind(rna.americas2$YD)) == emf.rna.var.to.label(cbind(rna.americas2$Y)))
cat("[VALIDACAO] Erro: ", rna.americas2$ET, ", Acurácia: ", length(corretos) / length(rna.americas2$Y) * 100.0, "%", "\n")

