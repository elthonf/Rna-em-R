rm(list=ls())
#carrega dados e normaliza!
VARIACOES = read.csv("dados/variacoes.indices.csv", header = TRUE, sep = ";", dec = ",");
VARIACOES$Data = as.Date(VARIACOES$Data, format="%d/%m/%y");
VARIACOES[,3:19] = tanh(VARIACOES[,3:19]/5); #Normalizando . inverso seria log(sqrt( (1 + y) / (1 - y)) *5, base = exp(1))

########## ########## ########## ########## ########## ##########

inicial = list();
treinamentos = list();
validacoes = list();

#Define dados iniciais para treinamento e dados de validacao para cenário 1
inicial[[1]] = emf.rna.read.csv.files(
    cenario = "TRE 01", qtIn = 4, qtHid = 4, qtOut = 1,
    X = as.matrix( VARIACOES[1:41, c("SPX", "MXX", "IPSA", "MERV")] ), #Define datas e bolsas (entre dias 01-abr-2016 e 31-mai-2016!)
    YD = as.matrix( VARIACOES[2:42, c("BVSP")] ) )

validacoes[[1]] = inicial[[1]];
validacoes[[1]]$cenario = "VAL 01";
validacoes[[1]]$X = as.matrix( VARIACOES[42:63, c("SPX", "MXX", "IPSA", "MERV")] );
validacoes[[1]]$YD = as.matrix( VARIACOES[43:64, c("BVSP")] );

#Define treinamento e validacao para cenário 2
inicial[[2]] = emf.rna.read.csv.files(
    cenario = "TRE 02", qtIn = 4, qtHid = 3, qtOut = 1,
    X = as.matrix( VARIACOES[1:41, c("SPX", "MXX", "IPSA", "MERV")] ), #Define datas e bolsas (entre dias 01-abr-2016 e 31-mai-2016!)
    YD = as.matrix( VARIACOES[2:42, c("BVSP")] ) )

validacoes[[2]] = inicial[[2]];
validacoes[[2]]$cenario = "VAL 02";
validacoes[[2]]$X = as.matrix( VARIACOES[42:63, c("SPX", "MXX", "IPSA", "MERV")] );
validacoes[[2]]$YD = as.matrix( VARIACOES[43:64, c("BVSP")] );

#Define treinamento e validacao para cenário 3
inicial[[3]] = emf.rna.read.csv.files(
    cenario = "TRE 03", qtIn = 4, qtHid = 8, qtOut = 1,
    X = as.matrix( VARIACOES[1:41, c("SPX", "MXX", "IPSA", "MERV")] ), #Define datas e bolsas (entre dias 01-abr-2016 e 31-mai-2016!)
    YD = as.matrix( VARIACOES[2:42, c("BVSP")] ) )

validacoes[[3]] = inicial[[3]];
validacoes[[3]]$cenario = "VAL 03";
validacoes[[3]]$X = as.matrix( VARIACOES[42:63, c("SPX", "MXX", "IPSA", "MERV")] );
validacoes[[3]]$YD = as.matrix( VARIACOES[43:64, c("BVSP")] );


##### ##### ##### ##### LOOP de execuções!!!

for( k in 3 : 3){
    #1 - Forward da rede inicial!
    redeTre = inicial[[k]] #Copia rede a ser treinada do vetor de redes (isola)
    redeTre = emf.rna.forward(redeTre)
    corretos = which(emf.rna.var.to.label(cbind(redeTre$YD)) == emf.rna.var.to.label(cbind(redeTre$Y)))
    cat("[", redeTre$cenario, "]", "Erro inicial: ", redeTre$ET, ", Acurácia inicial: ", length(corretos) / length(redeTre$Y) * 100.0, "%", "\n")

    inicial[[k]] = redeTre; #Armazena rede inicial ativada no vetor de redes

    #2 - Treina rede para o cenário recebido para américas
    for(i in 1:100000){
        redeTre = emf.rna.backward(rna = redeTre, alpha = runif(1, min=0.1, max=0.9))
        corretos = which(emf.rna.var.to.label(cbind(redeTre$YD)) == emf.rna.var.to.label(cbind(redeTre$Y)))
        cat("[", redeTre$cenario, "]", "Iter:", i, "Erro:", redeTre$ET, ", Acurácia: ", length(corretos) / length(redeTre$Y) * 100.0, "%", "\n")
    }
    cat("[", redeTre$cenario, "]", "Erro: ", redeTre$ET, ", Acurácia: ", length(corretos) / length(redeTre$Y) * 100.0, "%", "\n")
    treinamentos[[k]] = redeTre #Devolve rede treinada para o vetor de redes


    #3 - Valida
    redeVal = validacoes[[k]] #Copia rede a ser validada do vetor de redes (isola)

    #4 - Copia pesos definidos
    redeVal$A0 = redeTre$A0
    redeVal$A = redeTre$A
    redeVal$B0 = redeTre$B0
    redeVal$B = redeTre$B
    redeVal = emf.rna.forward(redeVal)
    corretos = which(emf.rna.var.to.label(cbind(redeVal$YD)) == emf.rna.var.to.label(cbind(redeVal$Y)))
    cat("[", redeVal$cenario, "]", "Erro: ", redeVal$ET, ", Acurácia: ", length(corretos) / length(redeVal$Y) * 100.0, "%", "\n")
    validacoes[[k]] = redeVal #Devolve rede validada para o vetor de redes

    rm(i, k, redeVal, redeTre, corretos)
}


for( k in 1 : 2){
    r = inicial[[k]]
    corretos = which(emf.rna.var.to.label(cbind(r$YD)) == emf.rna.var.to.label(cbind(r$Y)))
    cat("[", r$cenario, "]", "Erro inicial: ", r$ET, ", Acurácia inicial: ", length(corretos) / length(r$Y) * 100.0, "%", "\n")
    r = treinamentos[[k]]
    corretos = which(emf.rna.var.to.label(cbind(r$YD)) == emf.rna.var.to.label(cbind(r$Y)))
    cat("[", r$cenario, "]", "Erro: ", r$ET, ", Acurácia: ", length(corretos) / length(r$Y) * 100.0, "%", "\n")
    r = validacoes[[k]]
    corretos = which(emf.rna.var.to.label(cbind(r$YD)) == emf.rna.var.to.label(cbind(r$Y)))
    cat("[", r$cenario, "]", "Erro: ", r$ET, ", Acurácia: ", length(corretos) / length(r$Y) * 100.0, "%", "\n")
    rm(r, corretos)
}
