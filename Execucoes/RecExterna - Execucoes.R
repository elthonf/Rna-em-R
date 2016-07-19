rm(list=ls())
# ########## ########## ########## Carrega dados e normaliza!  ########## ########## ##########
COTACOES = read.csv("dados/cotacoes2.csv", header = TRUE, sep = ",", dec = ".");
COTACOES$Date = as.Date(COTACOES$Date, format="%Y-%m-%d");
COTACOES[,c( 11, 16:32) ] = tanh(COTACOES[,c( 11, 16:32) ]/5); #Normalizando . inverso seria log(sqrt( (1 + y) / (1 - y)), base = exp(1))*5
COTACOES[,20:32 ][is.na(COTACOES[,20:32 ])] = 0.0   #Transforma NA´S em Zeros
#Dados entre 1 e 80 para treinamento (entre 2016-01-04 e 2016-04-29 : 4 meses )
#Dados entre 81 e 123 para validacao (entre 2016-05-02 e 2016-06-30 : 2 meses )
########## ########## ########## ########## ########## ##########
treina.X = list(); treina.Y = list(); valida.X = list(); valida.Y = list(); treina.rede = list();  valida.rede = list();
qtde.neuronios = c( 4, 3, 4, 5, 5, 5, 5, 10, 2, 3, 4, 5, 5, 5, 5, 3, 5, 5, 5, 5, 3, 5, 5, 5, 5 )

#Cenário 1
treina.X[[1]] = cbind( COTACOES[1:79, "Var"], COTACOES[2:80, "Var"])
treina.Y[[1]] = COTACOES[2:80, "D1v"]
valida.X[[1]] = cbind( COTACOES[81:122, "Var"], COTACOES[82:123, "Var"])
valida.Y[[1]] = COTACOES[82:123, "D1v"]
#Cenário 2
treina.X[[2]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, "Var"])
treina.Y[[2]] = COTACOES[3:80, "D1v"]
valida.X[[2]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, "Var"])
valida.Y[[2]] = COTACOES[83:123, "D1v"]
#Cenário 3
treina.X[[3]] = cbind( COTACOES[1:77, "Var"], COTACOES[2:78, "Var"], COTACOES[3:79, "Var"], COTACOES[4:80, "Var"])
treina.Y[[3]] = COTACOES[4:80, "D1v"]
valida.X[[3]] = cbind( COTACOES[81:120, "Var"], COTACOES[82:121, "Var"], COTACOES[83:122, "Var"], COTACOES[84:123, "Var"])
valida.Y[[3]] = COTACOES[84:123, "D1v"]
#Cenário 4 - Adiciona Americas
treina.X[[4]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "GSPC", "MXX", "IPSA", "MERV")] )
treina.Y[[4]] = COTACOES[3:80, "D1v"]
valida.X[[4]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "GSPC", "MXX", "IPSA", "MERV")])
valida.Y[[4]] = COTACOES[83:123, "D1v"]








logExec <- function( i, rede ){
    corretos = which(emf.rna.var.to.label(cbind(rede$YD)) == emf.rna.var.to.label(cbind(rede$dynamic$Y)))
    if(i%%100 == 0) cat("[", rede$cenario, "]", "Iter:", i, "Erro:", rede$dynamic$ET, ", Acurácia: ", length(corretos) / length(rede$YD) * 100.0, "%", "\n")
}

idCenario = 1 #Seta para teste

execCenario <- function (idCenario){
    #Cria a rede de treinamento
    treina.rede[[idCenario]] = emf.rna.read.csv.files(
        cenario = paste( "TRE", idCenario) ,
        qtIn = dim(treina.X[[idCenario]])[2], qtHid = qtde.neuronios[idCenario], qtOut = 1, qtRec = 3,
        X = as.matrix( treina.X[[idCenario]] ), YD = as.matrix( treina.Y[[idCenario]] ),
        func2 = emf.rna.func.tunnel, func2der = emf.rna.func.tunnel.der);
    #Cria a rede de validacao
    valida.rede[[idCenario]] = emf.rna.read.csv.files(
        cenario = paste( "VAL", idCenario) ,
        qtIn = dim(valida.X[[idCenario]])[2], qtHid = qtde.neuronios[idCenario], qtOut = 1, qtRec = 3,
        X = as.matrix( valida.X[[idCenario]] ), YD = as.matrix( valida.Y[[idCenario]] ),
        func2 = emf.rna.func.tunnel, func2der = emf.rna.func.tunnel.der);

    #Executa Inicial
    treina.rede[[idCenario]]$dynamic = emf.rna.forward.padrao(treina.rede[[idCenario]]);
    print("Início rede.")
    logExec( 0, treina.rede[[idCenario]] );

    print("Início treinamento.")
    #LOOP de treinamento
    for(i in 1:10000){
        treina.rede[[idCenario]]$dynamic = emf.rna.backward.recorrencia(rna = treina.rede[[idCenario]], alpha.min = 0.01, alpha.max = 0.01);
        logExec( i, treina.rede[[idCenario]] );
    }

    #Copia pesos para rede de validacao
    valida.rede[[idCenario]]$dynamic$A0 = treina.rede[[idCenario]]$dynamic$A0;
    valida.rede[[idCenario]]$dynamic$B0 = treina.rede[[idCenario]]$dynamic$B0;
    valida.rede[[idCenario]]$dynamic$A = treina.rede[[idCenario]]$dynamic$A;
    valida.rede[[idCenario]]$dynamic$B = treina.rede[[idCenario]]$dynamic$B;
    valida.rede[[idCenario]]$dynamic$C = treina.rede[[idCenario]]$dynamic$C;

    #Executa Validacao
    valida.rede[[idCenario]]$dynamic = emf.rna.forward.padrao(valida.rede[[idCenario]]);
    logExec( 0, valida.rede[[idCenario]] );
}





execCenario(1)
execCenario(2)
execCenario(3)
execCenario(4)

