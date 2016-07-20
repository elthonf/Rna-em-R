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
qtde.neuronios = c( 2, 3, 4, 5, 5, 5, 5, 10, 2, 3, 4, 5, 5, 5, 5, 3, 5, 5, 5, 5, 3, 5, 5, 5, 5 )

########## ########## CENÁRIOS 1:8 = 24horas ########## ########## ##########
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
#Cenário 5 - Adiciona Europa
treina.X[[5]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "FTSE", "GDAXI", "FCHI")] )
treina.Y[[5]] = COTACOES[3:80, "D1v"]
valida.X[[5]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "FTSE", "GDAXI", "FCHI")] )
valida.Y[[5]] = COTACOES[83:123, "D1v"]
#Cenário 6 - Adiciona Asia
treina.X[[6]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "N225", "HSI", "BSESN")] )
treina.Y[[6]] = COTACOES[3:80, "D1v"]
valida.X[[6]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "N225", "HSI", "BSESN")] )
valida.Y[[6]] = COTACOES[83:123, "D1v"]
#Cenário 7 - Adiciona Oceania
treina.X[[7]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "AXJO", "NZ50", "JKSE")] )
treina.Y[[7]] = COTACOES[3:80, "D1v"]
valida.X[[7]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "AXJO", "NZ50", "JKSE")] )
valida.Y[[7]] = COTACOES[83:123, "D1v"]
#Cenário 8 - Adiciona TUDO
treina.X[[8]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "GSPC", "MXX", "IPSA", "MERV", "FTSE", "GDAXI", "FCHI", "N225", "HSI", "BSESN", "AXJO", "NZ50", "JKSE")] )
treina.Y[[8]] = COTACOES[3:80, "D1v"]
valida.X[[8]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "GSPC", "MXX", "IPSA", "MERV", "FTSE", "GDAXI", "FCHI", "N225", "HSI", "BSESN", "AXJO", "NZ50", "JKSE")] )
valida.Y[[8]] = COTACOES[83:123, "D1v"]

########## ########## CENÁRIOS 9:15 = 48horas ########## ########## ##########
#Cenário 9
treina.X[[9]] = cbind( COTACOES[1:79, "Var"], COTACOES[2:80, "Var"])
treina.Y[[9]] = COTACOES[2:80, "D2v"]
valida.X[[9]] = cbind( COTACOES[81:122, "Var"], COTACOES[82:123, "Var"])
valida.Y[[9]] = COTACOES[82:123, "D2v"]
#Cenário 10
treina.X[[10]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, "Var"])
treina.Y[[10]] = COTACOES[3:80, "D2v"]
valida.X[[10]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, "Var"])
valida.Y[[10]] = COTACOES[83:123, "D2v"]
#Cenário 11
treina.X[[11]] = cbind( COTACOES[1:77, "Var"], COTACOES[2:78, "Var"], COTACOES[3:79, "Var"], COTACOES[4:80, "Var"])
treina.Y[[11]] = COTACOES[4:80, "D2v"]
valida.X[[11]] = cbind( COTACOES[81:120, "Var"], COTACOES[82:121, "Var"], COTACOES[83:122, "Var"], COTACOES[84:123, "Var"])
valida.Y[[11]] = COTACOES[84:123, "D2v"]
#Cenário 12 - Adiciona Americas
treina.X[[12]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "GSPC", "MXX", "IPSA", "MERV")] )
treina.Y[[12]] = COTACOES[3:80, "D2v"]
valida.X[[12]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "GSPC", "MXX", "IPSA", "MERV")])
valida.Y[[12]] = COTACOES[83:123, "D2v"]
#Cenário 13 - Adiciona Europa
treina.X[[13]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "FTSE", "GDAXI", "FCHI")] )
treina.Y[[13]] = COTACOES[3:80, "D2v"]
valida.X[[13]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "FTSE", "GDAXI", "FCHI")] )
valida.Y[[13]] = COTACOES[83:123, "D2v"]
#Cenário 14 - Adiciona Asia
treina.X[[14]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "N225", "HSI", "BSESN")] )
treina.Y[[14]] = COTACOES[3:80, "D2v"]
valida.X[[14]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "N225", "HSI", "BSESN")] )
valida.Y[[14]] = COTACOES[83:123, "D2v"]
#Cenário 15 - Adiciona Oceania
treina.X[[15]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "AXJO", "NZ50", "JKSE")] )
treina.Y[[15]] = COTACOES[3:80, "D2v"]
valida.X[[15]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "AXJO", "NZ50", "JKSE")] )
valida.Y[[15]] = COTACOES[83:123, "D2v"]

########## ########## CENÁRIOS 16:20 = 1 semana ########## ########## ##########

#Cenário 16
treina.X[[16]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, "Var"])
treina.Y[[16]] = COTACOES[3:80, "D7v"]
valida.X[[16]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, "Var"])
valida.Y[[16]] = COTACOES[83:123, "D7v"]
#Cenário 17 - Adiciona Americas
treina.X[[17]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "GSPC", "MXX", "IPSA", "MERV")] )
treina.Y[[17]] = COTACOES[3:80, "D7v"]
valida.X[[17]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "GSPC", "MXX", "IPSA", "MERV")])
valida.Y[[17]] = COTACOES[83:123, "D7v"]
#Cenário 18 - Adiciona Europa
treina.X[[18]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "FTSE", "GDAXI", "FCHI")] )
treina.Y[[18]] = COTACOES[3:80, "D7v"]
valida.X[[18]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "FTSE", "GDAXI", "FCHI")] )
valida.Y[[18]] = COTACOES[83:123, "D7v"]
#Cenário 19 - Adiciona Asia
treina.X[[19]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "N225", "HSI", "BSESN")] )
treina.Y[[19]] = COTACOES[3:80, "D7v"]
valida.X[[19]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "N225", "HSI", "BSESN")] )
valida.Y[[19]] = COTACOES[83:123, "D7v"]
#Cenário 20 - Adiciona Oceania
treina.X[[20]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "AXJO", "NZ50", "JKSE")] )
treina.Y[[20]] = COTACOES[3:80, "D7v"]
valida.X[[20]] = cbind( COTACOES[81:121, "Var"], COTACOES[82:122, "Var"], COTACOES[83:123, c("Var", "AXJO", "NZ50", "JKSE")] )
valida.Y[[20]] = COTACOES[83:123, "D7v"]

########## ########## CENÁRIOS 21:25 = 1 mês ########## ########## ##########

#Cenário 21
treina.X[[21]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, "Var"])
treina.Y[[21]] = COTACOES[3:80, "D30v"]
valida.X[[21]] = cbind( COTACOES[81:106, "Var"], COTACOES[82:107, "Var"], COTACOES[83:108, "Var"])
valida.Y[[21]] = COTACOES[83:108, "D30v"]
#Cenário 22 - Adiciona Americas
treina.X[[22]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "GSPC", "MXX", "IPSA", "MERV")] )
treina.Y[[22]] = COTACOES[3:80, "D30v"]
valida.X[[22]] = cbind( COTACOES[81:106, "Var"], COTACOES[82:107, "Var"], COTACOES[83:108, c("Var", "GSPC", "MXX", "IPSA", "MERV")])
valida.Y[[22]] = COTACOES[83:108, "D30v"]
#Cenário 23 - Adiciona Europa
treina.X[[23]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "FTSE", "GDAXI", "FCHI")] )
treina.Y[[23]] = COTACOES[3:80, "D30v"]
valida.X[[23]] = cbind( COTACOES[81:106, "Var"], COTACOES[82:107, "Var"], COTACOES[83:108, c("Var", "FTSE", "GDAXI", "FCHI")] )
valida.Y[[23]] = COTACOES[83:108, "D30v"]
#Cenário 24 - Adiciona Asia
treina.X[[24]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "N225", "HSI", "BSESN")] )
treina.Y[[24]] = COTACOES[3:80, "D30v"]
valida.X[[24]] = cbind( COTACOES[81:106, "Var"], COTACOES[82:107, "Var"], COTACOES[83:108, c("Var", "N225", "HSI", "BSESN")] )
valida.Y[[24]] = COTACOES[83:108, "D30v"]
#Cenário 25 - Adiciona Oceania
treina.X[[25]] = cbind( COTACOES[1:78, "Var"], COTACOES[2:79, "Var"], COTACOES[3:80, c("Var", "AXJO", "NZ50", "JKSE")] )
treina.Y[[25]] = COTACOES[3:80, "D30v"]
valida.X[[25]] = cbind( COTACOES[81:106, "Var"], COTACOES[82:107, "Var"], COTACOES[83:108, c("Var", "AXJO", "NZ50", "JKSE")] )
valida.Y[[25]] = COTACOES[83:108, "D30v"]

########## ########## FUNÇÕES DE LOG E EXECUCAO ########## ########## ##########
#log.env <- new.env()
#log.env$log.data = list()
if(!exists("log.data")) log.data = list()

logExec <- function( i, rede){
    corretos = which(emf.rna.var.to.label(cbind(rede$YD)) == emf.rna.var.to.label(cbind(rede$dynamic$Y)))

    .GlobalEnv$log.data$hora = c(.GlobalEnv$log.data$hora, Sys.time())
    #print(log.env$log.data$hora)
    if(length(.GlobalEnv$log.data$hora)==1) class(.GlobalEnv$log.data$hora) = class(Sys.time())
    .GlobalEnv$log.data$cenario = c(.GlobalEnv$log.data$cenario, rede$cenario)
    .GlobalEnv$log.data$iteracao = c(.GlobalEnv$log.data$iteracao, i)
    .GlobalEnv$log.data$ET = c(.GlobalEnv$log.data$ET, rede$dynamic$ET)
    .GlobalEnv$log.data$acuracia = c(.GlobalEnv$log.data$acuracia,length(corretos) / length(rede$YD) )

    if(i%%100 == 0) cat("[", rede$cenario, "]", "Iter:", i, "Erro:", rede$dynamic$ET, ", Acurácia: ", length(corretos) / length(rede$YD) * 100.0, "%", "\n")

}

#idCenario = 1 #Seta para testes

execCenario <- function (idCenario){
    #Cria a rede de treinamento
    treina.rede[[idCenario]] = emf.rna.read.csv.files(
        cenario = paste( "TRE", idCenario) ,
        qtIn = dim(treina.X[[idCenario]])[2], qtHid = qtde.neuronios[idCenario], qtOut = 1,
        X = as.matrix( treina.X[[idCenario]] ), YD = as.matrix( treina.Y[[idCenario]] ),
        func2 = emf.rna.func.tunnel, func2der = emf.rna.func.tunnel.der);
    #Cria a rede de validacao
    valida.rede[[idCenario]] = emf.rna.read.csv.files(
        cenario = paste( "VAL", idCenario) ,
        qtIn = dim(treina.X[[idCenario]])[2], qtHid = qtde.neuronios[idCenario], qtOut = 1,
        X = as.matrix( valida.X[[idCenario]] ), YD = as.matrix( valida.Y[[idCenario]] ),
        func2 = emf.rna.func.tunnel, func2der = emf.rna.func.tunnel.der);

    #Executa Inicial
    treina.rede[[idCenario]]$dynamic = emf.rna.forward(treina.rede[[idCenario]]);
    print("Início rede.")
    logExec( 0, treina.rede[[idCenario]] );

    #Valida Inicial
    valida.rede[[idCenario]]$dynamic$A0 = treina.rede[[idCenario]]$dynamic$A0;
    valida.rede[[idCenario]]$dynamic$B0 = treina.rede[[idCenario]]$dynamic$B0;
    valida.rede[[idCenario]]$dynamic$A = treina.rede[[idCenario]]$dynamic$A;
    valida.rede[[idCenario]]$dynamic$B = treina.rede[[idCenario]]$dynamic$B;

    #Executa Validacao
    valida.rede[[idCenario]]$dynamic = emf.rna.forward(valida.rede[[idCenario]]);
    logExec( 0, valida.rede[[idCenario]] );



    print("Início treinamento.")
    #LOOP de treinamento
    alpha.min = 0.1;
    alpha.max = 0.1;
    for(i in 1:10000){
        if(i == 3000){ #Diminui o Alfa após algumas execucoes
            alpha.min = 0.05;
            alpha.max = 0.05;
        }
        if(i == 6000){ #Diminui o Alfa após algumas execucoes
            alpha.min = 0.03;
            alpha.max = 0.03;
        }
        lastET = treina.rede[[idCenario]]$dynamic$ET;
        treina.rede[[idCenario]]$dynamic = emf.rna.backward.padrao(rna = treina.rede[[idCenario]], alpha.min = alpha.min, alpha.max = alpha.max);
        logExec( i, treina.rede[[idCenario]] );

        #Copia pesos para rede de validacao
        valida.rede[[idCenario]]$dynamic$A0 = treina.rede[[idCenario]]$dynamic$A0;
        valida.rede[[idCenario]]$dynamic$B0 = treina.rede[[idCenario]]$dynamic$B0;
        valida.rede[[idCenario]]$dynamic$A = treina.rede[[idCenario]]$dynamic$A;
        valida.rede[[idCenario]]$dynamic$B = treina.rede[[idCenario]]$dynamic$B;

        #Executa Validacao
        valida.rede[[idCenario]]$dynamic = emf.rna.forward(valida.rede[[idCenario]]);
        logExec( i, valida.rede[[idCenario]] );

    }
}


for(i in 1:25){
    execCenario(i)
}
rm(i)
#execCenario(1)
#execCenario(2)
#execCenario(3)
#execCenario(4)

log.tab = data.frame(log.data)
log.resumo = rbind(  log.tab[(log.tab$iteracao==00000) ,] , log.tab[(log.tab$iteracao==10000) ,] )

write.csv( x = log.tab, file = "dados/MLP.log.tab.csv" )
write.csv( x = log.resumo, file = "dados/MLP.log.resumo.csv" )

View( log.resumo )
save.image("dados/MLP Exec.RData")


