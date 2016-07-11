emf.rna.backward2 <- function (
    rna,
    padrao, #Registro, de 1 a N, padrao a padrao
    alpha = 0.1,
    camada1 = TRUE,
    camada2 = TRUE
)
{
    E = rna$E;
    e = rna$e;

    Z = cbind(1, rna$Z); #ADD Bias
    X = cbind(1, rna$X); #ADD Bias
    A = cbind(rna$A0, rna$A); #ADD Peso do Bias
    B = cbind(rna$B0, rna$B); #ADD Peso do Bias

    dEdb = matrix(data=0, nrow = rna$info$qtOut, ncol = rna$info$qtHid+1);
    bnew = matrix(nrow = rna$info$qtOut, ncol = rna$info$qtHid+1);
    if(is.null(rna$dEdb)) rna$dEdb = dEdb; #Armazena ultima atualizacao

    for(h in 1:(rna$info$qtHid+1)){
        for(o in 1:rna$info$qtOut){
            dEdb[o,h] = e[padrao, o] * rna$func2der(rna$Yin[padrao,o]) * Z[padrao, h];
            bnew[o,h] = B[o,h] - alpha * dEdb[o,h];# - alpha * 0.2 * rna$dEdb[o,h]; #Atencao ao fator extra
            rna$dEdb[o, h] = dEdb[o, h]; #Salva  ultima atualizacao para usar como fator extra
        }
    }

    dEda = matrix(data=0, nrow = rna$info$qtHid, ncol = rna$info$qtIn+1);
    anew = matrix(nrow = rna$info$qtHid, ncol = rna$info$qtIn+1);
    if(is.null(rna$dEda)) rna$dEda = dEda; #Ultima atualizacao

    for(i in 1:(rna$info$qtIn+1)){
        for(h in 1:rna$info$qtHid){
            #o = 1; #temporario
            dEda[h,i] = sum( e[padrao, ] * rna$func2der(rna$Yin[padrao, ]) * rna$B[ ,h]) * rna$func1der(rna$Zin[padrao,h]) * X[padrao,i];
            anew[h,i] = A[h,i] - alpha * dEda[h,i];# - alpha * 0.2 * rna$dEda[h,i]; #Atencao ao fator extra
            rna$dEda[h, i] = dEda[h, i]; #Salva  ultima atualizacao para usar como fator extra
        }
    }

    #3 Atualiza A e B
    if(camada2){
        rna$B  = bnew[,2:dim(bnew)[2], drop=FALSE];
        rna$B0 = bnew[,1, drop=FALSE];
    }
    if(camada1){
        rna$A  = anew[,2:dim(anew)[2], drop=FALSE];
        rna$A0 = anew[,1, drop=FALSE];
    }

    return ( rna );
}
