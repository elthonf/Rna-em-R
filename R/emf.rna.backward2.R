emf.rna.backward2 <- function (
    rna,
    padrao, #Registro, de 1 a N, padrao a padrao
    alpha = 0.1,
    camada1 = TRUE,
    camada2 = TRUE
)
{
    E = rna$E;

    Z = cbind(1, rna$Z);
    X = cbind(1, rna$X);
    A = cbind(rna$A0, rna$A);
    B = cbind(rna$B0, rna$B);

    dEdb = matrix(data=0, nrow = rna$info$qtOut, ncol = rna$info$qtHid+1);
    bnew = matrix(nrow = rna$info$qtOut, ncol = rna$info$qtHid+1);
    if(is.null(rna$dEdb)) rna$dEdb = dEdb; #Ultima atualizacao

    for(h in 1:(rna$info$qtHid+1)){
        for(o in 1:rna$info$qtOut){
            dEdb[o,h] = E[padrao] * rna$func2der(rna$Yin[padrao,,drop = FALSE]) * Z[h];
            bnew[o,h] = B[o,h] - alpha * dEdb[o,h] - alpha * 0.2 * rna$dEdb[o,h]; #Atencao ao fator extra
        }
    }
    rna$dEdb = dEdb; #Salva  ultima atualizacao para usar como fator extra

    dEda = matrix(data=0, nrow = rna$info$qtHid, ncol = rna$info$qtIn+1);
    anew = matrix(nrow = rna$info$qtHid, ncol = rna$info$qtIn+1);
    if(is.null(rna$dEda)) rna$dEda = dEda; #Ultima atualizacao

    for(i in 1:(rna$info$qtIn+1)){
        for(h in 1:rna$info$qtHid){
            dEda[h,i] = sum(E[padrao] * rna$func2der(rna$Yin[padrao,,drop = FALSE]) * rna$B[,h, drop = FALSE]) * rna$func1der(rna$Zin[padrao,h,drop=FALSE]) * X[padrao,i];
            anew[h,i] = A[h,i] - alpha * dEda[h,i] - alpha * 0.2 * rna$dEda[h,i]; #Atencao ao fator extra
        }
    }
    rna$dEda = dEda; #Salva  ultima atualizacao para usar como fator extra

    #3 Atualiza A e B
    if(camada2){
        rna$B0 = matrix( bnew[,1], nrow = nrow(bnew) );
        rna$B  = matrix( bnew[,2:dim(bnew)[2]], nrow = nrow(bnew) );
    }
    if(camada1){
        rna$A0 = matrix( anew[,1], nrow = nrow(anew) );
        rna$A  = matrix( anew[,2:dim(anew)[2]], nrow = nrow(anew) );
    }

    return ( rna );
}
