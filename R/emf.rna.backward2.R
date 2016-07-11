emf.rna.backward2 <- function (
    rna,
    padrao = NULL, #Registro, ou colecao de 1 a N, padrao a padrao
    alpha.min = 0.1,
    alpha.max = 0.9,
    camada1 = TRUE,
    camada2 = TRUE
)
{
    if(is.null(padrao)){ #Se nao passou o padrao, faz todos
        padrao = 1 : dim(rna$X)[1]
    }

    for(pad in padrao){
        alpha = runif(n = 1, min = alpha.min, max = alpha.max)

        E = rna$dynamic$E;
        e = rna$dynamic$e;

        Z = cbind(1, rna$dynamic$Z); #ADD Bias
        X = cbind(1, rna$X); #ADD Bias
        A = cbind(rna$dynamic$A0, rna$dynamic$A); #ADD Peso do Bias
        B = cbind(rna$dynamic$B0, rna$dynamic$B); #ADD Peso do Bias


        dEdb = matrix(data=0, nrow = rna$info$qtOut, ncol = rna$info$qtHid+1);
        bnew = matrix(nrow = rna$info$qtOut, ncol = rna$info$qtHid+1);
        if(is.null(rna$dynamic$dEdb)) rna$dynamic$dEdb = dEdb; #Armazena ultima atualizacao

        for(h in 1:(rna$info$qtHid+1)){
            for(o in 1:rna$info$qtOut){
                dEdb[o,h] = e[pad, o] * rna$func2der(rna$dynamic$Yin[pad,o]) * Z[pad, h];
                bnew[o,h] = B[o,h] - alpha * dEdb[o,h];# - alpha * 0.2 * rna$dynamic$dEdb[o,h]; #Atencao ao fator extra
                rna$dynamic$dEdb[o, h] = dEdb[o, h]; #Salva  ultima atualizacao para usar como fator extra
            }
        }

        dEda = matrix(data=0, nrow = rna$info$qtHid, ncol = rna$info$qtIn+1);
        anew = matrix(nrow = rna$info$qtHid, ncol = rna$info$qtIn+1);
        if(is.null(rna$dynamic$dEda)) rna$dynamic$dEda = dEda; #Ultima atualizacao

        for(i in 1:(rna$info$qtIn+1)){
            for(h in 1:rna$info$qtHid){
                #o = 1; #temporario
                dEda[h,i] = sum( e[pad, ] * rna$func2der(rna$dynamic$Yin[pad, ]) * rna$dynamic$B[ ,h]) * rna$func1der(rna$dynamic$Zin[pad,h]) * X[pad,i];
                anew[h,i] = A[h,i] - alpha * dEda[h,i];# - alpha * 0.2 * rna$dynamic$dEda[h,i]; #Atencao ao fator extra
                rna$dynamic$dEda[h, i] = dEda[h, i]; #Salva  ultima atualizacao para usar como fator extra
            }
        }

        #3 Atualiza A e B
        if(camada2){
            rna$dynamic$B  = bnew[,2:dim(bnew)[2], drop=FALSE];
            rna$dynamic$B0 = bnew[,1, drop=FALSE];
        }
        if(camada1){
            rna$dynamic$A  = anew[,2:dim(anew)[2], drop=FALSE];
            rna$dynamic$A0 = anew[,1, drop=FALSE];
        }

        rna$dynamic = emf.rna.forward( rna );
    }

    return ( rna$dynamic );
}
