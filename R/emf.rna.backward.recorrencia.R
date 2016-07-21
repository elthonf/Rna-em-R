emf.rna.backward.recorrencia <- function (
    rna,
    alpha.min = 0.1,
    alpha.max = 0.9,
    camada1 = TRUE,
    camada2 = TRUE
)
{

    #rna = treina.rede[[idCenario]]
    #padrao = 1 : 28
    #dYdb[1,,1:31]

    padrao = 1 : dim(rna$X)[1]; #Com recorrência, sempre faz todos os padrões.

    Z = cbind(1, rna$dynamic$Z); #ADD Bias
    X = cbind(1, rna$X, rna$dynamic$R); #ADD Bias e recorrencia externa

    dYdb = array(data=0, dim =  c(rna$info$qtOut, rna$info$qtHid+1, dim(rna$X)[1])); #Cria array de 3 DIMENSÕES!!!
    #Atualiza dYdb
    for(pad in padrao){
        #dYdb = array(data=0, dim =  c(rna$info$qtOut, rna$info$qtHid+1, dim(rna$X)[1])); #Cria array de 3 DIMENSÕES!!!
        #Variaveis a usar na recorrencia!
        padMax = max(0, pad - 1);
        padMin = max(ifelse(padMax ==0, 0, 1), pad - rna$info$qtRec);
        cMax = rna$info$qtRec;
        cMin = cMax - (padMax - padMin);

        for(h in 1:(rna$info$qtHid+1)){
            for(o in 1:rna$info$qtOut){ # Obs.: Qtde Out = 1 APENAS!!!
                #Z[pad, h] = Z atual para o neurônio H em questão. O BIAS é o primeiro
                #Em seguida, SOMA o produto de : B (Sem o BIAS)

                if(pad == 1){
                    dYdbTermo2 = 0;
                }else{ #Não possui a derivada de f´ foi é linear
                    dYdbTermo2 = t(rna$dynamic$B[o,]) %*% ( rna$dynamic$C[,cMin:cMax] * dYdb[o,2:(rna$info$qtHid+1),padMin:padMax ] );
                }

                dYdb[o,h, pad] = Z[pad, h] + sum( dYdbTermo2 );
            }
        }
    #}

    #for(pad in padrao){
        alpha = runif(n = 1, min = alpha.min, max = alpha.max)

        E = rna$dynamic$E;
        e = rna$dynamic$e;

        A = cbind(rna$dynamic$A0, rna$dynamic$A, rna$dynamic$C); #ADD Peso do Bias e da recorrencia externa
        B = cbind(rna$dynamic$B0, rna$dynamic$B); #ADD Peso do Bias


        #Atualiza dEdb
        dEdb = matrix(data=0, nrow = rna$info$qtOut, ncol = rna$info$qtHid+1);
        bnew = matrix(nrow = rna$info$qtOut, ncol = rna$info$qtHid+1);
        if(is.null(rna$dynamic$dEdb)) rna$dynamic$dEdb = dEdb; #Armazena ultima atualizacao

        for(h in 1:(rna$info$qtHid+1)){
            for(o in 1:rna$info$qtOut){
                dEdb[o,h] = e[pad, o] * dYdb[o,h, pad]; #rna$func2der(rna$dynamic$Yin[pad,o]) * Z[pad, h];
                bnew[o,h] = B[o,h] - alpha * dEdb[o,h];# - alpha * 0.2 * rna$dynamic$dEdb[o,h]; #Atencao ao fator extra
                rna$dynamic$dEdb[o, h] = dEdb[o, h]; #Salva  ultima atualizacao para usar como fator extra
            }
        }

        #Atualiza dEda
        dEda = matrix(data=0, nrow = rna$info$qtHid, ncol = (rna$info$qtIn + rna$info$qtRec + 1));
        anew = matrix(nrow = rna$info$qtHid, ncol = (rna$info$qtIn + rna$info$qtRec + 1));
        if(is.null(rna$dynamic$dEda)) rna$dynamic$dEda = dEda; #Ultima atualizacao

        for(i in 1:(rna$info$qtIn + rna$info$qtRec + 1)){
            for(h in 1:rna$info$qtHid){
                #o = 1; #temporario
                dEda[h,i] = sum( e[pad, ] * rna$func2der(rna$dynamic$Yin[pad, ]) * rna$dynamic$B[ ,h]) * rna$func1der(rna$dynamic$Zin[pad,h]) * X[pad,i];
                anew[h,i] = A[h,i] - alpha * dEda[h,i];# - alpha * 0.2 * rna$dynamic$dEda[h,i]; #Atencao ao fator extra
                rna$dynamic$dEda[h, i] = dEda[h, i]; #Salva  ultima atualizacao para usar como fator extra
            }
        }

        #3 Atualiza A e B
        if(camada2){
            rna$dynamic$B0 = bnew[,1, drop=FALSE];
            rna$dynamic$B  = bnew[,2:dim(bnew)[2], drop=FALSE];
        }
        if(camada1){
            rna$dynamic$A0 = anew[,1, drop=FALSE];
            rna$dynamic$A  = anew[,2:(rna$info$qtIn+1), drop=FALSE];
            rna$dynamic$C  = anew[,(rna$info$qtIn+2):dim(anew)[2], drop=FALSE];
        }

        #Atualiza o erros para usar no próximo padrão.
        rna$dynamic = emf.rna.forward.padrao( rna, padrao = pad );
    }

    rna$dynamic$dYdb = dYdb;

    return ( rna$dynamic );
}
