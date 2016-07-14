emf.rna.forward.padrao <- function (
    rna,
    padrao = NULL
)
{
    qtdeRegistros = dim(rna$X)[1];
    if(is.null(padrao)) padrao = 1 : qtdeRegistros; #Com recorrência, se não informar o padrão, refaz todos os padrões.

    #A e B são iguais para todos os padroes
    A = cbind(rna$dynamic$A0, rna$dynamic$A, rna$dynamic$C);
    B = cbind(rna$dynamic$B0, rna$dynamic$B);

    for(n in padrao){
        #1 - Processa Zin e Z
        X = c(1, rna$X[n,], rna$dynamic$R[n,]);

        rna$dynamic$Zin[n,] = X %*% t(A);
        rna$dynamic$Z[n,] = rna$func1(rna$dynamic$Zin[n,]);

        #2 - Processa Yin e Y
        Z = c(1, rna$dynamic$Z[n,]);
        rna$dynamic$Yin[n,] = Z %*% t(B);
        rna$dynamic$Y[n,] = rna$func1(rna$dynamic$Yin[n,]);

        #3 - Se tiver YD, calcula erro
        if(! is.null(rna$YD) ){
            rna$dynamic$e[n,] = rna$dynamic$Y[n,] - rna$YD[n,];
            rna$dynamic$E[n] = 0.5 * sum(rna$dynamic$e[n,] ^2) ;
        }

        #4 - Insere a saída na próxima camada, exceto última execucao
        if( n < qtdeRegistros ){
            R = c( rna$dynamic$Y[n,], rna$dynamic$R[n,]);
            rna$dynamic$R[n+1,] = R[1: (rna$info$qtRec * rna$info$qtOut)];
        }

    }

    #Ultima execucao, calcula erro médio quadrátivo
    if( n == qtdeRegistros )
        rna$dynamic$ET = ( 1.0 / length(rna$dynamic$E) ) * sum(rna$dynamic$E);

    return ( rna$dynamic );
}
