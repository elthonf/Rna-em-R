emf.rna.forward <- function (
    rna
)
{
    #1 - Processa Zin e Z
    X = cbind(1, rna$X);
    A = cbind(rna$dynamic$A0, rna$dynamic$A);

    if(rna$info$qtRec > 0){
        stop("Para problemas com recorrência, utilize o método padrão a padrão: emf.rna.forward.padrao");
    }

    rna$dynamic$Zin = X %*% t(A);
    rna$dynamic$Z = rna$func1(rna$dynamic$Zin);

    #2 - Processa Yin e Y
    Z = cbind(1, rna$dynamic$Z);
    B = cbind(rna$dynamic$B0, rna$dynamic$B);
    rna$dynamic$Yin = Z %*% t(B);
    rna$dynamic$Y = rna$func1(rna$dynamic$Yin);

    #3 - Se tiver YD, calcula erro
    if(! is.null(rna$YD) ){
        rna$dynamic$e = rna$dynamic$Y - rna$YD;
        rna$dynamic$E = 0.5 * rowSums(rna$dynamic$e ^2) ;
        rna$dynamic$ET = ( 1.0 / length(rna$dynamic$E) ) * sum(rna$dynamic$E);
    }

    return ( rna$dynamic );
}
