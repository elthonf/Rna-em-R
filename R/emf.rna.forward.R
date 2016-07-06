emf.rna.forward <- function (
    rna
)
{
    #1 - Processa Zin e Z
    X = cbind(1, rna$X);
    A = cbind(rna$A0, rna$A);
    rna$Zin = X %*% t(A);
    rna$Z = rna$func1(rna$Zin);

    #2 - Processa Yin e Y
    Z = cbind(1, rna$Z);
    B = cbind(rna$B0, rna$B);
    rna$Yin = Z %*% t(B);
    rna$Y = rna$func1(rna$Yin);

    #3 - Se tiver YD, calcula erro
    if(! is.null(rna$YD) ){
        rna$e = rna$Y - rna$YD;
        rna$E = 0.5 * rowSums(rna$e ^2) ;
        rna$ET = ( 1.0 / length(rna$E) ) * sum(rna$E);
    }

    return ( rna );
}
