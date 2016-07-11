emf.rna.backward <- function (
    rna, alpha = 1,
    camada1 = TRUE,
    camada2 = TRUE
)
{
    #1 - Processa B (camada 1)
    e = rna$dynamic$e
    fd2 = rna$func2der(rna$dynamic$Yin)
    temp = e * fd2
    z = cbind(1, rna$dynamic$Z )

    dEdb = t(temp) %*% z * ( 1/ dim(z)[1]);
    bnew = cbind( rna$dynamic$B0, rna$dynamic$B ) - alpha * dEdb;


    #2 - Processa A
    x = cbind(1, rna$X)
    dEda2 = matrix(NA, nrow = dim(rna$dynamic$A)[1], ncol = dim(x)[2])
    temp2 = temp %*% rna$dynamic$B
    temp2 = temp2 * rna$func1der( rna$dynamic$Zin )
    dEda = t(temp2) %*% x * ( 1/ dim(z)[1]);
    anew = cbind( rna$dynamic$A0, rna$dynamic$A ) - alpha * dEda;



    #3 Atualiza A e B
    if(camada2){
        rna$dynamic$B0 = matrix( bnew[,1], nrow = nrow(bnew) );
        rna$dynamic$B  = matrix( bnew[,2:dim(bnew)[2]], nrow = nrow(bnew) );
    }
    if(camada1){
        rna$dynamic$A0 = matrix( anew[,1], nrow = nrow(anew) );
        rna$dynamic$A  = matrix( anew[,2:dim(anew)[2]], nrow = nrow(anew) );
    }


    ret = emf.rna.forward( rna );
    return ( ret );
}
