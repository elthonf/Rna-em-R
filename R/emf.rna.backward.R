emf.rna.backward <- function (
    rna, alpha = 1,
    camada1 = TRUE,
    camada2 = TRUE
)
{
    #1 - Processa B (camada 1)
    e = rna$e
    fd2 = rna$func2der(rna$Yin)
    temp = e * fd2
    z = cbind(1, rna$Z )

    dEdb = t(temp) %*% z * ( 1/ dim(z)[1]);
    bnew = cbind( rna$B0, rna$B ) - alpha * dEdb;

    #Para testar, método 1 a 1!
#     dEdb2 = t(temp) %*% z * ( 1/ dim(z)[1]);
#     for(k in 1:dim(rna$B)[1]){
#         for(i in 1:dim(z)[2]){
#             soma = 0;
#             for(n in 1:dim(z)[1]){
#                 soma = soma + e[n,k] * rna$func2der(rna$Yin[n,k]) * z[n,i]
#             }
#             dEdb2[k,i] =  ( 1/ dim(z)[1]) * soma;
#         }
#     }


    #2 - Processa A
    x = cbind(1, rna$X)
    dEda2 = matrix(NA, nrow = dim(rna$A)[1], ncol = dim(x)[2])
    temp2 = temp %*% rna$B
    temp2 = temp2 * rna$func1der( rna$Zin )
    dEda = t(temp2) %*% x * ( 1/ dim(z)[1]);
    anew = cbind( rna$A0, rna$A ) - alpha * dEda;



#     dEda2 = dEda;
#     for(i in 1:dim(rna$A)[1]){
#         for(j in 1:dim(x)[2]){
#             for(k in 1:dim(rna$B)[1]){
#
#             }
#             soma = 0;
#             for(n in 1:dim(x)[1]){
#                 soma = soma + e[n,k] * rna$func2der(rna$Yin[n,k]) * z[n,i]
#             }
#             dEdb2[k,i] =  ( 1/ dim(z)[1]) * soma;
#         }
#     }



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
