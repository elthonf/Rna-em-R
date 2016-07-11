rm(list = ls())
#netIMG = emf.rna.read.csv.files( qtIn = 10, qtHid = 10, qtOut = 1, xfile = "dados/x.csv", yfile = "dados/y.csv", func2 = emf.rna.func.tunnel, func2der = emf.rna.func.tunnel.der )
netXOR = emf.rna.read.csv.files( qtIn = 2, qtHid = 2, qtOut = 1,
                                 xfile = "dados/XORx.csv", yfile = "dados/XORy.csv",
                                 afile = "dados/XORa.csv", bfile = "dados/XORb.csv",
                                 func1 = emf.rna.func.tanh, func2 = emf.rna.func.tanh, func1der = emf.rna.func.tanh.der, func2der = emf.rna.func.tanh.der )

#Executa o primeiro Forward
netXOR = emf.rna.forward( netXOR )

#Prepara objeto para excucao em batelada, e executa.
netXOR1batelada = netXOR
for( i in 1:1000 ){
    netXOR1batelada = emf.rna.backward( rna = netXOR1batelada, alpha = runif(1, min=0.1, max = 0.9));
    print(netXOR1batelada$ET)
}
netXOR1batelada$Y

#Prepara objeto para execucao padrão a padrão, e executa. (não está convergindo)
netXOR1padrao = netXOR
qtde = dim(netXOR1padrao$X)[1];
for( i in 1:10000 ){
    for( n in 1: qtde){
        netXOR1padrao = emf.rna.backward2( rna = netXOR1padrao, padrao =n, alpha = runif(1, min=0.1, max = 0.9));
        netXOR1padrao = emf.rna.forward( netXOR1padrao )
    }
    #netXOR1padrao = emf.rna.forward( netXOR1padrao )
    if(i %% 100 == 0) cat(i, netXOR1padrao$ET, "\n")
}
rm(i, n)
netXOR1padrao$Y
#netXOR1padrao$YD
#netXOR1padrao$e






