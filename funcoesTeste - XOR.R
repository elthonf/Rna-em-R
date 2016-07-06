rm(list = ls())
#netIMG = emf.rna.read.csv.files( qtIn = 10, qtHid = 10, qtOut = 1, xfile = "dados/x.csv", yfile = "dados/y.csv", func2 = emf.rna.func.tunnel, func2der = emf.rna.func.tunnel.der )
netXOR = emf.rna.read.csv.files( qtIn = 2, qtHid = 2, qtOut = 1,
                                 xfile = "dados/XORx.csv", yfile = "dados/XORy.csv",
                                 func1 = emf.rna.func.tanh, func2 = emf.rna.func.tanh, func1der = emf.rna.func.tanh.der, func2der = emf.rna.func.tanh.der )


netXOR1 = emf.rna.forward( netXOR )

netXOR1batelada = netXOR1
for( i in 1:1000 ){
    netXOR1batelada = emf.rna.backward( rna = netXOR1batelada, alpha = 0.5);
    print(netXOR1batelada$ET)
}

netXOR1padrao = netXOR1
for( i in 1:1000 ){
    for( n in 1:netXOR1padrao$info$records){
        netXOR1padrao = emf.rna.backward2( rna = netXOR1padrao, padrao =n, alpha = 0.5);
    }
    netXOR1padrao = emf.rna.forward( netXOR1padrao )
    print(netXOR1padrao$ET)
}


padrao =1

netXOR1padrao = emf.rna.backward2( rna = netXOR1padrao, padrao =1, alpha = 0.5);
netXOR1padrao = emf.rna.backward2( rna = netXOR1padrao, padrao =2, alpha = 0.5);
