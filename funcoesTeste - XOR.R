rm(list = ls())
#netIMG = emf.rna.read.csv.files( qtIn = 10, qtHid = 10, qtOut = 1, xfile = "dados/x.csv", yfile = "dados/y.csv", func2 = emf.rna.func.tunnel, func2der = emf.rna.func.tunnel.der )
netXOR = emf.rna.read.csv.files( qtIn = 2, qtHid = 2, qtOut = 1,
                                 xfile = "dados/XORx.csv", yfile = "dados/XORy.csv",
                                 func1 = emf.rna.func.tanh, func2 = emf.rna.func.tanh, func1der = emf.rna.func.tanh.der, func2der = emf.rna.func.tanh.der )


netXOR1 = emf.rna.forward( netXOR )

netXOR1b = netXOR1
for( i in 1:1000 ){
    for( n in 1:netXOR1b$info$records){
        netXOR1b = emf.rna.backward2( netXOR1b, alpha = 0.5);
        netXOR1b = emf.rna.forward( netXOR1b )
        print(netXOR1b$ET)
    }
}


