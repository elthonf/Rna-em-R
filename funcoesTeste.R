rm(list = ls())
#netIMG = emf.rna.read.csv.files( qtIn = 10, qtHid = 10, qtOut = 1, xfile = "dados/x.csv", yfile = "dados/y.csv", func2 = emf.rna.func.tunnel, func2der = emf.rna.func.tunnel.der )
netXOR = emf.rna.read.csv.files( qtIn = 2, qtHid = 2, qtOut = 1,
                                 xfile = "dados/XORx.csv", yfile = "dados/XORy.csv",
                                 func1 = emf.rna.func.tanh, func2 = emf.rna.func.tanh, func1der = emf.rna.func.tanh.der, func2der = emf.rna.func.tanh.der )

netXOR$A = matrix( c(1,-1,-1,1), ncol = 2, nrow = 2)#Certo
netXOR$B = matrix( c(1,1), ncol = 2, nrow = 1)#Certo

netXOR$A = matrix( c(1,1,1,1), ncol = 2, nrow = 2)#Errado
netXOR$B = matrix( c(1,1), ncol = 2, nrow = 1)
netXOR$A0 = matrix( c(1,1), ncol = 1, nrow = 2)
netXOR$B0 = matrix( c(1), ncol = 1, nrow = 1)




netXOR$A = matrix( c(0.1,0.1,0.1,0.1), ncol = 2, nrow = 2)#Errado
netXOR$B = matrix( c(0.1,0.1), ncol = 2, nrow = 1)
netXOR$A0 = matrix( c(0.1,0.1), ncol = 1, nrow = 2)
netXOR$B0 = matrix( c(0.1), ncol = 1, nrow = 1)



############# ############# ############# ############# #############

#emf.rna.image.plot(netIMG$X[3001,])
#emf.rna.image.plot(netIMG$X[2001:2002,])



#XOR
############# ############# ############# ############# #############


netXOR1 = emf.rna.forward( netXOR )
netXOR1b = emf.rna.backward( netXOR1, alpha = 0.01)

netXOR1b = netXOR1
for( i in 1:1000 ){
    for( n in 1:netXOR1b$info$records){
        netXOR1b = emf.rna.backward( netXOR1b, alpha = 0.5);
        netXOR1b = emf.rna.forward(netXOR1b);
        print(netXOR1b$ET)
    }
}


netXOR1b = netXOR1;



for(i in 1:1000){
netXOR1b = emf.rna.backward( netXOR1b, alpha = 0.01)
}


for(i in 1:300){
    netXOR1b = emf.rna.backward.padrao( netXOR1b, alpha = 0.1, registro =  sample( netXOR1b$info$records, 1) )
    netXOR1b = emf.rna.forward( netXOR1b )
}
netXOR1b$Y


netXOR2 = emf.rna.forward( netXOR1b )
netXOR2b = emf.rna.backward( netXOR2, alpha = 0.1 )

netXOR3 = emf.rna.forward( netXOR2b )
netXOR3b = emf.rna.backward( netXOR3, alpha = 0.1  )

for(i in 1:30){
    netXOR3 = emf.rna.forward( netXOR3b )
    netXOR3b = emf.rna.backward( netXOR3, alpha = 0.1 )
    print( netXOR3$e )
}


############# ############# ############# ############# #############



#IMAGEM
############# ############# ############# ############# #############
netIMG1 = emf.rna.forward( netIMG )
netIMG1b = emf.rna.backward( netIMG1 )

netIMG2 = emf.rna.forward( netIMG1b )
netIMG2b = emf.rna.backward( netIMG2 )


netIMG3 = emf.rna.forward( netIMG2b )
netIMG3b = emf.rna.backward( netIMG3 )

for(i in 1:30){
netIMG3 = emf.rna.forward( netIMG3b )
netIMG3b = emf.rna.backward( netIMG3, alpha = 100 )
print( netIMG3$ET )
}
rm(i)

View( cbind(netIMG3$YD, netIMG3$Y))
############# ############# ############# ############# #############
