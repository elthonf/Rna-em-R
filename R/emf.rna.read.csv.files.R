emf.rna.read.csv.files <- function(
    X, xfile,
    YD, yfile,
    A, afile, B, bfile,

    func1 = emf.rna.func.sigmoid, func1der = emf.rna.func.sigmoid.der,
    func2 = emf.rna.func.sigmoid, func2der = emf.rna.func.sigmoid.der
)
{
    #Define a rede
    rede = list()

    #1 - Lê X e YD (y desejado)
    rede$X = read.csv(xfile, header = FALSE); #x = x[,200:210]
    rede$X = data.matrix(rede$X);
    dimnames(rede$X) <- NULL;
    rede$YD = read.csv(yfile, header = FALSE);
    rede$YD = data.matrix(rede$YD);
    dimnames(rede$YD) <- NULL;
    #rede$YD = as.vector(rede$YD);

    #Insere o BIAS no X
    #rede$X = cbind(rep(1, dim(rede$X)[1]), rede$X);


    ################

    rede$info = emf.rna.generate.info(rede);


    #Define pesos iniciais aleatórios (3 camadas)
    rede$A = runif(rede$info$qtIn * rede$info$qtHid, min = -1, max = 1)
    rede$A = matrix( data = rede$A, ncol = rede$info$qtIn, nrow = rede$info$qtHid)
    rede$A0 = matrix(data = 0, ncol = 1, nrow = rede$info$qtHid) #Peso BIAS

    #Define pesos iniciais aleatórios (3 camadas)
    rede$B = runif(rede$info$qtHid * rede$info$qtOut, min = -1, max = 1)
    rede$B = matrix( data = rede$B, ncol = rede$info$qtHid, nrow = rede$info$qtOut)
    rede$B0 = matrix(data = 0, ncol = 1, nrow = rede$info$qtOut) #Peso BIAS

    #Define as funcoes
    rede$func1 = func1;
    rede$func1der = func1der;
    rede$func2 = func2;
    rede$func2der = func2der;

    return (rede);

}
