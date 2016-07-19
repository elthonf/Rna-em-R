emf.rna.read.csv.files <- function(
    qtIn,
    qtRec = 0, #Quantidade de recorrências (Delays) a ser adicionada como entradas
    qtHid,
    qtOut,

    X = NULL, xfile = NULL,
    YD = NULL, yfile = NULL,
    A = NULL, afile = NULL, B = NULL, bfile = NULL,

    func1 = emf.rna.func.tanh, func1der = emf.rna.func.tanh.der,
    func2 = emf.rna.func.tanh, func2der = emf.rna.func.tanh.der,
    cenario = "Genérico"
)
{
    #Define a rede
    rede = list()
    rede$info = list();
    rede$cenario = cenario;
    rede$info$qtIn = qtIn                      #Quantidade de entradas (X)
    rede$info$qtRec = qtRec                    #Quantidade de recorrências EXTERNAS (Na entrada, X)
    rede$info$qtHid = qtHid                    #Quantidade na camada escondida (Z)
    rede$info$qtOut = qtOut                    #Quantidade de saídas (Y)

    #1 - Lê X e YD (y desejado)
    if(is.null(X) && is.null(xfile))
        stop("Informe o array X ou o arquivo equivalente em xfile.");

    if(!is.null(X)){ #Seta X direto
        if(!is.matrix(X))
            stop("X deve ser uma matrix.");
        rede$X = X;
    }else if(!is.null(xfile)){ #Le arquivo
        rede$X = read.csv(xfile, header = FALSE); #x = x[,200:210]
        rede$X = data.matrix(rede$X);
        dimnames(rede$X) <- NULL;
    }

    if(!is.null(YD)){ #Seta YD direto
        if(!is.matrix(YD))
            stop("YD deve ser uma matrix.");
        rede$YD = YD;
    }else if(!is.null(yfile)){ #Le arquivo
        rede$YD = read.csv(yfile, header = FALSE); #x = x[,200:210]
        rede$YD = data.matrix(rede$YD);
        dimnames(rede$YD) <- NULL;
    }

    #Define as funcoes
    rede$func1 = func1;
    rede$func1der = func1der;
    rede$func2 = func2;
    rede$func2der = func2der;

    rede$dynamic = list(); #Local onde ficam armazenados os dados dinâmicos da rede (Z, Y, Erros e pesos)

    #Seta pesos A (1a. camada) e B (2a. camada)
    if(!is.null(afile)){                 #Le arquivo de A
        A = read.csv(afile, header = FALSE); #x = x[,200:210]
        A = data.matrix(A);
        dimnames(A) <- NULL;
    }
    if(is.null(afile) && is.null(A)){   #Gera aleatório
        A = runif((rede$info$qtIn+1+rede$info$qtRec) * rede$info$qtHid, min = -1, max = 1)
        A = matrix( data = A, ncol = (rede$info$qtIn+1+rede$info$qtRec), nrow = rede$info$qtHid)
    }
    if(!is.null(A)){                     #Seta A , seja direto ou previamente gerado
        if(!is.matrix(A))
            stop("A deve ser uma matrix.");
        rede$dynamic$A0 = A[,1, drop=FALSE]; #Peso BIAS
        rede$dynamic$A = A[,2:(rede$info$qtIn+1), drop=FALSE];
        if(qtRec > 0)
            rede$dynamic$C = A[,(rede$info$qtIn+2):(rede$info$qtIn+1+rede$info$qtRec), drop=FALSE]; #Recorrencia!
    }

    #Seta pesos B (2a. camada)
    if(!is.null(bfile)){                 #Le arquivo de B
        B = read.csv(bfile, header = FALSE); #x = x[,200:210]
        B = data.matrix(B);
        dimnames(B) <- NULL;
    }
    if(is.null(bfile) && is.null(B)){   #Gera aleatório
        B = runif((rede$info$qtHid+1) * rede$info$qtOut, min = -1, max = 1)
        B = matrix( data = B, ncol = (rede$info$qtHid+1), nrow = rede$info$qtOut)
    }
    if(!is.null(B)){                     #Seta B , seja direto ou previamente gerado
        if(!is.matrix(B))
            stop("B deve ser uma matrix.");
        rede$dynamic$B0 = B[,1, drop=FALSE]; #Peso BIAS
        rede$dynamic$B = B[,2:dim(B)[2], drop=FALSE];
    }

    #Recorrencia: Gera camada R vazia (zeros).
    if(qtRec > 0){
        R = matrix( data = 0, ncol = rede$info$qtRec * rede$info$qtOut, nrow = dim(rede$X)[1]);
        rede$dynamic$R = R;
    }

    #Define memória para ativação e erros (Zin, Z, Yin, Y, e e E)
    rede$dynamic$Zin = matrix(data = NA, nrow = dim(rede$X)[1], ncol = qtHid);
    rede$dynamic$Z = matrix(data = NA, nrow = dim(rede$X)[1], ncol = qtHid);
    rede$dynamic$Yin = matrix(data = NA, nrow = dim(rede$X)[1], ncol = qtOut);
    rede$dynamic$Y = matrix(data = NA, nrow = dim(rede$X)[1], ncol = qtOut);
    rede$dynamic$e = matrix(data = NA, nrow = dim(rede$X)[1], ncol = qtOut);
    rede$dynamic$E = rep(x = NA, dim(rede$X)[1]);

    return (rede);

}
