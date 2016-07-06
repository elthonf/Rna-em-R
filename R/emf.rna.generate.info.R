emf.rna.generate.info <- function( rede, ... ){

    info = list();

    info$records = dim(rede$X)[1]              #Quantidade de registros de treino
    info$qtIn = dim(rede$X)[2]                 #Quantidade de entradas (X)

    if(!is.null(rede$YD)){
        info$labels = unique(rede$YD)          #Quantidade de neuronios de saída. 1 para cada label.
        info$qtHid = length(info$labels)       #Quantidade na camada escondida (Z)
        info$qtOut = dim(rede$YD)[2]           #Quantidade de saídas (Y)
    }else{
        info$qtHid = 3
        info$qtOut = 1
        info$labels = 0
    }

    return ( info );
}
