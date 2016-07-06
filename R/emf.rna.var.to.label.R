emf.rna.var.to.label <- function(x){

    if(length(x) < 1 ) return (x);
    ret = x;

    for(i in 1:length(x)){
        if(x[i] <= tanh(-0.2/5) ){
            ret[i] = "Baixa";
        }else if(x[i] >= tanh(0.2/5) ){
            ret[i] = "Alta";
        }else{
            ret[i] = "Estável";
        }
    }

    return ( ret );
}
