emf.rna.func.perceptron.der <- function(x){

    for(i in 1:length(x)){
        x[i] = 1.0;
    }

    return ( x );

}
