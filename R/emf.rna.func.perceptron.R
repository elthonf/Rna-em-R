emf.rna.func.perceptron <- function (z){

    if(length(z) < 1 ) return (z);

    for(i in 1:length(z)){
        if(z[i] <= 0.0 ){;
            z[i] = 0.0;
        }else{
            z[i] = 1.0;
        }
    }

    return ( z );

}
