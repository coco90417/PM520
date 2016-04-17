# f(x) = -(x+4)^2

# a function we will work with
F1<-function(x){
    return(-(x+4)^2)
}

GoldenSection<-function(F, epsi){
    # initiate
    p = (1+sqrt(5))/2
    # get start points
    Xl = 0
    Xm = 0
    Xr = 0
    while((Xm - Xl <= epsi) || (F(Xl) > F(Xm)) || (F(Xm) < F(Xr))){
        ThreeValues = sort(rnorm(3,mean=0,sd=100))
        Xl = ThreeValues[1]
        Xm = ThreeValues[2]
        Xr = ThreeValues[3]
    }
    while((Xr - Xl) > epsi){
        if(Xr - Xm > Xm - Xl){
            Y = Xm + (Xr-Xm)/(1+p)
            if(F(Y)>=F(Xm)){
                Xl=Xm
                Xm = Y
            }else{
                Xr = Y
            }
        }else{
            Y = Xm - (Xm-Xl)/(1+p)
            if(F(Y)>=F(Xm)){
                Xr = Xm
                Xm = Y
            }else{
                Xl = Y
            }
        }
    }
    return((Xr+Xl)/2)
}


