celsius <-(0:8)*10
farheneit <-9/5*celsius+32
conversion <-data.frame(C=celsius, F=farheneit)
rm(celsius,farheneit)
plot(C~F,data=conversion, pch=16)


