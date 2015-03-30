# simplest possible markov example
# 
# build a matrix
# loading MASS because it containes the function "fractions"
# that converts decimals into rational numbers

library(MASS)
P<-c(.6,.4,.3,.7)
P<-matrix(P,ncol=2)
P

# Px=x  -- steady state
# (P-I)x = 0  homogeneous equation
# calculate eigen values for P-I

eig<-eigen(P)
eig

vec<-eig$vectors[,1]
vec
P%*%vec

steady<-vec/sum(vec)
steady

fractions(steady)

