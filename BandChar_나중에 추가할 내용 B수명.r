

TINY = .Machine$double.neg.eps
BIG = .Machine$double.xmax^0.25
#==============================================================================
EQ1 = function(x, prob, shape, scale) { 
    1 - pweibull(x, shape=shape, scale=scale,lower.tail=F) - prob
}

prob= 1 - exp(-1)
shape = 3
scale = runif(1)*100

# Char. Value
scale 
uniroot(EQ1, interval=c(TINY, BIG), prob=prob, shape=shape, scale=scale)
qweibull(prob, shape, scale)

# B10
prob = 0.1
uniroot(EQ1, interval=c(TINY, BIG), prob=prob, shape=shape, scale=scale)
1 - (1-qweibull(prob, shape, scale))

#==============================================================================
EQ2 = function(x, prob, shape1, scale1, shape2, scale2) { 
   1-pweibull(x,shape=shape1, scale=scale1,lower.tail=F)*pweibull(x,shape=shape2,scale=scale2,lower.tail=F) - prob
}
#----------------------------
prob= 1 - exp(-1)
## prob= 0.1 
shape1=0.6710; scale1=449442; 
shape2=4.3373; scale2=340379; 
uniroot(EQ2, interval=c(TINY, BIG), prob=prob, shape1=shape1, scale1=scale1, shape2=shape2, scale2=scale2)
#----------------------------
prob= 1 - exp(-1)
## prob= 0.1 
shape1=0.67; scale1=449430; 
shape2=4.49; scale2=340380; 
uniroot(EQ2, interval=c(TINY, BIG), prob=prob, shape1=shape1, scale1=scale1, shape2=shape2, scale2=scale2)

#==============================================================================
EQ3 = function(x, prob, shape1, scale1, shape2, scale2, shape3, scale3) { 
   1-pweibull(x,shape=shape1,scale=scale1,lower.tail=F) * 
     pweibull(x,shape=shape2,scale=scale2,lower.tail=F) * 
     pweibull(x,shape=shape3,scale=scale3,lower.tail=F) - prob
}

#----------------------------
prob= 1 - exp(-1)
## prob= 0.1
shape1=1.35; scale1=5806500
shape2=4.25; scale2=2660800
shape3=3.97; scale3=3316600
uniroot(EQ3, interval=c(TINY, BIG), prob=prob, 
             shape1=shape1, scale1=scale1, shape2=shape2, scale2=scale2, shape3=shape3, scale3=scale3)

#----------------------------
prob= 1 - exp(-1)
## prob= 0.1
shape1=1.33; scale1=8850255
shape2=2.27; scale2=4657881
shape3=3.77; scale3=5444598
uniroot(EQ3, interval=c(TINY, BIG), prob=prob, 
             shape1=shape1, scale1=scale1, shape2=shape2, scale2=scale2, shape3=shape3, scale3=scale3)



