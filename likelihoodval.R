
set.seed(1)
ITER = 8
t = c(2000000, 3000000, 5000000, 1000000, 4000000,  1000000, 5000000, 3000000)
##f = (shape/scale)*{(t/scale)^(shape-1)}

#누설 2, 6
shape = 1.35
scale = 58065000
f = r = numeric(ITER)
for( i in seq_len(ITER) ) {
      p = t[i]
      r[i] = exp(-(p/scale)^shape)
      f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
a = r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*f[2]*f[6]
a

#최저작동압력 1, 3, 4, 7
shape = 4.25
scale = 26608000
r = numeric(ITER)
for( i in seq_len(ITER) ) {
  p = t[i]
  r[i] = exp(-(p/scale)^shape)
  f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
b = r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*f[1]*f[3]*f[4]*f[7]
b

#행정속도 5,8
shape = 3.97
scale = 33166000
r = numeric(ITER)
for( i in seq_len(ITER) ) {
  p = t[i]
  r[i] = exp(-(p/scale)^shape)
  f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
c = r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*f[5]*f[8]
c

EM.likelihood = log(a*b*c)


#=============================================================================
set.seed(1)
ITER = 8
t = c(2000000, 3000000, 5000000, 1000000, 4000000,  1000000, 5000000, 3000000)
scal = W$lam = W$alpha = numeric(ITER)

source("https://raw.githubusercontent.com/AppliedStat/R-code/master/2006b/Rpp5.R")

M = list(2,1,2,2,3,1,2,3) 
X = c(2000000, 3000000, 5000000, 1000000, 4000000, 1000000, 5000000, 3000000 )
W = weibull.cm.EM(X,M)  

#scal[i] = W$lam[i]^(-(1/W$alpha[i]))


set.seed(1)
ITER = 8
t = c(2000000, 3000000, 5000000, 1000000, 4000000,  1000000, 5000000, 3000000)
##f = (shape/scale)*{(t/scale)^(shape-1)}

#누설 2, 6

f = r = numeric(ITER)
for( i in seq_len(ITER) ) {
  scal[i] = W$lam[i]^(-(1/W$alpha[i]))
  shape = W$alpha[1]
  scale = scal[1]
  p = t[i]
  r[i] = exp(-(p/scale)^shape)
  f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
a = r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*f[2]*f[6]
a

#최저작동압력 1, 3, 4, 7

r = numeric(ITER)
for( i in seq_len(ITER) ) {
  scal[i] = W$lam[i]^(-(1/W$alpha[i]))
  shape = W$alpha[2]
  scale = scal[2]
  p = t[i]
  r[i] = exp(-(p/scale)^shape)
  f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
b = r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*f[1]*f[3]*f[4]*f[7]
b

#행정속도 5,8

r = numeric(ITER)
for( i in seq_len(ITER) ) {
  scal[i] = W$lam[i]^(-(1/W$alpha[i]))
  shape = W$alpha[3]
  scale = scal[3]
  p = t[i]
  r[i] = exp(-(p/scale)^shape)
  f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
c = r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*f[5]*f[8]
c

Reliasoft.likelihood = log(a*b*c)

#=============================================================================
cat("\n\n ====================================\n")
cat("ReliaSoft.likelihood\n")
print(Reliasoft.likelihood)

cat("\n\n ====================================\n")
cat("EM.likelihood\n")
print(EM.likelihood)
