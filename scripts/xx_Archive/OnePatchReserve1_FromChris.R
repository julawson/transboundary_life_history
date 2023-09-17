rm(list = ls(all = TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)   # Data visualization
library(tidyr)   # Tidy data management
library(dplyr)
library(cowplot)
library(akima)


#This solves patch A's optimal dynamic problem when patch B is a permanent reserve

sizexA = 12 #size of the state grid A
sizexB = 10 #size of B

T=20 #time horizon for backward induction
small = .1

#adult survival
muA = .9
muB = .9

#adult movement
MAA = .95
MAB = 1-MAA
MBB = .7
MBA = 1-MBB

#larval survival
sigA = .8
sigB = .85

#larval movement
DAA = .7
DAB = 1-DAA
DBB = .2
DBA = 1-DBB

#larval production
rA = .8
KA = 100
rB = .8
KB = 100

#prices
bA = 1
delta = .95

xAgrid = seq(small,KA,length.out=sizexA)
xBgrid = seq(small,KB,length.out=sizexB)
  
  
fA = function(eA)
{
  growA = rA*eA*(1-eA/KA)
  return(growA)
}

fB = function(eB)
{
  growB = rB*eB*(1-eB/KB)
  return(growB)
}

nextxA = function(eA,eB)
{
  xAnext = muA*MAA*eA + muB*MBA*eB + sigA*(DAA*fA(eA) + DBA*fB(eB))
  return(xAnext)
}

nextxB = function(eA,eB)
{
  xBnext = muB*MBB*eB + muA*MAB*eA + sigB*(DBB*fB(eB) + DAB*fA(eA))
  return(xBnext)
}

piA = function(xA,eA)
{
  profitA = bA*(xA-eA)
  return(profitA)
}

#xA,xB,eA are scalars.  V is a matrix corresponding to the values in xA and xB
Payoff = function(eA,xA,xB,V)
{
  xAnext = nextxA(eA=eA,eB=xB) #eB=xB because B is a reserve
  xBnext = nextxB(eA=eA,eB=xB) #eB=xB because B is a reserve
  Vnext =  bilinear(xAgrid, xBgrid, V, xAnext, xBnext)
  negout = -(piA(xA=xA,eA=eA) + delta*Vnext$z) 
  return(negout)
}

DFall = data.frame()
Vnext = matrix(0,sizexA,sizexB)
V = matrix(0,sizexA,sizexB)

#Try payoff function
try = Payoff(0,30,40,V)

for(t in T:1)
{
  print(t)
  for(i in 1:sizexA)
  {
    xA = xAgrid[i]
    for(j in 1:sizexB)
    {
      xB = xBgrid[j]
      guess = xA/2
      low = 0 #lower bound on escapement in A
      high = xA #upper bound on escapement in A
      Thing = optim(par=guess,fn=Payoff,lower=low,upper=high,xA=xA,xB=xB,V=V,method='L-BFGS-B')
      eAstar = Thing$par
      Vstar = -Thing$value
      Vnext[i,j] = Vstar
      DFnow = data.frame(time=t,xA=xA,xB=xB,eAstar=eAstar,Vstar=Vstar)
      DFall = bind_rows(DFall,DFnow)
    }
  }
  V = Vnext
}

DFinf = DFall %>% filter(time==1)

PeAxA = ggplot(data=DFinf) +
  geom_line(aes(x=xA,y=eAstar,color=factor(xB)),size=1.3) +
  xlab("Stock, xA") +
  ylab("Escapeemnt, eA") +
  theme_bw() 
PeAxA

PeAxB = ggplot(data=DFinf) +
  geom_line(aes(x=xB,y=eAstar,color=factor(xA)),size=1.3) +
  xlab("Stock, xB") +
  ylab("Escapeemnt, eA") +
  theme_bw() 
PeAxB

PV =ggplot(data=DFinf) +
  geom_line(aes(x=xA,y=Vstar,color=factor(xB)),size=1.3) +
  xlab("Stock, x") +
  ylab("Value Fun, V") +
  theme_bw() 
PV



