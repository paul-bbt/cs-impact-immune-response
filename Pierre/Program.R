rm(list=ls()) # reset des variables

library(deSolve)

#Parametre initiaux
current_directory <- getwd()
print(getwd())
source("Pierre/parametre.R")

#Traitement imitinib
td <- 0
te <- 10000000

#Donnée du patient - 7.6 * 10^(-6)
C <- 7
Lsn0 <- 2.4 * 10^(-6)
Lsr0 <- 0
sT <- 9 * 10^(-7)
dTc<- 0.0022
n <- 2.2

#Parametre d'évolution & initialisation

an = function(t){ifelse(t>td | t<te, an1, an0)}
bn = function(t){ifelse(t>td | t<te, bn1, bn0)}
cn = function(t){ifelse(t>td | t<te, cn1, cn0)}

Lpn0 = an0*Lsn0/dP
Ldn0 = bn0*Lpn0/dD
Lten0 = cn0*Ldn0/dTe
Lpr0 = ar*Lsr0/dP
Ldr0 = br*Lpr0/dD
Lter0 = cr*Ldr0/dTe
T0 = sT/dTc
qC * (p0 * exp(-C*(Lsn0+Lpn0+Ldn0+Lten0+Lsr0+Lpr0+Ldr0+Lter0))*k*T0)

#Programme de Resolution
equations <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    #Variable à temps décalé
    Lsn_ = ifelse(t < n*tau, Lsn0, lagvalue(t- n*tau, 1))
    Lpn_ = ifelse(t < n*tau, Lpn0, lagvalue(t- n*tau, 2))
    Ldn_ = ifelse(t < n*tau, Ldn0, lagvalue(t- n*tau, 3))
    Lten_ = ifelse(t < n*tau, Lten0, lagvalue(t- n*tau, 4))
    Lsr_ = ifelse(t < n*tau, Lsr0, lagvalue(t- n*tau, 5))
    Lpr_ = ifelse(t < n*tau, Lpr0, lagvalue(t- n*tau, 6))
    Ldr_ = ifelse(t < n*tau, Ldr0, lagvalue(t- n*tau, 7))
    Lter_ = ifelse(t < n*tau, Lter0, lagvalue(t- n*tau, 8))
    T_ = ifelse(t < n*tau, T0, lagvalue(t- n*tau, 9))
    
    #Les équations
    dLsn = (rn * (1 - u) - dS) * Lsn - qC * (p0 * exp(-C*(Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter))*k*T) * Lsn
    dLpn = an(t) * Lsn - dP * Lpn - qC * (p0 * exp(-C*(Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter))*k*T) * Lpn
    dLdn = bn(t) * Lpn - dD * Ldn - qC * (p0 * exp(-C*(Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter))*k*T) * Ldn
    dLten = cn(t) * Ldn - dTe * Lten - qC * (p0 * exp(-C*(Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter))*k*T) * Lten
    
    dLsr = (rr - dS) * Lsr + rn * u * Lsn - qC * (p0 * exp(-C*(Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter))*k*T) * Lsr
    dLpr = ar * Lsr - dP * Lpr - qC * (p0 * exp(-C*(Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter))*k*T) * Lpr
    dLdr = br * Lpr - dD * Ldr - qC * (p0 * exp(-C*(Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter))*k*T) * Ldr
    dLter = cr * Ldr - dTe * Lter - qC * (p0 * exp(-C*(Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter))*k*T) * Lter
    
    dT = sT - dTc * T - (p0 * exp(-C*(Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter))*k*T) * (Lsn+Lpn+Ldn+Lten+Lsr+Lpr+Ldr+Lter) + 2^n * qT * (p0 * exp(-C*(Lsn_+Lpn_+Ldn_+Lten_+Lsr_+Lpr_+Ldr_+Lter_))*k*T_) * (Lsn_+Lpn_+Ldn_+Lten_+Lsr_+Lpr_+Ldr_+Lter_)
    
    #Retour des variables
    list(c(dLsn, dLpn, dLdn, dLten, dLsr, dLpr, dLdr, dLter, dT))
  })
}


initiaux <- c(Lsn = Lsn0, Lpn = Lpn0, Ldn = Ldn0, Lten = Lten0, Lsr = Lsr0, Lpr = Lpr0, Ldr = Ldr0, Lter = Lter0, T = T0)
times <- seq(0, 1500, by = 0.1)
res <- dede(y = initiaux, times = times, func = equations, parms = NULL)

#Graphique
c = res[, 2] + res[, 3] + res[, 4] + res[, 5] + res[, 6] + res[, 7] + res[, 8] +  res[, 9]
plot(res[, 10], type = "l", xlab = "Temps [m]", ylab = "T cells [mol.L-1]", col = "purple")

#Patient B
points(c(0, 6, 9, 18, 24, 32, 34, 42)*300, c(1, 16.5, 33, 30, 26, 11, 15, 12)/2500)
plot(res)

c[c > 0.1] <- 0
plot(c)

plot(res[, 10])
