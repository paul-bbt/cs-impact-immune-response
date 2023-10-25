rm(list=ls())

###Parametre du système ---
#{n : non-resistant ; r : resistant}
#{0 : without imatinib treatment ; 1 : with imatinib treatment}

lambda <- 0.75 # parametre d'ajustement
dS <- 0.003 * lambda #death rate Ls
dP <- 0.008 * lambda #death rate Lp
dD <- 0.05 * lambda #death rate Ld
dTe <- lambda #death rate Lte

rn <- 0.008 #growth rate for non-resistant cells (to imatinib)
rr <- 0.0023 #growth rate for resistant cells

an0 <- 1.6 #Transfer rate. 
an1 <- an0 / 100
ar0 <- an0
ar1 <- ar0
ar <- an0


bn0 <- 10 
bn1 <- bn0 / 750
br0 <- bn0
br1 <- br0
br <- bn0

cn0 <- 100 
cn1 <- cn0 
cr0 <- cn0
cr1 <- cr0
cr <- cn0

u <- 4 * 10^(-8) #mutation rate from non-resistant to resistant
ur <- 0 #mutation rate from resistant to non-resistant

p0 <- 0.8 #Prob T engage a cell cancer
qC <- 0.75 #Prob cancer cell dies from encounter
qT <- 0.5 #Pro T survive an encounter

k <- 1 #kinetic coefficient
tau <- 1 #Duration of one T cell division (day)
