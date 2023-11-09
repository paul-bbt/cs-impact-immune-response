# script pour le TD-TP sur analyse d'incertitudes et de sensibilité
# MODV
rm(list=ls())

getwd()

setwd(getwd())
tab = read.table("cs/ymax.txt")
ym = tab$ymax

# approche par histogrammmes : 
# changer le nombre de classes (4 ou 100) via argument breaks
hist(ym, breaks=4)  # affiche les effectifs dans chaque classe : pas normalis?
hist(ym, breaks=3, freq=FALSE) # freq=FALSe fait la normalisation de sorte que sum(aire_rectangle_i)=1
# sur-lissage
hist(ym, breaks=99, freq=FALSE) # sous-lissage (=pas assez lissé)
hist(ym, freq=F) # densité avec nombre de classes libre, optimisé par la fonction elle-même (compromis biais-variance)

# approche par méthode des noyaux :
?density
lines(density(ym), col="orange", lwd=3)

# approche paramétrique sous hypothèse loi normale :
m_estim = mean(ym)
sd_estim = sd(ym)
shapiro.test(ym) # on ne rejette pas H0

curve(dnorm(x, mean=m_estim, sd= sd_estim), add=TRUE, col="red", lwd=3)

# on privilégierait plutôt histogramme ou méthode à noyaux 
# s'il s'agit seulement de propager l'incertitude de manière numérique
# Et plutôt la loi normale (approche paramétrique) s'il s'agit 
# de faire des calculs analytiques. Ici on aurait d'autant plus tendance à
# prendre la loi normale qu'elle est plausible biologiquement : on ne voit
# pas de raisons biologiques aux petites fluctuations locales de la densité.

# Analyse de sensibilité :
library(sensitivity)
simulation <- function(param){
  out <-NULL
  for (i in 1:nrow(param)){
    out<-c(out, param$y0[i]+10*param$w[i]*param$a[i])
  }
  return(out)
}

Nrep=10000 # il faut Nrep plutôt de l'ordre 10^4~10^5 pour avoir des intervalles d'incertitude potables.
# Matrice d'échantillonage :
X1<-data.frame(y0=runif(Nrep, 0, 1), a=rnorm(Nrep, 0.025, 0.005), w=rnorm(Nrep, 40, 5))
# Matrice de ré-échantillonage :
X2<-data.frame(y0=runif(Nrep, 0, 1), a=rnorm(Nrep, 0.025, 0.005), w=rnorm(Nrep, 40, 5))
# indices d'ordre 1 et total
ressob<-sobol2002(simulation, X1, X2, nboot=10)
plot(ressob)
summary(ressob)
print(ressob)

# First order indices:
#   original          bias  std. error  min. c.i.  max. c.i.
# y0 0.02020646  0.0008841061 0.002269576 0.01533524 0.02273803
# a  0.69018617  0.0049873549 0.017352750 0.64972188 0.70525997
# w  0.24799618 -0.0013219957 0.010707694 0.23749437 0.26793183
# 
# Total indices:
#   original          bias  std. error   min. c.i.  max. c.i.
# y0 0.01079343 -0.0008979044 0.002085107 0.009071749 0.01588422
# a  0.71445016 -0.0079547750 0.019967743 0.699990450 0.76031602
# w  0.31225005 -0.0003426194 0.013167825 0.287380956 0.32694267

# théoriques : 
# S_y0 (y10) =1/12/5.71144
# [1] 0.01459

# S_a (y10) =400^2*2.5*10^(-5)/5.71144
# [1] 0.7003488

# S_a (y10) =0.25^2*25/5.71144
# [1] 0.2735737
