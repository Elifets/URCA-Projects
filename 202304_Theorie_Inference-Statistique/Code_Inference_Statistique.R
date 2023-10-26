# Entete ----

# Fichier : TP1_MA0814.R
# Desc : TP Statistique Inferentielle - M1 SEP - 2022-2023
# Date : 06/03/2023
# Auteur : Elif Ertas (elif.ertas@etudiant.univ-reims.fr)

#sink("resultat.txt")

#1) Tirages d'echantillons ----
#Variables discretes -----

##Partie A : Loi de Bernouilli -----

N = 1000 #nb de nombre au hasard a generer 
print(N)
n = 20 #parametre de la loi B(n,p)
print(n)
p = 0.3 #parametre de la loi B(n,p)
print(p)

Bin = rbinom(N,n,p) #genere des realisations aleatoires
print(Bin)
fa = table(Bin) #calcul des frequences absolus
print(fa)
fr = prop.table(fa) #calcul des frequences relatives 
print(fr)
res = summary(Bin)
print(res)

par(mfrow = c(2,2))
#graphique de la loi generee 
plot(fr, main = "Variable B(n,p)", col = "red")
#graphique de la loi theorique 
x = seq(min(Bin),max(Bin),by=1) #abscisses 
yBin = dbinom(x,n,p) #ordonnees 
plot(x, yBin, main = "Graphe de la loi theorique B(n,p)", col = "blue", type = "h")
#superposer les deux graphes 
plot(fr, main = "Graphes de la loi B(n,p)", col = "red")
lines(x, yBin, col = "blue", type = "h")
legend("topleft", c("graphe obs","graphe th"), col = c("red","blue"), lty=1:1, cex = 0.3)
#graphe de la fct de repartition empirique 
plot(ecdf(Bin), main = "Fonction de repartition empirique", col = "red")
y = pbinom(x, n,p) #ordonnee pour la fct de rep
lines(x, y, col = "blue", type = "p")
legend("bottomright", c("graphe obs","graphe th"), col = c("red","blue"), lty=c(1,0), cex = 0.5, pch = c(19,1))

##Partie B : Loi de Poisson -----

N = 1000 #nb de nombre au hasard a generer 
print(N)
lambda = 20 #parametre de la loi P(lambda)
print(lambda)

Poiss = rpois(N,lambda) #genere des realisations aleatoires
print(Poiss)
fa = table(Poiss) #calcul des frequences absolus
print(fa)
fr = prop.table(fa) #calcul des frequences relatives 
print(fr)
res = summary(Poiss)
print(res)

par(mfrow = c(2,2))
#graphique de la loi generee 
plot(fr, main = "Variable Poisson P(lambda)", col = "red")
#graphique de la loi theorique 
x = seq(min(Poiss),max(Poiss),by=1) #abscisses 
yPoiss = dpois(x,lambda) #ordonnees 
plot(x,yPoiss, main = "Graphe de la loi theorique P(lambda)", col = "blue", type = "h")
#superposer les deux graphes 
plot(fr, main = "Graphes de la loi de Poisson P(lambda)", col = "red")
lines(x, yPoiss, col = "blue", type = "h")
legend("topleft", c("graphe obs","graphe th"), col = c("red","blue"), lty=1:1, cex = 0.5)
#graphe de la fct de repartition empirique 
plot(ecdf(Poiss), main = "Fonction de repartition empirique", col = "red")
y = ppois(x,lambda) #ordonnee pour la fct de rep
lines(x, y, col = "blue", type = "p")
legend("bottomright", c("graphe obs","graphe th"), col = c("red","blue"), lty=c(1,0), cex = 0.5, pch = c(19,1))

#Variables continues -----
##Partie C : Loi Exponentielle ----- 

N = 10000
lambda = 2.5  
Exp=rexp(N,lambda)
summary(Exp)
par(mfrow = c(1,2))
hist(Exp,freq=FALSE,main="Histogramme de E(lambda)",col="yellow",nclass=20)
#Courbe de la densite de la exponentielle 
x=seq(0,lambda+10,by=0.1)
yExp=dexp(x,lambda)
lines(x,yExp,type="l",col="red",lwd=3)
legend("topright", paste(c("N = ","Mean = ", "Var = "), round(c(N,mean(Exp),var(Exp)),2), sep =""), text.col = c("blue","red","green"), cex = 0.5)
#courbe de la fonction de r?partition empirique
plot(ecdf(Exp),main="Fonction de repartition empirique",col="blue",lwd=3)
#courbe de la fonction de r?partition th?orique
FrtExp=pexp(x,lambda)
lines(x,FrtExp,main="Fonction de repartition theorique",type="l",col="green",lwd=3)
legend("bottomright", c("graphe obs","graphe th"), col = c("blue","green"), lty=c(1,1), cex = 0.5, pch = c(19,19))

##Partie D : Loi Normale -----

#generer des nbs au hasard issu de la loi N(mu,sigma)
N = 10000
mu = 2
sigma = 1.2
Nor = rnorm(N,mu,sigma)
resnorm = summary(Nor)
print(resnorm)
#Histogramme de la loi normale
par(mfrow = c(1,2))
hist(Nor, freq = FALSE, main = "Histogramme de la loi Normale", col.main = "purple", nclass = 70, col=rainbow(10))
#Courbe de la densite de la loi Normale N(mu,sigma)
MNor = mean(Nor) #moyenne de Nor
VNor = var(Nor) #variance de Nor 
x = seq(MNor - 4*sd(Nor), MNor + 4*sd(Nor), by=0.1) # sd (standart deviation) = ecart type 
dNor = dnorm(x, MNor, sd(Nor)) #densite de la loi Normale pour tout les points x 
lines(x, dNor, col = "red", type = "l", lwd = 3)
legend("topleft", paste(c("N = ","Mean = ", "Var = "), round(c(N,mean(Nor),var(Nor)),2), sep =""), text.col = c("blue","red","green"), cex = 0.5)
#Fonction de repartition empirique F_n(x) = 1/n (somme de 1 à n de l'indicatrice (X_i <= x)) 
m = length(x)
FN = rep(0,times=m) #m fois que des zeros 
for (i in 1:m){
  for (j in 1:N){
    if (Nor[j] <= x[i]){
      FN[i] = FN[i]+1 
    }
  }
  FN[i] = FN[i]/N
}
plot(x, FN, main = "Fonction de répartition empirique", col.main = "green", col = "blue", type = "l", lwd = 3)
y = pnorm(x, MNor, sd(Nor))
lines(x, y, col = "red", type = "l", lwd = 3, lty = 2)
legend("topleft", c("F.Repart. emp. observée","F.Repart. emp. théorique"), col = c("blue","red"), lty=c(1,2), cex = 0.45)

#2) Loi des grands nombres (LGN) ----

#Soit X une v.a et X1,...,Xn un echantillon de X. Alors Xn barre = 1/n (somme de 1 a n des Xi) tend en proba vers E[X]
N = 10000
m = 20
p = 0.3
Bin = rbinom(N,m,p)
nN = 1:N
nBin = cumsum(Bin)/nN 
eBin = rep(m*p, times=N) 
plot(nN, nBin, main = "Loi des Grands Nombres pour la loi Binomiale", col.main = "red", col = "red", type ="l")
lines(nN, eBin, col = "blue", type = "l")

#Pour avoir la vitesse on utilise le theoreme central limite (TCL) (de lordre de grand O(sqrt(n))) 
#On peut faire la meme chose pour n'importe quelle loi 

par(mfrow = c(2,2))
N=500
m=10
p=0.4
nb=1:N
moy=rep(0,times=N)
Bin=rbinom(N,m,p)
moy=cumsum(Bin)/nb
esp=rep(m*p,times=N)
plot(nb,moy,type="l",col="blue",lwd=3,main="LGN pour B(m,p)")
lines(nb,esp,type="l",col="red",lwd=3)
legend("bottomright", c("Moyenne emp.","Moyenne th."), col = c("blue","red"), lty=c(1,1), cex = 0.45)
#
# Loi de Poisson
#
N=500
lambda=4.0
nb=1:N
moyp=rep(0,times=N)
Poi=rpois(N,lambda)
moyp=cumsum(Poi)/nb
espp=rep(lambda,times=N)
plot(nb,moyp,type="l",col="blue",lwd=3,main="LGN pour P(lambda)")
lines(nb,espp,type="l",col="red",lwd=3)
legend("topright", c("Moyenne emp.","Moyenne th."), col = c("blue","red"), lty=c(1,1), cex = 0.45)
#
# Loi normale
#
N=500
mu=4.0
sd=1.4
nb=1:N
moyn=rep(0,times=N)
Nor=rnorm(N,mu,sd)
moyn=cumsum(Nor)/nb
espn=rep(mu,times=N)
plot(nb,moyn,type="l",col="blue",lwd=3,main="LGN pour N(mu,sd)")
lines(nb,espn,type="l",col="red",lwd=3)
legend("topright", c("Moyenne emp.","Moyenne th."), col = c("blue","red"), lty=c(1,1), cex = 0.45)
#
# Loi exponentielle
#
N=500
lambda=4.0
nb=1:N
moye=rep(0,times=N)
Exp=rpois(N,1/lambda)
moye=cumsum(Exp)/nb
espe=rep(1/lambda,times=N)
plot(nb,moye,type="l",col="blue",lwd=3,main="LGN pour E(lambda)")
lines(nb,espe,type="l",col="red",lwd=3)
legend("topright", c("Moyenne emp.","Moyenne th."), col = c("blue","red"), lty=c(1,1), cex = 0.45)

#4) Theoreme centrale limite ----- 

par(mfrow = c(2,2))
#Pour la vitesse de convergence de la loi des GN 
##Loi Normale -----
N = 1000 #Taille de lechantillon 
n = 2000 #Nombre de Un 
mu = 0
sd = 1
#construire un echantillon de Un 
Un = rep(0, times = n)
for (i in 1:n){
  Nor = rnorm(N,mu,sd) 
  Un[i] = sqrt(N)*(mean(Nor)-mu)/sqrt(sd)
}
#histogramme avec courbe 
hist(Un, freq=FALSE, main="TCL pour la loi Normale", nclass=16, col="blue")
x = seq(-4, 4, by = 0.1)
y = dnorm(x,0,1)
lines(x, y, type="l", col="red", lwd=2)
legend(x='topleft',legend=paste(c("N =","n ="),c(N,n)), cex = 0.5)
legend(x='topright',legend=paste(c("mu=","sd="),c(mu,sd)), cex = 0.5)

##Loi Binomiale -----
N = 1000 #Taille de lechantillon 
n = 2000 #Nombre de Un 
m = 10
p = 0.4
  #construire un echantillon de Un 
Un = rep(0, times = n)
for (i in 1:n){
  Bin = rbinom(N,m,p) 
  Un[i] = ( mean(Bin) - (m*p) ) * ( sqrt(N/(m*p*(1-p))) )  
}
  #histogramme avec courbe 
hist(Un, freq=FALSE, main="TCL pour la loi Binomiale", nclass=16, col="yellow")
x = seq(-4 * sd(Un), 4*sd(Un), by = 0.1)
y = dnorm(x,0,1)
lines(x, y, type="l", col="red", lwd=2)
legend(x='topleft',legend=paste(c("N =","n ="),c(N,n)), cex = 0.5)

##Loi Poisson ----- 
N = 1000 #Taille de lechantillon 
n = 2000 #Nombre de Un 
lambda = 2
#construire un echantillon de Un 
Un = rep(0, times = n)
for (i in 1:n){
  Poi = rpois(N,lambda) 
  Un[i] = sqrt(N) * (mean(Poi)-lambda)/sqrt(lambda)
}
#histogramme avec courbe 
hist(Un, freq=FALSE, main="TCL pour la loi de Poisson", nclass=16, col="green")
x = seq(-4 * sd(Un), 4*sd(Un), by = 0.1)
y = dnorm(x,0,1)
lines(x, y, type="l", col="red", lwd=2)
legend(x='topleft',legend=paste(c("N =","n =","lambda = "),c(N,n,lambda)), cex = 0.5)

##Loi Exponentielle ----- 
N = 1000 #Taille de lechantillon 
n = 2000 #Nombre de Un 
lambda = 2
#construire un echantillon de Un 
Un = rep(0, times = n)
for (i in 1:n){
  Exp = rexp(N,lambda) 
  Un[i] = sqrt(N) * (mean(Exp)-1/lambda)/sqrt(1/(lambda^2))
}
#histogramme avec courbe 
hist(Un, freq=FALSE, main="TCL pour la loi Exponentielle", nclass=16, col="purple")
x = seq(-4 * sd(Un), 4*sd(Un), by = 0.1)
y = dnorm(x,0,1)
lines(x, y, type="l", col="red", lwd=2)
legend(x='topleft',legend=paste(c("N =","n =","lambda = "),c(N,n,lambda)), cex = 0.5)

#5) Estimateurs E[X] et Var(X) ----

##Loi Binomiale ----
par(mfrow = c(1,2))
N = 1000 #taille de lechantillon 
n = 500 #nb dechantillon 
m = 20
p = 0.3
  #construire un echantillon 
EX = rep(0, times = n)
VX = rep(0, times = n)
for (i in 1:n){
  Bin = rbinom(N,m,p)
  EX[i] = mean(Bin)
  VX[i] = var(Bin)
}
DE = rep(m*p, times = n)
DV = rep(m*p*(1-p), times = n)

nn = 1:n 
plot(nn, EX, main = "Nuage de Xbarre", type = "p", col = "blue", lwd=2)
lines(nn, DE, type = "l", col = "red", lwd=2)
legend(x='topleft',legend=paste(c("N =","n =","lambda = "),c(N,n,lambda)), cex = 0.5)
legend(x='topright',legend=paste(c("m =","p ="),c(m,p)), cex = 0.5)
plot(nn,VX, main = "Nuage de variance", type = "p", col = "green", lwd=2)
lines(nn, DV, type = "l", col = "red", lwd=2)
legend(x='topleft',legend=paste(c("N =","n =","lambda = "),c(N,n,lambda)), cex = 0.5)
legend(x='topright',legend=paste(c("m =","p ="),c(m,p)), cex = 0.5)


##Loi Normale ----
par(mfrow = c(1,2))
N = 1000 #taille de lechantillon 
n = 500 #nb dechantillon 
mu = 1.5
sd = 0.3
#construire un echantillon 
EX = rep(0, times = n)
VX = rep(0, times = n)
for (i in 1:n){
  Nor = rnorm(N,mu,sd)
  EX[i] = mean(Nor)
  VX[i] = var(Nor)
}
DE = rep(mu, times = n)
DV = rep(sd^2, times = n)

nn = 1:n 
plot(nn, EX, main = "Nuage de Xbarre", type = "p", col = "blue", lwd=2)
lines(nn, DE, type = "l", col = "red", lwd=2)
legend(x='topleft',legend=paste(c("N =","n ="),c(N,n)), cex = 0.5)
legend(x='topright',legend=paste(c("mu = ","sd = "),c(mu,sd)), cex = 0.5)
plot(nn,VX, main = "Nuage de variance", type = "p", col = "green", lwd=2)
lines(nn, DV, type = "l", col = "red", lwd=2)
legend(x='topleft',legend=paste(c("N =","n ="),c(N,n)), cex = 0.5)
legend(x='topright',legend=paste(c("mu = ","sd = "),c(mu,sd)), cex = 0.5)

# + N est grand est + le nuage de point se reserre sur la droite rouge 

#7) Intervalle de confiance ----- 

par(mfrow=c(1,2))
alpha = 0.05
n = 50 #nb dechantillons 
N = 20 #taille  
  # loi normale de variance connue 
mu = 1.5
sigma = 0.5 
k = 100 
Est = rep(0, times = k)
for (l in 1:k){
  alphaest = 0
  for (i in 1:n){
    Nor = rnorm(N, mu, sigma)
    if ( (abs(mean(Nor) - mu)) > qnorm(1 - alpha/2)*sigma/sqrt(N) ) { #quantile loi normale 
      alphaest = alphaest +  1 
    }
  Est[l] = alphaest/n
  }
}
x = 1:k
plot (x, Est, main = "Convergence de l'estimateur alpha (sigma connu)", col = "red", type = "l")
xalpha = rep(alpha, times=k)
lines(x, xalpha, col="blue", type="l", lwd = 2)
legend(x='topleft',legend=paste(c("n =","alpha ="),c(n,alpha)), cex = 0.5)
for (l in 1:k){
  alphaest = 0
  for (i in 1:n){
    Nor = rnorm(N, mu, sigma)
    if ( (abs(mean(Nor) - mu)) > qt(1 - alpha/2, N-1)*sd(Nor)/sqrt(N) ) { #quantile loi poisson : variance pas connue
      alphaest = alphaest +  1 
    }
    Est[l] = alphaest/n
  }
}
x = 1:k
plot (x, Est, main = "Convergence de l'estimateur alpha (sigma non connu)", col = "green", type = "l")
xalpha = rep(alpha, times=k)
lines(x, xalpha, col="blue", type="l", lwd = 2)
legend(x='topleft',legend=paste(c("n =","alpha ="),c(n,alpha)), cex = 0.5)
# + n augmente, + l'intervalle se ressert autour d'alpha 
# le alpha estimée va convergé vers le alpha 

alphaest = alphaest / n 
#cat("alphaest = ", alphaest)

alpha = 0.05
N = 20
mu1 = 2 
sd = 1.2
Nor = rnorm(N,mu1,sigma)
B = mean(Nor)
mu = seq(B-4*sd, B+4*sd, by = 0.1)
NN = length(mu)
sigma1 = rep(0, NN)
sigma2 = rep(0, NN)
t1 = qchisq(alpha/2, N-1)
t2 = qchisq(1 - alpha/2, N-1)
mn = rep(mean(Nor), times = N)
for (i in 1:NN){
  a = mu[i]
  sigma1[i] = sum((Nor - mn)^2) + N*(mean(Nor)-a)^2/t1
  sigma2[i] = sum((Nor - mn)^2) + N*(mean(Nor)-a)^2/t2
}
plot(mu, sigma2, main = "Regions de confiance |R^2", type = "l", col = "blue")
polygon(mu, sigma2, border = TRUE, col = "lightblue")
lines(mu, sigma1, type = "l", col = "red")
polygon(mu, sigma1, border = FALSE, col = "white")

#6) Test d'hypothèses ----

# 1. On teste H0 : 'mu = mu0' contre H1 : 'mu != mu0'
sigma = 0.5
mu0 = 1.5 
N = 20 
n = 100 
alpha = 0.05 
mu = seq(mu0 - 4*sigma, mu0 + 4*sigma, by = 0.1)
m = length(mu)
Beta = rep(0, time = m)
Beta1 = rep(0, time = m)
for (i in 1:m){
  for (j in 1:n){
    Nor = rnorm(N, mu[i], sigma)
    if( abs((mean(Nor) - mu0) * sqrt(N) / sigma) > qnorm((1 - alpha/2)) ){
      Beta[i] = Beta[i] + 1
    }
  }
  Beta[i] = Beta[i] / n
}
for (i in 1:m){
  for (j in 1:n){
    Nor = rnorm(N, mu[i], sigma)
    if( abs((mean(Nor) - mu0) * sqrt(N) / sd(Nor)) > qt((1 - alpha/2),N-1) ){
      Beta1[i] = Beta1[i] + 1
    }
  }
  Beta1[i] = Beta1[i] / n
}
Calpha = rep(alpha, times = m)
plot(mu, Beta, main = "Test bilateral sur la moyenne avec la variance connue et inconnue", col = "red", type = "l", lwd = 2)
lines(mu, Beta1, col = "green", type = "l", lwd = 2)
lines(mu, Calpha, col = "blue", type = "l", lwd = 2)
legend(x='right',legend=c("Fonction puissance (sigma connue)","Fonction puissance (sigma inconnue)","Seuil de significativité"), col=c("red","green","blue"), lty = c(1,1), cex = 0.6, lwd = 2)
#on observe que lorsqu'on connait sigma la convergence est plus rapide (pente + raide)

# 2. On teste H0 : 'mu <= mu0' contre H1 : 'mu > mu0'
sigma = 0.5
mu0 = 1.5 
N = 20 
n = 100 
alpha = 0.05 
mu = seq(mu0 - 4*sigma, mu0 + 4*sigma, by = 0.1)
m = length(mu)
Beta = rep(0, time = m)
Beta1 = rep(0, time = m)
for (i in 1:m){
  for (j in 1:n){
    Nor = rnorm(N, mu[i], sigma)
    if( (mean(Nor) - mu0) * sqrt(N) / sigma > qnorm((1 - alpha)) ){
      Beta[i] = Beta[i] + 1
    }
  }
  Beta[i] = Beta[i] / n
}
for (i in 1:m){
  for (j in 1:n){
    Nor = rnorm(N, mu[i], sigma)
    if( (mean(Nor) - mu0) * sqrt(N) / sd(Nor) > qt((1 - alpha),N-1) ){
      Beta1[i] = Beta1[i] + 1
    }
  }
  Beta1[i] = Beta1[i] / n
}
Calpha = rep(alpha, times = m)
plot(mu, Beta, main = "Test unilateral sur la moyenne avec la variance connue et inconnue", col = "red", type = "l", lwd = 2)
lines(mu, Beta1, col = "green", type = "l", lwd = 2)
lines(mu, Calpha, col = "blue", type = "l", lwd = 2)
legend(x='right',legend=c("Fonction puissance (sigma connue)","Fonction puissance (sigma inconnue)","Seuil de significativité"), col=c("red","green","blue"), lty = c(1,1), cex = 0.6, lwd = 2)
#on observe que lorsqu'on connait sigma la convergence est plus rapide (pente + raide)

# 3. Cas général 
lambda0 = 1.5 
N = 1000
n = 100 
alpha = 0.05 
mu = seq(0.05, 4/lambda0, by = 0.05)
m = length(mu)
Beta = rep(0, time = m)
for (i in 1:m){
  for (j in 1:n){
    Exp = rexp(N, mu[i])
    if( abs((mean(Exp) - 1/lambda0) * sqrt(N) / sd(Exp)) > qnorm((1 - alpha/2)) ){
      Beta[i] = Beta[i] + 1
    }
  }
  Beta[i] = Beta[i] / n
}
Calpha = rep(alpha, times = m)
plot(mu, Beta, main = "Test bilateral sur la moyenne avec la variance inconnue", col = "red", type = "l", lwd = 2)
lines(mu, Calpha, col = "blue", type = "l", lwd = 2)
legend(x='right',legend=c("Fonction puissance","Seuil de significativité"), col=c("red","blue"), lty = c(1,1), cex = 0.6, lwd = 2)

lambda0 = 1.5 
N = 1000
n = 100 
alpha = 0.05 
mu = seq(0.05, 4/lambda0, by = 0.05)
m = length(mu)
Beta = rep(0, time = m)
for (i in 1:m){
  for (j in 1:n){
    Exp = rexp(N, mu[i])
    if( ((mean(Exp) - 1/lambda0) * sqrt(N) / sd(Exp)) > qt((1 - alpha),N-1) ){
      Beta[i] = Beta[i] + 1
    }
  }
  Beta[i] = Beta[i] / n
}
Calpha = rep(alpha, times = m)
plot(mu, Beta, main = "Test unilateral sur la moyenne avec la variance inconnue", col = "red", type = "l", lwd = 2)
lines(mu, Calpha, col = "blue", type = "l", lwd = 2)
legend(x='right',legend=c("Fonction puissance","Seuil de significativité"), col=c("red","blue"), lty = c(1,1), cex = 0.6, lwd = 2)

#8) Etude globale ----- 

data(AirPassengers)
# Extraire la différence entre chaque mois et le mois précédent
diff_passengers <- diff(AirPassengers)
# Tracer l'histogramme des différences avec la courbe de densité normale
hist(diff_passengers, breaks = 15, freq = FALSE, main = "Histogramme et courbe de densité",
     xlab = "Différence de passagers", ylab = "Densité", col = "yellow")
curve(dnorm(x, mean = mean(diff_passengers), sd = sd(diff_passengers)), add = TRUE, col = "red", lwd=3)

shapiro.test(diff_passengers)
# Définir la taille des échantillons
n <- 30
# Nombre total d'échantillons
num_samples <- length(diff_passengers) - n + 1
# Vecteur pour stocker les moyennes des échantillons
sample_means <- numeric(num_samples)
# Calculer les moyennes des échantillons
for (i in 1:num_samples) {
  sample_means[i] <- mean(diff_passengers[i:(i+n-1)])
}

mu <- mean(AirPassengers)
sigma <- sd(AirPassengers)
cat("Estimation de la moyenne : ", mu, "\n")
cat("Estimation de l'écart type : ", sigma, "\n")

# Effectuer le test t à deux échantillons
t_test <- t.test(diff_passengers, mu = 0)

# Afficher les résultats
t_test

# Estimation du risque de première espèce
alpha <- 0.05
p_value <- t_test$p.value
if (p_value < alpha) {
  cat("L'hypothèse nulle est rejetée avec un risque de première espèce de", alpha, "\n")
} else {
  cat("L'hypothèse nulle n'est pas rejetée avec un risque de première espèce de", alpha, "\n")
}

# Spécifier la vraie moyenne
true_mean <- 10

# Simuler des échantillons aléatoires de la même taille que nos données
n_simulations <- 1000
n_obs <- length(diff_passengers)
simulated_means <- replicate(n_simulations, mean(rnorm(n_obs, true_mean, sd(diff_passengers))))

# Calculer le nombre de fois où l'hypothèse nulle est rejetée
power <- mean(simulated_means < t_test$conf.int[1] | simulated_means > t_test$conf.int[2])

# Afficher la fonction puissance
cat("La fonction puissance est estimée à", power, "\n")

## Intervalles de confiance
# Choisir un niveau de confiance
alpha <- 0.05
# Calculer la moyenne et l'écart-type des données
moyenne <- mean(AirPassengers)
ecart_type <- sd(AirPassengers)
# Calculer la marge d'erreur
n <- length(AirPassengers)
z <- qnorm(1 - alpha/2)
marge_erreur <- z * ecart_type / sqrt(n)
# Calculer l'intervalle de confiance
intervalle_confiance <- c(moyenne - marge_erreur, moyenne + marge_erreur)
# Afficher l'intervalle de confiance
cat("Intervalle de confiance à", 100*(1-alpha), "% : [", round(intervalle_confiance[1], 2), ",", round(intervalle_confiance[2], 2), "]\n")


# Calculer la moyenne et l'écart-type des données
mean_ap <- mean(AirPassengers)
sd_ap <- sd(AirPassengers)

# Calculer l'intervalle de confiance à 95% de la moyenne
n <- length(AirPassengers)
se <- sd_ap / sqrt(n)
lower_ci <- mean_ap - qt(0.975, df = n-1) * se
upper_ci <- mean_ap + qt(0.975, df = n-1) * se

# Afficher les résultats graphiquement
hist(AirPassengers, breaks = 20, main = "Intervalle de confiance à 95% de la moyenne",
     xlab = "Nombre de passagers", col = "yellow")
abline(v = mean_ap, col = "red", lwd = 4)
abline(v = lower_ci, col = "green", lwd = 2, lty = 3)
abline(v = upper_ci, col = "green", lwd = 2, lty = 3)
legend("topright", legend = c("Moyenne", "Intervalle de confiance à 95%"),
       col = c("red", "green"), lty = c(1, 2), lwd = 2)

# Tracer l'histogramme des moyennes des échantillons avec la courbe de densité normale
hist(sample_means, breaks = 15, freq = FALSE, main = "Histogramme et courbe de densité des moyennes d'échantillons",
     xlab = "Moyenne d'échantillon de différence de passagers", ylab = "Densité", col = "yellow")
curve(dnorm(x, mean = mean(sample_means), sd = sd(sample_means)), add = TRUE, col = "red", lwd=3)




#sink()