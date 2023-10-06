from pylab import * 
import numpy as np
import matplotlib.tri as mtri
import matplotlib.pyplot as plt
import matplotlib.animation as animation


#******************** FONCTIONS & PROCEDURES ******************************#

def lit_fichier_msh(fichier_msh):
    f = open(fichier_msh,'r') #ouvrir le fichier 
    ligne = f.readline() #lire la ligne (ici la 1ere)
    data = ligne.split() #decomposer la ligne en morceaux (par les espaces)
    nbn = int(data[0])
    nbe = int(data[1])
    nba = int(data[2])
    refn = zeros(nbn,int) #liste refn => reference des noeuds
    coord = zeros((nbn,2),double) #tableau coordonnees de tous les noeuds 
    for i in range(nbn):
        ligne = f.readline() #lire la ligne suivante 
        data = ligne.split() #decomposer la ligne 
        for j in range(2):
            coord[i,j] = double(data[j]) 
        refn[i] = int(data[2])
    reft = zeros(nbe,int) #liste reft => reference des triangles
    tri = zeros((nbe,3),double) #tableau indices des sommets de chaque triangle 
    for i in range(nbe):
        ligne = f.readline() #lire la ligne suivante
        data = ligne.split() #decomposer la ligne 
        for j in range(3):
            tri[i,j] = double(data[j])
        reft[i] = int(data[3])
    refa = zeros(nba,int) #liste refa => reference des aretes
    ar = zeros((nba,2),double) #tableau indices des sommets de chaque arete 
    for i in range(nba):
        ligne = f.readline() #lire la ligne suivante 
        data = ligne.split() #decomposer la ligne 
        for j in range(2):
            ar[i,j] = double(data[j])
        refa[i] = int(data[2])
    f.close() #fermer le fichier 
    tri = tri - 1 
    ar = ar -1 
    return [nbn,nbe,nba,coord,tri,ar,refn,reft,refa]

def pas_et_qualite_maillage(fichier_msh,nbe,coord,tri): 
    hT = [] #Pas de tous les triangles
    QT = [] #Qualite de tous les triangles 
    for i in range(nbe):
        arT = [] #taille de chaque arete du triangle i 
        arT.append(norm(coord[int(tri[i][0])]-coord[int(tri[i][1])])) 
        arT.append(norm(coord[int(tri[i][0])]-coord[int(tri[i][2])]))
        arT.append(norm(coord[int(tri[i][1])]-coord[int(tri[i][2])]))
        hT.append(max(arT))
        dT = (1/2) * sum(arT) # demi perimetre du triangle 
        mesT = sqrt(dT*(dT-arT[0])*(dT-arT[1])*(dT-arT[2]))# aire du triangle
        rT = mesT / dT #rayon du cercle inscrit 
        QT.append((sqrt(3)/6) * (max(arT)/rT))
    h = max(hT)
    Q = max(QT)
    return [h,Q]

def trace_maillage_ind(nbn,nbe,nba,coord,tri,ar): 
    triplot(coord[:,0],coord[:,1],tri,'g-',lw=2) # tracer les triangles  
    for j in range(nbe): # indices des triangles 
        S1 = coord[int(tri[j][0])] 
        S2 = coord[int(tri[j][1])] 
        S3 = coord[int(tri[j][2])] 
        I = [0,0]
        I[0] = (1/3) * ((S1[0]+S2[0])+S3[0])
        I[1] = (1/3) * ((S1[1]+S2[1])+S3[1])
        text(I[0],I[1],str(j+1),c='orange') 
    for j in range(nbn): # indices des noeuds 
        Nx = coord[j][0]
        Ny = coord[j][1]
        text(Nx,Ny,str(j+1),c='red')
    for j in range(nba): # indices des aretes exterieures 
        S1 = coord[int(ar[j][0])]
        S2 = coord[int(ar[j][1])]
        I = [0,0]
        I[0] = (1/2) * (S1[0]+S2[0])
        I[1] = (1/2) * (S1[1]+S2[1])
        text(I[0],I[1],str(j+1),c='blue')
    title('Maillage avec les indices') 
    xlabel('axe Ox')
    ylabel('axe Oy')
    show() 

def trace_maillage_ref(nbn,nbe,nba,coord,tri,ar,refn,reft,refa): 
    triplot(coord[:,0],coord[:,1],tri,'g-',lw=2) # tracer les triangles  
    for j in range(nbe): # ref des triangles (reft)
        S1 = coord[int(tri[j][0])] 
        S2 = coord[int(tri[j][1])] 
        S3 = coord[int(tri[j][2])] 
        I = [0,0]
        I[0] = (1/3) * ((S1[0]+S2[0])+S3[0])
        I[1] = (1/3) * ((S1[1]+S2[1])+S3[1])
        text(I[0],I[1],str(reft[j]),c='orange') 
    for j in range(nbn): # ref des noeuds (refn)
        Nx = coord[j][0]
        Ny = coord[j][1]
        text(Nx,Ny,str(refn[j]),c='red')
    for j in range(nba): # ref des aretes exterieures (refa)
        S1 = coord[int(ar[j][0])]
        S2 = coord[int(ar[j][1])]
        I = [0,0]
        I[0] = (1/2) * (S1[0]+S2[0])
        I[1] = (1/2) * (S1[1]+S2[1])
        text(I[0],I[1],str(refa[j]),c='blue')
    title('Maillage avec les references')
    xlabel('axe Ox')
    ylabel('axe Oy')
    show() 
    
def charge_et_affiche_maillage(FichierMaillage):
    [nbn,nbe,nba,coord,tri,ar,refn,reft,refa] = lit_fichier_msh(FichierMaillage) # lecture du fichier 
    print('nbn =',nbn)
    print('nbe =',nbe)
    print('nba =',nba)
    print('coord =',coord)
    print('refn =',refn)
    print('tri =',tri)
    print('reft =',reft)
    print('ar =',ar) 
    print('refa =',refa) 
    [h,Q] = pas_et_qualite_maillage(FichierMaillage,nbe,coord,tri) # pas et qualite du maillage 
    print("Pas : {:.2f}".format(round(h, 2)))
    print("Qualite : {:.2f}".format(round(Q, 2)))
    trace_maillage_ind(nbn,nbe,nba,coord,tri,ar) # indices (noeuds,tri,ar)
    trace_maillage_ref(nbn,nbe,nba,coord,tri,ar,refn,reft,refa)  # références (noeuds,tri,ar)
    triplot(coord[:,0],coord[:,1],tri,'g-',lw=2) # tracer les triangles 
    
    ## PARAMETRES DE REGLAGES ## 
    
def fct_V():
    # vitesse du flot 
    V = 0.5/3.6 
    return V
    
def fct_alpha():
    # facteur de transfert 
    alpha = 0.5/0.003
    return alpha 

def fct_uC(): 
    # temperature à la sortie du climatisateur 
    uC = 15.0
    return uC 

    ## PARAMETRES FIXES ## 

def fct_c():
    # constante calorifique 
    c = 4.2*1e6
    return c

def fct_kappa():
    # fonction de conductivite thermique de l'eau
    kappa = 0.6
    return kappa

def fct_f():
    # fonction source de chaleur 
    f = 0
    return f 

def fct_uE(): 
    # temperature exterieure 
    uE = 40.0
    return uE 

def fct_L():
    # longueur de la conduite 
    L = 50.0
    return L

def fct_R():
    # rayon de la section 
    R = 0.05 
    return R

    ## PARAMETRES INTER-DEPENDANTS ##

def fct_T():
    # temps de parcours du flot (en secondes)
    T = fct_L()/fct_V()
    return T

def fct_delta(n):
    # pas de temps dans le schéma 
    delta = fct_T()/n
    return delta

def coeffelem_P1_rigid(l,coord,tri):
    # matrice 3x3 k^l pour l element T^l 
    k = np.zeros((3,3)) 
    arT = [] # taille de chaque arete du triangle l  
    arT.append(norm(coord[int(tri[l][0])]-coord[int(tri[l][1])])) 
    arT.append(norm(coord[int(tri[l][0])]-coord[int(tri[l][2])]))
    arT.append(norm(coord[int(tri[l][1])]-coord[int(tri[l][2])]))
    dT = 0.5 * sum(arT) # demi perimetre du triangle 
    mesT = sqrt(dT*(dT-arT[0])*(dT-arT[1])*(dT-arT[2])) # aire du triangle
    k[0,0] = (fct_kappa() / (4*mesT)) * ( (coord[int(tri[l][1])][0] - coord[int(tri[l][2])][0])**2 + (coord[int(tri[l][1])][1] - coord[int(tri[l][2])][1])**2 )
    k[1,1] = (fct_kappa() / (4*mesT)) * ( (coord[int(tri[l][2])][0] - coord[int(tri[l][0])][0])**2 + (coord[int(tri[l][2])][1] - coord[int(tri[l][0])][1])**2 )
    k[2,2] = (fct_kappa() / (4*mesT)) * ( (coord[int(tri[l][0])][0] - coord[int(tri[l][1])][0])**2 + (coord[int(tri[l][0])][1] - coord[int(tri[l][1])][1])**2 )
    k[0,1] = (fct_kappa() / (4*mesT)) * ( -(coord[int(tri[l][0])][0] - coord[int(tri[l][2])][0])*(coord[int(tri[l][1])][0] - coord[int(tri[l][2])][0]) - (coord[int(tri[l][0])][1] - coord[int(tri[l][2])][1])*(coord[int(tri[l][1])][1] - coord[int(tri[l][2])][1]) )
    k[1,0] = (fct_kappa() / (4*mesT)) * ( -(coord[int(tri[l][0])][0] - coord[int(tri[l][2])][0])*(coord[int(tri[l][1])][0] - coord[int(tri[l][2])][0]) - (coord[int(tri[l][0])][1] - coord[int(tri[l][2])][1])*(coord[int(tri[l][1])][1] - coord[int(tri[l][2])][1]) )
    k[0,2] = (fct_kappa() / (4*mesT)) * ( -(coord[int(tri[l][2])][0] - coord[int(tri[l][1])][0])*(coord[int(tri[l][0])][0] - coord[int(tri[l][1])][0]) - (coord[int(tri[l][2])][1] - coord[int(tri[l][1])][1])*(coord[int(tri[l][0])][1] - coord[int(tri[l][1])][1]) )
    k[2,0] = (fct_kappa() / (4*mesT)) * ( -(coord[int(tri[l][2])][0] - coord[int(tri[l][1])][0])*(coord[int(tri[l][0])][0] - coord[int(tri[l][1])][0]) - (coord[int(tri[l][2])][1] - coord[int(tri[l][1])][1])*(coord[int(tri[l][0])][1] - coord[int(tri[l][1])][1]) )
    k[0,2] = (fct_kappa() / (4*mesT)) * ( -(coord[int(tri[l][2])][0] - coord[int(tri[l][1])][0])*(coord[int(tri[l][0])][0] - coord[int(tri[l][1])][0]) - (coord[int(tri[l][2])][1] - coord[int(tri[l][1])][1])*(coord[int(tri[l][0])][1] - coord[int(tri[l][1])][1]) )
    k[2,0] = (fct_kappa() / (4*mesT)) * ( -(coord[int(tri[l][2])][0] - coord[int(tri[l][1])][0])*(coord[int(tri[l][0])][0] - coord[int(tri[l][1])][0]) - (coord[int(tri[l][2])][1] - coord[int(tri[l][1])][1])*(coord[int(tri[l][0])][1] - coord[int(tri[l][1])][1]) )
    k[1,2] = (fct_kappa() / (4*mesT)) * ( -(coord[int(tri[l][1])][0] - coord[int(tri[l][0])][0])*(coord[int(tri[l][2])][0] - coord[int(tri[l][0])][0]) - (coord[int(tri[l][1])][1] - coord[int(tri[l][0])][1])*(coord[int(tri[l][2])][1] - coord[int(tri[l][0])][1]) )
    k[2,1] = (fct_kappa() / (4*mesT)) * ( -(coord[int(tri[l][1])][0] - coord[int(tri[l][0])][0])*(coord[int(tri[l][2])][0] - coord[int(tri[l][0])][0]) - (coord[int(tri[l][1])][1] - coord[int(tri[l][0])][1])*(coord[int(tri[l][2])][1] - coord[int(tri[l][0])][1]) )
    return k
    
def coeffelem_P1_masse(l,coord,tri):
    arT = [] # taille de chaque arete du triangle l  
    arT.append(norm(coord[int(tri[l][0])]-coord[int(tri[l][1])])) 
    arT.append(norm(coord[int(tri[l][0])]-coord[int(tri[l][2])]))
    arT.append(norm(coord[int(tri[l][1])]-coord[int(tri[l][2])]))
    dT = (1/2) * sum(arT) # demi perimetre du triangle 
    mesT = sqrt(dT*(dT-arT[0])*(dT-arT[1])*(dT-arT[2])) # aire du triangle
    m = (mesT/3) * eye(3)
    return m

def coeffelem_P1_source(l,coord,tri):
    # vecteur de dim 3 f^l pour l element T^l 
    f = np.ones(3)
    arT = [] # taille de chaque arete du triangle l  
    arT.append(norm(coord[int(tri[l][0])]-coord[int(tri[l][1])])) 
    arT.append(norm(coord[int(tri[l][0])]-coord[int(tri[l][2])]))
    arT.append(norm(coord[int(tri[l][1])]-coord[int(tri[l][2])]))
    dT = (1/2) * sum(arT) # demi perimetre du triangle 
    mesT = sqrt(dT*(dT-arT[0])*(dT-arT[1])*(dT-arT[2])) # aire du triangle
    f = (mesT/3) * fct_f() * f
    return f
    
def coeffelem_P1_trans(a,coord,ar,alpha):
    # vecteur de dim 2 e^a pour l arete A^a 
    e = np.ones(2)
    mesA = norm(coord[int(ar[a][0])]-coord[int(ar[a][1])]) 
    e = (mesA/2) * alpha * fct_uE() * e
    return e 

def coeffelem_P1_poids(a,coord,ar,alpha):
    # matrice p^a pour l arete A^a 
    p = np.array([[2,1],[1,2]])
    mesA = norm(coord[int(ar[a][0])]-coord[int(ar[a][1])]) 
    p = (mesA/6) * alpha * p
    return p 
    
def assemblage_EF_P1(coord,tri,nbe,nbn):
    # Etape 1: initialisation a zero 
    A = np.zeros((nbn,nbn))
    M = np.zeros((nbn,nbn))
    F = np.zeros(nbn)    
    # Etape 2: addition des termes volumiques 
    for l in range(nbe):
        k = coeffelem_P1_rigid(l,coord,tri)
        f = coeffelem_P1_source(l,coord,tri)
        m = coeffelem_P1_masse(l, coord, tri)
        I1 = int(tri[l][0])
        I2 = int(tri[l][1])
        I3 = int(tri[l][2])  
        I = [I1,I2,I3]
        for j in range(3) :
            for i in range(3):
                A[I[j]][I[i]] += k[j][i]
                M[I[j]][I[i]] += m[j][i]
            F[I[j]] += f[j]
    K = A.copy() # conservation de la matrice de rigidite 
    # Etape 3: addition des termes de bord (transfert thermique)        
    alpha = fct_alpha()
    gamma = []
    for i in range(nba): 
        if refa[i]==1:
             gamma.append(ar[i]) 
    for a in range(nba):
        if refa[a] == 1:
            p = coeffelem_P1_poids(a,coord,ar,alpha)
            e = coeffelem_P1_trans(a,coord,ar,alpha)
            I1 = int(ar[a][0])
            I2 = int(ar[a][1])
            A[I1][I1] += p[0][0]
            A[I2][I1] += p[1][0]
            A[I1][I2] += p[0][1]
            A[I2][I2] += p[1][1]
            F[I1] += e[0]
            F[I2] += e[1]
    return A,F,K,M
    
def validation_pen(n,coord,tri,nbe,nbn):
    [A,F,K,M] = assemblage_EF_P1(coord,tri,nbe,nbn)
    B = (fct_c()/fct_delta(n)) * M + A 
    t = 0.0
    Vold = fct_uC() * ones(nbn) # instant initial 
    Uh = zeros((n,nbn))
    Gold = zeros(nbn)
    d = zeros(n)
    for i in range(0,n): 
        t = t + fct_delta(n) # nouvel instant 
        d[i] = t * fct_V() # distance à la climatisation (nouvelle position) 
        Gold = F + ( (fct_c()/fct_delta(n)) * dot(M , Vold) ) # GAXPY (matrice*vecteur + vecteur)
        Vnew = solve(B,Gold) # résolution d'un système linéaire 
        Uh[i] = Vold
        Vold = Vnew 
    return [Uh,A,F,K,M,Gold,Vnew,Vold,n,d]
        

#******************* CODE PRINCIPAL **************************************#

    #### Pre - traitement ####
    
FichierMaillage="./maillage_cercle.msh" # Nom du fichier de maillage 
#charge_et_affiche_maillage(FichierMaillage)
[nbn,nbe,nba,coord,tri,ar,refn,reft,refa] = lit_fichier_msh(FichierMaillage) # Lecture du fichier 
[h,Q] = pas_et_qualite_maillage(FichierMaillage,nbe,coord,tri)

n = 10  # Autant que nécessaire pour que le flot atteigne Romeo 
[Uh,A,F,K,M,Gold,Vnew,Vold,n,d] = validation_pen(n,coord,tri,nbe,nbn)

    
    #### Post - traitement #### 
    
print("\n===================== climatisation_romeo.py =======================\n")
print(f"* Resultats sur le maillage {FichierMaillage} ... *")
print("nbn = ",nbn)
print("nbe = ",nbe)
print("nba = ",nba)
print("A =",A)
print("F =",F)
print("M =",M)
print("Vnew =",Vnew)

print("\n===================== climatisation_romeo.py =======================\n")


print("\n        ___---===***   RESULTATS:   ***===---___")
print("--------------------------------------------------------")
print(f" -->              min(Uh) : {min(Uh[n-1]):.5f} °C             <--")
print(f" -->              max(Uh) : {max(Uh[n-1]):.5f} °C             <--")
print(f" -->             mean(Uh) : {mean(Uh[n-1]):.5f} °C             <--")
print(f" -->                pas h : {h:.3f}                   <--")
print(f" -->            qualité Q : {Q:.3f}                   <--")
print("--------------------------------------------------------\n")
print("===================== climatisation_romeo.py =======================")


    #### Visualisation #### 

Rep=False # L'animation des graphiques se répète en boucle 

# Créer la fenêtre et les sous-graphiques
fig, axs = plt.subplots(nrows=2, ncols=2)
fig.subplots_adjust(hspace=0.5, wspace=0.5)

# Premier graphique
moyUh = np.zeros(n)
for i in range(n):
    moyUh[i] = np.mean(Uh[i])
axs[0, 0].plot(d, moyUh, color="r", marker='*', label='moyUh')
axs[0, 0].set_xlabel('CLIM <= Position du flot d eau => ROMEO')
axs[0, 0].set_ylabel('Température moyenne en degré C')
axs[0, 0].set_title('Température Moy vs. éloignement de la clim')
axs[0, 0].legend()
axs[0, 0].grid()

# Deuxième graphique
triang = mtri.Triangulation(coord[:,0],coord[:,1],tri)
cb = axs[0, 1].tricontourf(triang, Uh[n-1], cmap='jet', alpha=0.8)
axs[0, 1].set_title('Température Uh(x,y) dans la section à l arrivée de Romeo')
axs[0, 1].set_xlabel('abscisses x')
axs[0, 1].set_ylabel('ordonnees y')
cbax = plt.colorbar(cb,ax=axs[0, 1])
# Fonction pour la mise à jour de l'animation
def update_temp(i):
    axs[0, 1].clear()
    axs[0, 1].set_title(f'Température Uh(x,y) dans la section à l\'arrivée de Romeo (t = {i})')
    axs[0, 1].set_xlabel('abscisses x')
    axs[0, 1].set_ylabel('ordonnees y')
    triang = mtri.Triangulation(coord[:,0],coord[:,1],tri)
    cb = axs[0, 1].tricontourf(triang, Uh[i], cmap='jet', alpha=0.8)
    return cb
ani2 = animation.FuncAnimation(fig, update_temp, frames=range(1,n), interval=1500, repeat=Rep)

# Troisième graphique
ax = fig.add_subplot(2, 2, 3, projection='3d')
ax.plot_trisurf(coord[:, 0], coord[:, 1], Uh[n-1, :], cmap="jet")
ax.set_xlabel('abscisses x')
ax.set_ylabel('ordonnees y')
ax.set_zlabel('température Uh')
ax.set_title('Température Uh(x,y) dans la section')
fig.delaxes(axs[1, 0])
def update_sec(i):
    ax.clear()
    sec = ax.plot_trisurf(coord[:, 0], coord[:, 1], Uh[i, :], cmap="jet")
    ax.set_xlabel('abscisses x')
    ax.set_ylabel('ordonnees y')
    ax.set_zlabel('température Uh')
    ax.set_title(f'Température Uh(x,y) dans la section (t = {i})')
    return sec
ani3 = animation.FuncAnimation(fig, update_sec, frames=n, interval=1500, repeat=Rep)

# Quatrième graphique 
x = ones(n)
pos = axs[1, 1].plot(1, d[n-1], marker='', label='d')
axs[1, 1].axvline(x=1, color='b')
axs[1, 1].set_ylabel('CLIM <= Position du flot d eau => ROMEO')
axs[1, 1].set_title('Point du flot')
axs[1, 1].set_xticks([])
axs[1, 1].set_xlabel('')
axs[1, 1].legend()
axs[1, 1].grid()
def update_pos(i):
    axs[1, 1].clear()
    axs[1, 1].axvline(x=1, color='b')
    axs[1, 1].set_title(f'Point du flot d eau depuis la sortie du climatisateur (t = {i})')
    axs[1, 1].set_ylabel('CLIM <= Position du flot d eau => ROMEO')
    axs[1, 1].set_xticks([])
    axs[1, 1].set_xlabel('')
    axs[1, 1].grid()
    pos = axs[1, 1].plot(1, d[i], color="r", marker='s', markersize=15, label='d')
    axs[1, 1].legend()
    return pos
ani4 = animation.FuncAnimation(fig, update_pos, frames=range(1,n), interval=1500, repeat=Rep)

plt.show()


    
