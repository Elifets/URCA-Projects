from pylab import * 
import subprocess
import os

print("\n\n ~~~~~~~~~~~~~~~~~~~~~~~ DEBUT PROGRAMME main.py ~~~~~~~~~~~~~~~~~~~~~~~ \n\n")

print("\n\n ########################## PRE - TRAITEMENT ################################\n\n")
N = [] # Tableau des intervalles 

# Ouverture du fichier NI.TXT en mode lecture
f1 = open('NI.TXT', 'r') # 'r' pour read : lecture 
# Lecture de toutes les lignes et stockage dans une liste
lignes = []
lignes = f1.readlines()
# Parcours de chaque ligne du fichier et ajout des chiffres dans le tableau
for i in lignes:
    # Suppression des espaces en début et fin de ligne (ici les '\n')
    i = i.strip()
    # Si la ligne contient un chiffre, l'ajouter au tableau
    if i.isdigit():
        N.append(int(i))
f1.close()
# Affichage du tableau
print("--> Voici les nombres d'intervalles étudiés : ",N)

# Demande à l'utilisateur s'il veut changez le contenue de NI.txt
reponse = input("? Voulez-vous changez les intervalles ? Oui = 1 / Non = 0\n")
if reponse == "1":
    # Demande à l'utilisateur les nouvelles valeurs à écrire dans le fichier
    new_N = input("? Entrez les nouvelles valeurs, séparées par des espaces [4 au maximum]: ")
    # Crée une nouvelle liste d'entiers à partir d'une chaîne de caractères 
    new_N = [int(val) for val in new_N.split()] # split() correspond au séparateur, par defaut l'espace
    # Écrit les nouvelles valeurs dans le fichier NI.TXT
    f1 = open('NI.TXT', 'w') # 'w' pour write : l'écriture (écrase le contenu déjà présent)
    for val in new_N:
        f1.write(str(val) + '\n')
    f1.close()
    f1 = open('NI.TXT', 'r')
    N = [] # On réinitialise la liste N pour ne pas garder les anciennes valeurs
    lignes = f1.readlines()
    # Parcours de chaque ligne du fichier et ajout des chiffres dans le tableau
    for i in lignes:
        # Suppression des espaces en début et fin de ligne (ici les '\n')
        i = i.strip()
        # Si la ligne contient un chiffre, l'ajouter au tableau
        if i.isdigit():
            N.append(int(i))
    f1.close()
    print("--> Les nouvelles valeurs ont été enregistrées dans le fichier NI.TXT")
    print("--> Voici la liste des nouveaux nombres d'intervalles étudiés : ",N)
else:
    print("Le contenu du fichier NI.TXT n'a pas été modifié.")
    
print("\n\n ########################## TRAITEMENT ################################\n\n")

# Exécution du programme solveurMDF.exe
result = subprocess.run(["./solveurMDF.exe"], capture_output=True)
# Affichage de la sortie du programme en C
print(result.stdout.decode())

print("\n\n ######################## POST - TRAITEMENT ###########################\n\n")

# Lecture du fichier X.txt et stockage du contenu dans un tableau
f1 = open('X.txt', 'r')
X = []
lignes = []
lignes = f1.readlines()
# Parcours de chaque ligne du fichier et ajout des chiffres dans le tableau
for i in lignes:
    # Suppression des espaces en début et fin de ligne (ici les '\n')
    i = i.strip()
    # Ajout du contenu de chaque ligne dans liste X
    X.append(float(i))
f1.close()

# Lecture du fichier U.txt et stockage du contenu dans un tableau
f1 = open('U.txt', 'r')
U = []
lignes = []
lignes = f1.readlines()
# Parcours de chaque ligne du fichier et ajout des chiffres dans le tableau
for i in lignes:
    # Suppression des espaces en début et fin de ligne (ici les '\n')
    i = i.strip()
    # Ajout du contenu de chaque ligne dans liste X
    U.append(float(i))
f1.close()

# Lecture du fichier Uh.txt et stockage du contenu dans un tableau
f1 = open('Uh.txt', 'r')
Uh = []
lignes = []
lignes = f1.readlines()
# Parcours de chaque ligne du fichier et ajout des chiffres dans le tableau
for i in lignes:
    # Suppression des espaces en début et fin de ligne (ici les '\n')
    i = i.strip()
    # Ajout du contenu de chaque ligne dans liste X
    Uh.append(float(i))
f1.close()

# Création du graphique pour la solution exacte et la solution approchée
plt.plot(X, U, marker = '*', label='U')
plt.plot(X, Uh, marker = 'x', label='Uh')
plt.xlabel('X')
plt.ylabel('U, Uh')
plt.title('Tracé de U et Uh en fonction de X')
plt.legend()
plt.show()

os.system("commande >nul 2>&1") # Pour éviter d'avoir le 0 au début de l'affichage du fichier .txt
os.system("type Convergence.txt") # Affichage du contenu du fichier Convergence.txt 
    # Decommenter la ligne suivante pour l'affichage sous Linux 
#os.system("les Convergence.txt") # Affichage du contenu du fichier Convergence.txt 

f1 = open('Convergence.txt', 'r')
lignes = []
einf = [] # Initialisation de la liste einf = ||U - Uh||_inf
lignes = f1.readlines()
for i in lignes:
    # Diviser la ligne de chaîne de caractères en plusieurs parties
    conv = i.split()
    # Ajout du 3eme contenu de chaque ligne dans la liste einf
    einf.append(float(conv[2]))
f1.close()

# Création du graphique pour l'erreur U - Uh 
plt.plot(N, einf, marker = '*', label='einf')
plt.xlabel('N')
plt.ylabel('einf')
plt.title('Tracé de einf en fonction de N')
plt.legend()
plt.show()

f1 = open('Convergence.txt', 'r')
lignes = []
h = [] # Initialisation de la liste einf = ||U - Uh||_inf
lignes = f1.readlines()
for i in lignes:
    # Suppression des espaces en début et fin de ligne (ici les '\n')
    conv = i.split()
    # Ajout du 3eme contenu de chaque ligne dans la liste einf
    h.append(float(conv[1]))
f1.close()

p = []
for i in range(0,len(einf)-1):
    p.append(float((1.0/log(2) * log(einf[i]/einf[i+1]))))


# Tracé de la courbe de convergence en maillages 
x = -log(h)
y = -log(einf)
plt.plot(x, y, marker = '*', label = '- log(einf)')
plt.xlabel('- log h')
plt.ylabel('- log eh')
plt.title('Courbe de convergence en maillages')
plt.legend()
plt.show()

pente = (y[len(y)-1] - y[0]) / (x[len(x)-1] - x[0]) 
print("\n --> pente p : {:.2f}".format(round(pente, 4)))



print("\n\n ~~~~~~~~~~~~~~~~~~~~~~~ FIN PROGRAMME main.py ~~~~~~~~~~~~~~~~~~~~~~~ \n\n")