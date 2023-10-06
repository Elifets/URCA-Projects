#include <stdio.h> /*pour utiliser printf, scanf... */
#include <stdlib.h> /* abs, ...*/
#include <math.h> /* pow,... */

typedef struct{
    int dim;
    double *tab;
} Vecteur;

/* ------ FONCTIONS & PROCEDURES ------- */

void affiche_matrice(char message [256], double ** M, int m, int n){
    int i,j;

    printf("--> %s",message);
    for (i=0; i<m; i++){
        printf("\n [");
        for(j=0;j<n;j++){
            printf(" %e ", M[i][j]);
        }
        printf("]");
    }
    printf("\n\n");
}

void affiche_Vecteur(char message[256], Vecteur * V){
    int i;

    printf("\n--> %s",message);
    printf("[ ");
    for (i=0; i<(V->dim); i++){
        printf("%e ",V->tab[i]); /* afficher chaque composante */
    }
    printf("] \n");
}

Vecteur *alloc_Vecteur(int n){
    Vecteur *V = NULL;
    V = (Vecteur *)malloc(sizeof(Vecteur)+1); /*reserve de la memoire necessaire*/
    V -> dim = n; /* ou (*V).dim = n */ /*on affecte la valeur n pour dim de Vecteur*/
    V -> tab = (double *)calloc(n,sizeof(double)+1);
    if (V->tab == NULL){ printf("! Pas de place en m�moire dans alloc_vecteur => SORTIE!"); exit(1);}
    return V;
}

double ** alloc_Matrice(int n){
  int i;
  double ** M = NULL;
  M = malloc(n*sizeof(double*)+1);
  for(i=0; i<n; i++){
    M[i] = malloc(n*sizeof(double)+1);
  }
  if (M==NULL){printf("Plus de mémoire !\n"); exit(1);}

  return M;
}

void affecte_Vecteur(double valeur, Vecteur *V, int i){ /* :setter */ /* affectation d'une valeur*/
    V->tab[i-1] = valeur;
}

void init_Vecteur(Vecteur *V){ /*initialiser vecteur 0 de dim i) */
    int i;
    int n = V->dim;

    for (i=0; i<n; i++){
        V->tab[i-1] = 0.0;
    }
}

void init_matrice(double **M, int m, int n){ /*initialiser matrice 0 de taille mxn) */
    int i,j;

    for (i=0; i<m; i++){
        for (j=0; j<n; j++){
            M[i][j] = 0.0;
        }
    }
}

void free_matrice(double** M, int n){
  int i;
  for(i=0; i<n; i++){
    free(M[i]);
    M[i] = NULL;
  }
  free(M);
  M = NULL;
}

void free_vecteur(Vecteur * v){
  free(v);
  v = NULL;
}

double NORM_INF(Vecteur *x){ /* ||x||_Inf = max (|x_i|) pour i allant de 1 à n */
    double norminf = 0.0;
    int i;

    for (i=0; i<(x->dim); i++){
            if (norminf<=(fabs(x->tab[i]))){norminf = fabs(x->tab[i]);}
    }

    return norminf;
}

void factorisation_LU(double** A, double** L, double** U, int n){
	int i,j;
	for(i=0; i<(n-1); i++){
		L[i+1][i] = A[i+1][i] / A[i][i];
		for(j=i; j<i+2; j++){
			U[i][j] = A[i][j];
		}
		A[i+1][i+1] = A[i+1][i+1] - L[i+1][i] * U[i][i+1];
	}
	U[n-1][n-1] = A[n-1][n-1];
}

Vecteur * remontee(double ** U, Vecteur * y, int n){
  int i;
  Vecteur * x = NULL;

  x = alloc_Vecteur(n);
  x->tab[n-1] = y->tab[n-1] / U[n-1][n-1];
  for(i=n-2; i>=0; i--){
    x->tab[i]=(y->tab[i]-U[i][i+1]*x->tab[i+1])/U[i][i];
  }

  return x;
}

Vecteur * descente(double ** L, Vecteur * b, int n){
  int i;
  Vecteur * x = NULL;

  x = alloc_Vecteur(n);
  x->tab[0] = b->tab[0];
  for(i=1; i<n; i++){
    x->tab[i] = b->tab[i] - x->tab[i-1] * L[i][i-1];
  }

  return x;
}

double ** assemblage_A(float eps, float h, int n){
    double ** A1 = NULL;
    int i;
    A1 = alloc_Matrice(n-2);
    init_matrice(A1,n-2,n-2);
    A1[0][0] = 1.0 + 2.0 * (eps/(h*h));
    A1[0][1] = -eps/(h*h);
    for (i=1; i<(n-3); i++){
        A1[i][i-1] = -eps/(h*h);
        A1[i][i] = 1.0 + 2.0 * (eps/(h*h));
        A1[i][i+1] = -eps/pow(h,2);
    }
    A1[n-3][n-4] = -eps/(h*h);
    A1[n-3][n-3] = 1.0 + 2.0 * (eps/(h*h));

    return A1;
}

Vecteur * assemblage_CL(float u0, float u1, float eps, float h, int n){
    Vecteur * A2 = NULL;
    int i;
    A2 = alloc_Vecteur(n-2);
    init_Vecteur(A2);
    A2->tab[0] = (-eps/(h*h)) * u0;
    for (i=1; i<=(n-2); i++){
        A2->tab[i] = 0.0;
    }
    A2->tab[n-3] = (-eps/(h*h)) * u1;

    return A2;
}

Vecteur * assemblage_F(int n){
    Vecteur * F = NULL;
    int i;
    F = alloc_Vecteur(n-2);
    for (i=0; i<(n-2); i++){
        F->tab[i] = 0.0;
    }
    return F;
}

Vecteur * assemblage_B(float u0, float u1, float eps, float h, int n){
    Vecteur * B = NULL;
    int i;
    B = alloc_Vecteur(n-2);
    B->tab[0] = 0.0 - (-eps/(h*h)) * u0;
    for (i=1; i<=(n-2); i++){
        B->tab[i] = 0.0;
    }
    B->tab[n-3] = 0.0 - (-eps/(h*h)) * u1;

    return B;

}

Vecteur * Solveur_LU_TriDiagonal(double ** A, Vecteur * b){
    Vecteur * y = NULL;
    Vecteur * x = NULL;
    double ** L = NULL;
    double ** U = NULL;
    int n = b->dim;

    y = alloc_Vecteur(n);
    x = alloc_Vecteur(n);
    init_Vecteur(y);
    init_Vecteur(x);
    L = alloc_Matrice(n);
    U = alloc_Matrice(n);
    init_matrice(L,n,n);
    init_matrice(U,n,n);

    factorisation_LU(A,L,U,n);
    /*affiche_matrice("L : ",L,n,n);*/
    /*affiche_matrice("U : ",U,n,n);*/

    y = descente(L,b,n);
    /*affiche_Vecteur("y = ",y);*/
    x = remontee(U,y,n);
    /*affiche_Vecteur("x = ",x);*/

    free_vecteur(y);
    free_matrice(L,n);
    free_matrice(U,n);

    return x;
}

float Solveur_MDF_1D(int N){ /* retourne einf pour N donné... */
    double ** A = NULL;
    Vecteur * B = NULL;
    Vecteur * Uh = NULL;
    Vecteur * U = NULL;
    Vecteur * y = NULL;
    int i;
    int n = (N+1.0);
    float L = 1.0;
    float h = L/N;
    float eps = pow(10,-5);
    float u0 = 1.0;
    float u1 = 0.0;
    float einf = 0.0;
    FILE *f2;
    FILE *f3;

    U = alloc_Vecteur(n-2);
    init_Vecteur(U);
    y = alloc_Vecteur(n-2);
    init_Vecteur(y);

    /* Assemblage de la matrice A */
    A = assemblage_A(eps,h,n);
    /*affiche_matrice("A1 : ", A1, n-2, n-2);*/

    B = assemblage_B(u0,u1,eps,h,n);
    /*affiche_Vecteur("B : ", B);*/

    /* Resolution du systeme Uh? \ A1*Uh = B */
    f2 = fopen("./Uh.txt","w+"); /* ouverture ou creation + ecriture en supprimant dans Uh.txt */
    Uh = Solveur_LU_TriDiagonal(A,B);
    for (i=0; i<(n-2); i++){
        fprintf(f2,"%e\n",Uh->tab[i]);
    }
    fclose(f2);
    /*affiche_Vecteur("Uh : ", Uh);*/

    /* Solution analytique du probleme */
    for (i=0; i<(n-2); i++){
        U->tab[i] = (1.0/(1.0-exp(-2.0/sqrt(eps)))) * exp(-((i+1)*(1.0/(double)N))/sqrt(eps)) + (1.0/(1.0-exp(2.0/sqrt(eps)))) * exp(((i+1)*(1.0/(double)N))/sqrt(eps));
    }
    f2 = fopen("./U.txt","w+"); /* ouverture ou creation + ecriture en supprimant dans U.txt */
    f3 = fopen("./X.txt","w+"); /* ouverture ou creation + ecriture en supprimant dans X.txt */
    for (i=0; i<(n-2); i++){
        fprintf(f2,"%e\n",U->tab[i]);
        fprintf(f3,"%f\n",(i+1)*h);
    }
    fclose(f2);
    fclose(f3);
    /*affiche_Vecteur("U : ", U);*/

    /* Calcul de l'erreur ||U - Uh||_inf */
    for (i=0; i<(n-2); i++){
        y->tab[i] = U->tab[i] - Uh->tab[i] ;
    }
    /*affiche_Vecteur("y : ", y);*/

    einf = NORM_INF(y);

    /*free_matrice(A,n-2);
    free_vecteur(B);
    free_vecteur(Uh);
    free_vecteur(U);
    free_vecteur(y);*/

    return einf;
}

/* ------ PROGRAMME PRINCIPAL ------- */

 int main(){
    int N = 0;
    int nbl = 0;
    Vecteur * NI = NULL;
    Vecteur * einf = NULL;
    int nbNI = 0;
    int i=0;
    FILE *f1;
    FILE *f2;

    printf("\n\n ------------->> DEBUT DU PROGRAMME SOLVEURMDF <<--------------\n\n");

    /* Lecture du fichier NI.txt pour stocker les intervalles dans NI[] */
    f1 = fopen("./NI.txt","r"); /* lecture du fichier NI.txt */
    if (f1==NULL) {printf("Fichier Inexistant!\n");exit(1);}
    NI = alloc_Vecteur(4);
    while ( 1 ) {
        nbl = fscanf(f1,"%d",&N); /* :lecture(s) */
        if ( nbl==EOF ){break;}
        NI->tab[nbNI] = N;
        nbNI = nbNI + 1; /*nb d'intervalles*/
    }
    fclose(f1);

    /* On calcule einf = Solveur_MDF_1D pour chaque intervalle qui se trouve dans NI[] */
    f2 = fopen("./Convergence.txt","w+"); /* ouverture ou creation + ecriture en supprimant dans Convergence.txt */
    einf = alloc_Vecteur(4);
    for (i=0; i<nbNI; i++){
        einf->tab[i] = Solveur_MDF_1D((int)NI->tab[i]);
        printf("N = %d  h = %f  einf = %e \n",(int)NI->tab[i], 1.0/NI->tab[i], einf->tab[i]);
        fprintf(f2,"%d %f %e \n",(int)NI->tab[i], 1.0/NI->tab[i], einf->tab[i]);
    }

    printf("\n ------------->> FIN DU PROGRAMME SOLVEURMDF <<--------------\n\n");

    return 0;
}
