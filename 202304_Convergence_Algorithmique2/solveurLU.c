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
            printf(" %10f ", M[i][j]);
        }
        printf("]");
    }
    printf("\n");
}

void affiche_Vecteur(char message[256], Vecteur * V){
    int i;

    printf("\n--> %s",message);
    printf("[ ");
    for (i=0; i<(V->dim); i++){
        printf("%f ",V->tab[i]); /* afficher chaque composante */
    }
    printf("] \n");
}

Vecteur *alloc_Vecteur(int n){
    Vecteur *V = NULL;
    V = (Vecteur *)malloc(sizeof(Vecteur)); /*reserve de la memoire necessaire*/
    V -> dim = n; /* ou (*V).dim = n */ /*on affecte la valeur n pour dim de Vecteur*/
    V -> tab = (double *)calloc(n,sizeof(double));
    if (V->tab == NULL){ printf("! Pas de place en mémoire dans alloc_vecteur => SORTIE!"); exit(1);}

    return V;
}

double ** alloc_Matrice(int i, int j){
    double ** M = NULL;
    int p;

    M = (double **)malloc(i*sizeof(double *));
    for (p=0; p<i; p++){
        M[p]= (double *)malloc(j*sizeof(double));
    }

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

Vecteur * free_Vecteur(Vecteur *V){ /*liberer la memoire occupee*/
    free(V->tab);
    free(V);

    return NULL;
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

double NORM_INF(Vecteur *x){ /* ||x||_Inf = max (|x_i|) pour i allant de 1 à n */
    double norminf = 0.0;
    int i;
    int n = (x->dim);

    for (i=0; i<n; i++){
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

Vecteur * descente(double ** L, Vecteur * b, int n){
  int i;
  Vecteur * y = NULL;

  y = alloc_Vecteur(n);
  y->tab[0] = b->tab[0];
  for (i=1; i<n; i++){
      y->tab[i] = b->tab[i] - y->tab[i-1] * L[i][i-1];
  }

  return y;
}

Vecteur * remontee(double ** U, Vecteur * y, int n){
  int i;
  Vecteur * x = NULL;

  x = alloc_Vecteur(n);
  x->tab[n-1] = y->tab[n-1] / U[n-1][n-1];
  for (i=n-2; i>=0; i--){
    x->tab[i] = (y->tab[i] - U[i][i+1] * x->tab[i+1]) / U[i][i];
  }

  return x;
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
    L = alloc_Matrice(n,n);
    U = alloc_Matrice(n,n);
    factorisation_LU(A,L,U,n);
    y = descente(L,b,n);
    x = remontee(U,y,n);
    y = free_Vecteur(y);
    L = NULL;
    U = NULL;

    return x;
}

/* ------ PROGRAMME PRINCIPAL ------- */

 int main(){
    double ** A = NULL;
    Vecteur * b = NULL;
    Vecteur * x = NULL;
    Vecteur * y = NULL;
    int i,j;
    double res;
    int n = 4;

    printf("\n\n####################### DEBUT DU PROGRAMME ###############################\n\n");
    A = alloc_Matrice(n,n);
    A[0][0] = 2.0; A[0][1] = -1.0 ; A[0][2] = 0.0; A[0][3] = 0.0;
    A[1][0] = -1.0; A[1][1] = 4.0 ; A[1][2] = -1.0; A[1][3] = 0.0;
    A[2][0] = 0.0; A[2][1] = -1.0 ; A[2][2] = 4.0; A[2][3] = -1.0;
    A[3][0] = 0.0; A[3][1] = 0.0 ; A[3][2] = -1.0; A[3][3] = 2.0;
    affiche_matrice("A = ",A,n,n);

    b = alloc_Vecteur(n);
    affecte_Vecteur(0.0,b,1);
    affecte_Vecteur(4.0,b,2);
    affecte_Vecteur(6.0,b,3);
    affecte_Vecteur(5.0,b,4);
    affiche_Vecteur("b = ", b);

    x = alloc_Vecteur(n);
    init_Vecteur(x);
    x = Solveur_LU_TriDiagonal(A,b);

    affiche_Vecteur("Solution de x? / Ax = b : x = ",x);

    A[0][0] = 2.0; A[0][1] = -1.0 ; A[0][2] = 0.0; A[0][3] = 0.0;
    A[1][0] = -1.0; A[1][1] = 4.0 ; A[1][2] = -1.0; A[1][3] = 0.0;
    A[2][0] = 0.0; A[2][1] = -1.0 ; A[2][2] = 4.0; A[2][3] = -1.0;
    A[3][0] = 0.0; A[3][1] = 0.0 ; A[3][2] = -1.0; A[3][3] = 2.0;

    y = alloc_Vecteur(n);
    init_Vecteur(y);

    for (i=0; i<n; i++){
        res = 0.0;
        for (j=0; j<n; j++){
            res = res + A[i][j] * x->tab[j];
        }
        y->tab[i] = res - b->tab[i];
    }

    printf("\n--> Residu du systeme : ||Ax - b||_inf = %e ",NORM_INF(y));

    free_matrice(A,n);
    free_Vecteur(b);
    free_Vecteur(x);
    /*free_Vecteur(y);*/

    printf("\n\n####################### FIN DU PROGRAMME ###############################\n\n");
    return 0;
}
