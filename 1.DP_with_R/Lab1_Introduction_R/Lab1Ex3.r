vector=1:10
matrice1=matrix(vector, ncol=2)
matrice1
matrice2=matrix(1:10,nrow=2,byrow=T)
matrice2
m=matrix(1:4,nrow=3,ncol=3)
m
print(matrice2)
dim(matrice1)
ncol(matrice1)
nrow(matrice1)
resultat = matrice1%*%matrice2
resultat