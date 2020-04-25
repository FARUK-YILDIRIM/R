library(stats)
library(lsa)
library(scrime)
library(vegan)

# Minkowski Uzaklığının R Çözümü
minkowskiC <- c( 60,80,75,55,70,45 )
minkowskiX <- matrix( minkowskiC, byrow=TRUE, ncol=2, nrow=3 )
minkowskiSonuc <- dist( minkowskiX, method = "minkowski", p=1 )
print( minkowskiSonuc )

# Pearson Korelasyon Katsayısı ve Korelasyon Uzaklığının R Çözümü
pearsonA <- c( 55,85,70 )
pearsonB <- c( 65,55,75 )
pearsonKorelasyon <- cor( pearsonA, pearsonB, method = c("pearson") )
pearsonSonuc <- ( 1-pearsonKorelasyon ) / 2
print( pearsonSonuc )

# Öklid ve Karesel Öklid Uzaklığının R Çözümü
oklidC <- c( 60,80,75,55,70,45 )
oklidX <- matrix( oklidC, byrow=TRUE, ncol=2, nrow=3 )
oklidSonuc <- dist( oklidX, method = "euclidean" )
print( oklidSonuc )

# Manhattan (City-Block) Uzaklığının R Çözümü
manhattanC <- c( 60,80,75,55,70,45 )
manhattanX <- matrix( manhattanC, byrow=TRUE, ncol=2, nrow=3 )
manhattanSonuc <- dist( manhattanX, method = "minkowski", p=1 )
print( manhattanSonuc )

# Açısal Benzerlik (Cosine Similarity) R Çözümü
vec1 <- c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
vec2 <- c( 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0 )
cosineSonuc <- cosine(vec1,vec2) 
print ( cosineSonuc )

# Mahalanobis Uzaklığının R Çözümü
mahalanobisC <- c( 14,17,19,12,9,35,39,41,33,28 )
mahalanobisVeri <- matrix( mahalanobisC, nrow=5, ncol=2, byrow=F )
S <- var( mahalanobisVeri )
mahalanobisSonuc <- mahalanobis( mahalanobisVeri, apply(mahalanobisVeri, 2, mean), S )
print (mahalanobisSonuc )

# Basit Eşleştirme Katsayısı ve Uzaklığı R Çözümü
basitC <- c( 1,2,1,1,1,2,1,2 )
basitX <- matrix(basitC, nrow=2)
basit  <- smc( basitX )
print ( basit )
uzaklik <- smc( basitX, dist=TRUE )
print ( uzaklik )

# Binary Öklid ve Binary Karesel Öklid Uzaklığı R Çözümü
vegC <- c( 1,0,1,1,1,0,1,0 )
vegX <- matrix( vegC, nrow=2 )
binary_oklid <- vegdist( vegX,  method  =  "euclidean",  binary= TRUE )
print( binary_oklid )
binary_karesel_oklid <- binary_oklid ^ 2
print ( binary_karesel_oklid )

# Jaccard Benzerlik Katsayısı ve Uzaklığı R Çözümü
jaccardC <- c( 1,0,1,1,0,0,0,1,1,1,0,1,1,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1 )
jaccardX <- matrix (jaccardC, nrow=2)
jaccard <- vegdist(jaccardX, method = "jaccard", binary = TRUE)
print( jaccard )







