########################################################
# Yazar: 
#     Dr. Uzay Çetin
# Amaç: 
#     istatistik kullanarak, pi'nin değerini hesaplamak
# Yöntem: 
#     hem x, hem de y-ekseni için [0,1] aralığında 
#     noktalar üretiliyor. Üretilen noktalardan, 
#     orijine olan mesafesi 1'den küçük olanlar, pi/4'lük
#     alan içinde kalır.
########################################################
piR <- function(N) { 
  # N adet nokta üretelim
  x <- runif(N,0,1) # Her noktanın x bileşeni
  y <- runif(N,0,1) # Her noktanın y bileşeni
  
  # orijine olan mesafeleri hesaplayalım
  mesafe <- sqrt(x^2 + y^2) 
  
  # mesafe, r=1'den küçükse, nokta pi/4'lük alan içinde kalır
  icerde <- mesafe<1.0
  
  # FALSE ve TRUE değerleri, 1 ve 2 indekslerine dönüştürelim
  indeks <- icerde + 1
  
  # dışarıda kalanlar kırmızı, icerde kalanlar yeşil renk olsun
  renk <- c('red', 'green')
  renkler <- renk[indeks]
  
  # Çizim
  plot(x,y, col=renkler, main=paste("N =",N), 
       xlab="x", ylab="y", pch=20, 
       xlim=range(-1,1), ylim=range(-1,1))
  
  # yatay ve düşey ekseni çizelim
  abline(v=0);abline(h=0)
  
  return(4*sum(icerde)/N) # pi/4'lük alan içinde kalan nokta sayısı
}
set.seed(7) #Tekrar aynı sayıları üretmeye yarar.
par(mfrow = c(2, 2))
cat(piR(100), piR(1000),piR(10000),piR(100000))



