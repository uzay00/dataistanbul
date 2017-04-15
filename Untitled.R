# Uzay Çetin

########################################################
# Amaç, istatistik kullanarak
#     pi'nin değerini hesaplamak
########################################################
# N adet nokta üretir
piR <- function(N) {
  x <- runif(N,0,1) # Her noktanın x bileşeni
  y <- runif(N,0,1) # Her noktanın y bileşeni
  
  # orijine olan mesafe
  d <- sqrt(x^2 + y^2) 
  # mesafe, r=1'den küçükse, nokta pi/4'lük alan içinde kalır
  return(4*sum(d<1.0)/N) # pi/4'lük alan içinde kalan nokta sayısı
}
set.seed(7) #for reproducibility
cat(piR(1000),piR(10000),piR(100000),piR(1000000))