#dataistanbul talk - April 5th, 2017
#HS Olmez (c) 2017
# Türkçe açıklamalar: Uzay Çetin

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

#Let's plot the points in and out of the circle
par(mfrow = c(2, 2))
nc <- c(10,100,1000,10000)
for (j in 1:4) {
  xin <- vector(); yin <- vector(); xout <- vector(); yout <- vector()
  x <- runif(nc[j],-1,1) ; y <- runif(nc[j],-1,1)
  for (i in 1:nc[j]) {
    d <- sqrt(x^2 + y^2)
    if (sqrt(x[i]^2 + y[i]^2) < 1.0) {
      xin <- append(xin,x[i]) ; yin <- append(yin,y[i])
    } else {
      xout <- append(xout,x[i]) ; yout <- append(yout,y[i])
    }
  }
  par(pty="s")
  plot(xin,yin, col='red', main=paste("N =",nc[j]), xlab="x", ylab="y", pch=20, xlim=range(-1,1), ylim=range(-1,1))
  points(xout,yout, pch=20, col='blue')
}
