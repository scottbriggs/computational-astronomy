
# Calculate the obliquity of the ecliptic in radians using the formula of Laskar 
# which is good for 10,000 years on either side of J2000
# jd is the julian day number
# nut_angles are the nutation in longitude and obliquity angles in radians
Obliquity <- function(jd, nut_angles)
{
  T <- (jd - EPOCHJ2000) / DAYSJULCENT
  U <- T / 100
  term1 <- DMSToDeg(23, 26, 21.448)
  term2 <- DMSToDeg(0, 0, -4680.93)
  term3 <- DMSToDeg(0, 0, -1.55)
  term4 <- DMSToDeg(0, 0, 1999.25)
  term5 <- DMSToDeg(0, 0, -51.38)
  term6 <- DMSToDeg(0, 0, -249.67)
  term7 <- DMSToDeg(0, 0, -39.05)
  term8 <- DMSToDeg(0, 0, 7.12)
  term9 <- DMSToDeg(0, 0, 27.87)
  term10 <- DMSToDeg(0, 0, 5.79)
  term11 <- DMSToDeg(0, 0, 2.45)
  
  mean_obliquity <- term1 + U * (term2 + U * (term3 + U * (term4 + U * (
    term5 + U * (term6 + U * (term7 + U * (term8 + U * (term9 + U * (
      term10 + U * term11)))))))))
  
  mean_obliquity <- mean_obliquity * DEG2RAD
  true_obliquity <- mean_obliquity + nut_angles[[2]][[1]]
  
  obliq <- c(mean_obliquity, true_obliquity)

  return(obliq)
}