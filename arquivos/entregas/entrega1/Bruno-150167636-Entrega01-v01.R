for (A in 1:3){
  r = 2
  volume_hiperesfera <- (pi^(r/2) * A^r) / gamma(r/2 + 1)
  volume_hipercubo <- (2*A)^r
  prop = (volume_hipercubo-volume_hiperesfera)/volume_hipercubo
  print("A Proporção do volume do hipercubo que está fora da hiperesfera para r = 2 fixo")
  print(paste("e A =",A,"é:", prop))
}

for (r in seq(2,34,2)){
  A = 1
  volume_hiperesfera <- (pi^(r/2) * A^r) / gamma(r/2 + 1)
  volume_hipercubo <- (2*A)^r
  prop = (volume_hipercubo-volume_hiperesfera)/volume_hipercubo
  print("A Proporção do volume do hipercubo que está fora da hiperesfera para A = 1 fixo")
  print(paste("e r =",r,"é:", prop))
}
