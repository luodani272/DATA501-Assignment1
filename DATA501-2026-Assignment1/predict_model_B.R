
predict_model_B <- function(x, par) {
  
  a <- par[1]
  b <- par[2]
  c <- par[3]
  A <- par[4]
  B <- par[5]
  freq <- par[6] 

  b*x+c +  A*sin(2*freq*pi*x) + B*cos(2*freq*pi*x) 
}
