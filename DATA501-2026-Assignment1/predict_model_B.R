
predict_model_B <- function(x, par) {
  
  b <- par[1]
  c <- par[2]
  A <- par[3]
  B <- par[4]
  freq <- par[5] 

  b*x+c +  A*sin(2*freq*pi*x) + B*cos(2*freq*pi*x) 
}
