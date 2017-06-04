


markov <- function(){
  tmp = runif(1,0,1)
  if (tmp >= 0.8){
    return(1)
  }  
  else{
    return(0)
  }
}


test1 <- function(cum){
  output <- cum
  flag <- 0
  while(flag == 0){
    if (markov() == 1){
      output <- output + 1
      if (markov() == 1){
        output <- output + 1
        flag <- 1
        return(c(output, flag))
      }
      else{
        output <- output + 1
        x <- test1(output)
        outputNew <- x[1]
        flag <- x[2]
      }
    }
    else{
      output <- output + 1
      x <- test1(output)
      outputNew <- x[1]
      flag <- x[2]
    }
  }
  
  return(c(outputNew,flag))
  
}







days = c()
for (i in 1:10000){
  x = test1(0)
  days <- c(days, x[1])
}

aaa <- mean(days)




