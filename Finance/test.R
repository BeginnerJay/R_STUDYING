x < c()
y <- system.time(for(i in 100000000){
  x[i] <- i
})
print(y)

