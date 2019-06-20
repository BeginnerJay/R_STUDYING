print("hello, world!")
a = 1
b = 2
print(a+b*2)

# installing / loading the package

if(!require(installr)) {
  install.packages("installr")
  require(installr)
}
updateR()
