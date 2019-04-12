print("hello, world!")
# installing / loading the package
if(!require(installr)) {
  install.packages("installr")
  require(installr)
}
updateR()
