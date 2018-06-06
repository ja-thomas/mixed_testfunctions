# generate mixed testfunctions
# mostly from bitbucket repository bernd_bischl-2015_gowerkrig
################################################################################
generateMixedTestFunction = function(type, funs, name){
  
  if(type == "mixed"){
    fun = function(x)
      ifelse(x$disc1 == "a", funs[[1]](x$num1), funs[[2]](x$num1))
    ps = makeParamSet(
      makeDiscreteParam("disc1", values = c("a", "b")),
      makeNumericParam("num1", lower = 0, upper = 1)
    )
  }
  
  if(type == "mixedComplex1"){
    require(DiceOptim)
    fun = function(x) {
      tmp = numeric(0)
      if(x$disc1 == "a" && x$disc2 == "d")
        tmp = funs[[1]](x$vec.num1)
      if(x$disc1 == "a" && x$disc2 == "e")
        tmp = funs[[2]](x$vec.num1)
      if(x$disc1 == "a" && x$disc2 == "f")
        tmp = funs[[3]](x$vec.num1)
      if(x$disc1 == "a" && x$disc2 == "g")
        tmp = funs[[4]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "d")
        tmp = funs[[5]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "e")
        tmp = funs[[6]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "f")
        tmp = funs[[7]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "g")
        tmp = funs[[8]](x$vec.num1)
      tmp
    }
    # the value of the parameter dic2 = "g" is not turned on!
    ps = makeParamSet(
      makeDiscreteParam("disc1", values = c("a", "b")),
      makeDiscreteParam("disc2", values = c("d", "e", "f")),
      makeNumericVectorParam("vec.num1", len = 2, lower = c(0, 0), upper = c(1, 1))
    )
  }
  
  if(type == "mixedComplex2"){
    require(DiceOptim)
    fun = function(x) {
      tmp = numeric(0)
      if(x$disc1 == "a" && x$disc2 == "d")
        tmp = funs[[1]](x$vec.num1)
      if(x$disc1 == "a" && x$disc2 == "e")
        tmp = funs[[2]](x$vec.num1)
      if(x$disc1 == "a" && x$disc2 == "f")
        tmp = funs[[3]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "d")
        tmp = funs[[4]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "e")
        tmp = funs[[5]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "f")
        tmp = funs[[6]](x$vec.num1)
      tmp
    }
    ps = makeParamSet(
      makeDiscreteParam("disc1", values = c("a", "b")),
      makeDiscreteParam("disc2", values = c("d", "e", "f")),
      makeNumericVectorParam("vec.num1", len = 6, lower = rep(0, 6), upper = rep(1, 6))
    )
  }
  
  if(type == "mixedComplex3"){
    require(DiceOptim)
    fun = function(x) {
      tmp = numeric(0)
      if(x$disc1 == "a" && x$disc2 == "d" && x$disc3 == "f")
        tmp = funs[[1]](x$vec.num1)
      if(x$disc1 == "a" && x$disc2 == "d" && x$disc3 == "g")
        tmp = funs[[2]](x$vec.num1)
      if(x$disc1 == "a" && x$disc2 == "e" && x$disc3 == "f")
        tmp = funs[[3]](x$vec.num1)
      if(x$disc1 == "a" && x$disc2 == "e" && x$disc3 == "g")
        tmp = funs[[4]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "d" && x$disc3 == "f")
        tmp = funs[[5]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "d" && x$disc3 == "g")
        tmp = funs[[6]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "e" && x$disc3 == "f")
        tmp = funs[[7]](x$vec.num1)
      if(x$disc1 == "b" && x$disc2 == "e" && x$disc3 == "g")
        tmp = funs[[8]](x$vec.num1)
      if(x$disc1 == "c" && x$disc2 == "d" && x$disc3 == "f")
        tmp = funs[[9]](x$vec.num1)
      if(x$disc1 == "c" && x$disc2 == "d" && x$disc3 == "g")
        tmp = funs[[10]](x$vec.num1)
      if(x$disc1 == "c" && x$disc2 == "e" && x$disc3 == "f")
        tmp = funs[[11]](x$vec.num1)
      if(x$disc1 == "c" && x$disc2 == "e" && x$disc3 == "g")
        tmp = funs[[12]](x$vec.num1)
      tmp
    }
    ps = makeParamSet(
      makeDiscreteParam("disc1", values = c("a", "b", "c")),
      makeDiscreteParam("disc2", values = c("d", "e")),
      makeDiscreteParam("disc3", values = c("f", "g")),
      makeNumericVectorParam("vec.num1", len = 5, lower = rep(0, 5), upper = rep(1, 5))
    )
  }
  
  if(type == "mixedComplex4"){
    require(DiceOptim)
    fun = function(x) {
      tmp = numeric(0)
      if(x$disc1 == "a")
        tmp = funs[[1]](x$vec.num1)
      if(x$disc1 == "b")
        tmp = funs[[2]](x$vec.num1)
      if(x$disc1 == "c")
        tmp = funs[[3]](x$vec.num1)
      if(x$disc1 == "d")
        tmp = funs[[4]](x$vec.num1)
      if(x$disc1 == "e")
        tmp = funs[[5]](x$vec.num1)
      if(x$disc1 == "f")
        tmp = funs[[6]](x$vec.num1)
      if(x$disc1 == "g")
        tmp = funs[[7]](x$vec.num1)
      if(x$disc1 == "h")
        tmp = funs[[8]](x$vec.num1)
      if(x$disc1 == "k")
        tmp = funs[[9]](x$vec.num1)
      if(x$disc1 == "l")
        tmp = funs[[10]](x$vec.num1)
      if(x$disc1 == "m")
        tmp = funs[[11]](x$vec.num1)
      if(x$disc1 == "n")
        tmp = funs[[12]](x$vec.num1)
      tmp
    }
    ps = makeParamSet(
      makeDiscreteParam("disc1", values = c("a", "b", "c", "d", "e", "f", "g", "h", "k", "l", "m", "n")),
      makeNumericVectorParam("vec.num1", len = 5, lower = rep(0, 5), upper = rep(1, 5))
    )
  }
  
  if(type == "oneDependent"){
    fun = function(x)
      funs[[1]](x$num2) + ifelse(x$disc1 == "a", funs[[2]](x$num1), 0)
    ps = makeParamSet(
      makeDiscreteParam("disc1", values = c("a", "b")),
      makeNumericParam("num1", lower = 0, upper = 1,
                       requires = quote(disc1 == "a")),
      makeNumericParam("num2", lower = 0, upper = 1)
    )
  }
  
  if(type == "twoDependent"){
    fun = function(x)
      ifelse(x$disc1 == "a", funs[[1]](x$num1), funs[[2]](x$num2))
    ps = makeParamSet(
      makeDiscreteParam("disc1", values = c("a", "b")),
      makeNumericParam("num1", lower = 0, upper = 1,
                       requires = quote(disc1 == "a")),
      makeNumericParam("num2", lower = 0, upper = 1,
                       requires = quote(disc1 == "b"))
    )
  }
  
  if(type == "oneDependentLiang") {
    fun = function(x)
      ifelse(x$disc1 == "a", funs[[1]](x1 = x$num1, x2 = x$num2), funs[[2]](x$num1))
    ps = makeParamSet(
      makeDiscreteParam("disc1", values = c("a", "b")),
      makeNumericParam("num1", lower = 0, upper = 1),
      makeNumericParam("num2", lower = 0, upper = 1, 
                       requires = quote(disc1 == "a"))
    )
  }
 

  makeSingleObjectiveFunction(name = name, fn = fun, par.set = ps, has.simple.signature = FALSE)
}

