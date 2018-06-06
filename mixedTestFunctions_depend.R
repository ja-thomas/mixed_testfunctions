#testfunctions with mixed space and dependencies
################################################################################
library(soobench)
rastrigin1 = rastrigin_function(1)
rastrigin2 = rastrigin_function(2)

# functions (Liang) aus Bachelor thesis Schork und repo bernd_bischl-2015_gowerkrig
funs = list(
  
  ### Schork
  oneDependentQuadraticLiang = generateMixedTestFunction("oneDependentLiang",
                                                     list(function(x1, x2) (x1 - 0.3)^2 + (x2 - 0.7)^2,
                                                          function(x) -(x - 0.5)^2 + 0.3),
                                                     name = "oneDependentQuadraticLiang"),
   
  oneDependentRastrigin = generateMixedTestFunction("oneDependentLiang",
                                                     list(function(x1, x2) 1/10 * rastrigin2(3 * (x1 - 0.5), 3 * (x2 - 0.5)),
                                                          function(x) 1/10 * rastrigin1(3 * (x - 0.5)) + 0.1),
                                                     name = "oneDependentRastrigin"),
   
  ### Bernd
  oneDependentLinear = generateMixedTestFunction("oneDependent",
                                             list(function(x) x*2-1, function(x) 1-x),
                                             name = "oneDependentLinear"),
   
  oneDependentTrigo = generateMixedTestFunction("oneDependent",
                                            list(function(x) sin(x*2*pi)/1.5, function(x) cos(x*2*pi)/1.5),
                                            name = "oneDependentTrigo"),
   
  oneDependentQuadratic = generateMixedTestFunction("oneDependent",
                                               list(function(x) (x-0.5)^2 + 0.5, function(x)(x-0.5)^2 - 0.5),
                                               name = "oneDependentQuadratic"),
  
  twoDependentLinear = generateMixedTestFunction("twoDependent",
                                             list(function(x) x*2-1, function(x) 1-x),
                                             name = "twoDependentLinear"),
  
  twoDependentTrigo = generateMixedTestFunction("twoDependent",
                                            list(function(x) sin(x*2*pi), function(x) cos(x*2*pi)),
                                            name = "twoDependentTrigo"),
  
  twoDependentQuadratic = generateMixedTestFunction("twoDependent",
                                                list(function(x) (x-0.5)^2 + 0.5, function(x)(x-0.5)^2 - 0.5),
                                                name = "twoDependentQuadratic")
   
  
  )

################################################################################