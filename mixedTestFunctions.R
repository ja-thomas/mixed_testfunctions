#testfunctions with mixed space
################################################################################
# functions (Liang) Bachelor thesis Schork and repo bernd_bischl-2015_gowerkrig

library(soobench)
ackley1 = ackley_function(1)
rastrigin1 = rastrigin_function(1)
rastrigin2 = rastrigin_function(2)
weierstrass1 = weierstrass_function(1)


f2 = function(x) {
  D = length(x)
  b = seq(from = 0, to = (D - 1), by = 1) / D
  b = 1 ^ (1 * b)
  x.tr = 5 * x + (1 - x) * (-5)  
  out = sum(x.tr^2 * b) - 2.75
  return(out)
}



funs = list(
  
  ######################################
  # 2 dimensional without dependencies --> Bachelor thesis Schork
  
  mixedAckley = generateMixedTestFunction("mixed",
                                          list(function(x) 1/10 * ackley1(60 * (x + 0.1) - 30),
                                               function(x) 1/15 * ackley1(60 * (x - 0.1) - 30) + 0.1),
                                          name = "mixedAckley"),
  
  mixedQian = generateMixedTestFunction("mixed",
                                        list(function(x) exp(1.4 * x) * cos(7 * pi * x/2) + 13.5746,
                                             function(x) exp(3 * x) * cos(7 * pi * x/2) + 13.5746),
                                        name = "mixedQian"),
  
  mixedQuadraticLiang = generateMixedTestFunction("mixed",
                                             list(function(x) 6 * (x - 0.5)^2 + 0.5,
                                                  function(x) -6 * (x - 0.5)^2 + 1.5),
                                             name = "mixedQuadraticLiang"),
  
  mixedRastrigin = generateMixedTestFunction("mixed",
                                             list(function(x) 1/5 * rastrigin1(5 * (x - 0.5)),
                                                  function(x) 1/8 * rastrigin1(5 * (x - 0.5) + 0.1)),
                                             name = "mixedRastrigin"),
  
  mixedWeierstrassRastrigin = generateMixedTestFunction("mixed",
                                                        list(function(x) weierstrass1(4 * (x - 0.3) - 2) + 0.2,
                                                             function(x) 1/5 * rastrigin1(4 * (x - 0.3) - 2)),
                                                        name = "mixedWeierstrassRastrigin"),
  
  ########################################
  # more dimensional without dependencies --> repo Bernd
  
  ComplexFunction1 = generateMixedTestFunction(type = "mixedComplex1",
                                          list(function(x) 0.5 * x[1]^2 * x[2]^2 - 0.5 * x[1]^3 + 0.2 * x[2]^3 + 0.17 * x[1] ^2 - x[2]^2 - x[1] * x[2] * 0.5 + 0.5 * x[1] + 0.3 * x[2] - 0.45,
                                               function(x) (0.5 * x[1]^2 * x[2]^2 - 0.5 * x[1]^3 + 0.2 * x[2]^3 + 0.17 * x[1] ^2 - x[2]^2 - x[1] * x[2] * 0.5 + 0.5 * x[1] + 0.3 * x[2] - 0.45) * 0.9,
                                               function(x) (0.5 * x[1]^2 * x[2]^2 - 0.5 * x[1]^3 + 0.2 * x[2]^3 + 0.17 * x[1] ^2 - x[2]^2 - x[1] * x[2] * 0.5 + 0.5 * x[1] + 0.3 * x[2] - 0.45) * 1.05,
                                               function(x) stop("not turned on"),#0.5 * x[1]^3 - 0.25 * x[1]^2 - 0.025 * x[2] - 0.86,
                                               function(x) 0.5 * x[1]^3 - 0.25 * x[1]^2 - 0.025 * x[2] - 0.86 + 0.04,
                                               function(x) (((15 * x[2] - 5.1 * (15 * x[1] - 5)^2/(4 * pi^2) + 5 * (15 * x[1] - 5)/pi - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(15 * x[1] - 5) + 10) - 54.8104) / 51.9496,
                                               function(x) 0.5 * x[1]^2 * x[2]^2 - 0.5 * x[1]^3 + 0.2 * x[2]^3 + 0.17 * x[1] ^2 - x[2]^2 - x[1] * x[2] * 0.5 + 0.5 * x[1] + 0.3 * x[2] - 0.45 + 0.03,
                                               function(x) stop("not turned on")#(0.5 * x[1]^3 - 0.25 * x[1]^2 - 0.025 * x[2] - 0.86) * 0.9
                                          ), name = "ComplexFunction1"),
  
  ComplexFunction2Alpine = generateMixedTestFunction(type = "mixedComplex2",
                                                list(function(x) prod(sqrt(x * 10) * sin(x * 10)),
                                                     function(x) sum(abs((20 * x - 10) * (sin(20 * x - 10) + 0.1))) - 370,
                                                     function(x) sum(abs((20 * x - 10) * (sin(20 * x - 10) + 0.1))) - 375,
                                                     function(x) (prod(sqrt(x * 10) * sin(x * 10))) * 0.97,
                                                     function(x) sum(abs((20 * x - 10) * (sin(20 * x - 10) + 0.1))) - 365,
                                                     function(x) (prod(sqrt(x * 10) * sin(x * 10))) * 0.99
                                                ), name = "ComplexFunction2Alpine"),
  
  # Repo fun nr. 4:
  ComplexFunction4Ellipsoid = generateMixedTestFunction(type = "mixedComplex3",
                                                   list(function(x) f2(x),
                                                        function(x) f2(x) + 0.2,
                                                        function(x) f2(x) * 0.90,
                                                        function(x) f2(x) + 0.25,
                                                        function(x) f2(x) + 0.5,
                                                        function(x) f2(x) * 0.8,
                                                        function(x) f2(x) * 0.5,
                                                        function(x) f2(x) + 0.8,
                                                        function(x) f2(x) + 0.9,
                                                        function(x) f2(x) * 0.5,
                                                        function(x) f2(x) + 1,
                                                        function(x) f2(x) + 1.25
                                                   ), name = "ComplexFunction4Ellipsoid"),
  
  # Repo fun nr. 5:
  ComplexFunction4EllipsoidVariant2 = generateMixedTestFunction(type = "mixedComplex4",
                                                           funs = list(
                                                                function(x) f2(x),
                                                                function(x) f2(x) + 0.2,
                                                                function(x) f2(x) * 0.90,
                                                                function(x) f2(x) + 0.25,
                                                                function(x) f2(x) + 0.5,
                                                                function(x) f2(x) * 0.8,
                                                                function(x) f2(x) * 0.5,
                                                                function(x) f2(x) + 0.8,
                                                                function(x) f2(x) + 0.9,
                                                                function(x) f2(x) * 0.5,
                                                                function(x) f2(x) + 1,
                                                                function(x) f2(x) + 1.25
                                                           ), name = "ComplexFunction4EllipsoidVariant2"),
  
  #####################
  # with dependencies in mixedTestFunctions_depend.R
  
  # oneDependentQuadraticLiang = generateMixedTestFunction("oneDependentLiang",
  #                                                   list(function(x1, x2) (x1 - 0.3)^2 + (x2 - 0.7)^2,
  #                                                        function(x) -(x - 0.5)^2 + 0.3),
  #                                                   name = "oneDependentQuadraticLiang"),
  # 
  # oneDependentRastrigin = generateMixedTestFunction("oneDependentLiang",
  #                                                   list(function(x1, x2) 1/10 * rastrigin2(3 * (x1 - 0.5), 3 * (x2 - 0.5)),
  #                                                        function(x) 1/10 * rastrigin1(3 * (x - 0.5)) + 0.1),
  #                                                   name = "oneDependentRastrigin"),
  # 
  
  
  
  
  #####################
  # with dependencies # # and some functions without dependences to compare # repo bischl
  
  mixedLinear = generateMixedTestFunction("mixed",
                                     list(function(x) x*2-1, function(x) 1-x),
                                     name = "mixedLinear"),
  
  mixedQuadraticRepoB = generateMixedTestFunction("mixed",
                                        list(function(x) 6*(x-0.5)^2 - 0.5, function(x) -6*(x-0.5)^2  + 0.5),
                                        name = "mixedQuadraticRepoB"),
  
  mixedTrigo = generateMixedTestFunction("mixed",
                                    list(function(x) sin(x*2*pi), function(x) cos(x*2*pi)),
                                    name = "mixedTrigo")
  
  # in mixedTestFunctions_depend.R
  # oneDependentLinear = generateMixedTestFunction("oneDependent",
  #                                           list(function(x) x*2-1, function(x) 1-x),
  #                                           name = "oneDependentLinear"),
  # 
  # oneDependentTrigo = generateMixedTestFunction("oneDependent",
  #                                          list(function(x) sin(x*2*pi)/1.5, function(x) cos(x*2*pi)/1.5),
  #                                          name = "oneDependentTrigo"),
  # 
  # oneDependentQuadratic = generateMixedTestFunction("oneDependent",
  #                                              list(function(x) (x-0.5)^2 + 0.5, function(x)(x-0.5)^2 - 0.5),
  #                                              name = "oneDependentQuadratic"),
  # 
  # twoDependentLinear = generateMixedTestFunction("twoDependent",
  #                                           list(function(x) x*2-1, function(x) 1-x),
  #                                           name = "twoDependentLinear"),
  # 
  # twoDependentTrigo = generateMixedTestFunction("twoDependent",
  #                                          list(function(x) sin(x*2*pi), function(x) cos(x*2*pi)),
  #                                          name = "twoDependentTrigo"),
  # 
  # twoDependentQuadratic = generateMixedTestFunction("twoDependent",
  #                                              list(function(x) (x-0.5)^2 + 0.5, function(x)(x-0.5)^2 - 0.5),
  #                                              name = "twoDependentQuadratic")
  # 
  

  )

################################################################################
################################################################################
# other functions (not used):
#
# mixedPoly = generateMixedTestFunction("mixed", 
#                                  list( function(x) (x-0.3)^2*(x+2)*(x+4)*(x+0.1), function(x) (x+0.2)^2*(x-1.1)^2),
#                                  name = "mixedPoly"),
# 
# twoDependentPoly = generateMixedTestFunction("twoDependent",
#                                         list( function(x) (x-0.3)^2*(x+2)*(x+4)*(x+0.1), function(x) (x+0.2)^2*(x-1.1)^2),
#                                         name = "twoDependentPoly"),
# 
# mixedTrigo2 = generateMixedTestFunction("mixed", 
#                                    list(function(x)  cos(5*x^2)+1, function(x) x^2*cos(0.5*x*pi)+0.1),
#                                    name = "mixedTrigo2"),
# 
# twoDependentTrigo2 = generateMixedTestFunction("twoDependent", 
#                                           list(function(x)  cos(5*x^2)+1, function(x) x^2*cos(0.5*x*pi)+0.1),
#                                           name = "twoDependentTrigo2"),
# 
# mixedTrigo3 = generateMixedTestFunction("mixed",
#                                    list(function(x) sin(6*(x-0.5)^2+6*(x-0.5)+1)+1, function(x) sin(x)*tan(x)+0.1),
#                                    name = "mixedTrigo3"),
# 
# twoDependentTrigo3 = generateMixedTestFunction("twoDependent",
#                                           list(function(x) sin(6*(x-0.5)^2+6*(x-0.5)+1)+1, function(x) sin(x)*tan(x)+0.1),
#                                           name = "twoDependentTrigo3")

# mixedTrigQuad = generateMixedTestFunction("mixed",
#                                      list(function(x) sin(sin(x+0.4)+tan(x+0.4))+1, function(x) 4*(x-0.5)^2+0.1),
#                                      name = "mixedTrigQuad"),
# 
# twoDependentTrigQuad = generateMixedTestFunction("twoDependent",
#                                             list(function(x) sin(sin(x+0.4)+tan(x+0.4))+1, function(x) 4*(x-0.5)^2+0.1),
#                                             name = "twoDependentTrigQuad")

# mixedExpQuad = generateMixedTestFunction("mixed", 
#                                     list(function(x)(x-0.2)^2*exp(5*(x-1.2)^2), function(x) (x-0.4)^2+0.1)),
# 
# twoDependentExpQuad = generateMixedTestFunction("twoDependent", 
#                                            list(function(x)(x-0.2)^2*exp(5*(x-1.2)^2), function(x) (x-0.4)^2+0.1)),
# 
# mixedAbs = generateMixedTestFunction("mixed", 
#                                 list(function(x) abs(sin(3*(x-0.3))), function(x) abs(tan((x-0.7)))+0.1)),
# 
# twoDependentAbs = generateMixedTestFunction("twoDependent", 
#                                        list(function(x) abs(sin(3*(x-0.3))), function(x) abs(tan((x-0.7)))+0.1))


################################################################################
# other functions Schork:
# 2 dimensional mixed test-functions ###########################################
# 1 numeric and 1 discrete parameter without dependencies 

# # min = 0
# mixedPoly = generateMixedTestFunction("mixed", 
#                                  list( function(x) (x-0.3)^2*(x+2)*(x+4)*(x+0.1), function(x) (x+0.2)^2*(x-1.1)^2))
# 
# # min = 0
# mixedTrigo2 = generateMixedTestFunction("mixed", 
#                                    list(function(x)  cos(5*x^2)+1, function(x) x^2*cos(0.5*x*pi)+0.1))
# 
# 
# # min = 0 (funciton1)
# mixedTrigQuad = generateMixedTestFunction("mixed",
#                                      list(function(x) sin(sin(x+0.4)+tan(x+0.4))+1, function(x) 4*(x-0.5)^2+0.1))
# 
# # min =  0 (function2)
# mixedAbs = generateMixedTestFunction("mixed", 
#                                 list(function(x) abs(sin(3*(x-0.3))), function(x) abs(tan((x-0.7)))+0.1))


  
################################################################################
################################################################################