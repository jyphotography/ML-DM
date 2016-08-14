# How can we calculate the area of a circle without knowing pi? (A=pi*r^2)
# One approach: Monte Carlo

# Suppose our circle has radius 1.

# Throw 1e6 darts at the circle
x = runif(1e6,-1,1)
y = runif(1e6,-1,1)

# Count proportion of darts that landed in the circle
p = sum( (x^2+y^2)<=1.0 )/1e6

# Multiply by the area of the square sampling region.

A = p*2*2
A

#--------------------------------------------------------
#--------------------------------------------------------

# Market condition simulation
# (borrowed from http://www.solver.com/monte-carlo-simulation-tutorial)
# Suppose your company is selling a product on the market
# Further, suppose the following information concerning next
# year's performance.
#  1. fixed costs are unknown but average $120000 with std $10000
#  2. unit costs are between $5.5 and $7.5, but most likely $6.5
#  3. market conditions are either hot, ok, or slow
#      hot market => 100000 units sold at $8 ea.
#       ok market =>  75000 units sold at $10 ea.
#     slow market =>  50000 units sold at $11 ea.

# Profit = volume*(retail_price-unit_cost) - fixed_cost
# What do we forcast for next year's profit?

# market conditions (assume uniform) average 75000 units at $9.66
# unit costs average $6.5
# fixed costs average $120000

# => we forcast approx. $117,500 of profit for next year.

# BUT WAIT! What does Monte Carlo have to say about this?
# Need some more assumptions....
#  suppose fixed costs follow a normal distribution
#  suppose unit costs follow a triangle distribution
#  suppose market conditions follow a discrete uniform distribution

triangle <- function(n,a,b,c){
  # generate n random samples from the triangle distribution
  # with peak at b and 0 for x<=a or x>=c
  y = runif(n)
  A1 = 0.5*(b-a)
  A = A1+0.5*(c-b)
  x = ifelse(y<A1/A,
      a+sqrt(2*A*y*(b-a)),
      c-sqrt(2*A*(1-y)*(c-b)) )
  return(x)
}

SimulateMarket <- function(n){
  # generate n values of profit under random 
  # choices for market conditions

  fixed_cost = rnorm(n,120000,10000) # fixed costs are unknown normally distributed

  unit_cost = triangle(n,5.5,6.5,7.5) # unit costs are unknown triangle distributed

  market_condition = floor(runif(n,0,3)) # market conditions are unknown uniformly distributed
    # market is hot
    volume = rep(100000,n)
      retail_price = rep(8,n)
    # market is slow
    volume[market_condition==0] = 50000
      retail_price[market_condition==0] = 11
    # market is ok
    volume[market_condition==1] = 75000
      retail_price[market_condition==1] = 10

  profit = volume*(retail_price-unit_cost) - fixed_cost
  return(profit)
}

Samp <- SimulateMarket(1e6)
mean(Samp)
plot(density(Samp))

